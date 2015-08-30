package org.omg.oti.uml.loader

import org.omg.oti.uml.canonicalXMI._
import org.omg.oti.uml.write.api._
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.read.operations._
import scala.util.{Failure, Success, Try}
import scala.xml.{ Document => XMLDocument, _}
import java.net.URL

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._

import org.eclipse.ease.modules.EnvironmentModule

/**
 * In-progress: search for 'TODO'
 */
trait DocumentLoader[Uml <: UML] {
  val catalog: CatalogURIMapper
  val umlF: UMLFactory[Uml]
  val umlU: UMLUpdate[Uml]
  implicit val umlOps: UMLOps[Uml]
  
  val env: EnvironmentModule = new EnvironmentModule()
    
  def show(message: String): Unit = {
    if (null != env.getScriptEngine)
      env.print(message)
    else
      println(message)
  }
  
  /**
   * Load an OTI UML 2.5 Package from its OTI Canonical XMI Document serialization.
   *
   * @param url The Document URL of an OTI UML 2.5 Canonical XMI document to load.
   * @param ds The DocumentSet against which cross references from the loaded document will be resolved
   * @return if successful, a pair of:
   *         - the SerializableDocument corresponding to the single root UML Package loaded
   *         - the new DocumentSet with the loaded SerializableDocument
   */
	def loadDocument
  (url: URL, ds: DocumentSet[Uml])
  (implicit nodeT: TypeTag[Document[Uml]], edgeT: TypeTag[DocumentEdge[Document[Uml]]])
  : Try[(SerializableDocument[Uml], DocumentSet[Uml])] = {
     
    show(s"Loading model from: $url")
    
    // Cannonical XMI: B2.2: Always use a root xmi:XMI element
    
    val xmiRoot: Node = XML.load(url.openStream())
    require("XMI" == xmiRoot.label)
    
    val namespaces: Map[String, String] = XMIPattern.collectNamespaces(xmiRoot)
    
    for {
      ns <- Seq("xmi", "xsi", "uml", "mofext")
    } require(namespaces.contains(ns), s"'$ns' namespace must be declared in root XMI node")
    
    // there must be at least 1 child, which is a kind of UML Package, Profile or Model
    // subsequent children are XML Element nodes representing mof tags or stereotype instances
    
    val xmiElements: Seq[Elem] = XMIPattern.childElements(xmiRoot)
    
    for {
      (document, xmi2umlRoot, xmi2contents, tags) <- makeDocumentFromRootNode(url, xmiElements)
      (xmi2umlFull, xmiReferences) <- processXMIContents(document, ds)(xmi2umlRoot, xmi2contents, Map())
      _ <- processXMIReferences(document, ds, xmi2umlFull, xmiReferences)
      _ <- processXMITags(document, ds, xmi2umlFull, tags)
    } yield (document, ds.copy(serializableDocuments = ds.serializableDocuments + document))
    
   }
  
  @annotation.tailrec private def processXMIContents
  (d: SerializableDocument[Uml], 
   ds: DocumentSet[Uml])
  (xmi2uml1: Map[XMIElementDefinition, UMLElement[Uml]], 
   xmi2contents1: Seq[(XMIElementDefinition, Seq[Elem])],
   references: Map[XMIElementDefinition, Seq[Elem]])
  : Try[(Map[XMIElementDefinition, UMLElement[Uml]], Map[XMIElementDefinition, Seq[Elem]])] =
    if (xmi2contents1.isEmpty)
      Success((xmi2uml1, references))
    else
      processXMIElementAttributesAndNestedContent(d, ds, xmi2uml1)(xmi2contents1.head._1, xmi2contents1.head._2) match {
        case Failure(f) =>
          Failure(f)
        case Success((xmi2uml2, xmiReferences)) => 
          processXMIContents(d, ds)(xmi2uml2, xmi2contents1.tail, references ++ xmiReferences)
      }
    
  /**
   * The attributes appear before other content (references or nested children)
   */
  def processXMIElementAttributesAndNestedContent
  (d: SerializableDocument[Uml], 
   ds: DocumentSet[Uml],
   xmi2uml: Map[XMIElementDefinition, UMLElement[Uml]])
  (xmiPattern: XMIElementDefinition,
   attributesAndOther: Seq[Elem])
  : Try[(Map[XMIElementDefinition, UMLElement[Uml]], Seq[(XMIElementDefinition, Seq[Elem])])] = {
   
    @annotation.tailrec def processAttributes
    (xmi2uml: Map[XMIElementDefinition, UMLElement[Uml]],
     xmiP: XMIElementDefinition,
     content: Seq[Elem],
     other: Seq[Elem])
    : Try[Seq[Elem]] =
      if (content.isEmpty)
        Success(other)
      else 
        XMIPattern.matchXMINestedText(content.head) match {
        case Some(attributeValue) =>                    
          // check intermediate computations that may fail and ensure the recursive call is in the last position
          updateElementAttribute(xmiP, xmi2uml.get(xmiP), content.head.label, attributeValue).get
          processAttributes(xmi2uml, xmiP, content.tail, other)
        case None =>
          processAttributes(xmi2uml, xmiP, content.tail, other :+ content.head)
      }
             
    @annotation.tailrec def processNestedContent
    (xmi2uml: Map[XMIElementDefinition, UMLElement[Uml]], 
     xmiPattern: XMIElementDefinition,
     content: Seq[Elem], 
     more: Seq[(XMIElementDefinition, Seq[Elem])],
     references: Map[XMIElementDefinition, Seq[Elem]])
    : Try[(Map[XMIElementDefinition, UMLElement[Uml]], Seq[(XMIElementDefinition, Seq[Elem])])] =
      if (content.isEmpty)
        if (more.isEmpty)
          Success((xmi2uml, references.toSeq))
         else
           processNestedContent(xmi2uml, more.head._1, more.head._2, more.tail, references)   
      else 
        XMIPattern.matchXMIPattern(content) match {
        case Some((xmiE @ XMIElementDefinition(e, xmiID, xmiUUID, xmiType, nsPrefix, nestedContents), restContents)) =>
          umlF.reflectiveFactoryLookup.get(xmiType) match {
            case Some(factory) =>
              factory(umlF) match {
                case Success(umlE) =>
                  val x2u = xmi2uml + (xmiE -> umlE)
                  show(s"* Factory => $xmiE")
                  processAttributes(x2u, xmiE, nestedContents, Seq()) match {
                    case Success(nestedOther) =>
                      processNestedContent(x2u, xmiE, nestedOther, more :+ (xmiPattern, restContents), references)
                    case Failure(f) =>
                      Failure(f)
                  }
                case Failure(t) => 
                  Failure(t)
              } 
            case None =>
              Failure(new IllegalArgumentException(s"No factory found for xmiType=uml:$xmiType"))
          }
        case Some((xmiOther, otherElements)) =>
          // TODO
          ???
        case None =>
          val xmiReferences = references.getOrElse(xmiPattern, Seq()) :+ content.head
          processNestedContent(xmi2uml, xmiPattern, content.tail, more, references.updated(xmiPattern, xmiReferences))
      }
    
    
    for {
      otherContent <- processAttributes(xmi2uml, xmiPattern, attributesAndOther, Seq())
      (xmi2umlFull, xmiReferences) <- processNestedContent(xmi2uml, xmiPattern, otherContent, Seq(), Map())
    } yield (xmi2umlFull, xmiReferences)
      
  }
    
  @annotation.tailrec private final def processXMIReferences
  (d: SerializableDocument[Uml], 
   ds: DocumentSet[Uml],
   xmi2uml: Map[XMIElementDefinition, UMLElement[Uml]],
   xmiReferences: Map[XMIElementDefinition, Seq[Elem]])
  : Try[Unit] = 
    if (xmiReferences.isEmpty)
      Success(Unit)
    else
      xmi2uml.get(xmiReferences.head._1) match {
      case Some(umlE) =>      
        updateElementReferences(d, ds, xmi2uml, xmiReferences.head._1, umlE, xmiReferences.head._2) match {
          case Success(_) =>
            processXMIReferences(d, ds, xmi2uml, xmiReferences.tail)
          case Failure(t) =>
            Failure(t)
        }
      case None =>
        Failure(new IllegalArgumentException(s"Missing entry for ${xmiReferences.head._1}"))
    }
  
  def updateElementReferences
  (d: SerializableDocument[Uml], 
   ds: DocumentSet[Uml],
   xmi2uml: Map[XMIElementDefinition, UMLElement[Uml]],
   xmiElement: XMIElementDefinition, 
   umlElement: UMLElement[Uml],
   xmiReferences: Seq[Elem])
  : Try[Unit] = {
    // TODO
    show(s"* Update ${xmiReferences.size} references for $xmiElement (uml: $umlElement)")
    xmiReferences.foreach { ref => show(s"* ref: $ref") }
    Success(Unit)
  }
    
  def processXMITags
  (d: SerializableDocument[Uml], 
   ds: DocumentSet[Uml],
   xmi2uml: Map[XMIElementDefinition, UMLElement[Uml]], 
   tags: Seq[Elem])
  : Try[Unit] = {
    // TODO
    show(s"* Update ${tags.size} tags")
    tags.foreach { t => show(s"* tag: $t") }
    Success(Unit)
  }
  
  def updateElementAttribute
  (xmiE: XMIElementDefinition, 
   e: Option[UMLElement[Uml]], 
   attributeName: String,  
   attributeValue: String)
  : Try[Unit] =
    e match {
    case Some(umlElement) =>
      // TODO
      show(s"TODO: update ${xmiE.xmiType}::$attributeName = $attributeValue")
      Success(Unit)
    case None =>
      Failure(new IllegalArgumentException(
          s"There should be a UML element corresponding to" +
          s"to $xmiE to update the attribute $attributeName with value $attributeValue"))
  }
  
  def makeDocumentFromRootNode
  (url: URL, xmiElements: Seq[Elem])
  : Try[(SerializableDocument[Uml], Map[XMIElementDefinition, UMLElement[Uml]], Seq[(XMIElementDefinition, Seq[Elem])], Seq[Elem])] =
    XMIPattern.matchXMIPattern(xmiElements) match {
    case Some((xmiPattern, xmiTags)) =>
      xmiPattern match {
        case xmiElement: XMIElementDefinition =>
          makeDocumentFromRootNode(url, xmiElement, xmiTags)
        case _ =>
          Failure(new IllegalArgumentException(s"Not supported: $xmiPattern"))
      }
    case None =>
      Failure(new IllegalArgumentException("No Document Root Node found in the XML!"))
  }
  
  def makeDocumentFromRootNode
  (url: URL, pattern: XMIElementDefinition, tags: Seq[Elem])
  : Try[(SerializableDocument[Uml], Map[XMIElementDefinition, UMLElement[Uml]], Seq[(XMIElementDefinition, Seq[Elem])], Seq[Elem])] =
    pattern match {
    case xmiPattern @ XMIElementDefinition(e, xmiID, xmiUUID, xmiType, Some(MOFExtTagNSPrefix(_, nsPrefix, _)), contents) =>
      contents
      .foldLeft(Option.empty[String])({
        case (s: Some[_], _) =>
          s
        case (None, e) =>
          XMIPattern.lookupElementText("URI")(e)
      })
      .fold(missingURIAttribute)( (uri: String) => {
          umlF.reflectiveFactoryLookup.get(xmiType) match {
            case Some(factory) =>
              for {
                root <- factory(umlF)
                _ = show(s"* Factory => $xmiPattern")
                sd = SerializableDocument[Uml](new java.net.URI(uri), nsPrefix, uuidPrefix=nsPrefix, documentURL=url.toURI, scope=root)
                xmi2uml = Map[XMIElementDefinition, UMLElement[Uml]](xmiPattern -> root)
                xmi2contents = Seq[(XMIElementDefinition, Seq[Elem])]((xmiPattern -> contents))                
              } yield (sd, xmi2uml, xmi2contents, tags)
            case None =>
              Failure(new IllegalArgumentException(s"No factory found for xmiType=uml:$xmiType"))
          }
      })
      
    case _ =>
      Failure(new IllegalArgumentException("No Document Root Node found in the XML!"))
  }
  
  protected def missingURIAttribute
  : Try[(SerializableDocument[Uml], Map[XMIElementDefinition, UMLElement[Uml]], Seq[(XMIElementDefinition, Seq[Elem])], Seq[Elem])] =
    Failure(new IllegalArgumentException(s"Missing URI attribute for root element"))
    
}