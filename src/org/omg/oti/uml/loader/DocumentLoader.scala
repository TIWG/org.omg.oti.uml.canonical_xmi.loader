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
    
    val xmiElements1: Seq[Elem] = XMIPattern.childElements(xmiRoot)
    
    for {
      (document, xmi2umlRoot, xmi2contents, tags) <- makeDocumentFromRootNode(url, xmiElements1)
      xmi2umlFull <- processXMIContents(document, ds)(xmi2umlRoot, xmi2contents)
      _ <- processXMITags(document, ds, xmi2umlFull, tags)
    } yield (document, ds.copy(serializableDocuments = ds.serializableDocuments + document))
    
   }
  
  @annotation.tailrec private def processXMIContents
  (d: SerializableDocument[Uml], 
   ds: DocumentSet[Uml])
  (xmi2uml1: Map[XMIPattern, UMLElement[Uml]], 
   xmi2contents1: Seq[(XMIPattern, Seq[Elem])])
  : Try[Map[XMIPattern, UMLElement[Uml]]] =
    if (xmi2contents1.isEmpty)
      Success(xmi2uml1)
    else
      processXMIElementAttributesAndNestedContent(d, ds, xmi2uml1)(xmi2contents1.head._1, xmi2contents1.head._2) match {
        case Failure(f) =>
          Failure(f)
        case Success((xmi2uml2, xmi2contents2)) => 
          processXMIContents(d, ds)(xmi2uml2, xmi2contents1.tail ++ xmi2contents2)
      }
    
  /**
   * The attributes appear before other content (references or nested children)
   */
  def processXMIElementAttributesAndNestedContent
  (d: SerializableDocument[Uml], 
   ds: DocumentSet[Uml],
   xmi2uml: Map[XMIPattern, UMLElement[Uml]])
  (xmiPattern: XMIPattern,
   attributesAndOther: Seq[Elem])
  : Try[(Map[XMIPattern, UMLElement[Uml]], Seq[(XMIPattern, Seq[Elem])])] = {
   
    @annotation.tailrec def processAttributes
    (content: Seq[Elem])
    : Try[Seq[Elem]] =
      if (content.isEmpty)
        Success(Seq())
      else 
        XMIPattern.asXMIAttribute(content.head) match {
        case Some(attributeValue) =>
          val attributeName = content.head.label
          // TODO: update the element at xmiPattern with attributeName, attributeValue
          processAttributes(content.tail)
        case None =>
          Success(content)
      }
             
    for {
      otherContent <- processAttributes(attributesAndOther)
    } yield ???
      
  }
    
  def processXMITags
  (d: SerializableDocument[Uml], 
   ds: DocumentSet[Uml],
   xmi2uml: Map[XMIPattern, UMLElement[Uml]], 
   tags: Seq[Elem])
  : Try[Unit] =
    ???
    
  def makeDocumentFromRootNode
  (url: URL, xmiElements1: Seq[Elem])
  : Try[(SerializableDocument[Uml], Map[XMIPattern, UMLElement[Uml]], Seq[(XMIPattern, Seq[Elem])], Seq[Elem])] =
    XMIPattern.matchXMIPattern(xmiElements1) match {
    case Some((xmiPattern, xmiElements2)) =>
      makeDocumentFromRootNode(url, xmiPattern, xmiElements2)
    case None =>
      Failure(new IllegalArgumentException("No Document Root Node found in the XML!"))
  }
  
  protected def missingURIAttribute
  : Try[(SerializableDocument[Uml], Map[XMIPattern, UMLElement[Uml]], Seq[(XMIPattern, Seq[Elem])], Seq[Elem])] =
    Failure(new IllegalArgumentException(s"Missing URI attribute for root element"))
    
  def makeDocumentFromRootNode
  (url: URL, pattern: XMIPattern, tags: Seq[Elem])
  : Try[(SerializableDocument[Uml], Map[XMIPattern, UMLElement[Uml]], Seq[(XMIPattern, Seq[Elem])], Seq[Elem])] =
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
                sd = SerializableDocument[Uml](new java.net.URI(uri), nsPrefix, uuidPrefix=nsPrefix, documentURL=url.toURI, scope=root)
                xmi2uml = Map[XMIPattern, UMLElement[Uml]](xmiPattern -> root)
                xmi2contents = Seq[(XMIPattern, Seq[Elem])]((xmiPattern -> contents))                
              } yield (sd, xmi2uml, xmi2contents, tags)
            case None =>
              Failure(new IllegalArgumentException(s"No factory found for xmiType=uml:$xmiType"))
          }
      })
      
    case _ =>
      Failure(new IllegalArgumentException("No Document Root Node found in the XML!"))
  }
}