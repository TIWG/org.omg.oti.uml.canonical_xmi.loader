/*
 *
 *  License Terms
 *
 *  Copyright (c) 2015, California Institute of Technology ("Caltech").
 *  U.S. Government sponsorship acknowledged.
 *
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are
 *  met:
 *
 *
 *   *   Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *   *   Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the
 *       distribution.
 *
 *   *   Neither the name of Caltech nor its operating division, the Jet
 *       Propulsion Laboratory, nor the names of its contributors may be
 *       used to endorse or promote products derived from this software
 *       without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 *  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 *  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 *  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 *  OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 *  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 *  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package org.omg.oti.uml.loader

import org.omg.oti.uml.canonicalXMI._
import org.omg.oti.uml.write.api._
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.read.operations._
import org.omg.oti.uml.xmi._
import scala.annotation
import scala.{Option,None,Some,StringContext,Unit}
import scala.collection.immutable._
import scala.collection.Seq
import scala.Predef.{Set => _, Map => _, _}
import scala.util.{Failure, Success, Try}
import scala.xml.{Document => XMLDocument, _}
import java.io.InputStream
import java.net.URI
import java.lang.IllegalArgumentException

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._

/**
 * The inverse of the XMI Document production rules
 *
 * @see OMG XMI 2.5.1, formal/15-06-07, Section 9
 *
 * @tparam Uml Type signature of a tool-specific adaptation of OMG UML 2.5
 */
trait DocumentLoader[Uml <: UML] {

  implicit val umlOps: UMLOps[Uml]
  implicit val documentOps: DocumentOps[Uml]

  import documentOps._

  /**
   * Load an OTI UML 2.5 Package from its OTI Canonical XMI Document serialization.
   *
   * @param url The Document URL of an OTI UML 2.5 Canonical XMI document to load.
   * @param ds The DocumentSet against which cross references from the loaded document will be resolved
   * @param umlF The tool-specific OTI UML Factory
   * @param umlU The tool-specific OTI UML Update
   * @param nodeT The Scala type information about Document graph nodes
   * @param edgeT The Scala type information about Document to Document graph edges
   * @return if successful, a pair of:
   *         - the SerializableDocument corresponding to the single root UML Package loaded
   *         - the new DocumentSet with the loaded SerializableDocument
   */
  def loadDocument
  (url: Uml#LoadURL, ds: DocumentSet[Uml])
  (implicit umlF: UMLFactory[Uml], umlU: UMLUpdate[Uml],
   nodeT: TypeTag[Document[Uml]],
   edgeT: TypeTag[DocumentEdge[Document[Uml]]])
  : Try[(SerializableDocument[Uml], DocumentSet[Uml])] = {

    // Cannonical XMI: B2.2: Always use a root xmi:XMI element

    val xmiRoot: Node = XML.load(openExternalDocumentStreamForImport(url))
    require("XMI" == xmiRoot.label)

    val namespaces: Map[String, String] =
      XMIPattern.collectNamespaces(xmiRoot)

    for {
      ns <- Seq("xmi", "xsi", "uml", "mofext")
    } require(
      namespaces.contains(ns),
      s"'$ns' namespace must be declared in root XMI node")

    // there must be at least 1 child, which is a kind of UML Package,
    // Profile or Model subsequent children are XML Element nodes 
    // representing mof tags or stereotype instances

    val xmiElements: Seq[Elem] = XMIPattern.childElements(xmiRoot)

    implicit val _ds = ds
    
    for {
      (document, xmi2umlRoot, xmi2contents, tags) <- makeDocumentFromRootNode(url, xmiElements)
      (xmi2umlFull, xmiReferences) <- processXMIContents(document, ds)(xmi2umlRoot, xmi2contents, Map())
      _ <- processXMIReferences(document, ds, xmi2umlFull, xmiReferences)
      _ <- processXMITags(document, ds, xmi2umlFull, tags)
      ds1 <- addDocument(ds, document)
    } yield (document, ds1)

  }

  /**
   * Maps a pattern of an XMI tree to the UML Element created
   */
  type XMI2UMLElementMap = Map[XMIElementDefinition, UMLElement[Uml]]

  /**
   *
   * @param d The Document being loaded (i.e., XMI Import in the sense of the OMG XMI specification)
   * @param ds The DocumentSet against which cross references from the loaded document will be resolved
   * @param xmi2uml1 Map of XMI tree patterns to UML Elements created
   * @param xmi2contents1 Nested XMI Element contents to be processed for a given XMI tree pattern
   * @param references Nested XMI cross-references to be processed for a given XMI tree pattern
   * @param umlF The tool-specific OTI UML Factory
   * @param umlU The tool-specific OTI UML Update
   * @return
   */
  @annotation.tailrec private def processXMIContents
  (d: SerializableDocument[Uml],
   ds: DocumentSet[Uml])
  (xmi2uml1: XMI2UMLElementMap,
   xmi2contents1: Seq[(XMIElementDefinition, Seq[Elem])],
   references: Map[XMIElementDefinition, Seq[Elem]])
  (implicit umlF: UMLFactory[Uml], umlU: UMLUpdate[Uml])
  : Try[(XMI2UMLElementMap, Map[XMIElementDefinition, Seq[Elem]])] =
    if (xmi2contents1.isEmpty)
      Success((xmi2uml1, references))
    else
      (processXMIElementAttributesAndNestedContent(d, ds, xmi2uml1) _).tupled(xmi2contents1.head) match {
        case Failure(f)                         =>
          Failure(f)
        case Success((xmi2uml2, xmiReferences)) =>
          processXMIContents(d, ds)(xmi2uml2, xmi2contents1.tail, references ++ xmiReferences)
      }

  /**
   * Process the nested XMI attribute elements for a given XMI tree pattern
   *
   * @param xmi2uml Map of XMI tree patterns to UML Elements created
   * @param xmiP An XMI tree pattern
   * @param content Nested XMI attribute elements or cross-references for the XMI tree element
   * @param other Nested XMI cross-references for the XMI tree element
   * @return The nested XMI cross-references from the input `content` and `other`
   */
  @annotation.tailrec final def processAttributes
  (xmi2uml: XMI2UMLElementMap,
   xmiP: XMIElementDefinition,
   content: Seq[Elem],
   other: Seq[Elem])
  : Try[Seq[Elem]] =
    if (content.isEmpty)
      Success(other)
    else
      XMIPattern.matchXMINestedText(content.head) match {
        case Some(attributeValue) =>
          // check intermediate computations that may fail and 
          // ensure the recursive call is in the last position
          updateElementAttribute(xmiP, xmi2uml.get(xmiP), content.head.label, attributeValue).get
          processAttributes(xmi2uml, xmiP, content.tail, other)
        case None                 =>
          processAttributes(xmi2uml, xmiP, content.tail, other :+ content.head)
      }

  /**
   * Process XMI nested content recursively, constructing nested UML Elements
   * whenever XMI nesting corresponds to composite association in the OMG UML metamodel
   *
   * @todo Are the XMI patterns exhaustive w.r.t. the OMG XMI specification (i.e., production rules)?
   *
   * @param xmi2uml Map of XMI tree patterns to UML Elements created
   * @param xmiPattern A pattern of an XMI tree corresponding to a UML Element of some kind
   * @param content Nested XMI Element nodes as children of the XMI tree element pattern
   * @param more Additional nested content to be processed tail-recursively
   * @param references Nested content in the form of XMI cross-references to be processed in a subsequent phase
   * @param umlF The tool-specific OTI UML Factory
   * @param umlU The tool-specific OTI UML Update
   * @return The XMI elements mapped to UML elements (composite nesting) and the XMI cross-references to be processed
   *         in a subsequent phase
   */
  @annotation.tailrec final def processNestedContent
  (xmi2uml: XMI2UMLElementMap,
   xmiPattern: XMIElementDefinition,
   content: Seq[Elem],
   more: Seq[(XMIElementDefinition, Seq[Elem])],
   references: Map[XMIElementDefinition, Seq[Elem]])
  (implicit umlF: UMLFactory[Uml], umlU: UMLUpdate[Uml])
  : Try[(XMI2UMLElementMap, Seq[(XMIElementDefinition, Seq[Elem])])] =
    if (content.isEmpty)
      if (more.isEmpty)
        Success((xmi2uml, references.toSeq))
      else
        processNestedContent(xmi2uml, more.head._1, more.head._2, more.tail, references)
    else
      XMIPattern.matchXMIPattern(content) match {
        case Some((xmiE@
          XMIElementDefinition(e, xmiID, xmiUUID, xmiType, nsPrefix, nestedContents), restContents)) =>
          umlF.reflectiveFactoryLookup.get(xmiType) match {
            case Some(factory) =>
              factory(umlF) match {
                case Success(umlE) =>
                  xmi2uml.get(xmiPattern) match {
                    case Some(umlParent) =>
                      val x2u = xmi2uml + (xmiE -> umlE)
                      val compositeMetaPropertyName = xmiE.element.label
                      val parent2childOK =
                        umlParent.compositeMetaProperties.find((mpf) =>
                          mpf.propertyName == compositeMetaPropertyName &&
                          mpf.domainType.runtimeClass.isInstance(umlParent) &&
                          mpf.rangeType.runtimeClass.isInstance(umlE)) match {

                          case Some(mpf) =>

                            show(s"* Nested Composite ($mpf) => $xmiE")
                            (umlU.AssociationMetaPropertyOption2LinksUpdate.find(_.links_query == mpf),
                              umlU.AssociationMetaPropertyIterable2LinksUpdate.find(_.links_query == mpf),
                              umlU.AssociationMetaPropertySequence2LinksUpdate.find(_.links_query == mpf),
                              umlU.AssociationMetaPropertySet2LinksUpdate.find(_.links_query == mpf)
                              ) match {

                              case (Some(cru), _, _, _) =>
                                cru.update1Link(umlParent, umlE)

                              case (_, Some(cru), _, _) =>
                                cru.update1Link(umlParent, umlE)

                              case (_, _, Some(cru), _) =>
                                cru.update1Link(umlParent, umlE)

                              case (_, _, _, Some(cru)) =>
                                cru.update1Link(umlParent, umlE)

                              case (None, None, None, None) =>
                                Failure(new IllegalArgumentException(
                                  s"No composite meta property update found for" +
                                  s" '$compositeMetaPropertyName' on $umlParent"))

                            }

                          case None =>
                            Failure(new IllegalArgumentException(
                              s"No composite meta property update found for " +
                              s"'$compositeMetaPropertyName' on $umlParent"))

                        }
                      parent2childOK match {
                        case Success(_) =>
                          processAttributes(x2u, xmiE, nestedContents, Seq()) match {
                            case Success(nestedOther) =>
                              processNestedContent(
                                x2u, xmiE, nestedOther,
                                more :+ ((xmiPattern, restContents)), references)
                            case Failure(f)           =>
                              Failure(f)
                          }
                        case Failure(f) =>
                          Failure(f)
                      }
                    case None            =>
                      Failure(new IllegalArgumentException(s"There should be an element for $xmiPattern"))
                  }
                case Failure(t)    =>
                  Failure(t)
              }
            case None          =>
              Failure(new IllegalArgumentException(s"No factory found for xmiType=uml:$xmiType"))
          }
        case Some((xmiOther, otherElements))                                                         =>
          // @todo
          ???
        case None                                                                                    =>
          val xmiReferences = references.getOrElse(xmiPattern, Seq()) :+ content.head
          processNestedContent(
            xmi2uml, xmiPattern, content.tail,
            more, references.updated(xmiPattern, xmiReferences))
      }

  /**
   *
   * Note: The attributes appear before other content (references or nested children)
   *
   * @param d The Document being loaded (i.e., XMI Import in the sense of the OMG XMI specification)
   * @param ds The DocumentSet against which cross references from the loaded document will be resolved
   * @param xmi2uml Map of XMI tree patterns to UML Elements created
   * @param xmiPattern A pattern of an XMI tree corresponding to a UML Element of some kind
   * @param attributesAndOther XMI Element nodes corresponding to UML attributes or non-composite cross-references
   * @param umlF The tool-specific OTI UML Factory
   * @param umlU The tool-specific OTI UML Update
   * @return The XMI elements mapped to UML elements (composite nesting) and the XMI cross-references to be processed
   *         in a subsequent phase
   */
  def processXMIElementAttributesAndNestedContent
  (d: SerializableDocument[Uml],
   ds: DocumentSet[Uml],
   xmi2uml: XMI2UMLElementMap)
  (xmiPattern: XMIElementDefinition,
   attributesAndOther: Seq[Elem])
  (implicit umlF: UMLFactory[Uml], umlU: UMLUpdate[Uml])
  : Try[(XMI2UMLElementMap, Seq[(XMIElementDefinition, Seq[Elem])])] =
    for {
      otherContent <- processAttributes(xmi2uml, xmiPattern, attributesAndOther, Seq())
      (xmi2umlFull, xmiReferences) <- processNestedContent(xmi2uml, xmiPattern, otherContent, Seq(), Map())
    } yield (xmi2umlFull, xmiReferences)

  /**
   * Processing of XMI Element cross-references as updates of UML non-composite (meta) Properties
   *
   * @param d The Document being loaded (i.e., XMI Import in the sense of the OMG XMI specification)
   * @param ds The DocumentSet against which cross references from the loaded document will be resolved
   * @param xmi2uml Map of XMI tree patterns to UML Elements created
   * @param xmiReferences XMI Elements corresponding to the serialization
   *                      of non-composite UML Properties in the OMG UML metamodel
   * @return Success or Failure
   */
  @annotation.tailrec final def processXMIReferences
  (d: SerializableDocument[Uml],
   ds: DocumentSet[Uml],
   xmi2uml: XMI2UMLElementMap,
   xmiReferences: Map[XMIElementDefinition, Seq[Elem]])
  : Try[Unit] =
    if (xmiReferences.isEmpty)
      Success(())
    else
      xmi2uml.get(xmiReferences.head._1) match {
        case Some(umlE) =>
          updateElementReferences(d, ds, xmi2uml, xmiReferences.head._1, umlE, xmiReferences.head._2) match {
            case Success(_) =>
              processXMIReferences(d, ds, xmi2uml, xmiReferences.tail)
            case Failure(t) =>
              Failure(t)
          }
        case None       =>
          Failure(new IllegalArgumentException(s"Missing entry for ${xmiReferences.head._1}"))
      }

  /**
   * Processing of XMI Element cross-references as updates of UML non-composite (meta) Properties of a single UML Element
   *
   * @todo Lookup the OTI update function to update the corresponding non-composite UML Element property reference
   *
   * @param d The Document being loaded (i.e., XMI Import in the sense of the OMG XMI specification)
   * @param ds The DocumentSet against which cross references from the loaded document will be resolved
   * @param xmi2uml Map of XMI tree patterns to UML Elements created
   * @param xmiElement An XMI tree pattern
   * @param umlElement The UML Element created from the XMI tree pattern
   * @param xmiReferences The XMI Elements corresponding to the serialization of non-composite UML Properties
   *                      of the UML Element
   * @return Success or Failure
   */
  def updateElementReferences
  (d: SerializableDocument[Uml],
   ds: DocumentSet[Uml],
   xmi2uml: XMI2UMLElementMap,
   xmiElement: XMIElementDefinition,
   umlElement: UMLElement[Uml],
   xmiReferences: Seq[Elem])
  : Try[Unit] = {
    // @todo
    //show(s"* Update ${xmiReferences.size} references for $xmiElement (uml: $umlElement)")
    //xmiReferences.foreach { ref => show(s"* ref: $ref") }
    Success(())
  }

  /**
   * Processing of XMI Elements corresponding to the serialization of MOF tags or stereotypes applied to UML Elements
   *
   * @todo Lookup the OTI UML Update function corresponding to either a MOF tag or a stereotype tag property
   *
   * @param d The Document being loaded (i.e., XMI Import in the sense of the OMG XMI specification)
   * @param ds The DocumentSet against which cross references from the loaded document will be resolved
   * @param xmi2uml Map of XMI tree patterns to UML Elements created
   * @param tags XMI Element nodes representing the serialization of stereotypes applied to UML Elements
   * @return Success or Failure
   */
  def processXMITags
  (d: SerializableDocument[Uml],
   ds: DocumentSet[Uml],
   xmi2uml: XMI2UMLElementMap,
   tags: Seq[Elem])
  : Try[Unit] = {
    // @todo
    //show(s"* Update ${tags.size} tags")
    //tags.foreach { t => show(s"* tag: $t") }
    Success(())
  }

  /**
   * Process the serialization of a UML Property attribute value as an update of a corresponding UML Element
   *
   * @todo Lookup the OTI UML Update function corresponding to the UML Property attribute name
   *       and invoke it to update its value
   *
   * @param xmiE The XMI tree element pattern
   * @param e The corresponding UML Element
   * @param attributeName the XMI serialization of a UML Property attribute name
   * @param attributeValue the XMI serialization of a UML Property attribute value
   * @return Success or Failure
   */
  def updateElementAttribute
  (xmiE: XMIElementDefinition,
   e: Option[UMLElement[Uml]],
   attributeName: String,
   attributeValue: String)
  : Try[Unit] =
    e
    .fold[Try[Unit]] {
      Failure(new IllegalArgumentException(
          s"There should be a UML element corresponding to" +
          s"to $xmiE to update the attribute $attributeName with value $attributeValue"))
    }{ umlElement =>
        // @todo
        show(s"TODO<: update ${xmiE.xmiType}::$attributeName = $attributeValue")
        Success(())
    }

  /**
   * Import an XMI Document (from its serialization) as an OTI SerializableDocument
   *
   * @param url The Document URL of an OTI UML 2.5 Canonical XMI document to load.
   * @param xmiElements The XMI Element nodes corresponding to the serialization of the document
   * @param ds The DocumentSet against which cross references from the loaded document will be resolved
   * @param umlF The tool-specific OTI UML Factory
   * @param umlU The tool-specific OTI UML Update
   * @return A tuple of:
   *         - the serialized document constructed from the `xmiElements`
   *         - the map of XMI Element nodes to UML Elements
   *         - The XMI Element cross-references for each XMI tree pattern
   *         - the XMI Element serialization of MOF tags and of stereotypes applied to UML Elements
   */
  def makeDocumentFromRootNode
  (url: Uml#LoadURL, xmiElements: Seq[Elem])
  (implicit ds: DocumentSet[Uml], umlF: UMLFactory[Uml], umlU: UMLUpdate[Uml])
  : Try[(SerializableDocument[Uml], XMI2UMLElementMap, Seq[(XMIElementDefinition, Seq[Elem])], Seq[Elem])] =
    XMIPattern.matchXMIPattern(xmiElements)
    .fold[Try[(SerializableDocument[Uml], XMI2UMLElementMap, Seq[(XMIElementDefinition, Seq[Elem])], Seq[Elem])]]{
      Failure(new IllegalArgumentException("No Document Root Node found in the XML!"))
    } { case ((xmiPattern:XMIPattern, xmiTags: Seq[Elem])) =>
        xmiPattern match {
          case xmiElement: XMIElementDefinition =>
            makeDocumentFromRootNode(url, xmiElement, xmiTags)
          case _                                =>
            Failure(new IllegalArgumentException(s"Not supported: $xmiPattern"))
        }
    }

  /**
   * Import an XMI Document (from XMI tree patterns and XMI Element serializations of MOF tags and stereotypes aplied)
   * as an OTI SerializableDocument
   *
   * @param url The Document URL of an OTI UML 2.5 Canonical XMI document to load.
   * @param pattern A pattern of an XMI tree corresponding to a UML Element of some kind
   * @param tags XMI Element nodes representing the serialization of stereotypes applied to UML Elements
   * @param ds The DocumentSet against which cross references from the loaded document will be resolved
   * @param umlF The tool-specific OTI UML Factory
   * @param umlU The tool-specific OTI UML Update
   * @return A tuple of:
   *         - the serialized document constructed from the `xmiElements`
   *         - the map of XMI Element nodes to UML Elements
   *         - The XMI Element cross-references for each XMI tree pattern
   *         - the XMI Element serialization of MOF tags and of stereotypes applied to UML Elements
   */
  def makeDocumentFromRootNode
  (url: Uml#LoadURL, pattern: XMIElementDefinition, tags: Seq[Elem])
  (implicit ds: DocumentSet[Uml], umlF: UMLFactory[Uml], umlU: UMLUpdate[Uml])
  : Try[(SerializableDocument[Uml], XMI2UMLElementMap, Seq[(XMIElementDefinition, Seq[Elem])], Seq[Elem])] =
    pattern match {
      case xmiPattern@
        XMIElementDefinition(e, xmiID, xmiUUID, xmiType, Some(MOFExtTagNSPrefix(_, nsPrefix, _)), contents) =>
        contents
        .foldLeft(Option.empty[String])({
          case (s: Some[_], _) =>
            s
          case (None, e)       =>
            XMIPattern.lookupElementText("URI")(e)
        })
        .fold(missingURIAttribute)((uri: String) => {
          umlF.reflectivePackageFactoryLookup.get(xmiType)
          .fold[Try[(SerializableDocument[Uml], XMI2UMLElementMap, Seq[(XMIElementDefinition, Seq[Elem])], Seq[Elem])]]{
            Failure(new IllegalArgumentException(s"No Package-based factory found for xmiType=uml:$xmiType"))
          } { factory =>
              for {
                root <- factory(umlF)
                sd <- createSerializableDocumentFromImportedRootPackage(
                  uri = new java.net.URI(uri),
                  nsPrefix = nsPrefix,
                  uuidPrefix = nsPrefix,
                  documentURL = url,
                  scope = root)
                xmi2uml = Map[XMIElementDefinition, UMLElement[Uml]](xmiPattern -> root)
                xmi2contents = Seq[(XMIElementDefinition, Seq[Elem] )](xmiPattern -> contents)
              } yield (sd, xmi2uml, xmi2contents, tags)
          }
        })

      case _ =>
        Failure(new IllegalArgumentException("No Document Root Node found in the XML!"))
    }

  /**
   * Document root XML Element node (the serialization of a kind of UML Package) should have a URI attribute.
   *
   * @return A Scala Failure error for a missing URI attribute for the root UML Element of a Document root XML Element
   *         (it should be really a kind of UML Package)
   */
  protected def missingURIAttribute
  : Try[(SerializableDocument[Uml], XMI2UMLElementMap, Seq[(XMIElementDefinition, Seq[Elem])], Seq[Elem])] =
    Failure(new IllegalArgumentException(s"Missing URI attribute for root element"))

}