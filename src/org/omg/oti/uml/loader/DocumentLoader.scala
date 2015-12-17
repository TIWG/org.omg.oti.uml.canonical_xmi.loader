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

import org.omg.oti.uml.OTIPrimitiveTypes._
import org.omg.oti.uml.UMLError
import org.omg.oti.uml.canonicalXMI._
import org.omg.oti.uml.characteristics._
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.read.operations._
import org.omg.oti.uml.write.api._
import org.omg.oti.uml.xmi._
import org.omg.oti.uml.xmi.DocumentKind._

import scala.annotation
import scala.{Option,None,Some,StringContext,Unit}
import scala.collection.immutable._
import scala.collection.Seq
import scala.Predef.{Set => _, Map => _, _}
import scala.xml.{Document => XMLDocument, _}
import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._
import scalaz._, Scalaz._

class DocumentLoaderException[Uml <: UML]
( dLoader: DocumentLoader[Uml],
  message: String,
  cause: UMLError.OptionThrowableNel = UMLError.emptyThrowableNel)
  extends UMLError.UException(message, cause) {

  /**
   * This type member is intended to facilitate pattern matching
   * using a wildcard for the type parameter, i.e., DocumentLoaderException[_]
   * The type information can then be checked using the UmlType member.
   */
  type UmlType = Uml
}

object DocumentLoader {
  
  def show(message: String): Unit =
    System.out.println(message)
    
}
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
    * LoadDocumentResult is the type of the result of loadDocument()
    * This result is a value that can be of 3 possible cases of Scalaz `\&/` that can be matched as follows:
    * - `\&/.This(nels: NonEmptyList[java.lang.Throwable])`:
    *   Failure result: `nels` is a non-empty list of exceptions
    * - `\&/.That((lmd: LoadingMutableDocument[Uml], ds: DocumentSet[Uml]))`:
    *   Successful result without errors: a tuple `(lmd, ds)`
    * - `\&/.Both(`
    *      `nels: NonEmptyList[java.lang.Throwable],`
    *      `(lmd: LoadingMutableDocument[Uml], ds: DocumentSet[Uml]))`
    *   A combination of the two cases above.
    */
  type LoadDocumentResult =
  NonEmptyList[java.lang.Throwable] \&/ (LoadingMutableDocument[Uml], DocumentSet[Uml])
    
  /**
   * Load an OTI UML 2.5 Package from its OTI Canonical XMI Document serialization.
   *
   * @param url The Document URL of an OTI UML 2.5 Canonical XMI document to load.
   * @param ds The DocumentSet against which cross references from the loaded document will be resolved
   * @param umlF The tool-specific OTI UML Factory
   * @param umlU The tool-specific OTI UML Update
   * @param nodeT The Scala type information about Document graph nodes
   * @param edgeT The Scala type information about Document to Document graph edges
   * @return A scalaz `\&/` consisting of:
   *         - optionally, a non-empty list of errors (This or Both)
   *         - optionally, a tuple (That or Both):
   *            - the SerializableDocument corresponding to the single root UML Package loaded
   *            - the new DocumentSet with the loaded SerializableDocument
   */
  def loadDocument
  (url: Uml#LoadURL, 
   ds: DocumentSet[Uml])
  (implicit umlF: UMLFactory[Uml], umlU: UMLUpdate[Uml], idg: IDGenerator[Uml],
   nodeT: TypeTag[Document[Uml]],
   edgeT: TypeTag[DocumentEdge[Document[Uml]]])
  : LoadDocumentResult = {

    openExternalDocumentStreamForImport(url)
    .toThese
    .flatMap { is =>

        import scala.util.control.Exception._

        nonFatalCatch
        .either(XML.load(is))
        .fold[LoadDocumentResult](

          (cause: java.lang.Throwable) =>
            \&/.This(NonEmptyList(documentLoaderException(
              this,
              s"loadDocument($url) failed: ${cause.getMessage}",
              cause))),

          (xmiRoot: scala.xml.Node) =>
            // Cannonical XMI: B2.2: Always use a root xmi:XMI element
            if ("XMI" != xmiRoot.label)
              \&/.This(NonEmptyList(documentLoaderException(
                this,
                s"loadDocument($url) failed: Root node must be XMI")))
            else {
              val namespaces: Map[String, String] = XMIPattern.collectNamespaces(xmiRoot)

              val ns0: NonEmptyList[java.lang.Throwable] \&/ Unit =
                \&/.That(())
              val nsN: NonEmptyList[java.lang.Throwable] \&/ Unit =
                (ns0 /: Seq("xmi", "xsi", "uml", "mofext")) { (nsi, ns) =>
                    if (!namespaces.contains(ns))
                      nsi append 
                      \&/.This(NonEmptyList(documentLoaderException(
                        this,
                        s"loadDocument($url) failed: $ns' namespace must be declared in root XMI node")))
                    else
                      nsi
                }

              val result =
                nsN
                .flatMap[NonEmptyList[java.lang.Throwable], (LoadingMutableDocument[Uml], DocumentSet[Uml])] {
                _ =>

                // there must be at least 1 child, which is a kind of UML Package,
                // Profile or Model subsequent children are XML Element nodes
                // representing mof tags or stereotype instances

                val xmiElements: Seq[Elem] = XMIPattern.childElements(xmiRoot)

                implicit val _ds = ds

                makeDocumentFromRootNode(url, xmiElements)
                .flatMap[NonEmptyList[java.lang.Throwable], (LoadingMutableDocument[Uml], DocumentSet[Uml])] {

                  case (document: Document[Uml],
                        ds2: DocumentSet[Uml],
                        xmi2umlRoot: XMI2UMLElementMap,
                        xmi2contents: Seq[(XMIElementDefinition, Seq[scala.xml.Elem])],
                        tags: Seq[scala.xml.Elem]) =>

                    processXMIContents(document, ds2)(xmi2umlRoot, xmi2contents, Map())
                    .flatMap {

                        case (xmi2umlFull: XMI2UMLElementMap, 
                              xmiReferences: Map[XMIElementDefinition, Seq[Elem]]) =>
                          (
                            processXMIReferences(document, ds2, xmi2umlFull, xmiReferences) append
                            processXMITags(document, ds2, xmi2umlFull, tags)
                          )
                          .map { _ =>
                            (document, ds2)
                          }
                    }
                }
              }
              result
            }
        )
      }
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
  def processXMIContents
  (d: Document[Uml],
   ds: DocumentSet[Uml])
  (xmi2uml1: XMI2UMLElementMap,
   xmi2contents1: Seq[(XMIElementDefinition, Seq[Elem])],
   references: Map[XMIElementDefinition, Seq[Elem]])
  (implicit umlF: UMLFactory[Uml], umlU: UMLUpdate[Uml], idg: IDGenerator[Uml])
  : NonEmptyList[java.lang.Throwable] \&/ (XMI2UMLElementMap, Map[XMIElementDefinition, Seq[Elem]]) = {
     
     case class MapContentReferences
     (u: XMI2UMLElementMap, 
      c: Seq[(XMIElementDefinition, Seq[Elem])], 
      r: Map[XMIElementDefinition, Seq[Elem]])
        
    type Result = (XMI2UMLElementMap, Map[XMIElementDefinition, Seq[Elem]]) 
    
    def step(a: MapContentReferences)
    : NonEmptyList[java.lang.Throwable] \&/ (MapContentReferences \/ Result) =
      if (a.c.isEmpty)
        \&/.That(\/-((xmi2uml1, references)))
      else
        (processXMIElementAttributesAndNestedContent(d, ds, xmi2uml1) _)
        .tupled(xmi2contents1.head).map {
          case (xmi2uml2, xmiReferences) =>
            -\/(MapContentReferences(u=xmi2uml2, c=a.c.tail, r=a.r ++ xmiReferences))
      }
     
     val a0 = MapContentReferences(u=xmi2uml1, c=xmi2contents1, r=references)
     val aN = \&/.tailrecM[NonEmptyList[java.lang.Throwable], MapContentReferences, Result](step)(a0)
     aN
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
  def processAttributes
  (xmi2uml: XMI2UMLElementMap,
   xmiP: XMIElementDefinition,
   content: Seq[Elem],
   other: Seq[Elem])
  (implicit umlU: UMLUpdate[Uml], idg: IDGenerator[Uml])
  : NonEmptyList[java.lang.Throwable] \&/ Seq[Elem] = {
     
     case class ContentAndOther(c: Seq[Elem], o: Seq[Elem])
     
     def step(a: ContentAndOther)
     : NonEmptyList[java.lang.Throwable] \&/ (ContentAndOther \/ Seq[Elem]) =
       if (a.c.isEmpty)
         \&/.That(\/-(a.o))
       else
         XMIPattern.matchXMINestedText(a.c.head) match {
           case Some(attributeValue) =>
             // check intermediate computations that may fail and 
             // ensure the recursive call is in the last position
             updateElementAttribute(xmiP, xmi2uml.get(xmiP), a.c.head.label, attributeValue)
             .map { _ => 
               -\/(ContentAndOther(a.c.tail, a.o))
             }
           case None =>
             \&/.That(-\/(ContentAndOther(a.c.tail, a.o :+ a.c.head)))
         }
     
     val a0 = ContentAndOther(c=content, o=other)
     val aN = \&/.tailrecM[NonEmptyList[java.lang.Throwable], ContentAndOther, Seq[Elem]](step)(a0)
     aN
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
   * @param references Nested content in the form of XMI cross-references 
   *                   to be processed in a subsequent phase
   * @param umlF The tool-specific OTI UML Factory
   * @param umlU The tool-specific OTI UML Update
   * @return The XMI elements mapped to UML elements (composite nesting) and 
   *         the XMI cross-references to be processed in a subsequent phase
   */
  def processNestedContent
  (xmi2uml: XMI2UMLElementMap,
   xmiPattern: XMIElementDefinition,
   content: Seq[Elem],
   more: Seq[(XMIElementDefinition, Seq[Elem])],
   references: Map[XMIElementDefinition, Seq[Elem]])
  (implicit umlF: UMLFactory[Uml], umlU: UMLUpdate[Uml], idg: IDGenerator[Uml])
  : NonEmptyList[java.lang.Throwable] \&/ (XMI2UMLElementMap, Seq[(XMIElementDefinition, Seq[Elem])]) = {
    
    case class Info
    (u: XMI2UMLElementMap,
     p: XMIElementDefinition,
     c: Seq[Elem],
     m: Seq[(XMIElementDefinition, Seq[Elem])],
     r: Map[XMIElementDefinition, Seq[Elem]])
   
   type Result = (XMI2UMLElementMap, Seq[(XMIElementDefinition, Seq[Elem])])
   
   def step(a: Info)
   : NonEmptyList[java.lang.Throwable] \&/ (Info \/ Result) =
     if (a.c.isEmpty)
       if (a.m.isEmpty)
         \&/.That(\/-((a.u, a.r.toSeq)))
       else
         \&/.That(-\/(a.copy(p=a.m.head._1, c=a.m.head._2, m=a.m.tail)))
     else
       XMIPattern.matchXMIPattern(a.c) match {
       case None =>
        val xmiReferences = a.r.getOrElse(a.p, Seq()) :+ a.c.head
        \&/.That(-\/(a.copy(c=a.c.tail, r=a.r.updated(a.p, xmiReferences))))
      case Some((xmiE@XMIElementDefinition(e, xmiID, xmiUUID, xmiType, nsPrefix, nestedContents), 
                 restContents)) =>
          umlF
          .reflectiveFactoryLookup
          .get(xmiType) match {
            case None =>
              \&/.This(
                  NonEmptyList(
                      documentLoaderException(
                          this,
                          s"loadDocument: error in processNestedContent: "+
                          s"No factory found for xmiType=uml:$xmiType")))
            case Some(factory) =>
              factory(umlF) match {
                case -\/(nels) =>
                  \&/.This(nels)
                case \/-(umlE) =>
                  a.u.get(a.p) match {
                    case None =>
                      \&/.This(
                        NonEmptyList(
                          documentLoaderException(
                            this,
                            s"loadDocument: error in processNestedContent: "+
                            s"There should be an element for XMI pattern: ${a.p}")))
                    case Some(umlParent) =>
                      val x2u = a.u + (xmiE -> umlE)
                      val compositeMetaPropertyName = xmiE.element.label
                      val parent2childOK: NonEmptyList[java.lang.Throwable] \/ Unit =
                        umlParent.compositeMetaProperties.find((mpf) =>
                          mpf.propertyName == compositeMetaPropertyName &&
                          mpf.domainType.runtimeClass.isInstance(umlParent) &&
                          mpf.rangeType.runtimeClass.isInstance(umlE)) match {

                          case Some(mpf) =>

                            //DocumentLoader.show(s"* Nested Composite ($mpf) => $xmiE")
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
                                NonEmptyList(
                                  documentLoaderException(
                                    this,
                                    s"loadDocument: error in processNestedContent: " +
                                    s"No composite meta property update found for" +
                                    s" '$compositeMetaPropertyName' on $umlParent"))
                                .left

                            }

                          case None =>
                            NonEmptyList(
                              documentLoaderException(
                                this,
                                s"loadDocument: error in processNestedContent: " +
                                s"No composite meta property found for " +
                                s"'$compositeMetaPropertyName' on $umlParent"))
                            .left

                        }
                      parent2childOK match {
                        case -\/(nels) =>
                          \&/.This(nels)
                        case \/-(_) =>
                          processAttributes(x2u, xmiE, nestedContents, Seq())
                          .map { nestedOther =>
                            -\/(Info(x2u, xmiE, nestedOther, a.m :+ ((a.p, restContents)), a.r))
                          }
                      }
                  }
              }
          }
        case Some((xmiOther, otherElements)) =>
          // @todo
          ???
      }
    
    val a0 = Info(u=xmi2uml, p=xmiPattern, c=content, m=more, r=references)
    val aN = \&/.tailrecM[NonEmptyList[java.lang.Throwable], Info, Result](step)(a0)
    aN
  }
  
  /**
   *
   * Note: The attributes appear before other content (references or nested children)
   *
   * @param d The Document being loaded (i.e., XMI Import in the sense of the OMG XMI specification)
   * @param ds The DocumentSet against which cross references from the loaded document will be resolved
   * @param xmi2uml Map of XMI tree patterns to UML Elements created
   * @param xmiPattern A pattern of an XMI tree corresponding to a UML Element of some kind
   * @param attributesAndOther XMI Element nodes corresponding 
   *                           to UML attributes or non-composite cross-references
   * @param umlF The tool-specific OTI UML Factory
   * @param umlU The tool-specific OTI UML Update
   * @return The XMI elements mapped to UML elements (composite nesting) and 
   *         the XMI cross-references to be processed in a subsequent phase
   */
  def processXMIElementAttributesAndNestedContent
  (d: Document[Uml],
   ds: DocumentSet[Uml],
   xmi2uml: XMI2UMLElementMap)
  (xmiPattern: XMIElementDefinition,
   attributesAndOther: Seq[Elem])
  (implicit umlF: UMLFactory[Uml], umlU: UMLUpdate[Uml], idg: IDGenerator[Uml])
  : NonEmptyList[java.lang.Throwable] \&/ (XMI2UMLElementMap, Seq[(XMIElementDefinition, Seq[Elem])]) =
    processAttributes(xmi2uml, xmiPattern, attributesAndOther, Seq())
    .flatMap { otherContent =>
      processNestedContent(xmi2uml, xmiPattern, otherContent, Seq(), Map())
    }
  
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
  def processXMIReferences
  (d: Document[Uml],
   ds: DocumentSet[Uml],
   xmi2uml: XMI2UMLElementMap,
   xmiReferences: Map[XMIElementDefinition, Seq[Elem]])
  (implicit umlU: UMLUpdate[Uml])
  : NonEmptyList[java.lang.Throwable] \&/ Unit = {
    
    def step(a: Map[XMIElementDefinition, Seq[Elem]])
    : NonEmptyList[java.lang.Throwable] \&/ (Map[XMIElementDefinition, Seq[Elem]] \/ Unit) =     
      if (a.isEmpty)
        \&/.That(\/-(()))
      else
        xmi2uml.get(a.head._1) match {
          case None =>
            \&/.Both(
              NonEmptyList(
                  documentLoaderException(
                      this,
                      s"loadDocument: error in processXMIReferences: "+
                      s"Missing entry for ${a.head._1}")),
             -\/(a.tail))
         case Some(umlE) =>
           updateElementReferences(d, ds, xmi2uml, xmiReferences.head._1, umlE, xmiReferences.head._2)
           .map { _ =>
             -\/(a.tail)
           }
       }
    
    val aN = \&/.tailrecM[NonEmptyList[java.lang.Throwable], Map[XMIElementDefinition, Seq[Elem]], Unit](
        step)(xmiReferences)
    aN
  }
  
  /**
   * Processing of XMI Element cross-references as updates 
   * of UML non-composite (meta) Properties of a single UML Element
   *
   * @todo Lookup the OTI update function to update the corresponding 
   *       non-composite UML Element property reference
   *
   * @param d The Document being loaded (i.e., XMI Import in the sense of the OMG XMI specification)
   * @param ds The DocumentSet against which cross references from the loaded document will be resolved
   * @param xmi2uml Map of XMI tree patterns to UML Elements created
   * @param xmiElement An XMI tree pattern
   * @param umlElement The UML Element created from the XMI tree pattern
   * @param xmiReferences The XMI Elements corresponding to the serialization of 
   *                      non-composite UML Properties of the UML Element
   * @return Success or Failure
   */
  def updateElementReferences
  (d: Document[Uml],
   ds: DocumentSet[Uml],
   xmi2uml: XMI2UMLElementMap,
   xmiElement: XMIElementDefinition,
   umlElement: UMLElement[Uml],
   xmiReferences: Seq[Elem])
  (implicit umlU: UMLUpdate[Uml])
  : NonEmptyList[java.lang.Throwable] \&/ Unit = {
    val r0: NonEmptyList[java.lang.Throwable] \&/ Unit = \&/.That(())
    val rN: NonEmptyList[java.lang.Throwable] \&/ Unit = 
      ( r0 /: xmiReferences ) { (acc, xmiReference) =>
      acc.flatMap { _ =>
        updateElementReference(d, ds, xmi2uml, xmiElement, umlElement, xmiReference)
      }
    }
    rN
  }
  
  def updateElementReference
  (d: Document[Uml],
   ds: DocumentSet[Uml],
   xmi2uml: XMI2UMLElementMap,
   xmiElement: XMIElementDefinition,
   umlElement: UMLElement[Uml],
   xmiReference: Elem)
  (implicit umlU: UMLUpdate[Uml])
  : NonEmptyList[java.lang.Throwable] \&/ Unit = 
    XMIPattern.matchXMILocalReference(xmiReference)
    .fold[NonEmptyList[java.lang.Throwable] \&/ Unit](
      XMIPattern.matchXMICrossReference(xmiReference)
      .fold[NonEmptyList[java.lang.Throwable] \&/ Unit](
         \&/.This(NonEmptyList(
           UMLError.umlUpdateError[Uml, UMLElement[Uml]](
             umlU,
             Iterable(umlElement),
             s"Unrecognized XMI reference: $xmiReference")))
      ){ href =>
        umlU.metaclass_reference_updater_table.get(xmiElement.xmiType)
        .fold[NonEmptyList[java.lang.Throwable] \&/ Unit](
            \&/.This(
                NonEmptyList(
                    UMLError
                    .umlUpdateError[Uml, UMLElement[Uml]](
                        umlU,
                        Iterable(),
                        s"No metaclass updater available for ${xmiElement.xmiType} "+
                        s"for reference $xmiReference")))
        ){ metaclassReferenceMap =>
          metaclassReferenceMap.get(xmiReference.label)
          .fold[NonEmptyList[java.lang.Throwable] \&/ Unit](
            \&/.This(
                NonEmptyList(
                    UMLError
                    .umlUpdateError[Uml, UMLElement[Uml]](
                        umlU,
                        Iterable(),
                        s"No reference updater available for ${xmiElement.xmiType} "+
                        s"for reference '${xmiReference.label}' in: $xmiReference")))
          ){ updater =>
            DocumentLoader.show(s"* href: $href on: ${xmiElement.xmiType} for ${xmiReference.label}")
            \&/.That(())
          }
        }
      }
    ){ idref =>
        umlU.metaclass_reference_updater_table.get(xmiElement.xmiType)
        .fold[NonEmptyList[java.lang.Throwable] \&/ Unit](
            \&/.This(
                NonEmptyList(
                    UMLError
                    .umlUpdateError[Uml, UMLElement[Uml]](
                        umlU,
                        Iterable(),
                        s"No metaclass updater available for ${xmiElement.xmiType} "+
                        s"for reference $xmiReference")))
        ){ metaclassReferenceMap =>
          metaclassReferenceMap.get(xmiReference.label)
          .fold[NonEmptyList[java.lang.Throwable] \&/ Unit](
            \&/.This(
                NonEmptyList(
                    UMLError
                    .umlUpdateError[Uml, UMLElement[Uml]](
                        umlU,
                        Iterable(),
                        s"No reference updater available for ${xmiElement.xmiType} "+
                        s"for reference '${xmiReference.label}' in: $xmiReference")))
          ){ updater =>
            DocumentLoader.show(s"* idref: $idref on: ${xmiElement.xmiType} for ${xmiReference.label}")
            \&/.That(())
          }
        }
    }
  
  /**
   * Processing of XMI Elements corresponding to the serialization of 
   * MOF tags or stereotypes applied to UML Elements
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
  (d: Document[Uml],
   ds: DocumentSet[Uml],
   xmi2uml: XMI2UMLElementMap,
   tags: Seq[Elem])
  : NonEmptyList[java.lang.Throwable] \&/ Unit = {
    // @todo
    //DocumentLoader.show(s"* Update ${tags.size} tags")
    //tags.foreach { t => DocumentLoader.show(s"* tag: $t") }
    \&/.That(())
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
  (implicit umlU: UMLUpdate[Uml], idg: IDGenerator[Uml])
  : NonEmptyList[java.lang.Throwable] \&/ Unit =
    e
    .fold[NonEmptyList[java.lang.Throwable] \&/ Unit] {
      \&/.This(
        NonEmptyList(
          UMLError
          .umlUpdateError[Uml, UMLElement[Uml]](
            umlU,
            Iterable(),
            s"There should be a UML element corresponding to $xmiE to update "+
            s"the attribute $attributeName with value $attributeValue")))
    }{ umlElement =>
        umlU.metaclass_attribute_updaters.get(xmiE.xmiType)
        .fold[NonEmptyList[java.lang.Throwable] \&/ Unit](
            \&/.This(
                NonEmptyList(
                    UMLError
                    .umlUpdateError[Uml, UMLElement[Uml]](
                        umlU,
                        Iterable(),
                        s"No metaclass updater available for ${xmiE.xmiType} "+
                        s"for attribute '$attributeName' with value $attributeValue")))
        ){ metaclassAttributeMap =>
          metaclassAttributeMap.get(attributeName)
          .fold[NonEmptyList[java.lang.Throwable] \&/ Unit](
            \&/.This(
                NonEmptyList(
                    UMLError
                    .umlUpdateError[Uml, UMLElement[Uml]](
                        umlU,
                        Iterable(),
                        s"No attribute updater available for ${xmiE.xmiType} "+
                        s"for attribute '$attributeName' with value $attributeValue")))
          ){ updater =>
            updater.update(umlElement, attributeValue).toThese
          }
        }
        // @todo
        //DocumentLoader.show(s"TODO<: update ${xmiE.xmiType}::$attributeName = $attributeValue")
        //\/-(())
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
  (url: Uml#LoadURL, 
   xmiElements: Seq[Elem])
  (implicit ds: DocumentSet[Uml], umlF: UMLFactory[Uml], umlU: UMLUpdate[Uml])
  : NonEmptyList[java.lang.Throwable] \&/ 
    (LoadingMutableDocument[Uml], DocumentSet[Uml], XMI2UMLElementMap, 
     Seq[(XMIElementDefinition, Seq[Elem])], Seq[Elem]) =
    XMIPattern.matchXMIPattern(xmiElements)
    .fold[NonEmptyList[java.lang.Throwable] \&/
          (LoadingMutableDocument[Uml], DocumentSet[Uml], XMI2UMLElementMap, 
           Seq[(XMIElementDefinition, Seq[Elem])], Seq[Elem])]{
      \&/.This(
        NonEmptyList(
          documentLoaderException(
            this,
            "No Document Root Node found in the XML!")))
    } { case ((xmiPattern:XMIPattern, xmiTags: Seq[Elem])) =>
        xmiPattern match {
          case xmiElement: XMIElementDefinition =>
            makeDocumentFromRootNode(url, xmiElement, xmiTags)
          case _                                =>
            \&/.This(
              NonEmptyList(
                documentLoaderException(
                  this,
                  s"error in makeDocumentFromRootNode(url=$url, xmiElements):"+
                  s"Not supported: $xmiPattern")))
        }
    }

  /**
   * Import an XMI Document 
   * (from XMI tree patterns and XMI Element serializations of MOF tags and stereotypes aplied)
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
  (url: Uml#LoadURL, 
   pattern: XMIElementDefinition, 
   tags: Seq[Elem])
  (implicit ds: DocumentSet[Uml], umlF: UMLFactory[Uml], umlU: UMLUpdate[Uml])
  : NonEmptyList[java.lang.Throwable] \&/ 
    (LoadingMutableDocument[Uml], DocumentSet[Uml], XMI2UMLElementMap, 
     Seq[(XMIElementDefinition, Seq[Elem])], Seq[Elem]) =
    pattern match {
      case xmiPattern@
        XMIElementDefinition(e, xmiID, xmiUUID, xmiType, Some(MOFExtTagNSPrefix(_, nsPrefix, _)), contents) =>
        val uriAttribute = contents.foldLeft(Option.empty[String])({
          case (s: Some[_], _) =>
            s
          case (None, e)       =>
            XMIPattern.lookupElementText("URI")(e)
        })
        
        uriAttribute
        .fold(missingURIAttribute)((uri: String) => {
          umlF.reflectivePackageFactoryLookup.get(xmiType)
          .fold[NonEmptyList[java.lang.Throwable] \&/ 
                (LoadingMutableDocument[Uml], DocumentSet[Uml], XMI2UMLElementMap, 
                 Seq[(XMIElementDefinition, Seq[Elem])], Seq[Elem])]{
            \&/.This(
              NonEmptyList(
                documentLoaderException(
                  this,
                  s"error in makeDocumentFromRootNode(url=$url, pattern, tags): "+
                  s"No Package-based factory found for xmiType=uml:$xmiType")))
          } { factory =>
              val result =factory(umlF)
              .flatMap { root =>
                getExternalDocumentURL(url)
                .flatMap { externalURI =>

                  val info = OTISpecificationRootCharacteristics(
                    packageURI = OTI_URI(uri),
                    documentURL = OTI_URL(externalURI.toString),
                    artifactKind = OTILoadingArtifactKind,
                    nsPrefix = OTI_NS_PREFIX(nsPrefix),
                    uuidPrefix = OTI_UUID_PREFIX(nsPrefix)) 
                  
                  val result = for {
                    added <- addLoadingMutableDocument(ds, info, url, root)
                    (d, ds2) = added
                    xmi2uml = Map[XMIElementDefinition, UMLElement[Uml]](xmiPattern -> root)
                    xmi2contents = Seq[(XMIElementDefinition, Seq[Elem])](xmiPattern -> contents)
                  } yield (d, ds2, xmi2uml, xmi2contents, tags)
                  
                  result
                }
              }
              result.toThese
            }
        })

      case _ =>
        \&/.This(
          NonEmptyList(
            documentLoaderException(
              this,
              s"error in makeDocumentFromRootNode(url=$url, pattern, tags): "+
              s"No Document Root Node found in the XML!")))
    }

  /**
   * Document root XML Element node (the serialization of a kind of UML Package) should have a URI attribute.
   *
   * @return A Scala Failure error for a missing URI attribute for
   *         the root UML Element of a Document root XML Element
   *         (it should be really a kind of UML Package)
   */
  protected def missingURIAttribute
  : NonEmptyList[java.lang.Throwable] \&/ 
    (LoadingMutableDocument[Uml], DocumentSet[Uml], XMI2UMLElementMap, 
     Seq[(XMIElementDefinition, Seq[Elem])], Seq[Elem]) =
    \&/.This(
      NonEmptyList(
        documentLoaderException(
          this,
          s"missingURIAttribute: Missing URI attribute for root element")))

}
