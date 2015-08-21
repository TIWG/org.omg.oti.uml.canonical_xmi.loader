package org.omg.oti.uml.loader

import org.omg.oti.uml.canonicalXMI._
import org.omg.oti.uml.write.api._
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.read.operations._
import scala.util.Try
import java.net.URI

trait DocumentLoader[Uml <: UML] {
  val umlF: UMLFactory[Uml]
  val umlU: UMLUpdate[Uml]
  val umlOps: UMLOps[Uml]
  
  /**
   * Load an OTI UML 2.5 Package from its OTI Canonical XMI Document serialization.
   *
   * @param uri The URI of an OTI UML 2.5 Canonical XMI document to load.
   * @return if successful, a SerializableDocument corresponding to the single root UML Package loaded
   */
	def loadDocument(uri: URI): Try[SerializableDocument[Uml]]
  
}