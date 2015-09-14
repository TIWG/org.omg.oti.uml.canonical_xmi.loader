package test.org.omg.oti.uml.loader

import org.omg.oti.uml.canonicalXMI._
import org.omg.oti.uml.xmi._
import org.omg.oti.uml.write.api._
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.read.operations._
import scala.util.{Failure, Success, Try}
import scala.xml.{Document => XMLDocument, _}
import org.omg.oti.uml.loader.DocumentLoader
import java.net.URL

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._

trait Load2[Uml <: UML] {
  
  val umlF: UMLFactory[Uml]  
  implicit val umlU: UMLUpdate[Uml]
    
  val loader: DocumentLoader[Uml]
  val loadCL = classOf[Load2[Uml]].getClassLoader
  val modelPath1 = "resources/loadTests/SysML.xmi"
  val modelPath2 = "loadTests/SysML.xmi"

  val modelURL =
    Seq(modelPath1, modelPath2)
    .flatMap { path => Option.apply(loadCL.getResource(path)) }
    .headOption match {
      case Some(url) =>
        url
      case None =>
        throw new IllegalArgumentException(s"Cannot find model file!")
    }
     
  def url2loadURL(url: URL): loader.documentOps.LoadURL
  
  def load
  (ds: DocumentSet[Uml])  
  (implicit nodeT: TypeTag[Document[Uml]], edgeT: TypeTag[DocumentEdge[Document[Uml]]])
  : Try[(SerializableDocument[Uml], DocumentSet[Uml])] =
    loader.loadDocument(url2loadURL(modelURL), ds)
}