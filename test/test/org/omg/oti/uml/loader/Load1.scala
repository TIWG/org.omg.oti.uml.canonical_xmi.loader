package test.org.omg.oti.uml.loader

import org.omg.oti.uml.canonicalXMI._
import org.omg.oti.uml.write.api._
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.read.operations._
import scala.util._
import scala.xml._

trait Load1[Uml <: UML] extends LoadTest[Uml] {
  
  import umlF._
    
  val loadCL = classOf[Load1[Uml]].getClassLoader
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
     
  def makeRootPackage(xmiLabel: String): Try[UMLPackage[Uml]]
  
  override def load: Try[UMLPackage[Uml]] = {
    
    // Cannonical XMI: B2.2: Always use a root xmi:XMI element
    val xmiRoot: Node = XML.load(modelURL.openStream())
    require("XMI" == xmiRoot.label)
    
    // there must be at least 1 child, which is a kind of UML Package, Profile or Model
    // subsequent children are mof tags or stereotype instances
    // all hrefs must be resolved to built-in documents.
    
    val xmiChildren: Seq[Elem] = xmiRoot.child.flatMap {
      case e: Elem => Some(e)
      case _ => None
    }
      
    val rootUMLNode: Elem = xmiChildren.head
    val rootUMLPkg: UMLPackage[Uml] = makeRootPackage(rootUMLNode.label) match {
      case Success(pkg) => pkg
      case Failure(f) => return Failure(f)
    }
    System.out.println(s"uml root node label: ${rootUMLNode.label}")
    System.out.println(s"uml root element: ${rootUMLPkg}")
    Success(???)
  }
  
}