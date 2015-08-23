package test.org.omg.oti.uml.loader

import org.omg.oti.uml.canonicalXMI._
import org.omg.oti.uml.write.api._
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.read.operations._
import scala.util.Try

/**
 * Defines the interface for simple construction tests.
 */
trait ConstructionTest[Uml <: UML] {
    
  val umlF: UMLFactory[Uml]  
  implicit val umlU: UMLUpdate[Uml]
    
  def make: Try[UMLPackage[Uml]]
  
}