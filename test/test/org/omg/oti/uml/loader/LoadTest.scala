package test.org.omg.oti.uml.loader

import org.omg.oti.uml.canonicalXMI._
import org.omg.oti.uml.write.api._
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.read.operations._
import scala.util.Try
import java.io.File

/**
 * Defines the interface for simple load tests.
 */
trait LoadTest[Uml <: UML] {
    
  val umlF: UMLFactory[Uml]  
  implicit val umlU: UMLUpdate[Uml]
    
  def makeRootPackage(xmiLabel: String): Try[UMLPackage[Uml]]
  
  def load: Try[UMLPackage[Uml]]
  
}