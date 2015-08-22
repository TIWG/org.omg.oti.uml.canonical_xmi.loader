package test.org.omg.oti.uml.loader

import org.omg.oti.uml.canonicalXMI._
import org.omg.oti.uml.write.api._
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.read.operations._
import scala.util.Try

/**
 * Construct some OMG UML 2.5 models using the OTI API.
 * These examples illustrate the tool-neutral OTI API in the sense
 * that they can be executed on a particular OMG UML 2.5 compliant modeling tool
 * via a corresponding tool-specific OTI adapter.
 */
trait Construct1[Uml <: UML] {
  
  val umlF: UMLFactory[Uml]
  val umlU: UMLUpdate[Uml]
  
  /**
   * A toplevel package with two nested packages.
   */
  def make1: Try[UMLPackage[Uml]] = 
    for {
      top <- umlF.createUMLPackage 
      _ = umlU.set_NamedElement_name(top, Some("Top"))
      
      p1 <- umlF.createUMLPackage
      _ = umlU.set_NamedElement_name(p1, Some("P1"))
      
      p2 <- umlF.createUMLPackage
      _ = umlU.set_NamedElement_name(p2, Some("P2"))
      
      _ = umlU
      .links_Package_owningPackage_compose_packagedElement_PackageableElement(top, Set(p1, p2))
    } yield top
}