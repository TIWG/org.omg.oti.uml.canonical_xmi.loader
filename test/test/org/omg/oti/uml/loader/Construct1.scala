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
trait Construct1[Uml <: UML] extends ConstructionTest[Uml] {
  
  import umlF._
  
  /**
   * A toplevel package with two nested packages.
   */
  override def make: Try[UMLPackage[Uml]] = 
    for {
      top <- createUMLPackage 
      _ = top.setName( Some("Top") )
      
      p1 <- createUMLPackage
      _ = p1.setName( Some("P1") )
      
      p2 <- createUMLPackage
      _ = p2.setName( Some("P2") )
      
      _ = top.links_Package_owningPackage_compose_packagedElement_PackageableElement( Set(p1, p2) )
      
    } yield top
}