/*
 * Copyright 2014 California Institute of Technology ("Caltech").
 * U.S. Government sponsorship acknowledged.
 *
 * Copyright 2015 Airbus.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * License Terms
 */

package test.org.omg.oti.uml.loader

import org.omg.oti.uml.write.api._
import org.omg.oti.uml.read.api._
import scala.collection.immutable._
import scalaz._, Scalaz._

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
  override def make
  : Set[java.lang.Throwable] \/ UMLPackage[Uml] =
    for {
      top <- createUMLPackage
      _ = top.setName( "Top".some )
      
      p1 <- createUMLPackage
      _ = p1.setName( "P1".some )
      
      p2 <- createUMLPackage
      _ = p2.setName( "P2".some )
      
      _ = top.links_Package_owningPackage_compose_packagedElement_PackageableElement( Set(p1, p2) )
      
    } yield top
}