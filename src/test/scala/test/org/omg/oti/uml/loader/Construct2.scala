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

trait Construct2[Uml <: UML] extends ConstructionTest[Uml] {

  import umlF._

  // An example of a convenient construction macro.
  def addMetamodelReference
  ( pf: UMLProfile[Uml],
    mm: UMLPackage[Uml] )
  : Set[java.lang.Throwable] \/ UMLPackageImport[Uml] =
  for {
    new_pf_references_mm <- createUMLPackageImport
    all_pf_references_mms = pf.metamodelReference + new_pf_references_mm
    _ <- pf.links_Profile_profile_compose_metamodelReference_PackageImport( all_pf_references_mms )
    _ <- new_pf_references_mm.links_PackageImport_packageImport_reference_importedPackage_Package( mm.some )
  } yield new_pf_references_mm

  /**
    * A toplevel package with a metamodel and a profile extending that metamodel
    */
  override def make
  : Set[java.lang.Throwable] \/ UMLPackage[Uml] =
    for {
      top <- createUMLPackage
      _ <- top.setName( "Top".some )

      // 1) create a simplified UML metamodel.

      umlMM <- createUMLPackage
      _ <- umlMM.setName( "UML".some )

      // create UML::Class

      classMC <- createUMLClass
      _ <- classMC.setName( "Class".some )

      // UML::Class is an owned type of UML

      _ <- umlMM.links_Package_owningPackage_compose_packagedElement_PackageableElement( Set( classMC ))

      // 2) creater a simplified SysML profile

      sysmlPF <- createUMLProfile
      _ <- sysmlPF.setName( "SysML".some )

      // 3) SysML (profile) has a package import reference to the UML metamodel

      _ <- addMetamodelReference( sysmlPF, umlMM )

      // 4) Make UML & SysML nested packages in Top.

      _ <- top.links_Package_owningPackage_compose_packagedElement_PackageableElement( Set( umlMM, sysmlPF ) )

    } yield top
}