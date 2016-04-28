/*
 *
 * License Terms
 *
 * Copyright (c) 2014-2016, California Institute of Technology ("Caltech").
 * U.S. Government sponsorship acknowledged.
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * *   Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * *   Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * *   Neither the name of Caltech nor its operating division, the Jet
 *    Propulsion Laboratory, nor the names of its contributors may be
 *    used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package test.org.omg.oti.uml.loader

import org.omg.oti.uml.UMLError
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