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
  : NonEmptyList[java.lang.Throwable] \/ UMLPackage[Uml] =
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