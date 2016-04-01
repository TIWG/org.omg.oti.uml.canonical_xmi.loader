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
package org.omg.oti.uml.helper

import org.omg.oti.uml._
import org.omg.oti.uml.canonicalXMI._
import org.omg.oti.uml.characteristics.OTICharacteristicsProvider
import org.omg.oti.uml.read.api.UML
import org.omg.oti.uml.read.operations.UMLOps
import org.omg.oti.uml.write.api.{UMLFactory, UMLUpdate}
import org.omg.oti.uml.xmi.Document

import scala.collection.immutable._
import scala.reflect.runtime.universe._
import scalaz._

object OTIAdapter {

  def initialize
  [Uml <: UML,
   Uo <: UMLOps[Uml],
   Ch <: OTICharacteristicsProvider[Uml],
   Uf <: UMLFactory[Uml],
   Uu <: UMLUpdate[Uml]]
  (umlOpsCreator: => Set[java.lang.Throwable] \/  Uo,
   otiCharacteristicsCreator: Uo => Set[java.lang.Throwable] \/ Ch,
   factoryCreator: Uo => Set[java.lang.Throwable] \/ Uf,
   updateCreator: Uo => Set[java.lang.Throwable] \/ Uu)
  : Set[java.lang.Throwable] \/ OTIAdapter[Uml, Uo, Ch, Uf, Uu]
  = for {
    umlOps <- umlOpsCreator
    otiCharacteristicsProvider <- otiCharacteristicsCreator(umlOps)
    umlF <- factoryCreator(umlOps)
    umlU <- updateCreator(umlOps)
  } yield OTIAdapter(umlOps, otiCharacteristicsProvider, umlF, umlU)

}

case class OTIAdapter
[Uml <: UML,
 Uo <: UMLOps[Uml],
 Ch <: OTICharacteristicsProvider[Uml],
 Uf <: UMLFactory[Uml],
 Uu <: UMLUpdate[Uml]]
(umlOps: Uo,
 otiCharacteristicsProvider: Ch,
 umlF: Uf,
 umlU: Uu) {

  def withInitialDocumentSet[Do <: DocumentOps[Uml], Ds <: DocumentSet[Uml]]
  (documentOpsCreator: OTIAdapter[Uml, Uo, Ch, Uf, Uu] => Set[java.lang.Throwable] \/ Do,
   documentSetInitializer: Do => Set[java.lang.Throwable] \&/ Ds)
  ( implicit
    nodeT: TypeTag[Document[Uml]],
    edgeT: TypeTag[DocumentEdge[Document[Uml]]] )
  : Set[java.lang.Throwable] \&/ OTIDocumentSetAdapter[Uml, Uo, Ch, Uf, Uu, Do, Ds]
  = for {
    dOps <- documentOpsCreator(this).toThese
    ds <- documentSetInitializer(dOps)

  } yield
      OTIDocumentSetAdapter(this, dOps, ds)

}