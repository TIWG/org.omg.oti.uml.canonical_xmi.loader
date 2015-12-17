/*
 *
 *  License Terms
 *
 *  Copyright (c) 2015, California Institute of Technology ("Caltech").
 *  U.S. Government sponsorship acknowledged.
 *
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are
 *  met:
 *
 *
 *   *   Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *   *   Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the
 *       distribution.
 *
 *   *   Neither the name of Caltech nor its operating division, the Jet
 *       Propulsion Laboratory, nor the names of its contributors may be
 *       used to endorse or promote products derived from this software
 *       without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 *  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 *  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 *  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 *  OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 *  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 *  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package test.org.omg.oti.uml.loader

import java.net.URL

import org.omg.oti.uml.canonicalXMI._
import org.omg.oti.uml.characteristics._
import org.omg.oti.uml.loader.DocumentLoader
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.write.api._
import org.omg.oti.uml.xmi._
import org.omg.oti.uml.xmi.DocumentKind._

import scala.reflect.runtime.universe._
import scalaz._

/**
 * Defines the interface for simple load tests.
 */
trait LoadTest[Uml <: UML] {

  val loader: DocumentLoader[Uml]
  val umlF: UMLFactory[Uml]  
  implicit val umlU: UMLUpdate[Uml]
    
  def url2loadURL
  (url: URL)
  (implicit loader: DocumentLoader[Uml])
  : Uml#LoadURL

  def load
  (modelURL: java.net.URL,
   ds: DocumentSet[Uml])
  (implicit
   loader: DocumentLoader[Uml],
   idg: IDGenerator[Uml],
   umlF: UMLFactory[Uml],
   umlU: UMLUpdate[Uml],
   nodeT: TypeTag[Document[Uml]],
   edgeT: TypeTag[DocumentEdge[Document[Uml]]])
  : NonEmptyList[java.lang.Throwable] \&/ (LoadingMutableDocument[Uml], DocumentSet[Uml]) =
    loader.loadDocument(url2loadURL(modelURL), ds)
}