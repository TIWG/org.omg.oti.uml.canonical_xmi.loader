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

import org.omg.oti.uml.canonicalXMI._
import org.omg.oti.uml.loader.DocumentLoader
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.write.api._
import org.omg.oti.uml.xmi._

import scala.{Option, None, Some, StringContext}
import scala.collection.immutable._
import scala.reflect.runtime.universe._
import scalaz._
import scala.Predef.classOf

import java.net.URL
import java.lang.IllegalArgumentException


trait Load2[Uml <: UML] {

  val loadCL = classOf[Load2[Uml]].getClassLoader
  val modelPath1 = "resources/loadTests/SysML.xmi"
  val modelPath2 = "loadTests/SysML.xmi"

  val modelURL =
    Seq(modelPath1, modelPath2)
    .flatMap { path => Option.apply(loadCL.getResource(path)) }
    .headOption match {
      case Some(url) =>
        url
      case None =>
        throw new IllegalArgumentException(s"Cannot find model file!")
    }

  def url2loadURL
  (url: URL)
  (implicit loader: DocumentLoader[Uml])
  : Uml#LoadURL

  def load
  (ds: DocumentSet[Uml])
  (implicit
   loader: DocumentLoader[Uml],
   idg: IDGenerator[Uml],
   umlF: UMLFactory[Uml],
   umlU: UMLUpdate[Uml],
   nodeT: TypeTag[Document[Uml]],
   edgeT: TypeTag[DocumentEdge[Document[Uml]]])
  : Set[java.lang.Throwable] \&/ (LoadingMutableDocument[Uml], DocumentSet[Uml]) =
    loader.loadDocument(url2loadURL(modelURL), ds)
}