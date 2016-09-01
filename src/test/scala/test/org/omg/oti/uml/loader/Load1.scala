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

import org.omg.oti.uml.UMLError
import org.omg.oti.uml.canonicalXMI._
import org.omg.oti.uml.canonicalXMI.DocumentSet
import org.omg.oti.uml.loader.DocumentLoader
import org.omg.oti.uml.loader._
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.xmi._

import scala.{Option,None,Some,StringContext}
import scala.Predef.{classOf,require,String}
import scala.collection.Seq
import scala.collection.immutable.Set
import scala.xml._
import scala.util.control.Exception._

import java.lang.System

import scalaz._, Scalaz._

trait Load1[Uml <: UML] {

  val loadCL = classOf[Load1[Uml]].getClassLoader
  val modelPath1 = "resources/loadTests/SysML.xmi"
  val modelPath2 = "loadTests/SysML.xmi"

  val modelURL =
    Seq(modelPath1, modelPath2)
    .flatMap { path =>
      catching(classOf[java.io.IOException], classOf[java.lang.SecurityException])
        .apply(Option.apply(loadCL.getResource(path)))
    }
    .headOption
    .fold[Set[java.lang.Throwable] \/ java.net.URL](
      Set(
        UMLError.umlAdaptationError(
          s"canot find resource model as: $modelPath1 or $modelPath2"
        )
      ).left
    ){ url: java.net.URL =>
      url.right
    }
     
  def makeRootPackage(xmiLabel: String)
  : Set[java.lang.Throwable] \/ UMLPackage[Uml]
  
  def load
  (ds: DocumentSet[Uml])
  (implicit
   loader: DocumentLoader[Uml],
   idg: IDGenerator[Uml])
  : Set[java.lang.Throwable] \/ UMLPackage[Uml] = {

    modelURL
      .flatMap { url: java.net.URL =>

        catching(
          classOf[java.io.IOException],
          classOf[java.lang.IllegalArgumentException],
          classOf[org.xml.sax.SAXException]
        )
          .either(XML.load(url.openStream()))
          .fold[Set[java.lang.Throwable] \/ UMLPackage[Uml]](

          (cause: java.lang.Throwable) =>
            -\/(Set(documentLoaderException(
              loader,
              s"loadDocument($url) failed: ${cause.getMessage}",
              cause))),

          (xmiRoot: scala.xml.Node) => {
            // Cannonical XMI: B2.2: Always use a root xmi:XMI element
            require("XMI" == xmiRoot.label)

            // there must be at least 1 child, which is a kind of UML Package, Profile or Model
            // subsequent children are mof tags or stereotype instances
            // all hrefs must be resolved to built-in documents.

            val xmiChildren: Seq[Elem] = xmiRoot.child.flatMap {
              case e: Elem => Some(e)
              case _ => None
            }

            val rootUMLNode: Elem = xmiChildren.head
            for {
              rootUMLPkg <- makeRootPackage(rootUMLNode.label)
              _ = System.out.println(s"uml root node label: ${rootUMLNode.label}")
              _ = System.out.println(s"uml root element: ${rootUMLPkg}")
            } yield rootUMLPkg
          })
      }
  }
  
}