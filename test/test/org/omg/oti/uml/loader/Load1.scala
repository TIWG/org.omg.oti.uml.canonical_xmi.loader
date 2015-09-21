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

import org.omg.oti.uml.read.api._
import scala.{Option,None,Some,StringContext}
import scala.Predef.{???,classOf,require,String}
import scala.collection.Seq
import scala.util._
import scala.xml._
import java.lang.System
import java.lang.IllegalArgumentException

trait Load1[Uml <: UML] extends LoadTest[Uml] {

    
  val loadCL = classOf[Load1[Uml]].getClassLoader
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
     
  def makeRootPackage(xmiLabel: String): Try[UMLPackage[Uml]]
  
  override def load: Try[UMLPackage[Uml]] = {
    
    // Cannonical XMI: B2.2: Always use a root xmi:XMI element
    val xmiRoot: Node = XML.load(modelURL.openStream())
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
      rootUMLPkg: UMLPackage[Uml] <- makeRootPackage(rootUMLNode.label)
      _ = System.out.println(s"uml root node label: ${rootUMLNode.label}")
      _ = System.out.println(s"uml root element: ${rootUMLPkg}")
    } yield rootUMLPkg
  }
  
}