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
package org.omg.oti.uml.loader

import scala.xml._

import scala.xml._
import scala.annotation
import scala.{Option,None,Some,StringContext}
import scala.collection.immutable._
import scala.collection.Seq
import scala.Predef.{Set => _, Map => _, _}

/**
 * A pattern of an XMI tree rooted at an XML Element
 */
sealed trait XMIPattern {
  val element: Elem
}

/**
 * A pattern of an XMI tree corresponding to the MOF tag 'nsPrefix'
 * @param element The XML Element corresponding to the XMI MOF tag 'nsPrefix'
 * @param nsPrefix The value of the 'nsPrefix'
 * @param idRef The MOF element to which the 'nsPrefix' tag applies
 */
case class MOFExtTagNSPrefix(
    override val element: Elem,
    nsPrefix: String,
    idRef: String)
    extends XMIPattern {
  
  override def toString: String =
    s"MOFExtTagNSPrefix(nsPrefix=$nsPrefix, xmi:idref=$idRef)"
    
}

/**
 * A pattern of an XMI tree corresponding to a UML Element of some kind
 * @param element The XML Element that is the XMI Document representation of an UML Element
 * @param xmiID The xmi:id of this UML Element
 * @param xmiUUID The xmi:uuid of this UML ELement
 * @param xmiType The metaclass of this UML ELement
 * @param nsPrefix Optionally, a MOF nsPrefix tag
 * @param contents Nested XML ELements corresponding to the XMI Document representation of this UML Element's content
 */
case class XMIElementDefinition(
    override val element: Elem,
    xmiID: String,
    xmiUUID: String,
    xmiType: String,
    nsPrefix: Option[MOFExtTagNSPrefix],
    contents: Seq[Elem])
    extends XMIPattern {
  
  override def toString: String =
    s"XMIElementDefinition(type=$xmiType, id=$xmiID, nsPrefix=$nsPrefix, contents: ${contents.size})"
    
}
  
object XMIPattern {

  def matchXMIPattern(elems: Seq[Elem]): Option[(XMIPattern, Seq[Elem])] = 
    matchElementDefinitionAttributes(elems.head) match {
    case Some((xmiID, xmiUUID, xmiType)) =>
      val e = elems.head
      val init: (Option[MOFExtTagNSPrefix], Seq[Elem]) = (None, Seq())
      val (nsPrefix, rest) = elems.tail.foldLeft(init) {
        case ((Some(tag), s), e) =>
          (Some(tag), s :+ e)
        case ((None, s), e) =>
          lookup_mofext_nsPrefix(xmiID)(e) match {
            case Some(tag) =>
              (Some(tag), s)
            case None =>
              (None, s :+ e)
          }
      }
      Some((XMIElementDefinition(element=e, xmiID, xmiUUID, xmiType, nsPrefix, contents=childElements(e)), rest))
    case None =>
      None
  }    
    
  def lookup_mofext_nsPrefix(idRef: String)(e: Elem): Option[MOFExtTagNSPrefix] =
    (e.label, 
     matchUnprefixedAttributeOrNestedElementText(e, "name"), 
     matchUnprefixedAttributeOrNestedElementText(e, "value"), 
     childElements(e).toList) match {
    case ("Tag", Some("org.omg.xmi.nsPrefix"), Some(nsPrefix), e1 :: Nil) 
      if "element" == e1.label =>
      require(1 == e1.attributes.size && "idref" == e1.attributes.key && 1 == e1.attributes.value.size)
      e1.attributes.value.head match {
        case t: Text if idRef == t.data =>
          Some(MOFExtTagNSPrefix(e, nsPrefix, idRef))
        case _ =>
          None
      }
            
    case _ =>
      None
  }

  def matchElementDefinitionAttributes(elem: Elem): Option[(String, String, String)] = {
    
    val attribs = elem.attributes
    if (3 != attribs.size) 
      None
    else {
      val init: (Option[String], Option[String], Option[String]) = (None, None, None)     
      val fini: (Option[String], Option[String], Option[String]) = attribs.foldLeft(init) {
        // 1st attribute should be xmi:id
        case ((_id, _uuid, _type), a) =>          
          require(1 == a.value.size)
          val value = a.value.head match {
            case t: Text =>
              t.data
            case v =>
              require(false, s"Expected a Text value, instead got: $v for attribute $a")
              ""
          }
          a.key match {
            case "id" =>
              require(_id.isEmpty, s"got two values for attribute xmi:id: '${_id}' and '$value'")
              (Some(value), _uuid, _type)
            case "uuid" =>
              require(_uuid.isEmpty, s"got two values for attribute xmi:uuid: '${_uuid}' and '$value'")
              (_id, Some(value), _type)
            case "type" =>
              require(_type.isEmpty, s"got two values for attribute xmi:type: '${_type}' and '$value'")
              require(value.startsWith("uml:"))
              (_id, _uuid, Some(value.stripPrefix("uml:")))
            case _ =>
              require(false, s"unrecognized attribute: $a")
              (_id, _uuid, _type)
          }
      }
      
      fini match {
        case (Some(xmiID), Some(xmiUUID), Some(xmiType)) =>
          Some((xmiID, xmiUUID, xmiType))
        case _ =>
          None
      }
    }
  }
  
  /**
   * Lookup the string value of an element attribute whose label is the key or of a nested element whose label is the key
   *
   * if e = <x a="blah.." .../>, key="a", then returns Some("blah")
   * if e = <x ...><a>blah</a>...</x>, key="a", then returns Some("blah")
   * otherwise returns None
   */
  def matchUnprefixedAttributeOrNestedElementText(e: Elem, key: String): Option[String] =
    e.attribute(key) match {
    case Some(nodes) =>
      require(1 == nodes.size)
      nodes.head match {
        case t: Text =>
          Some(t.data)
        case _ =>
          require(false)
          None
      }
    case None =>
      e.child
      .flatMap(lookupElementText(key))
      .headOption
  }
    
  def lookupElementText(key: String)(n: Node): Option[String] =
    n match {
      case e: Elem if key == e.label =>
        matchXMINestedText(e)
      case _ =>
        None
  }
            
  def matchXMICrossReference(e: Elem): Option[String] =
    if (e.child.isEmpty && 1 == e.attributes.size) {
      val xmiNS = e.getNamespace("xmi")
      require(null != xmiNS)
      e.attribute(xmiNS, "href")
      .orElse(e.attribute("href")) match {
        case Some(nodes) =>
          require(1 == nodes.size)
          nodes.head match {
            case t: Text =>
              Some(t.data)
            case _ =>
              require(false)
              None
          }
        case None =>
           None
      }
    }
    else 
      None
    
  def matchXMILocalReference(e: Elem): Option[String] =
    if (e.child.isEmpty && 1 == e.attributes.size) {
      val xmiNS = e.getNamespace("xmi")
      require(null != xmiNS)
      e.attribute(xmiNS, "idref")
      .orElse(e.attribute("idref")) match {
        case Some(nodes) =>
          require(1 == nodes.size)
          nodes.head match {
            case t: Text =>
              Some(t.data)
            case _ =>
              require(false)
              None
          }
        case None =>
           None
      }
    }
    else 
      None
    
  def matchXMINestedText(e: Elem): Option[String] =
    if (1 == e.child.size && Null == e.attributes)
      e.child.head match {
        case t: Text =>
          Some(t.data)
        case _ =>
           None
      }
    else 
      None
    
  def collectNamespaces(n: Node): Map[String, String] = {
    
    @annotation.tailrec def acc
    (m: Map[String, String], n: NamespaceBinding)
    : Map[String, String] =
      if (null == n)
        m
      else
        acc(m + (n.prefix -> n.uri), n.parent)
    
    acc(Map(), n.scope)       
  }
    
  def childElements(node: Node): Seq[Elem] = 
    node.child.flatMap {
    case e: Elem =>
      Some(e)
    case _ => 
      None
  }
  
}    