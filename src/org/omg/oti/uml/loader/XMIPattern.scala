package org.omg.oti.uml.loader

import scala.xml._

import scala.util._
import scala.xml._
import java.net.URL

import org.eclipse.ease.modules.EnvironmentModule

sealed trait XMIPattern {
  val element: Elem
}

case class MOFExtTagNSPrefix(
    override val element: Elem,
    nsPrefix: String,
    idRef: String)
    extends XMIPattern {
  
  override def toString: String =
    s"MOFExtTagNSPrefix(nsPrefix=$nsPrefix, xmi:idref=$idRef)"
    
}
    
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
     lookupUnprefixedAttributeOrNestedElementText(e, "name"), 
     lookupUnprefixedAttributeOrNestedElementText(e, "value"), 
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
  def lookupUnprefixedAttributeOrNestedElementText(e: Elem, key: String): Option[String] =
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
        asXMIAttribute(e) match {
          case s: Some[_] => 
            s
          case None =>            
            require(false)
            None
        }
      case _ =>
        None
  }
            
  def asXMIAttribute(e: Elem): Option[String] =
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