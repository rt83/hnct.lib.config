package hnct.lib.config

import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlElementWrapper;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlRootElement;


case class Person(name : Option[String], age : Int, contact : List[String])


// for xml deserialization to work, need java bean style classes when there are collections involves
// for normal simple case, case class would be fine
@JacksonXmlRootElement(localName = "person")
class XmlPerson {
	
	private var name = ""
	private var age = 0
	private var contact = List("")	
	
	def getName = name
	def setName(n : String) = name = n
	
	def getAge = age
	def setAge(a : Int) = age = a
	
	@JacksonXmlElementWrapper(localName = "contacts")
	def getContact = contact
	@JacksonXmlElementWrapper(localName = "contacts")
	def setContact(c : List[String]) = contact = c
	
}

object ExampleReadWrite {
	
	def main(args : Array[String]) {
		// The two files person.json and person.xml will be searched for on the classpath. When running this program
		// need to add the folder example-data on the classpath
		val p : Option[Person] = Configuration.read(Some("person.json"), None, classOf[Person], ConfigurationFormat.JSON)
		
		// avoid checking None by using map
		p.map { x1 => println(x1.name) }
		
		val p1 : Option[XmlPerson] = Configuration.read(Some("person.xml"), None, classOf[XmlPerson], ConfigurationFormat.XML)
		
		p1. map { _.getContact.foreach { println(_) } }
		
		Configuration.write("person1.json", p1, false)
	}
	
}