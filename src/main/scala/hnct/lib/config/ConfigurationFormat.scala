package hnct.lib.config

import com.fasterxml.jackson.databind._
import com.fasterxml.jackson.dataformat.xml._
import com.fasterxml.jackson.module.scala._
import com.fasterxml.jackson.dataformat.xml.util.DefaultXmlPrettyPrinter
import com.fasterxml.jackson.core.util.DefaultPrettyPrinter

sealed class ConfigurationFormat {}

object ConfigurationFormat {
	
	case object JSON extends ConfigurationFormat
	case object XML extends ConfigurationFormat
	
	def configure(m : ObjectMapper) = m.registerModule(DefaultScalaModule)
											.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false)

		
		// implicit conversion to appropriate mapper for each ConfigurationFormat type
	implicit def mapper[FormatType <: ConfigurationFormat] = { x : FormatType => 
		x match {
			case XML => configure(new XmlMapper())
			case JSON => configure(new ObjectMapper())
			case _ => throw new RuntimeException("Unsupport configuration format!") 
		}
	}
	
	implicit def prettyPrinter[FormatType <: ConfigurationFormat] = { x : FormatType =>
		x match {
			case XML => new DefaultXmlPrettyPrinter
			case JSON => new DefaultPrettyPrinter
			case _ => throw new RuntimeException("Unsupported configuration format!");
		}
	}
}