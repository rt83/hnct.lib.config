package hnct.lib.config

import java.io.File
import java.io.PrintWriter
import com.fasterxml.jackson.databind._
import com.fasterxml.jackson.dataformat.xml._
import com.fasterxml.jackson.module.scala._
import ConfigurationFormat._
import java.net.URISyntaxException
import java.net.URL;
import hnct.lib.utility.Logable

object Configuration extends Logable {
	
	def read[T, FormatType <: ConfigurationFormat](defaultFileName : Option[String], systemVar : Option[String], resultClass : Class[T], format : FormatType) : Option[T] = {
		
		// a pattern matching to check all possible case of defaultFileName and systemVar
		val f = (defaultFileName, systemVar) match {
			// if the systemVar defines, the file name is extracted from there if the System property return a non-null value
			case (_ , Some(variable)) if (System.getProperty(variable) != null) => fromName(System.getProperty(variable))
			// if systemVar is none, or it is not null and System.getProperty return null value, and the defaultFileName is not empty
			// the file is extracted from the defaultFileName
			case (Some(name), _) if (!name.isEmpty()) => fromName(name)
			// Other case, such as (None, None), (None, Some) but getProperty return null, we cannot initialize the file, and have to
			// throw an exception
			case _ => throw new RuntimeException("""Cannot find the file name""")
		}
			
		// format.readValue is possible because of implicit conversion
		if (f.exists()) {
			
			log.info("File exist! Trying to read the file!")
			
			Some(format.readValue(f, resultClass))
		}
		else None
	}
	
	def read[T, FormatType <: ConfigurationFormat](defaultFileName : String, resultClass : Class[T], format : FormatType) : Option[T] = {
		if (defaultFileName == null && defaultFileName.isEmpty()) throw new RuntimeException("Cannot load null or empty filename!")
		
		read(Some(defaultFileName), None, resultClass, format)
	}
	
	private def fromName(name : String) : File = {
		
		log.info("Creating configuration file {}", name)
		
		var f = new File(name)
		
		if (!f.exists()) {	// file not exist, search in class path
			log.info("File doesn't exist. Look on class path!")
			
			val url = Thread.currentThread().getContextClassLoader().getResource(name);
			
			if (url == null) {
				
				log.info("Unable to find the file {} both on disk and on class path!", name)
				
				return f;
				
			}
			
			try {
				
				val path = url.toURI().getPath();
				
				log.info("Found the file path {}", path)
				
				new File(path)
				
			} catch { 
				case e : URISyntaxException => {
					log.error("Inferring file path from classpath not successful", e)
					f	
				}
			}
		} else f

	}
	
	def write[FT <: ConfigurationFormat](f : File, obj : AnyRef, format : FT, pretty : Boolean) = {
		// make the directory if necessary
		if (f.getParentFile != null && !f.exists()) f.getParentFile.mkdirs()
		
		// two implicit conversions are done from format to objectmapper and format to PrettyPrinter
		val generator = format.getFactory.createGenerator(new PrintWriter(f))
		
		if (pretty) generator.setPrettyPrinter(format)
		
		format.writeValue(generator, obj)
	}
	
	def write[FT <: ConfigurationFormat](fileName : String, obj : AnyRef, format : FT, pretty : Boolean) : Unit = 
		write(new File(fileName), obj, format, pretty)
		
	def write[FT <: ConfigurationFormat](fileName : String, obj : AnyRef, format : FT) : Unit = 
		write(new File(fileName), obj, format, true)
		
	def write(fileName : String, obj : AnyRef, pretty : Boolean) : Unit = 
		write(new File(fileName), obj, ConfigurationFormat.JSON, pretty)
		
	def write(fileName : String, obj : AnyRef) : Unit = 
		write(new File(fileName), obj, ConfigurationFormat.JSON, true)
}