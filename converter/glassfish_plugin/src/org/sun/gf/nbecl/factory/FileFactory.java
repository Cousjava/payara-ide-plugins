/*
 * FileFactory.java
 * 
 * Version 0.1
 *
 * 02/10/2010
 * 
 * 
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 2009=2019 Sun Microsystems, Inc. All rights reserved.
 *
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common Development
 * and Distribution License("CDDL") (collectively, the "License").  You
 * may not use this file except in compliance with the License. You can obtain
 * a copy of the License at https://glassfish.dev.java.net/public/CDDL+GPL.html
 * or glassfish/bootstrap/legal/LICENSE.txt.  See the License for the specific
 * language governing permissions and limitations under the License.
 *
 * When distributing the software, include this License Header Notice in each
 * file and include the License file at glassfish/bootstrap/legal/LICENSE.txt.
 * Sun designates this particular file as subject to the "Classpath" exception
 * as provided by Sun in the GPL Version 2 section of the License file that
 * accompanied this code.  If applicable, add the following below the License
 * Header, with the fields enclosed by brackets [] replaced by your own
 * identifying information: "Portions Copyrighted [year]
 * [name of copyright owner]"
 *
 * Contributor(s):
 *
 * If you wish your version of this file to be governed by only the CDDL or
 * only the GPL Version 2, indicate your decision by adding "[Contributor]
 * elects to include this software in this distribution under the [CDDL or GPL
 * Version 2] license."  If you don't indicate a single choice of license, a
 * recipient has the option to distribute your version of this file under
 * either the CDDL, the GPL Version 2 or to extend the choice of license to
 * its licensees as provided above.  However, if you add GPL Version 2 code
 * and therefore, elected the GPL Version 2 license, then the option applies
 * only if the new code is made subject to such option by the copyright
 * holder.
 *
 */
package org.sun.gf.nbecl.factory;

import java.io.File;
import java.io.IOException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.TransformerException;

import org.w3c.dom.Document;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.helpers.XMLReaderFactory;

import org.sun.gf.nbecl.utils.Util;

public abstract class FileFactory extends Util{
	protected String path;
	protected String url;
	
	/**
	 * 
	 * @throws TransformerException
	 * @throws IOException
	 * @throws ParserConfigurationException
	 * @throws SAXException
	 */
	public abstract void create() throws TransformerException, IOException, ParserConfigurationException, SAXException;
	
	/**
	 * 
	 * @param path
	 * @return
	 * @throws IOException
	 * @throws ParserConfigurationException
	 * @throws SAXException
	 */
	public static String getProjectName(String path) throws IOException, ParserConfigurationException, SAXException
	{
		class ProjectParserHelper extends DefaultHandler{
			
			boolean name = false;
			String projectName;
			public void startElement(String nsURI, String strippedName,
							String tagName, Attributes attributes) throws SAXException
			{
				if (tagName.equalsIgnoreCase("name"))
					name = true;
			}

			public void characters(char[] ch, int start, int length)
			{
				if (name)
					projectName = new String(ch, start, length);
				name = false;
			}
		}
		XMLReader parser = XMLReaderFactory.createXMLReader();
		parser.setContentHandler(new ProjectParserHelper());
		parser.parse(path+"/nbproject/project.xml");
		return ((ProjectParserHelper)parser.getContentHandler()).projectName;
		
	}
	
	
	
	/**
	 * 
	 * @param path
	 * @return
	 * @throws IOException
	 * @throws ParserConfigurationException
	 * @throws SAXException
	 */
	public static String getContextRoot(String path) throws IOException, ParserConfigurationException, SAXException
	{
		class ContextParserHelper extends DefaultHandler{
			
			boolean isRoot = false;
			String contextRoot;
			public void startElement(String nsURI, String strippedName,
							String tagName, Attributes attributes) throws SAXException
			{
				if (tagName.equalsIgnoreCase("context-root"))
					isRoot = true;
			}

			public void characters(char[] ch, int start, int length)
			{
				if (isRoot)
					contextRoot = new String(ch, start, length);
				isRoot = false;
			}
		}
		XMLReader parser = XMLReaderFactory.createXMLReader();
		parser.setContentHandler(new ContextParserHelper());
		parser.parse(path+"/web/WEB-INF/sun-web.xml");
		return ((ContextParserHelper)parser.getContentHandler()).contextRoot;
		
	}
	
	
	/**
	 * 
	 * @param file the File which the XML Document is to be parsed from
	 * @return the Document containing the XML version of the given file
	 * @throws ParserConfigurationException
	 * @throws SAXException
	 * @throws IOException
	 */
	public static Document getXML(File file) throws ParserConfigurationException, SAXException, IOException
	{
		DocumentBuilderFactory docBuilderFactory = DocumentBuilderFactory.newInstance();
		DocumentBuilder docBuilder = docBuilderFactory.newDocumentBuilder(); 
		Document doc = docBuilder.parse(file);
		doc.getDocumentElement().normalize();
		return doc;
	}
	
	
	
	
}
