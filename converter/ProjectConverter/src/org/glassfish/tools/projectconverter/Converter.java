/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 2010 Sun Microsystems, Inc. All rights reserved.
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
 */


package org.glassfish.tools.projectconverter;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.StringWriter;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;


/**
 * @author mwtidd
 *
 */
public class Converter {
	public static void main(String[] args) throws IOException, ParserConfigurationException, SAXException, TransformerException {


		//  prompt the user to enter the project location
		System.out.print("Enter the project path: ");

		BufferedReader br = new BufferedReader(new InputStreamReader(System.in));

		String path = null;

		try {
			path = br.readLine();
		} catch (IOException ioe) {
			System.out.println("IO error trying to read your name!");
			System.exit(1);
		}
		//TODO for now auto set the path, eventually this should take the user prompt
		//path="/Users/michaeltidd/NetBeansProjects/glassfish-samples/ws/javaee6/web/jsf/basic-ezcomp";

		//copy the default classpath
		FileCopier.copy("../NbEcl Converter/src/utils/.classpath", path+"/.classpath");

		
		
		
		
		//set the project file to the nb project file
		File projectFile = new File(path+"/nbproject/project.xml");
		check(projectFile);
		Document project = getXML(projectFile);
		NodeList nodes = project.getElementsByTagName("name");
		//load the nb project name
		String projectName = nodes.item(0).getTextContent();
		//edit the ecl project file
		projectFile = new File("../NbEcl Converter/src/utils/.project");
		project = getXML(projectFile);
		nodes = project.getElementsByTagName("name");
		nodes.item(0).setTextContent(projectName);
		//save the ecl project file
		save(project,"../NbEcl Converter/src/utils/.project");
		//copy the ecl file to the nb location
		FileCopier.copy("../NbEcl Converter/src/utils/.project", path+"/.project");
		
		
		
		
		
		project = null;
		projectFile = null;
		nodes = null;
		
		
		
		
		
		//set the context file the nb context file
		File contextFile = new File(path+"/web/WEB-INF/sun-web.xml");
		String contextRoot = "";
		check(contextFile);
		Document context = getXML(contextFile);
		nodes = context.getElementsByTagName("context-root");
		//load the nb context root
		contextRoot = nodes.item(0).getTextContent();

		//edit the ecl project file
		contextFile = new File("../NbEcl Converter/src/utils/.settings/org.eclipse.wst.common.component");
		context = getXML(contextFile);
		nodes = context.getElementsByTagName("property");
		Node contextNode = nodes.item(1).getAttributes().item(1);
		contextNode.setTextContent(contextRoot.substring(1));
		
		
		nodes = context.getElementsByTagName("wb-module");
		contextNode = nodes.item(0).getAttributes().item(0);
		contextNode.setTextContent(projectName);
		//save the ecl project file
		save(context,"../NbEcl Converter/src/utils/.settings/org.eclipse.wst.common.component");
		//copy the ecl file to the nb location
		FileCopier.copyDir("../NbEcl Converter/src/utils/.settings", path+"/.settings");

		
		
		trace("Conversion Complete");
	}

	public static Document getXML(File file) throws ParserConfigurationException, SAXException, IOException
	{
		DocumentBuilderFactory docBuilderFactory = DocumentBuilderFactory.newInstance();
		DocumentBuilder docBuilder = docBuilderFactory.newDocumentBuilder(); 
		Document doc = docBuilder.parse(file);
		doc.getDocumentElement().normalize();
		return doc;
	}
	
	public static void trace(String message)
	{
		System.out.println(message);
	}
	
	public static void check(File file) throws IOException
	{
		if (!file.exists())
			throw new IOException("FileCopy: " + "no such source file: "
					+ file.getName());
		else if (!file.isFile())
			throw new IOException("FileCopy: " + "can't copy directory: "
					+ file.getName());
	}

	public static void save(Document doc, String toFile) throws TransformerException, IOException
	{
		TransformerFactory transfac = TransformerFactory.newInstance();
		Transformer trans = transfac.newTransformer();
		StringWriter sw = new StringWriter();
		StreamResult result = new StreamResult(sw);
		DOMSource source = new DOMSource(doc);
		trans.transform(source, result);

		String xmlString = sw.toString();
		OutputStream out;
		byte buf[] = xmlString.getBytes();
		out = new FileOutputStream(toFile);
		for(int i=0;i<buf .length;i++) 
		{
			out.write(buf[i]);
		}
		out.close();
		buf = null;
	}

}
