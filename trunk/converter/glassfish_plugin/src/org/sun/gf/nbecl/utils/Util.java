/*
 * Util.java
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
package org.sun.gf.nbecl.utils;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.StringWriter;

import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;

public abstract class Util {
	/**
	 * 
	 * @param file
	 * @throws IOException
	 */
	public static void checkFile(File file) throws IOException
	{
		if (!file.exists())
			throw new IOException("FileCopy: " + "no such source file: "
					+ file.getName());
		else if (!file.isFile())
			throw new IOException("FileCopy: " + "can't copy directory: "
					+ file.getName());
	}
	
	/**
	 * 
	 * @param doc
	 * @param toFile
	 * @throws TransformerException
	 * @throws IOException
	 */
	public static void saveDoc(Document doc, String toFile) throws TransformerException, IOException
	{
		OutputStream out = new FileOutputStream(toFile);
		try{
			TransformerFactory transfac = TransformerFactory.newInstance();
			Transformer trans = transfac.newTransformer();
			StringWriter sw = new StringWriter();
			StreamResult result = new StreamResult(sw);
			DOMSource source = new DOMSource(doc);
			trans.transform(source, result);
	
			String xmlString = sw.toString();
			
			byte buf[] = xmlString.getBytes();
			for(int i=0;i<buf .length;i++) 
			{
				out.write(buf[i]);
			}
			out.close();
			buf = null;
		}finally{
			out.close();
		}
			
	}
	
	/**
	 * prompts the user to enter the path of the netbeans project they wish to have converted
	 * 
	 * @return the line entered in by the user
	 */
	public static String prompt()
	{
	//  prompt the user to enter the project location
		System.out.print("Enter the Netbeans project's path: ");

		BufferedReader br = new BufferedReader(new InputStreamReader(System.in));

		String path = null;

		try {
			path = br.readLine();
		} catch (IOException ioe) {
			System.out.println("IO error trying to read your name!");
			System.exit(1);
		}
		return path;
	}
	
	/**
	 * 
	 * @param message
	 */
	public static void trace(String message)
	{
		System.out.println(message);
	}
}
