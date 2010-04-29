/*
 * DotSettingsFactory.java
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

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.TransformerException;

import org.eclipse.core.runtime.Platform;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import org.sun.gf.nbecl.utils.Util;

public class DotSettingsFactory extends FileFactory {

	public DotSettingsFactory(String path) {
		this.path = path;
	}

	@Override
	public void create() throws TransformerException, IOException,
			ParserConfigurationException, SAXException {

		//load the nb context root
		String contextRoot = getContextRoot(path);
		
		//copy the ecl file to the nb location
		//FileCopier.copyDir(url + ".settings", path+"/.settings");

		//edit the ecl project file
		File contextFile = new File(path+"/.settings/org.eclipse.wst.common.component");
		Document context = getXML(contextFile);
		NodeList nodes = context.getElementsByTagName("property");
		Node contextNode = nodes.item(1).getAttributes().item(1);
		contextNode.setTextContent(contextRoot.substring(1));
		
		
		nodes = context.getElementsByTagName("wb-module");
		contextNode = nodes.item(0).getAttributes().item(0);
		contextNode.setTextContent(getProjectName(path));
		//save the ecl project file
		Util.saveDoc(context,path+"/.settings/org.eclipse.wst.common.component");

	}

}
