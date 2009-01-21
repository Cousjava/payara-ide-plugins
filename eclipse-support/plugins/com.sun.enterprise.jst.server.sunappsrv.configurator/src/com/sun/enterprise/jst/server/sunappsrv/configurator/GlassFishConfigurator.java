/*DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.

Copyright 1997-2009 Sun Microsystems, Inc. All rights reserved.

The contents of this file are subject to the terms of either the GNU
General Public License Version 2 only ("GPL") or the Common Development
and Distribution License("CDDL") (collectively, the "License").  You
may not use this file except in compliance with the License. You can obtain
a copy of the License at https://glassfish.dev.java.net/public/CDDL+GPL.html
or glassfish/bootstrap/legal/LICENSE.txt.  See the License for the specific
language governing permissions and limitations under the License.

When distributing the software, include this License Header Notice in each
file and include the License file at glassfish/bootstrap/legal/LICENSE.txt.
Sun designates this particular file as subject to the "Classpath" exception
as provided by Sun in the GPL Version 2 section of the License file that
accompanied this code.  If applicable, add the following below the License
Header, with the fields enclosed by brackets [] replaced by your own
identifying information: "Portions Copyrighted [year]
[name of copyright owner]"

Contributor(s):

If you wish your version of this file to be governed by only the CDDL or
only the GPL Version 2, indicate your decision by adding "[Contributor]
elects to include this software in this distribution under the [CDDL or GPL
Version 2] license."  If you don't indicate a single choice of license, a
recipient has the option to distribute your version of this file under
either the CDDL, the GPL Version 2 or to extend the choice of license to
its licensees as provided above.  However, if you add GPL Version 2 code
and therefore, elected the GPL Version 2 license, then the option applies
only if the new code is made subject to such option by the copyright
holder.
 */
package com.sun.enterprise.jst.server.sunappsrv.configurator;

import java.io.File;
import java.util.Iterator;
import java.util.Properties;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathFactory;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.datatools.connectivity.ConnectionProfileConstants;
import org.eclipse.datatools.connectivity.ConnectionProfileException;
import org.eclipse.datatools.connectivity.ProfileManager;
import org.eclipse.datatools.connectivity.drivers.DriverInstance;
import org.eclipse.datatools.connectivity.drivers.DriverManager;
import org.eclipse.datatools.connectivity.internal.InternalProfileManager;
import org.eclipse.wst.server.core.ServerPort;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;

@SuppressWarnings("restriction")
public class GlassFishConfigurator {

	public static ServerPort getPortForId(ServerPort[] serverPorts, String id) {
		for (ServerPort serverPort : serverPorts) {
			if (serverPort.getId().equals(id)) {
				return serverPort;
			}
		}
		return null;
	}

	public static void createV2Server(IProgressMonitor progressMonitor)
			throws CoreException {
		progressMonitor
				.setTaskName("Creating Glassfish V2 server configuration.");
		V2Configurator.configure(progressMonitor);
	}

	public static String createV3Server(IProgressMonitor progressMonitor)
			throws CoreException {
		progressMonitor
				.setTaskName("Creating Glassfish V3 prelude server configuration.");
		return V3Configurator.configure(progressMonitor);
	}

	public static void createDerbyDB(IProgressMonitor progressMonitor,
			String domainXml) throws CoreException {
		Properties properties = new Properties();

		String profileName = "Sample Derby database";
		DriverInstance di = DriverManager
				.getInstance()
				.createNewDriverInstance(
						"org.eclipse.datatools.connectivity.db.derby102.clientDriver",
						"DerbyForSampleDB", getJarLocation());
		readProperties(domainXml, properties);
		properties.setProperty(
				ConnectionProfileConstants.PROP_DRIVER_DEFINITION_ID, di
						.getId());
		try {
			ProfileManager
					.getInstance()
					.createProfile(
							profileName,
							"",
							"org.eclipse.datatools.connectivity.db.derby.embedded.connectionProfile",
							properties);
		} catch (ConnectionProfileException e) {
			e.printStackTrace();
		}

	}

	private static void readProperties(String domainXml, Properties properties)
			throws CoreException {
		String db = "";
		String pass = "";
		String user = "";
		int port = -1;
		String server = "";
		String conAttrib = "";

		try {
			DocumentBuilderFactory domFactory = DocumentBuilderFactory
					.newInstance();
			domFactory.setNamespaceAware(true);
			DocumentBuilder builder = domFactory.newDocumentBuilder();
			Document doc = builder.parse(domainXml);

			XPathFactory factory = XPathFactory.newInstance();
			XPath xpath = factory.newXPath();
			XPathExpression expr = xpath
					.compile("//jdbc-connection-pool[@datasource-classname='org.apache.derby.jdbc.ClientDataSource']/property");
			NodeList nl = (NodeList) expr.evaluate(doc, XPathConstants.NODESET);

			for (int i = 0; i < nl.getLength(); i++) {
				Node node = nl.item(i);
				if (node.getAttributes().getNamedItem("name").getNodeValue().equals(
						"PortNumber")) {
					port = Integer.parseInt(node.getAttributes().getNamedItem(
							"value").getNodeValue());

				} else if (node.getAttributes().getNamedItem("name").getNodeValue().equals(
						"Password")) {
					pass = node.getAttributes().getNamedItem("value")
							.getNodeValue();
				} else if (node.getAttributes().getNamedItem("name").getNodeValue().equals(
						"User")) {
					user = node.getAttributes().getNamedItem("value")
							.getNodeValue();
				} else if (node.getAttributes().getNamedItem("name").getNodeValue().equals(
						"serverName")) {
					server = node.getAttributes().getNamedItem("value")
							.getNodeValue();
				} else if (node.getAttributes().getNamedItem("name").getNodeValue().equals(
						"DatabaseName")) {
					db = node.getAttributes().getNamedItem("value")
							.getNodeValue();
				} else if (node.getAttributes().getNamedItem("name").getNodeValue(). equals(
						"connectionAttributes")) {
					conAttrib = node.getAttributes().getNamedItem("value")
							.getNodeValue();
				}

			}

		} catch (Exception e) {
			throw new CoreException(new Status(IStatus.ERROR,
					"com.sun.enterprise.jst.server.sunappsrv.configurator",
					"Configuration of ports failed because of: " + e));
		}

		properties.setProperty(
				"org.eclipse.datatools.connectivity.db.connectionProperties",
				"");
		properties.setProperty("org.eclipse.datatools.connectivity.db.savePWD",
				"true");
		properties.setProperty(
				"org.eclipse.datatools.connectivity.drivers.defnType",
				"org.eclipse.datatools.connectivity.db.derby102.clientDriver");
		properties.setProperty("jarList", getJarLocation());
		properties.setProperty(
				"org.eclipse.datatools.connectivity.db.username", user);
		properties.setProperty(
				"org.eclipse.datatools.connectivity.db.driverClass",
				"org.apache.derby.jdbc.ClientDriver");
		properties
				.setProperty(
						"org.eclipse.datatools.connectivity.driverDefinitionID",
						"DriverDefn.org.eclipse.datatools.connectivity.db.derby102.clientDriver.Derby Client JDBC Driver");
		properties.setProperty(
				"org.eclipse.datatools.connectivity.db.databaseName", db);
		properties.setProperty(
				"org.eclipse.datatools.connectivity.db.password", pass);
		properties.setProperty("org.eclipse.datatools.connectivity.db.URL",
				"jdbc:derby://" + server + ":" + port + "/" + db + conAttrib);
		properties.setProperty("org.eclipse.datatools.connectivity.db.version",
				"10.2");
		properties.setProperty("org.eclipse.datatools.connectivity.db.vendor",
				"Derby");

	}

	public static String getJarLocation() {
		String property = System.getProperty("gf3location");
		String glassfishLocation = null;
		if (property != null) {
			glassfishLocation = new Path(property).append("javadb").append(
					"lib").append("derbyclient.jar").toPortableString();

		} else {
			try {
				// Get the eclipse installation location and from it V2
				// installation directory.
				glassfishLocation = new Path(Platform.getInstallLocation()
						.getURL().getFile()).append("glassfishv3").append(
						"javadb").append("lib").append("derbyclient.jar")
						.toPortableString();

				SunAppSrvPlugin.logMessage("glassfishV3Loc ="
						+ glassfishLocation);
				return glassfishLocation;
			} catch (Exception e1) {
				e1.printStackTrace();
			}
		}
		return glassfishLocation;
	}

}
