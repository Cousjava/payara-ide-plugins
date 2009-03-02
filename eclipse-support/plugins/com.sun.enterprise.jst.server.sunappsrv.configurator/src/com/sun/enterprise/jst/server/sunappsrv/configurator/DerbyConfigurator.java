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
import java.text.MessageFormat;
import java.util.Properties;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
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
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class DerbyConfigurator {

	private static final String DERBY_PROVIDER_ID = "org.eclipse.datatools.connectivity.db.derby.embedded.connectionProfile"; //$NON-NLS-1$
	private static final String DERBY_TEMPLATE_ID = "org.eclipse.datatools.connectivity.db.derby102.clientDriver"; //$NON-NLS-1$
	private static final String DERBY_FOR_SAMPLE_DB = "DerbyForSampleDB"; //$NON-NLS-1$

	static void configure(IProgressMonitor progressMonitor, String domainXml) throws CoreException {
		DriverManager dm = DriverManager.getInstance();

		DriverInstance sample = dm.getDriverInstanceByName(DERBY_FOR_SAMPLE_DB);//$NON-NLS-1$
		// if the sample is already configured we don't need to do anything
		if (sample != null)
			return;

		Properties properties = new Properties();
		readProperties(domainXml, properties);

		String profileName = Messages.SAMPLE_JAVADB_DATABASE;
		String description = Messages.SAMPLE_JAVADB_DATABASE_DESCRIPTION;

		DriverInstance di = dm.createNewDriverInstance(DERBY_TEMPLATE_ID, DERBY_FOR_SAMPLE_DB, getJarLocation());
		properties.setProperty(ConnectionProfileConstants.PROP_DRIVER_DEFINITION_ID, di.getId());

		try {
			ProfileManager.getInstance().createProfile(profileName, description, DERBY_PROVIDER_ID, properties);
		} catch (ConnectionProfileException e) {
			Activator.showErrorAndLog(new Status(IStatus.ERROR, Activator.PLUGIN_ID, e.getMessage(), e), MessageFormat
					.format(Messages.CONFIGURING_SAMPLE_DERBY_DATABASE_ENCOUNTERED_A_PROBLEM, e.getMessage()),
					Messages.EXCEPTION_OCCURRED);
		}
	}

	private static void readProperties(String domainXml, Properties properties) {
		String db = ""; //$NON-NLS-1$
		String pass = ""; //$NON-NLS-1$
		String user = ""; //$NON-NLS-1$
		int port = -1;
		String server = ""; //$NON-NLS-1$
		String conAttrib = ""; //$NON-NLS-1$

		try {
			DocumentBuilderFactory domFactory = DocumentBuilderFactory.newInstance();
			domFactory.setNamespaceAware(true);
			DocumentBuilder builder = domFactory.newDocumentBuilder();
			Document doc = builder.parse(new File(domainXml));

			XPathFactory factory = XPathFactory.newInstance();
			XPath xpath = factory.newXPath();
			XPathExpression expr = xpath
					.compile("//jdbc-connection-pool[@datasource-classname='org.apache.derby.jdbc.ClientDataSource']/property"); //$NON-NLS-1$
			NodeList nl = (NodeList) expr.evaluate(doc, XPathConstants.NODESET);

			for (int i = 0; i < nl.getLength(); i++) {
				Node node = nl.item(i);
				if (node.getAttributes().getNamedItem("name").getNodeValue().equals("PortNumber")) { //$NON-NLS-1$ //$NON-NLS-2$
					port = Integer.parseInt(node.getAttributes().getNamedItem("value").getNodeValue()); //$NON-NLS-1$

				} else if (node.getAttributes().getNamedItem("name").getNodeValue().equals("Password")) { //$NON-NLS-1$ //$NON-NLS-2$
					pass = node.getAttributes().getNamedItem("value").getNodeValue(); //$NON-NLS-1$
				} else if (node.getAttributes().getNamedItem("name").getNodeValue().equals("User")) { //$NON-NLS-1$ //$NON-NLS-2$
					user = node.getAttributes().getNamedItem("value").getNodeValue(); //$NON-NLS-1$
				} else if (node.getAttributes().getNamedItem("name").getNodeValue().equals("serverName")) { //$NON-NLS-1$ //$NON-NLS-2$
					server = node.getAttributes().getNamedItem("value").getNodeValue(); //$NON-NLS-1$
				} else if (node.getAttributes().getNamedItem("name").getNodeValue().equals("DatabaseName")) { //$NON-NLS-1$ //$NON-NLS-2$
					db = node.getAttributes().getNamedItem("value").getNodeValue(); //$NON-NLS-1$
				} else if (node.getAttributes().getNamedItem("name").getNodeValue().equals("connectionAttributes")) { //$NON-NLS-1$ //$NON-NLS-2$
					conAttrib = node.getAttributes().getNamedItem("value").getNodeValue(); //$NON-NLS-1$
				}

			}

		} catch (Exception e) {
			Activator.showErrorAndLog(new Status(IStatus.ERROR, Activator.PLUGIN_ID,
					MessageFormat
					.format(Messages.CONFIGURATION_OF_PORTS_FAILED_BECAUSE_OF , e.getMessage()), e), e.getMessage(),
					Messages.EXCEPTION_OCCURRED);
		}

		properties.setProperty("org.eclipse.datatools.connectivity.db.connectionProperties", ""); //$NON-NLS-1$ //$NON-NLS-2$
		properties.setProperty("org.eclipse.datatools.connectivity.db.savePWD", "true"); //$NON-NLS-1$ //$NON-NLS-2$
		properties.setProperty("org.eclipse.datatools.connectivity.drivers.defnType", DERBY_TEMPLATE_ID); //$NON-NLS-1$
		properties.setProperty("jarList", getJarLocation()); //$NON-NLS-1$
		properties.setProperty("org.eclipse.datatools.connectivity.db.username", user); //$NON-NLS-1$
		properties.setProperty("org.eclipse.datatools.connectivity.db.driverClass", //$NON-NLS-1$
				"org.apache.derby.jdbc.ClientDriver"); //$NON-NLS-1$
		properties.setProperty("org.eclipse.datatools.connectivity.driverDefinitionID", //$NON-NLS-1$
				"DriverDefn.org.eclipse.datatools.connectivity.db.derby102.clientDriver.Derby Client JDBC Driver"); //$NON-NLS-1$
		properties.setProperty("org.eclipse.datatools.connectivity.db.databaseName", db); //$NON-NLS-1$
		properties.setProperty("org.eclipse.datatools.connectivity.db.password", pass); //$NON-NLS-1$
		properties.setProperty("org.eclipse.datatools.connectivity.db.URL", "jdbc:derby://" + server + ":" + port + "/" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
				+ db + conAttrib);
		properties.setProperty("org.eclipse.datatools.connectivity.db.version", "10.2"); //$NON-NLS-1$ //$NON-NLS-2$
		properties.setProperty("org.eclipse.datatools.connectivity.db.vendor", "Derby"); //$NON-NLS-1$ //$NON-NLS-2$

	}

	private static String getJarLocation() {
		String property = System.getProperty("gf3location"); //$NON-NLS-1$
		String glassfishLocation = null;
		if (property != null) {
			glassfishLocation = new Path(property).append("javadb").append("lib").append("derbyclient.jar") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
					.toPortableString();

		} else {
			try {
				// Get the eclipse installation location and from it V2
				// installation directory.
				glassfishLocation = new Path(Platform.getInstallLocation().getURL().getFile()).append(
						"glassfishv3prelude").append("javadb").append("lib").append("derbyclient.jar") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
						.toPortableString();

				Activator.getDefault().getLog().log(
						new Status(IStatus.INFO, Activator.PLUGIN_ID, "derbyJar =" + glassfishLocation)); //$NON-NLS-1$
				return glassfishLocation;
			} catch (Exception e) {
				Activator
						.showErrorAndLog(new Status(IStatus.ERROR, Activator.PLUGIN_ID,
								MessageFormat
								.format(Messages.PROBLEM_GETTING_DERBY_JAR_LOCATION, e.getMessage()), e), e.getMessage(),
								Messages.EXCEPTION_OCCURRED);
			}
		}
		return glassfishLocation;
	}

}
