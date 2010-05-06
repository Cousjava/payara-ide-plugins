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
package com.sun.enterprise.jst.server.sunappsrv.derby;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.HashMap;
import java.util.Properties;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathFactory;

import org.apache.tools.ant.listener.TimestampedLogger;
import org.eclipse.ant.core.AntRunner;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Platform;
import org.eclipse.datatools.connectivity.ConnectionProfileConstants;
import org.eclipse.datatools.connectivity.ConnectionProfileException;
import org.eclipse.datatools.connectivity.ProfileManager;
import org.eclipse.datatools.connectivity.drivers.DriverInstance;
import org.eclipse.datatools.connectivity.drivers.DriverManager;
import org.osgi.framework.Bundle;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;

public class DerbyConfigurator {

	private static final String DERBY_PROVIDER_ID = "org.eclipse.datatools.connectivity.db.derby.embedded.connectionProfile"; //$NON-NLS-1$
	private static final String DERBY_TEMPLATE_ID = "org.eclipse.datatools.connectivity.db.derby102.clientDriver"; //$NON-NLS-1$
	private static final String DERBY_FOR_SAMPLE_DB = "DerbyForSampleDB"; //$NON-NLS-1$
	public final static String DERBY_SAMPLE_INSTALL = "derby_sample_dir"; //$NON-NLS-1$
	public final static String DERBY_SAMPLE_ID = "com.sun.enterprise.jst.server.derbysample"; //$NON-NLS-1$

	static public void configure(IProgressMonitor progressMonitor, File serverDirectory, String domainXml) throws CoreException {
		DriverManager dm = DriverManager.getInstance();

		DriverInstance sample = dm.getDriverInstanceByName(DERBY_FOR_SAMPLE_DB);//$NON-NLS-1$
		// if the sample is already configured we don't need to do anything
		if (sample != null)
			return;

		Properties properties = new Properties();

		readProperties(serverDirectory, domainXml, properties);

		String profileName = Messages.SAMPLE_JAVADB_DATABASE;
		String description = Messages.SAMPLE_JAVADB_DATABASE_DESCRIPTION;

		DriverInstance di = dm.createNewDriverInstance(DERBY_TEMPLATE_ID, DERBY_FOR_SAMPLE_DB, getDerbyClientJarLocation(serverDirectory));
		properties.setProperty(ConnectionProfileConstants.PROP_DRIVER_DEFINITION_ID, di.getId());

		try {
			ProfileManager.getInstance().createProfile(profileName, description, DERBY_PROVIDER_ID, properties);
		} catch (ConnectionProfileException e) {
			SunAppSrvPlugin.logMessage("erororor", e);
		/*	Activator.showErrorAndLog(new Status(IStatus.ERROR, Activator.PLUGIN_ID, e.getMessage(), e), MessageFormat
					.format(Messages.CONFIGURING_SAMPLE_DERBY_DATABASE_ENCOUNTERED_A_PROBLEM, e.getMessage()),
					Messages.EXCEPTION_OCCURRED);*/
		}
	}

	private static void readProperties(File serverDirectory, String domainXml, Properties properties) {
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
	/*		Activator.showErrorAndLog(new Status(IStatus.ERROR, Activator.PLUGIN_ID,
					MessageFormat
					.format(Messages.CONFIGURATION_OF_PORTS_FAILED_BECAUSE_OF , e.getMessage()), e), e.getMessage(),
					Messages.EXCEPTION_OCCURRED);*/
			SunAppSrvPlugin.logMessage("erororor", e);
		}

		properties.setProperty("org.eclipse.datatools.connectivity.db.connectionProperties", ""); //$NON-NLS-1$ //$NON-NLS-2$
		properties.setProperty("org.eclipse.datatools.connectivity.db.savePWD", "true"); //$NON-NLS-1$ //$NON-NLS-2$
		properties.setProperty("org.eclipse.datatools.connectivity.drivers.defnType", DERBY_TEMPLATE_ID); //$NON-NLS-1$
		properties.setProperty("jarList", getDerbyClientJarLocation(serverDirectory)); //$NON-NLS-1$
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
	
    // Get the derby location from the server location in eclipse installation 

	private static String getDerbyClientJarLocation(File glassfishLocation) {

        String derbyLocation = new File(glassfishLocation.getAbsolutePath()).getParentFile().getAbsolutePath() + "/javadb/lib/derbyclient.jar"; //$NON-NLS-1$ 

     //   Activator.getDefault().getLog().log(new Status(IStatus.INFO, Activator.PLUGIN_ID, "derbyJar =" + glassfishLocation)); //$NON-NLS-1$
		SunAppSrvPlugin.logMessage("derbyJar =" + glassfishLocation, null);

        return derbyLocation;
	}

	public static String configureSample(IProgressMonitor progressMonitor) throws CoreException {
		String databaseLocation = Platform.getLocation().append(".metadata").append(".plugins").append( //$NON-NLS-1$ //$NON-NLS-2$
				DERBY_SAMPLE_ID).toOSString();
		File dbDirectory = new File(databaseLocation);
		
		if (dbDirectory.exists()) {	// already configured sample db in a previous run, we are done
			return databaseLocation;
		}

		// We use ant as it is better suited for this task.
		AntRunner ant = new AntRunner();
		HashMap<String, String> map = new HashMap<String, String>();

		map.put(DERBY_SAMPLE_INSTALL, databaseLocation);

		try {
			Bundle bundle = Platform.getBundle(SunAppSrvPlugin.SUNPLUGIN_ID);
			URL xml = bundle.getResource("derbysetup/unzipSampleDB.xml"); //$NON-NLS-1$
			String antFile = FileLocator.toFileURL(xml).getFile();

			// even though we don't use this here in the code, we must access it to make sure
			// it is available in the ant file (if we don't, it is not copied into the 
			// relevant osgi directory with the other libs)
			URL dbURL = bundle.getResource("derbysetup/derbysampledb.zip"); //$NON-NLS-1$
			@SuppressWarnings("unused")
			String dbSampleFile = FileLocator.toFileURL(dbURL).getFile();
			// end required access

			ant.setBuildFileLocation(antFile);
			ant.addUserProperties(map);
			// FIXME in production remove those lines, no need for the user to
			// see that much info
			ant.setArguments("-Dmessage=Building -verbose"); //$NON-NLS-1$
			ant.addBuildLogger(TimestampedLogger.class.getName());

			ant.run();
			return databaseLocation;
		} catch (IOException e) {
		/*	Activator.showErrorAndLog(new Status(IStatus.ERROR, Activator.PLUGIN_ID,
					MessageFormat.format(Messages.UNZIPPING_DERBY_SAMPLES_ENCOUNTERED_A_PROBLEM_0, e.getMessage()), e), e.getMessage(),
					Messages.EXCEPTION_OCCURRED);*/
			SunAppSrvPlugin.logMessage("erororor   cd", e);
			}
		return null;

	}
}
