/*
 * Copyright (c) 1997-2010 Oracle and/or its affiliates. All rights reserved.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Sun Microsystems
 *     Oracle
 */

package com.sun.enterprise.jst.server.sunappsrv.derby;

import java.io.File;
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
	private static final String DERBY_FOR_SAMPLE_DB = "GlassFishSampleDB"; //$NON-NLS-1$
	public final static String DERBY_SAMPLE_INSTALL = "derby_sample_dir"; //$NON-NLS-1$
	public final static String DERBY_SAMPLE_ID = "glassfish.javadb"; //$NON-NLS-1$

	static public void configure(IProgressMonitor progressMonitor, File serverDirectory, String domainXml) throws CoreException {
		DriverManager dm = DriverManager.getInstance();

		DriverInstance sample = dm.getDriverInstanceByName(DERBY_FOR_SAMPLE_DB);//$NON-NLS-1$
		// if the sample is already configured we don't need to do anything
		if (sample == null){			

			Properties properties = new Properties();
	
			readProperties(serverDirectory, domainXml, properties);
	
			String profileName = Messages.SAMPLE_JAVADB_DATABASE;
			String description = Messages.SAMPLE_JAVADB_DATABASE_DESCRIPTION;
	
			DriverInstance di = dm.createNewDriverInstance(DERBY_TEMPLATE_ID, DERBY_FOR_SAMPLE_DB, getDerbyClientJarLocation(serverDirectory));
			properties.setProperty(ConnectionProfileConstants.PROP_DRIVER_DEFINITION_ID, di.getId());
	
			try {
				ProfileManager.getInstance().createProfile(profileName, description, DERBY_PROVIDER_ID, properties);
			} catch (ConnectionProfileException e) {
				SunAppSrvPlugin.logMessage("Error Creating Database Profile", e);
			/*	Activator.showErrorAndLog(new Status(IStatus.ERROR, Activator.PLUGIN_ID, e.getMessage(), e), MessageFormat
						.format(Messages.CONFIGURING_SAMPLE_DERBY_DATABASE_ENCOUNTERED_A_PROBLEM, e.getMessage()),
						Messages.EXCEPTION_OCCURRED);*/
			}
		}
		configureSample(progressMonitor);
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
			SunAppSrvPlugin.logMessage("Error Reading DB data", e);
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
	
	public static String getSampleDBLocation(){
		return Platform.getLocation().append(".metadata").append(".plugins").append( //$NON-NLS-1$ //$NON-NLS-2$
				DERBY_SAMPLE_ID).toOSString();
	}

	public static String configureSample(IProgressMonitor progressMonitor) throws CoreException {
		String databaseLocation = getSampleDBLocation();
		File dbDirectory = new File(databaseLocation);
		
		if (new File(dbDirectory,"sun-appserv-samples").exists()) {	// already configured sample db in a previous run, we are done
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
		} catch (Exception e) {
		/*	Activator.showErrorAndLog(new Status(IStatus.ERROR, Activator.PLUGIN_ID,
					MessageFormat.format(Messages.UNZIPPING_DERBY_SAMPLES_ENCOUNTERED_A_PROBLEM_0, e.getMessage()), e), e.getMessage(),
					Messages.EXCEPTION_OCCURRED);*/
			SunAppSrvPlugin.logMessage("error creating java DB database", e);
			}
		return null;

	}
}
