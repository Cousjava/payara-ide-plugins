package com.sun.enterprise.jst.server.sunappsrv;

import java.io.File;
import java.util.LinkedHashMap;
import java.util.Map;

import org.eclipse.wst.server.core.internal.Server;
import org.glassfish.tools.ide.data.GlassFishVersion;
import org.glassfish.tools.ide.server.parser.HttpData;
import org.glassfish.tools.ide.server.parser.HttpListenerReader;
import org.glassfish.tools.ide.server.parser.JmxConnectorReader;
import org.glassfish.tools.ide.server.parser.NetworkListenerReader;
import org.glassfish.tools.ide.server.parser.TreeParser;

public class GlassfishV2Server extends GlassfishGenericServer {

	// For V2 only, the jmx port to issue some MBeans call that return server
	// loc
	private String jmxPort = "8686";

	@Override
	public void setDefaultPublishState() {
		setAttribute(Server.PROP_AUTO_PUBLISH_SETTING,
				Server.AUTO_PUBLISH_DISABLE);
	}

	@Override
	public GlassFishVersion getVersion() {
		return GlassFishVersion.GF_2;
	}

	public String getJMXPort() {
		return jmxPort;
	}

	@Override
	protected boolean readServerConfiguration(File domainXml) {
		boolean result = false;
		final Map<String, HttpData> httpMap = new LinkedHashMap<String, HttpData>();

		if (domainXml.exists()) {
			JmxConnectorReader jmxReader = new JmxConnectorReader();
			HttpListenerReader httpListenerReader = new HttpListenerReader();
			NetworkListenerReader networkListenerReader = new NetworkListenerReader();

			try {
				TreeParser.readXml(domainXml, httpListenerReader,
						networkListenerReader);
				// jmxPort = jmxReader.getResult();

				httpMap.putAll(httpListenerReader.getResult());
				httpMap.putAll(networkListenerReader.getResult());
				// !PW This probably more convoluted than it had to be, but
				// while
				// http-listeners are usually named "http-listener-1",
				// "http-listener-2", ...
				// technically they could be named anything.
				//
				// For now, the logic is as follows:
				// admin port is the one named "admin-listener"
				// http port is the first non-secure enabled port - typically
				// http-listener-1
				// https port is the first secure enabled port - typically
				// http-listener-2
				// disabled ports are ignored.
				//
				HttpData adminData = httpMap.remove("admin-listener"); //$NON-NLS-1$
				int adminPort = adminData.getPort();
				getProps().put(ADMINSERVERPORT,
						String.valueOf(adminData != null ? adminPort : -1)); //$NON-NLS-1$
				SunAppSrvPlugin
						.logMessage("reading from domain.xml adminServerPortNumber=" + getAdminServerPort()); //$NON-NLS-1$

				try {
					int port = Integer.parseInt(jmxReader.getResult());
					jmxPort = "" + port;
				} catch (NumberFormatException e) {
					SunAppSrvPlugin
							.logMessage("error reading one jmx port" + e);
				}

				result = adminPort != -1;
			} catch (IllegalStateException ex) {
				SunAppSrvPlugin.logMessage("error IllegalStateException ", ex); //$NON-NLS-1$
			}
		}
		return result;
	}

	public void setContextRoot(String value) {
		getProps().put(CONTEXTROOT, value);
	}

}
