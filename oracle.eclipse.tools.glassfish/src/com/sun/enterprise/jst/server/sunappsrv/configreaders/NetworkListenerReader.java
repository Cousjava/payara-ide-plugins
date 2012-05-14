package com.sun.enterprise.jst.server.sunappsrv.configreaders;

import java.util.HashMap;
import java.util.Map;

import org.glassfish.tools.ide.server.parser.DomainConfigReader;
import org.glassfish.tools.ide.server.parser.TreeParser.NodeListener;
import org.glassfish.tools.ide.server.parser.TreeParser.Path;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;

public class NetworkListenerReader extends NodeListener implements DomainConfigReader {
	
	public static final String DEFAULT_PATH = "/domain/configs/config/network-config/network-listeners/network-listener";
	
	private String path;
	
	private Map<String, HttpData> result;
	
	public NetworkListenerReader() {
		this(DEFAULT_PATH);
	}
	
	public NetworkListenerReader(String path) {
		this.path = path;
		this.result = new HashMap<String, HttpData>();
	}

	@Override
	public void readAttributes(String qname, Attributes attributes) throws SAXException {
		/*
		<network-listeners>
		<thread-pool max-thread-pool-size="20" min-thread-pool-size="2" thread-pool-id="http-thread-pool" max-queue-size="4096"></thread-pool>
		<network-listener port="8080" protocol="http-listener-1" transport="tcp" name="http-listener-1" thread-pool="http-thread-pool"></network-listener>
		<network-listener port="8181" enabled="false" protocol="http-listener-2" transport="tcp" name="http-listener-2" thread-pool="http-thread-pool"></network-listener>
		<network-listener port="4848" protocol="admin-listener" transport="tcp" name="admin-listener" thread-pool="http-thread-pool"></network-listener>
		</network-listeners>
		 */
		try {
			String id = attributes.getValue("name");
			if(id != null && id.length() > 0) {

				if (attributes.getValue("port").startsWith("$")){  //GlassFish v3.1 : ignore these template entries
					return;
				}
				int port = Integer.parseInt(attributes.getValue("port"));
				boolean secure = "true".equals(attributes.getValue("security-enabled"));
				boolean enabled = !"false".equals(attributes.getValue("enabled"));
				if(enabled) {
					HttpData data = new HttpData(id, port, secure);
					SunAppSrvPlugin.logMessage( " Adding " + data);
					result.put(id, data);
				} else {
					SunAppSrvPlugin.logMessage ("http-listener " + id + " is not enabled and won't be used.");
				}
			} else {
				SunAppSrvPlugin.logMessage( "http-listener found with no name");
			}
		} catch(NumberFormatException ex) {
			throw new SAXException(ex);
		}
	}
	
	@Override
	public Path[] getPathsToListen() {
		return new Path[] {new Path(path, this)};
	}

	public Map<String, HttpData> getResult() {
		return result;
	}
}
