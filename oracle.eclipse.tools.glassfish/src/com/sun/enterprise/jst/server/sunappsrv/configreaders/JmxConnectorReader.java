package com.sun.enterprise.jst.server.sunappsrv.configreaders;

import org.glassfish.tools.ide.server.parser.DomainConfigReader;
import org.glassfish.tools.ide.server.parser.TreeParser.NodeListener;
import org.glassfish.tools.ide.server.parser.TreeParser.Path;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;

public class JmxConnectorReader extends NodeListener implements DomainConfigReader {

	public static final String DEFAULT_PATH = "/domain/configs/config/admin-service/jmx-connector";
	
	private String path;
	private String result;
	
	public JmxConnectorReader() {
		this(DEFAULT_PATH);
	}
	
	public JmxConnectorReader(String path) {
		this.path = path;
	}
	
	@Override
	public void readAttributes(String qname, Attributes attributes)
			throws SAXException {
		/*
        <admin-service type="das-and-server" system-jmx-connector-name="system">
        <jmx-connector ..... port="8686" />
		 */
		String jmxAttr= attributes.getValue("port");	//$NON-NLS-1$
		try{
			int port = Integer.parseInt(jmxAttr);
			result = ""+port;	//$NON-NLS-1$
			SunAppSrvPlugin.logMessage("JMX Port is "+ result );	//$NON-NLS-1$
		} catch(NumberFormatException ex) {
			SunAppSrvPlugin.logMessage("error reading one jmx port"+ex );	//$NON-NLS-1$
		}

	}
	
	@Override
	public Path[] getPathsToListen() {
		return new Path[] {new Path(path, this)};
	}

	public String getResult() {
		return result;
	}

}
