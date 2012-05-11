package com.sun.enterprise.jst.server.sunappsrv.spi;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;
import com.sun.enterprise.jst.server.sunappsrv.spi.TreeParser.NodeReader;
import com.sun.enterprise.jst.server.sunappsrv.spi.TreeParser.Path;

public class JmxConnectorReader extends NodeReader implements DomainConfigReader {

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
