package com.sun.enterprise.jst.server.sunappsrv.spi;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;
import com.sun.enterprise.jst.server.sunappsrv.spi.TreeParser.NodeReader;

public class JmxConnectorReader extends NodeReader<String> {

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

}
