package com.sun.enterprise.jst.server.sunappsrv.spi;

import java.util.Map;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;
import com.sun.enterprise.jst.server.sunappsrv.spi.TreeParser.NodeReader;

public class HttpListenerReader extends NodeReader<Map<String, HttpData>> {

	@Override
    public void readAttributes(String qname, Attributes attributes) throws SAXException {
        // <http-listener 
        //   id="http-listener-1" port="8080" xpowered-by="true" 
        //   enabled="true" address="0.0.0.0" security-enabled="false" 
        //   family="inet" default-virtual-server="server" 
        //   server-name="" blocking-enabled="false" acceptor-threads="1">
		
        try {
            String id = attributes.getValue("id");	//$NON-NLS-1$
           if(id != null && id.length() > 0) {
                int port = Integer.parseInt(attributes.getValue("port"));	//$NON-NLS-1$
                SunAppSrvPlugin.logMessage("PORT is "+port );	//$NON-NLS-1$
               boolean secure = Boolean.TRUE.toString().equals(attributes.getValue("security-enabled"));	//$NON-NLS-1$
                boolean enabled = !Boolean.FALSE.toString().equals(attributes.getValue("enabled"));	//$NON-NLS-1$
                SunAppSrvPlugin.logMessage("secure "+secure );	//$NON-NLS-1$
               if(enabled) {
                    HttpData data = new HttpData(id, port, secure);
                    SunAppSrvPlugin.logMessage(" Adding " + data );	//$NON-NLS-1$
                    result.put(id, data);
                } else {
                    SunAppSrvPlugin.logMessage("http-listener " + id + " is not enabled and won't be used." );	//$NON-NLS-1$ //$NON-NLS-2$
                }
            } else {
                SunAppSrvPlugin.logMessage("http-listener found with no name" );	//$NON-NLS-1$
            }
        } catch(NumberFormatException ex) {
            SunAppSrvPlugin.logMessage("http-listener error reading this"+ex );	//$NON-NLS-1$
          // throw new SAXException(ex);
        }
    }
}
