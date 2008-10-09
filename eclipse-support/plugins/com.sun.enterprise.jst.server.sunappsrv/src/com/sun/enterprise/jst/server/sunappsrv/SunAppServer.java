// <editor-fold defaultstate="collapsed" desc="CDDL Licence">
/*
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * glassfishplugins/www/license/CDDLv1.0.txt or
 * https://glassfish.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * glassfishplugins/www/license/CDDLv1.0.txt.  If applicable,
 * add the following below this CDDL HEADER, with the
 * fields enclosed by brackets "[]" replaced with your
 * own identifying information: Portions Copyright [yyyy]
 * [name of copyright owner]
 */
// </editor-fold>

package com.sun.enterprise.jst.server.sunappsrv;

import java.io.File;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jst.server.generic.core.internal.GenericServer;
import org.eclipse.wst.server.core.IServerWorkingCopy;
import org.eclipse.wst.server.core.internal.Server;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

import com.sun.enterprise.jst.server.sunappsrv.spi.TreeParser;




/* wrapper to get rw access to all the props defined in the serverdef files
 *
 **/

public class SunAppServer extends GenericServer {
    
    public static final String ROOTDIR = "sunappserver.rootdirectory";
    // This property does not come from serverdef, but is used there and in the ant files
    // so we set it by synchronizing it with the value from the generic server framework
    public static final String ADDRESS = "sunappserver.serveraddress";
    
/* now we read these props from domain.xml
    public static final String SERVERPORT = "sunappserver.serverportnumber";
    public static final String ADMINSERVERPORT = "sunappserver.adminserverportnumber";
*/
    public static final String DOMAINNAME = "sunappserver.domainname";
    public static final String DOMAINDIR = "sunappserver.domaindir";
    public static final String ADMINNAME = "sunappserver.adminname";
    public static final String ADMINPASSWORD = "sunappserver.adminpassword";
    public static final String KEEPSESSIONS = "sunappserver.keepSessions";
    public static final String USEANONYMOUSCONNECTIONS = "sunappserver.useAnonymousConnection";
    
    //Default values
    String serverPortNumber="8080";
    String adminServerPortNumber="4848";
    
    public SunAppServer(){
    }
	
	
    /* (non-Javadoc)
     * @see org.eclipse.wst.server.core.model.ServerDelegate#initialize()
     */
    @Override
    protected void initialize() {
        super.initialize();
        syncHostValues();
        readServerConfiguration(new File(getDomainDir()+File.separator+getdomainName()+"/config/domain.xml"));
    }

  public Map<String, String> getProps(){
	  return getServerInstanceProperties();
  }
    
  public String getKeepSessions() {
	  String s =getProps().get(KEEPSESSIONS);
	  if (s==null){
		  s = "true";
	  }
      return  s;
  }
  public void setKeepSessions(String value) {
  	getProps().put(KEEPSESSIONS, value);
  }
  
  public String getUseAnonymousConnections() {
	  String s =getProps().get(USEANONYMOUSCONNECTIONS);
	  if (s==null){
		  s = "true";
	  }
      return  s;
  }
  public void setUseAnonymousConnections(String value) {
  	getProps().put(USEANONYMOUSCONNECTIONS, value);
  }

  public String getServerPort() {
        return serverPortNumber;
    }


    public String getAdminServerPort() {
        return adminServerPortNumber;
    }


    
    public String getAdminName() {
        return (String) getProps().get(ADMINNAME);
    }
    public void setAdminName(String value) {
    	getProps().put(ADMINNAME, value);
    }
        
    public String getdomainName() {
        return (String) getProps().get(DOMAINNAME);
    }
     
    public String getDomainDir() {
        return (String) getProps().get(DOMAINDIR);
    }

    private void syncHostValues() {
    	Map<String, String> props = getProps();
    	String currentHostValue = (String) props.get(ADDRESS);
    	String genericHostValue = getServer().getHost();

    	if ((currentHostValue == null) || !currentHostValue.equals(genericHostValue)) {
    		props.put(ADDRESS, genericHostValue);
    	}    	
    }

    public void  saveConfiguration(IProgressMonitor m) throws CoreException  {
        SunAppSrvPlugin.logMessage("in Save SunAppServer ");
        syncHostValues();
        super.saveConfiguration(m);
    }
    
    
    public String getAdminPassword() {
        
        return (String) getProps().get(ADMINPASSWORD);
    }
    public void setAdminPassword(String value) {
    	getProps().put(ADMINPASSWORD, value); 
        SunAppSrvPlugin.logMessage("In  setAdminPassword)"+ value);
        try {
            //this.saveConfiguration(new NullProgressMonitor());
            this.configurationChanged();
        } catch (Exception ex) {
            SunAppSrvPlugin.logMessage("error ="+ ex);
        }
    }
    
    
    public static SunAppServer getSunAppServer(IServerWorkingCopy server){
        SunAppServer  sunserver = (SunAppServer) server.getAdapter(SunAppServer.class);
        if (sunserver == null) {
            sunserver = (SunAppServer) server.loadAdapter(SunAppServer.class, new NullProgressMonitor());
        }
        return sunserver;
    }
    
    /* (non-Javadoc)
     * @see org.eclipse.jst.server.generic.core.internal.GenericServer#setDefaults(org.eclipse.core.runtime.IProgressMonitor)
     * This implementation overrides the automatic publishing option to disable it by default.
     */
    @Override
    public void setDefaults(IProgressMonitor monitor) {
        setAttribute(Server.PROP_AUTO_PUBLISH_SETTING, Server.AUTO_PUBLISH_DISABLE);
        super.setDefaults(monitor);
    }

    /**
     * 
     * @param host 
     * @param port 
     * @return 
     */
    public  boolean isRunning() {

        
        try {
            InetSocketAddress isa = new InetSocketAddress(getServer().getHost(), Integer.parseInt(getServerPort()));
            Socket socket = new Socket();
            socket.connect(isa, 1);
            socket.close();
            return true;
        } catch (Exception e) {
            return false;
        }
    }
    
    
     boolean readServerConfiguration(File domainXml) {
        boolean result = false;
        final Map<String, HttpData> httpMap = new LinkedHashMap<String, HttpData>();
        
        if (domainXml.exists()) {
            List<TreeParser.Path> pathList = new ArrayList<TreeParser.Path>();
            pathList.add(new TreeParser.Path("/domain/configs/config/http-service/http-listener",
                    new TreeParser.NodeReader() {
                @Override
                public void readAttributes(String qname, Attributes attributes) throws SAXException {
                    // <http-listener 
                    //   id="http-listener-1" port="8080" xpowered-by="true" 
                    //   enabled="true" address="0.0.0.0" security-enabled="false" 
                    //   family="inet" default-virtual-server="server" 
                    //   server-name="" blocking-enabled="false" acceptor-threads="1">
                    try {
                        String id = attributes.getValue("id");
                        if(id != null && id.length() > 0) {
                            int port = Integer.parseInt(attributes.getValue("port"));
                            boolean secure = "true".equals(attributes.getValue("security-enabled"));
                            boolean enabled = !"false".equals(attributes.getValue("enabled"));
                            if(enabled) {
                                HttpData data = new HttpData(id, port, secure);
                                Logger.getLogger("glassfish").log(Level.FINER, " Adding " + data);
                                httpMap.put(id, data);
                            } else {
                                Logger.getLogger("glassfish").log(Level.FINER, "http-listener " + id + " is not enabled and won't be used.");
                            }
                        } else {
                            Logger.getLogger("glassfish").log(Level.FINEST, "http-listener found with no name");
                        }
                    } catch(NumberFormatException ex) {
                        throw new SAXException(ex);
                    }
                }
            }));
            
            try {
                TreeParser.readXml(domainXml, pathList);
                
                // !PW This probably more convoluted than it had to be, but while
                // http-listeners are usually named "http-listener-1", "http-listener-2", ...
                // technically they could be named anything.
                // 
                // For now, the logic is as follows:
                //   admin port is the one named "admin-listener"
                //   http port is the first non-secure enabled port - typically http-listener-1
                //   https port is the first secure enabled port - typically http-listener-2
                // disabled ports are ignored.
                //
                HttpData adminData = httpMap.remove("admin-listener");
                
               adminServerPortNumber =""+(adminData != null ? adminData.getPort() : -1);
               SunAppSrvPlugin.logMessage("reading from domain.xml adminServerPortNumber="+adminServerPortNumber );
               
                
                HttpData httpData = null;
                HttpData httpsData = null;
                
                for(HttpData data: httpMap.values()) {
                    if(data.isSecure()) {
                        if(httpsData == null) {
                            httpsData = data;
                        }
                    } else {
                        if(httpData == null) {
                            httpData = data;
                        }
                    }
                    if(httpData != null && httpsData != null) {
                        break;
                    }
                }
                
                int httpPort = httpData != null ? httpData.getPort() : -1;
                serverPortNumber= ""+httpPort;
                SunAppSrvPlugin.logMessage("reading from domain.xml serverPortNumber="+serverPortNumber );
                /////ludo secure TODO   wi.setHttpsPort(httpsData != null ? httpsData.getPort() : -1);
                
                result = httpPort != -1;
            } catch(IllegalStateException ex) {
                Logger.getLogger("glassfish").log(Level.INFO, ex.getLocalizedMessage(), ex);
            }
        }
        return result;
    }
    
    private static class HttpData {

        private final String id;
        private final int port;
        private final boolean secure;
        
        public HttpData(String id, int port, boolean secure) {
            this.id = id;
            this.port = port;
            this.secure = secure;
        }
        
        public String getId() {
            return id;
        }

        public int getPort() {
            return port;
        }

        public boolean isSecure() {
            return secure;
        }
        
        @Override
        public String toString() {
            return "{ " + id + ", " + port + ", " + secure + " }";
        }
        
    }
}       
    

