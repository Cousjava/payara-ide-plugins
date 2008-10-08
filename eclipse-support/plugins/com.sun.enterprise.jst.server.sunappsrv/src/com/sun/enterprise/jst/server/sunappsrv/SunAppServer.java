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

import java.net.InetSocketAddress;
import java.net.Socket;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jst.server.generic.core.internal.GenericServer;
import org.eclipse.wst.server.core.IServerWorkingCopy;
import org.eclipse.wst.server.core.internal.Server;




/* wrapper to get rw access to all the props defined in the serverdef files
 *
 **/

public class SunAppServer extends GenericServer {
    
    public static final String ROOTDIR = "sunappserver.rootdirectory";
    // This property does not come from serverdef, but is used there and in the ant files
    // so we set it by synchronizing it with the value from the generic server framework
    public static final String ADDRESS = "sunappserver.serveraddress";
    public static final String SERVERPORT = "sunappserver.serverportnumber";
    public static final String ADMINSERVERPORT = "sunappserver.adminserverportnumber";
    public static final String DOMAINNAME = "sunappserver.domainname";
    public static final String DOMAINDIR = "sunappserver.domaindir";
    public static final String ADMINNAME = "sunappserver.adminname";
    public static final String ADMINPASSWORD = "sunappserver.adminpassword";
    public static final String KEEPSESSIONS = "sunappserver.keepSessions";
    public static final String USEANONYMOUSCONNECTIONS = "sunappserver.useAnonymousConnection";
       
    
    public SunAppServer(){
    }
	
	
    /* (non-Javadoc)
     * @see org.eclipse.wst.server.core.model.ServerDelegate#initialize()
     */
    @Override
    protected void initialize() {
        super.initialize();
        syncHostValues();
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
        return (String) getProps().get(SERVERPORT);
    }
    public void setServerPort(String value) {
    	getProps().put(SERVERPORT, value);
    }
    public String getAdminServerPort() {
        return (String) getProps().get(ADMINSERVERPORT);
    }
    public void setAdminServerPort(String value) {
    	getProps().put(ADMINSERVERPORT, value);
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
    
    
    
    
}
