// <editor-fold defaultstate="collapsed" desc="CDDL+GPL License">
/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 * 
 * Copyright 1997-2009 Sun Microsystems, Inc. All rights reserved.
 * 
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common Development
 * and Distribution License("CDDL") (collectively, the "License").  You
 * may not use this file except in compliance with the License. You can obtain
 * a copy of the License at https://glassfish.dev.java.net/public/CDDL+GPL.html
 * or glassfish/bootstrap/legal/LICENSE.txt.  See the License for the specific
 * language governing permissions and limitations under the License.
 * 
 * When distributing the software, include this License Header Notice in each
 * file and include the License file at glassfish/bootstrap/legal/LICENSE.txt.
 * Sun designates this particular file as subject to the "Classpath" exception
 * as provided by Sun in the GPL Version 2 section of the License file that
 * accompanied this code.  If applicable, add the following below the License
 * Header, with the fields enclosed by brackets [] replaced by your own
 * identifying information: "Portions Copyrighted [year]
 * [name of copyright owner]"
 * 
 * Contributor(s):
 * 
 * If you wish your version of this file to be governed by only the CDDL or
 * only the GPL Version 2, indicate your decision by adding "[Contributor]
 * elects to include this software in this distribution under the [CDDL or GPL
 * Version 2] license."  If you don't indicate a single choice of license, a
 * recipient has the option to distribute your version of this file under
 * either the CDDL, the GPL Version 2 or to extend the choice of license to
 * its licensees as provided above.  However, if you add GPL Version 2 code
 * and therefore, elected the GPL Version 2 license, then the option applies
 * only if the new code is made subject to such option by the copyright
 * holder.
 */
// </editor-fold>

package com.sun.enterprise.jst.server.sunappsrv;

import java.io.File;
import java.io.IOException;
import java.net.ServerSocket;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import javax.management.MBeanServerConnection;
import javax.management.ObjectName;
import javax.management.remote.JMXConnector;
import javax.management.remote.JMXConnectorFactory;
import javax.management.remote.JMXServiceURL;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jst.server.generic.core.internal.GenericServer;
import org.eclipse.jst.server.generic.core.internal.GenericServerRuntime;
import org.eclipse.wst.server.core.IServerWorkingCopy;
import org.eclipse.wst.server.core.ServerPort;
import org.eclipse.wst.server.core.internal.Server;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

import com.sun.enterprise.jst.server.sunappsrv.commands.CommandRunner;
import com.sun.enterprise.jst.server.sunappsrv.commands.Commands;
import com.sun.enterprise.jst.server.sunappsrv.commands.ServerCommand;
import com.sun.enterprise.jst.server.sunappsrv.commands.GlassfishModule.OperationState;
import com.sun.enterprise.jst.server.sunappsrv.spi.TreeParser;




/* wrapper to get rw access to all the props defined in the serverdef files
 *
 **/

public class SunAppServer extends GenericServer {
    
    public static final String ROOTDIR = "sunappserver.rootdirectory";
    // This property does not come from serverdef, but is used there and in the ant files
    // so we set it by synchronizing it with the value from the generic server framework
    public static final String ADDRESS = "sunappserver.serveraddress";
    
// now we read these props from domain.xml
    public static final String SERVERPORT = "sunappserver.serverportnumber";
    public static final String ADMINSERVERPORT = "sunappserver.adminserverportnumber";

    public static final String DOMAINNAME = "sunappserver.domainname";
    public static final String DOMAINDIR = "sunappserver.domaindir";
    public static final String ADMINNAME = "sunappserver.adminname";
    public static final String ADMINPASSWORD = "sunappserver.adminpassword";
    public static final String KEEPSESSIONS = "sunappserver.keepSessions";
    public static final String USEANONYMOUSCONNECTIONS = "sunappserver.useAnonymousConnection";

    public static final String SAMPLEDBDIR = "sunappserver.sampledbdir";
   
    //Default values
    String serverPortNumber="1118080";
    String adminServerPortNumber="1114848";
    String jmxPort = "8686";//For V2 only, the jmx port to issue some MBeans call that return server loc
    boolean initializedCalled = false;
    
    public SunAppServer(){
        SunAppSrvPlugin.logMessage("in SunAppServer CTOR");
     }
	
		
    /* (non-Javadoc)
     * @see org.eclipse.wst.server.core.model.ServerDelegate#initialize()
     */
    @Override
    protected void initialize() {
    	SunAppSrvPlugin.logMessage("in SunAppServer initialize"+this.getServer().getName());
    	super.initialize();
    	SunInitialize();

    }

    private void SunInitialize(){
    	if (initializedCalled){
    		return;
    	}
    	SunAppSrvPlugin.logMessage("in SunAppServer SunInitialize domain is"+getDomainDir());
    	if ((getDomainDir()!=null)&&(!getDomainDir().startsWith("${"))){ //only if we are correctly setup...
    		readServerConfiguration(new File(getDomainDir()+File.separator+getdomainName()+"/config/domain.xml"));
    		SunAppSrvPlugin.logMessage("in SunAppServer initialize done readServerConfiguration");
    		syncHostAndPortsValues();
    		initializedCalled = true;
    	}    	
    }

  public Map<String, String> getProps(){
	  return getServerInstanceProperties();
  }
  
  /* overide needed to store the admin server port  and server port immediately at server creation
   * (non-Javadoc)
   * @see org.eclipse.jst.server.generic.core.internal.GenericServer#setServerInstanceProperties(java.util.Map)
   */
  public void setServerInstanceProperties(Map map) {
	  String domdir = (String)map.get(DOMAINDIR);
	  if ((domdir!=null)&&(!domdir.startsWith("${"))){ //only if we are correctly setup...
		  SunInitialize();
		  map.put(ADMINSERVERPORT, adminServerPortNumber);
		  map.put(SERVERPORT, serverPortNumber);
	  }
	  SunAppSrvPlugin.logMessage("in SunAppServer setServerInstanceProperties new MAP IS"+map);
	  setAttribute(GenericServerRuntime.SERVER_INSTANCE_PROPERTIES, map);
  } 
 
  /* not yet, ui nor working well for generic validation
   * public IStatus validate() {
   
   	   SunAppSrvPlugin.logMessage("in SunAppServer -------validate"+getDomainDir()+getdomainName());
		IStatus s= super.validate();
		
		File f= new File(getDomainDir()+File.separator+getdomainName());
		if (!f.exists()){
			IStatus i= new Status(IStatus.ERROR,  "Glassfish",f.getAbsolutePath()+" does not exist" );
			return i;
		}
 		return s;
 	}	
 	*/
  
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


    /* (non-Javadoc)
     * Override this method to provide our server port for the http port
     * Called by getModuleRootURL in the superclass, which returns 8080 if it 
     * doesn't find something else.  The override of getServerPorts() 
     * implementation which provides our server port as an http port 
     * is not used by the superclass' method, so we must override this as well.
     * @see org.eclipse.jst.server.generic.core.internal.GenericServer#getHttpPort()
     */
    @Override
    protected int getHttpPort() {
	    return Integer.parseInt(serverPortNumber);
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

    public String getSampleDatabaseDir() {
        return (String) getProps().get(SAMPLEDBDIR);
    }

    private void syncHostAndPortsValues() {
    	Map<String, String> props = getProps();
    	String currentHostValue = (String) props.get(ADDRESS);
    	String genericHostValue = getServer().getHost();

    	if ((currentHostValue == null) || !currentHostValue.equals(genericHostValue)) {
    		props.put(ADDRESS, genericHostValue);
    	}

    	String currentServerPortValue = (String) props.get(SERVERPORT);
    	if ((currentServerPortValue == null) || !currentServerPortValue.equals(serverPortNumber)) {
    		props.put(SERVERPORT, serverPortNumber);
    	}
    	String currentServerAdminPortValue = (String) props.get(ADMINSERVERPORT);
    	if ((currentServerAdminPortValue == null) || !currentServerAdminPortValue.equals(adminServerPortNumber)) {
    		props.put(ADMINSERVERPORT, adminServerPortNumber);
    	}
	}

    public void  saveConfiguration(IProgressMonitor m) throws CoreException  {
        SunAppSrvPlugin.logMessage("in Save SunAppServer " +initializedCalled);
        if (initializedCalled==false){
        	SunInitialize(); 
            SunAppSrvPlugin.logMessage("in Save SunAppServer done" );
       }
        syncHostAndPortsValues();

        super.saveConfiguration(m);

    }
    
    
    public String getAdminPassword() {
        
        return (String) getProps().get(ADMINPASSWORD);
    }
    public void setAdminPassword(String value) {
    	getProps().put(ADMINPASSWORD, value); 
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
         SunAppSrvPlugin.logMessage("In  setDefaults for " +this.getServer().getServerType().getName());
        if (isV3()){
    		setAttribute(Server.PROP_AUTO_PUBLISH_SETTING, 2/*Server.AUTO_PUBLISH_OVERRIDE*/);
           	setAttribute(Server.PROP_AUTO_PUBLISH_TIME, 0);
        }
        else{
           	setAttribute(Server.PROP_AUTO_PUBLISH_SETTING, Server.AUTO_PUBLISH_DISABLE);
                    	
        }
        super.setDefaults(monitor);
        
        
    }
	public boolean isV3(){
		//test the server name to contain GlassFish v3
		return (this.getServer().getServerType().getName().indexOf("GlassFish v3")!=-1);
	}
	
	public ServerPort[] getServerPorts() {

		
		try {
			ServerPort[] sp = new ServerPort[2];
			sp[0] = new ServerPort("adminserver", "Admin Server Port", Integer.parseInt(adminServerPortNumber), "HTTP");
			sp[1] = new ServerPort("server", "Server Port", Integer.parseInt(serverPortNumber), "HTTP");

			return sp;
		} catch (Exception e) {
			return new ServerPort[0]; 
		}
	}
    /**
     * 
     * @return true if we can ping to the server
     * @throws CoreException 
     */
    public  boolean isRunning() throws CoreException {

    	if (adminServerPortNumber.equals("1114848")){
    		SunAppSrvPlugin.logMessage("catastrophic state where adminServerPortNumber is not initialize is null in SunAppServer.java");
    		SunAppSrvPlugin.logMessage("catastrophic Only thing to do is restart Eclipse");
    		initialize();
    		/*throw new CoreException(new Status(IStatus.ERROR,  SunAppSrvPlugin.SUNPLUGIN_ID, 
    				IJavaLaunchConfigurationConstants.ERR_INTERNAL_ERROR, 
    				"Error where adminServerPortNumber is not initialized and null in GlassFish Plugin. Restart Eclipse IDE", 
    				new RuntimeException ("Restart Eclipse. Internal State corrupted...")));
			*/

        }
    	return !isPortAvailable(Integer.parseInt(getServerPort()));
    }

    public static boolean isPortAvailable(int port) {
        // if the port is not in the allowed range - return false
        if ((port < 0) && (port > 65535)) {
            return false;
        }

        // if the port is not in the restricted list, we'll try to open a server
        // socket on it, if we fail, then someone is already listening on this port
        // and it is occupied
        ServerSocket socket = null;
        try {
            socket = new ServerSocket(port);
            return true;
        } catch (IOException e) {
            return false;
        } finally {
            if (socket != null) {
                try {
                    socket.close();
                } catch (IOException e) {

                }
            }
        }
    }

    enum ServerStatus {
       	DOMAINDIR_MATCHING,
       	DOMAINDIR_NOT_MATCHING,
    	CONNEXTION_ERROR,
    	CREDENTIAL_ERROR,
    	MBEAN_ERROR,
    	WRONG_SERVER_TYPE
    	
    }
    /**
     * 
     * @return ServerStatus for possible V3 server. If server is not a V3 one, we will know
     *  If the server is a V3 server but with a different install location that this one, we can also detect 
     *  this
     */
    public ServerStatus getV3ServerStatus() {

           Commands.LocationCommand command = new Commands.LocationCommand();
           try {
                Future<OperationState> result = execute(command);
                if(result.get(30, TimeUnit.SECONDS) == OperationState.COMPLETED) {
                	String installRoot = this.getDomainDir()+File.separator+this.getdomainName();
                	String targetInstallRoot = command.getDomainRoot();
                	//SunAppSrvPlugin.logMessage("IsReady is targetInstallRoot="+targetInstallRoot );
                	//SunAppSrvPlugin.logMessage("IsReady is installRoot="+installRoot );
                	if(installRoot != null && targetInstallRoot != null) {
                		File installDir = new File(installRoot);
                		File targetInstallDir = new File(targetInstallRoot);
                		if (installDir.getCanonicalPath().equals(targetInstallDir.getCanonicalPath())){
                			SunAppSrvPlugin.logMessage("getV3ServerStatus DOMAINDIR_MATCHING" );
                			return ServerStatus.DOMAINDIR_MATCHING;

                		}
                		else {
                			SunAppSrvPlugin.logMessage("getV3ServerStatus DOMAINDIR_NOT_MATCHING" );
                			return ServerStatus.DOMAINDIR_NOT_MATCHING;

                		}
                	} else {
                		SunAppSrvPlugin.logMessage("getV3ServerStatus 3 DOMAINDIR_NOT_MATCHING" );
                		return ServerStatus.DOMAINDIR_NOT_MATCHING;
                	}
                } else  {
                	SunAppSrvPlugin.logMessage("apparently CREDENTIAL_ERROR" );
                	return ServerStatus.CREDENTIAL_ERROR;

                }
            } catch(Exception ex) {
                SunAppSrvPlugin.logMessage("IsReady is failing=",ex );
                SunAppSrvPlugin.logMessage("getV3ServerStatus 2 CONNEXTION_ERROR" );
              	return ServerStatus.CONNEXTION_ERROR;
            }
      

    }    
    /**
     * 
     * @return ServerStatus for possible V2 server. If server is not a V2 one, we will know
     *  If the server is a V2 server but with a different install location that this one, we can also detect 
     *  this
     */   
    public ServerStatus getV2ServerStatus(){
    	JMXConnector jmxc = null;
    	try{

    		// Create an RMI connector client
    		//
    		JMXServiceURL url = new JMXServiceURL(
    		"service:jmx:rmi:///jndi/rmi://"+getServer().getHost()+":"+jmxPort+"/jmxrmi");
    		HashMap env = new HashMap();
    		env.put(JMXConnector.CREDENTIALS, new String[]{ getAdminName(), getAdminPassword()});
            SunAppSrvPlugin.logMessage("service:jmx:rmi:///jndi/rmi://"+getServer().getHost()+":"+jmxPort+"/jmxrmi" );
    		jmxc = JMXConnectorFactory.connect(url, env);
    		SunAppSrvPlugin.logMessage("after JMXConnectorFactory");
    		MBeanServerConnection mbsc = jmxc.getMBeanServerConnection();
   			ObjectName on = new ObjectName("com.sun.appserv:type=domain,category=config");

    		Object o = mbsc.invoke(on, "getConfigDir", null, null);
    		SunAppSrvPlugin.logMessage("mbsc.invoke="+o);
   		if (o != null) {
    			File domainDir=new File(""+o).getParentFile();
                File knownDomainRoot = new File(this.getDomainDir()+File.separator+this.getdomainName());
                if (domainDir.getCanonicalPath().equals(knownDomainRoot.getCanonicalPath())){
                	return ServerStatus.DOMAINDIR_MATCHING;
                }else {
                	return ServerStatus.DOMAINDIR_NOT_MATCHING;
               	
                }
    		}
    		else {
                SunAppSrvPlugin.logMessage("V2 not ready yet: o=null" );
            	return ServerStatus.MBEAN_ERROR;
   		}
   		

    	} catch (Exception e) {
            SunAppSrvPlugin.logMessage("V2 not ready yet:",e );
           	return ServerStatus.CONNEXTION_ERROR;
            
    	} finally {
    		if (jmxc!=null){
        		try {
					jmxc.close();
				} catch (IOException e) {
					// what can we do there
					
				}
   			
    		}
    	}

    }   
    public Future<OperationState> execute(ServerCommand command) {
        CommandRunner mgr = new CommandRunner(this);
        return mgr.execute(command);
    }   
     boolean readServerConfiguration(File domainXml) {
        boolean result = false;
        final Map<String, HttpData> httpMap = new LinkedHashMap<String, HttpData>();
        
        if (domainXml.exists()) {
            List<TreeParser.Path> pathList = new ArrayList<TreeParser.Path>();
            pathList.add(new TreeParser.Path("/domain/configs/config/admin-service/jmx-connector",
                    new TreeParser.NodeReader() {
                @Override
                public void readAttributes(String qname, Attributes attributes) throws SAXException {
 /*
        <admin-service type="das-and-server" system-jmx-connector-name="system">
        <jmx-connector ..... port="8686" />
  */
                	String jmxAttr= attributes.getValue("port");
                	try{
                		int port = Integer.parseInt(jmxAttr);
                		jmxPort = ""+port;
                		SunAppSrvPlugin.logMessage("JMX Port is "+jmxPort );
                	} catch(NumberFormatException ex) {
                        SunAppSrvPlugin.logMessage("error reading one jmx port"+ex );

                	}
                    
                }
            }));
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
                            SunAppSrvPlugin.logMessage("PORT is "+port );
                           boolean secure = "true".equals(attributes.getValue("security-enabled"));
                            boolean enabled = !"false".equals(attributes.getValue("enabled"));
                            SunAppSrvPlugin.logMessage("secure "+secure );
                           if(enabled) {
                                HttpData data = new HttpData(id, port, secure);
                                SunAppSrvPlugin.logMessage(" Adding " + data );
                                httpMap.put(id, data);
                            } else {
                                SunAppSrvPlugin.logMessage("http-listener " + id + " is not enabled and won't be used." );
                            }
                        } else {
                            SunAppSrvPlugin.logMessage("http-listener found with no name" );
                        }
                    } catch(NumberFormatException ex) {
                        SunAppSrvPlugin.logMessage("http-listener error reading this"+ex );
                      // throw new SAXException(ex);
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
                SunAppSrvPlugin.logMessage("error IllegalStateException ",ex);
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
    

