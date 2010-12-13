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


package com.sun.enterprise.jst.server.sunappsrv;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.IOException;
import java.net.ServerSocket;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
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
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants;
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
import com.sun.enterprise.jst.server.sunappsrv.commands.ServerCommand.SetPropertyCommand;
import com.sun.enterprise.jst.server.sunappsrv.derby.DerbyConfigurator;
import com.sun.enterprise.jst.server.sunappsrv.spi.TreeParser;




/* wrapper to get rw access to all the props defined in the serverdef files
 *
 **/

@SuppressWarnings("restriction")
public class SunAppServer extends GenericServer {
    
    public static final String ROOTDIR = "sunappserver.rootdirectory";	//$NON-NLS-1$
    // This property does not come from serverdef, but is used there and in the ant files
    // so we set it by synchronizing it with the value from the generic server framework
    public static final String ADDRESS = "sunappserver.serveraddress";	//$NON-NLS-1$
    
// now we read these props from domain.xml
    public static final String SERVERPORT = "sunappserver.serverportnumber";	//$NON-NLS-1$
    public static final String ADMINSERVERPORT = "sunappserver.adminserverportnumber";	//$NON-NLS-1$

    public static final String DEPRECATED_DOMAINNAME = "sunappserver.domainname";	//$NON-NLS-1$
    public static final String DEPRECATED_DOMAINDIR = "sunappserver.domaindir";	//$NON-NLS-1$
    public static final String DOMAINPATH = "sunappserver.domainpath";	//$NON-NLS-1$
    public static final String ADMINNAME = "sunappserver.adminname";	//$NON-NLS-1$
    public static final String ADMINPASSWORD = "sunappserver.adminpassword";	//$NON-NLS-1$
    public static final String KEEPSESSIONS = "sunappserver.keepSessions";	//$NON-NLS-1$
    public static final String FASTDEPLOY = "sunappserver.fastDeploy";	//$NON-NLS-1$
    public static final String USEANONYMOUSCONNECTIONS = "sunappserver.useAnonymousConnection";	//$NON-NLS-1$

    public static final String SAMPLEDBDIR = "sunappserver.sampledbdir";	//$NON-NLS-1$
   
    // used only for v2, populated from project properties or module name with no space
    public static final String CONTEXTROOT = "sunappserver.contextroot";	//$NON-NLS-1$

    //Default values
    String serverPortNumber="1118080";	//$NON-NLS-1$
    String adminServerPortNumber="1114848";	//$NON-NLS-1$
    String jmxPort = "8686";//For V2 only, the jmx port to issue some MBeans call that return server loc	//$NON-NLS-1$
    boolean initializedCalled = false;
    private String prevDomainDir, prevDomainName;
    
    public static final String DOMAINUPDATE = "domainupdate";	//$NON-NLS-1$
    private List<PropertyChangeListener> propChangeListeners;
    
    public SunAppServer(){
        SunAppSrvPlugin.logMessage("in SunAppServer CTOR");	//$NON-NLS-1$
     }
	
		
    /* (non-Javadoc)
     * @see org.eclipse.wst.server.core.model.ServerDelegate#initialize()
     */
    @Override
    protected void initialize() {
    	SunAppSrvPlugin.logMessage("in SunAppServer initialize"+this.getServer().getName());	//$NON-NLS-1$
    	super.initialize();
    	SunInitialize();

    }

    protected void SunInitialize(){
    	String domainDir = getDomainDir();
    	String domainName = getdomainName();
    	if (initializedCalled){
        	if ((prevDomainDir != null) && !prevDomainDir.startsWith("${") && !prevDomainDir.equals(domainDir)) {	//$NON-NLS-1$
        		initializedCalled = false;
        	}
        	if ((prevDomainName != null) && !prevDomainName.startsWith("${") && !prevDomainName.equals(domainName)) {	//$NON-NLS-1$
        		initializedCalled = false;
        	}
        	if (initializedCalled)
        		return;
    	}
    	SunAppSrvPlugin.logMessage("in SunAppServer SunInitialize domain is"+getDomainDir());	//$NON-NLS-1$
    	if ((domainDir!=null)&&(!domainDir.startsWith("${"))){ //only if we are correctly setup...	//$NON-NLS-1$
    		if (readServerConfiguration(new File(domainDir+File.separator+domainName+"/config/domain.xml"))) {	//$NON-NLS-1$
	    		SunAppSrvPlugin.logMessage("in SunAppServer initialize done readServerConfiguration");	//$NON-NLS-1$
	    		syncHostAndPortsValues();
	        	prevDomainDir = domainDir;
				prevDomainName = domainName;
				// this is mainly so serversection can listen and repopulate, 
				// but it is not working as intended because the sunserver instance to 
				// which the prop change listener is attached is a different one than is 
				// seeing the changes. in fact, we have multiple instances of this 
				// object and the SunAppServerBehaviour object per server - issue 140
				firePropertyChangeEvent(DOMAINUPDATE, null, null);
    		} else {
	    		SunAppSrvPlugin.logMessage("in SunAppServer could not readServerConfiguration - probably invalid domain");	//$NON-NLS-1$
    		}
			initializedCalled = true;
    	}    	
    }

  @SuppressWarnings("unchecked")
public Map<String, String> getProps(){
	  return getServerInstanceProperties();
  }

  public String validateDomainExists(String domainDir, String domainName) {
	  if ((domainDir!=null)&&(!domainDir.startsWith("${"))){ //only if we are correctly setup...	//$NON-NLS-1$
		File f= new File(domainDir+File.separator+domainName);
		if (!f.exists()){
			return MessageFormat.format(Messages.pathDoesNotExist, f.getAbsolutePath());
		}
		if (!f.isDirectory()){
			return MessageFormat.format(Messages.pathNotDirectory, f.getAbsolutePath());
		}
		if (!f.canWrite()){
			return MessageFormat.format(Messages.pathNotWritable, f.getAbsolutePath());
		}
		File configDir  = new File(f,"config");
		if (!configDir.exists()){
			return MessageFormat.format(Messages.pathDoesNotExist, configDir.getAbsolutePath());
		}
		if (!configDir.canWrite()){
			return MessageFormat.format(Messages.pathNotWritable, configDir.getAbsolutePath());
		}		
		File domain= new File(f,"config/domain.xml");	//$NON-NLS-1$
		if (!domain.exists()){
			return MessageFormat.format(Messages.pathNotValidDomain, domain.getAbsolutePath());
		}
		return null;
	  }
	  return Messages.incompleteDomainSetup;
  }
  String domainValidationError=null;
  /* overide needed to store the admin server port and server port immediately at server creation
   * (non-Javadoc)
   * @see org.eclipse.jst.server.generic.core.internal.GenericServer#setServerInstanceProperties(java.util.Map)
   */
  @SuppressWarnings("unchecked")
public void setServerInstanceProperties(Map map) {
		domainValidationError = null;
		String domdir = null, domainName = null;
		String val = (String) map.get(DOMAINPATH); // new as of August 2010
		if ((val == null) || (val.startsWith("${"))) {// we have an old config
														// we need to deal with
			setAttribute(GenericServerRuntime.SERVER_INSTANCE_PROPERTIES, map);
			return;

		}
		domdir = new File(val).getParentFile().getAbsolutePath();       		 		

		domainName = new File(val).getName();

		if ((domdir != null) && (!domdir.startsWith("${"))) { //only if we are correctly setup...	//$NON-NLS-1$
			domainValidationError = validateDomainExists(domdir, domainName);
			if (domainValidationError == null) {

			  SunInitialize();
			  map.put(ADMINSERVERPORT, adminServerPortNumber);
			  map.put(SERVERPORT, serverPortNumber);
		  }
	  }
	  setAttribute(GenericServerRuntime.SERVER_INSTANCE_PROPERTIES, map);
  }
  
  /* not yet, ui nor working well for generic validation
   */
  public IStatus validate() {
      
   	   SunAppSrvPlugin.logMessage("in SunAppServer -------validate"+getDomainDir()+"---"+getdomainName());
		IStatus s= super.validate();
        
		//File f= new File(getDomainDir()+File.separator+getdomainName());
		//if (!f.exists()){
		  if (domainValidationError!=null) {
              
			IStatus i= new Status(IStatus.ERROR,  "Glassfish:",domainValidationError );
			return i;
		}
 		return s;
 	}   
  
  public String getKeepSessions() {
	  String s =getProps().get(KEEPSESSIONS);
	  if (s==null){
		  s = Boolean.TRUE.toString();
	  }
      return  s;
  }
  public void setKeepSessions(String value) {
  	getProps().put(KEEPSESSIONS, value);
  }
  /* fast deploy for v2 and v2.1 (not using ANT and optimal like in v3
   * 
   */
  public String getFastDeploy() {
	  String s =getProps().get(FASTDEPLOY);
	  if (s==null){
		  s = Boolean.FALSE.toString(); //by default, false
	  }
      return  s;
  }
  public void setFastDeploy(String value) {
  	getProps().put(FASTDEPLOY, value);
  	if (value.equals (Boolean.TRUE.toString())){
    		setAttribute(Server.PROP_AUTO_PUBLISH_SETTING, 2/*Server.AUTO_PUBLISH_OVERRIDE*/);
           	setAttribute(Server.PROP_AUTO_PUBLISH_TIME, 0);
   		
  	}
  	else{
       	setAttribute(Server.PROP_AUTO_PUBLISH_SETTING, Server.AUTO_PUBLISH_DISABLE);
 		
  	}
  }  
  
  public String getUseAnonymousConnections() {
	  String s =getProps().get(USEANONYMOUSCONNECTIONS);
	  if (s==null){
		  if (getAdminPassword()!=null){
			 return Boolean.FALSE.toString();
		  }
		  s = Boolean.TRUE.toString();
	  }
      return  s;
  }
  public void setUseAnonymousConnections(String value) {
  	getProps().put(USEANONYMOUSCONNECTIONS, value);
  }

  public void setV2ContextRoot(String value) {
	  	getProps().put(CONTEXTROOT, value);
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
    	String val = getProps().get(DOMAINPATH); //new as of August 2010
    	if (val==null) {//we have an old config we need to deal with
    		return (String) getProps().get(DEPRECATED_DOMAINNAME);
    	}
    	else{
    		if (val.startsWith("$")) { //not expanded yet
    			return "domain1";
    		}
    		else {
    			return new File(val).getName();
    		}
   		
    	}
    }
     
    public String getDomainDir() {
    	String val = getProps().get(DOMAINPATH); //new as of August 2010
    	if (val==null) {//we have an old config we need to deal with
    		return (String) getProps().get(DEPRECATED_DOMAINDIR);
    	}
    	else {
    		if (val.startsWith("$")) { //not expanded yet
                return val;
        	}
        	else {
        		return new File(val).getParentFile().getAbsolutePath();       		 		
        	}
    	}
    }

    public String getSampleDatabaseDir() {
     //   return (String) getProps().get(SAMPLEDBDIR);
    	return DerbyConfigurator.getSampleDBLocation();
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

    public void saveConfiguration(IProgressMonitor m) throws CoreException  {
        SunAppSrvPlugin.logMessage("in Save SunAppServer " +initializedCalled);	//$NON-NLS-1$
        if (initializedCalled==false){
        	SunInitialize(); 
            SunAppSrvPlugin.logMessage("in Save SunAppServer done" );	//$NON-NLS-1$
       }
        syncHostAndPortsValues();

        super.saveConfiguration(m);

    }

	/**
	 * Add a property change listener to this server.
	 *
	 * @param listener java.beans.PropertyChangeListener
	 */
	public void addPropertyChangeListener(PropertyChangeListener listener) {
		if (propChangeListeners == null)
			propChangeListeners = new ArrayList<PropertyChangeListener>();
		propChangeListeners.add(listener);
	}

	/**
	 * Remove a property change listener from this server.
	 *
	 * @param listener java.beans.PropertyChangeListener
	 */
	public void removePropertyChangeListener(PropertyChangeListener listener) {
		if (propChangeListeners != null)
			propChangeListeners.remove(listener);
	}

	/**
	 * Fire a property change event.
	 * 
	 * @param propertyName a property name
	 * @param oldValue the old value
	 * @param newValue the new value
	 */
	public void firePropertyChangeEvent(String propertyName, Object oldValue, Object newValue) {
		if (propChangeListeners == null)
			return;
	
		PropertyChangeEvent event = new PropertyChangeEvent(this, propertyName, oldValue, newValue);
		try {
			Iterator<PropertyChangeListener> iterator = propChangeListeners.iterator();
			while (iterator.hasNext()) {
				try {
					PropertyChangeListener listener = (PropertyChangeListener) iterator.next();
					listener.propertyChange(event);
				} catch (Exception e) {
					SunAppSrvPlugin.logMessage("Error firing property change event", e);	//$NON-NLS-1$
				}
			}
		} catch (Exception e) {
			SunAppSrvPlugin.logMessage("Error in property event", e);	//$NON-NLS-1$
		}
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
            SunAppSrvPlugin.logMessage("error ="+ ex);	//$NON-NLS-1$
        }
    }
    
    
    public static SunAppServer getSunAppServer(IServerWorkingCopy server){
        SunAppServer  sunserver = (SunAppServer) server.getOriginal().getAdapter(SunAppServer.class);
        if (sunserver == null) {
            sunserver = (SunAppServer) server.getOriginal().loadAdapter(SunAppServer.class, new NullProgressMonitor());
        }
        return sunserver;
    }

    /* (non-Javadoc)
     * @see org.eclipse.jst.server.generic.core.internal.GenericServer#setDefaults(org.eclipse.core.runtime.IProgressMonitor)
     * This implementation overrides the automatic publishing option to disable it by default.
     */
    @Override
    public void setDefaults(IProgressMonitor monitor) {
         SunAppSrvPlugin.logMessage("In  setDefaults for " +this.getServer().getServerType().getName());	//$NON-NLS-1$
        if (isV3()){
    		setAttribute(Server.PROP_AUTO_PUBLISH_SETTING, 2/*Server.AUTO_PUBLISH_OVERRIDE*/);
           	setAttribute(Server.PROP_AUTO_PUBLISH_TIME, 0);
        }
        else{
           	setAttribute(Server.PROP_AUTO_PUBLISH_SETTING, Server.AUTO_PUBLISH_DISABLE);
                    	
        }
        super.setDefaults(monitor);
        
        
    }
    /* for both v3 or v3 prelude
     * 
     */
	public boolean isV3(){
		//test the server name to contain GlassFish v3
		return (
                        (this.getServer().getServerType().getId().equals("com.sun.enterprise.jst.server.sunappsrv92"))
                        ||
                        (this.getServer().getServerType().getId().equals("org.glassfish.jst.server.glassfish31"))
                        );
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

		String domainValidationError = validateDomainExists(getDomainDir(), getdomainName());
		if (domainValidationError != null) {
			throw new CoreException(new Status(IStatus.ERROR,  SunAppSrvPlugin.SUNPLUGIN_ID, 
    				IJavaLaunchConfigurationConstants.ERR_INTERNAL_ERROR, domainValidationError, 
    				new RuntimeException(domainValidationError)));
		}
    	if (adminServerPortNumber.equals("1114848")){	//$NON-NLS-1$
    		SunAppSrvPlugin.logMessage("catastrophic state where adminServerPortNumber is not initialize is null in SunAppServer.java");	//$NON-NLS-1$
    		SunAppSrvPlugin.logMessage("catastrophic Only thing to do is restart Eclipse");	//$NON-NLS-1$
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
    
	public  String getVersion3Only() {
		Commands.VersionCommand command = new Commands.VersionCommand();
		try {
			Future<OperationState> result = execute(command);
			if (result.get(30, TimeUnit.SECONDS) == OperationState.COMPLETED) {
				return command.getVersion();
			}

		} catch (Exception ex) {
			SunAppSrvPlugin.logMessage("getVersion3Only is failing=", ex); //$NON-NLS-1$
		}
		return "";
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
                			SunAppSrvPlugin.logMessage("getV3ServerStatus DOMAINDIR_MATCHING" );	//$NON-NLS-1$
                			return ServerStatus.DOMAINDIR_MATCHING;

                		}
                		else {
                			SunAppSrvPlugin.logMessage("getV3ServerStatus DOMAINDIR_NOT_MATCHING" );	//$NON-NLS-1$
                			return ServerStatus.DOMAINDIR_NOT_MATCHING;

                		}
                	} else {
                		SunAppSrvPlugin.logMessage("getV3ServerStatus 3 DOMAINDIR_NOT_MATCHING" );	//$NON-NLS-1$
                		return ServerStatus.DOMAINDIR_NOT_MATCHING;
                	}
                } else  {
                	SunAppSrvPlugin.logMessage("apparently CREDENTIAL_ERROR" );	//$NON-NLS-1$
                	return ServerStatus.CREDENTIAL_ERROR;

                }
            } catch(Exception ex) {
                SunAppSrvPlugin.logMessage("IsReady is failing=",ex );	//$NON-NLS-1$
                SunAppSrvPlugin.logMessage("getV3ServerStatus 2 CONNEXTION_ERROR" );	//$NON-NLS-1$
              	return ServerStatus.CONNEXTION_ERROR;
            }
      

    }    
    /**
     * 
     * @return ServerStatus for possible V2 server. If server is not a V2 one, we will know
     *  If the server is a V2 server but with a different install location that this one, we can also detect 
     *  this
     */   
    @SuppressWarnings("unchecked")
	public ServerStatus getV2ServerStatus(){
    	JMXConnector jmxc = null;
    	try{

    		// Create an RMI connector client
    		//
    		JMXServiceURL url = new JMXServiceURL(
    		"service:jmx:rmi:///jndi/rmi://"+getServer().getHost()+":"+jmxPort+"/jmxrmi");	//$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    		HashMap env = new HashMap();
    		env.put(JMXConnector.CREDENTIALS, new String[]{ getAdminName(), getAdminPassword()});
            SunAppSrvPlugin.logMessage("service:jmx:rmi:///jndi/rmi://"+getServer().getHost()+":"+jmxPort+"/jmxrmi" );	//$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    		jmxc = JMXConnectorFactory.connect(url, env);
    		SunAppSrvPlugin.logMessage("after JMXConnectorFactory");	//$NON-NLS-1$
    		MBeanServerConnection mbsc = jmxc.getMBeanServerConnection();
   			ObjectName on = new ObjectName("com.sun.appserv:type=domain,category=config");	//$NON-NLS-1$

    		Object o = mbsc.invoke(on, "getConfigDir", null, null);	//$NON-NLS-1$
    		SunAppSrvPlugin.logMessage("mbsc.invoke="+o);	//$NON-NLS-1$
   		if (o != null) {
    			File domainDir=new File(""+o).getParentFile(); //$NON-NLS-1$
                File knownDomainRoot = new File(this.getDomainDir()+File.separator+this.getdomainName());
                if (domainDir.getCanonicalPath().equals(knownDomainRoot.getCanonicalPath())){
                	return ServerStatus.DOMAINDIR_MATCHING;
                }else {
                	return ServerStatus.DOMAINDIR_NOT_MATCHING;
               	
                }
    		}
    		else {
                SunAppSrvPlugin.logMessage("V2 not ready yet: o=null" );	//$NON-NLS-1$
            	return ServerStatus.MBEAN_ERROR;
   		}
   		

    	} catch (Exception e) {
            SunAppSrvPlugin.logMessage("V2 not ready yet:",e );	//$NON-NLS-1$
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
            pathList.add(new TreeParser.Path("/domain/configs/config/admin-service/jmx-connector",	//$NON-NLS-1$
                    new TreeParser.NodeReader() {
                @Override
                public void readAttributes(String qname, Attributes attributes) throws SAXException {
 /*
        <admin-service type="das-and-server" system-jmx-connector-name="system">
        <jmx-connector ..... port="8686" />
  */
                	String jmxAttr= attributes.getValue("port");	//$NON-NLS-1$
                	try{
                		int port = Integer.parseInt(jmxAttr);
                		jmxPort = ""+port;	//$NON-NLS-1$
                		SunAppSrvPlugin.logMessage("JMX Port is "+jmxPort );	//$NON-NLS-1$
                	} catch(NumberFormatException ex) {
                        SunAppSrvPlugin.logMessage("error reading one jmx port"+ex );	//$NON-NLS-1$

                	}
                    
                }
            }));
            pathList.add(new TreeParser.Path("/domain/configs/config/http-service/http-listener",	//$NON-NLS-1$
                    new TreeParser.NodeReader() {
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
                                httpMap.put(id, data);
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
            }));
                        
            //New grizzly config for latest GF v3 builds (after build 44)
            pathList.add(new TreeParser.Path("/domain/configs/config/network-config/network-listeners/network-listener",
                                new TreeParser.NodeReader() {
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
                                            httpMap.put(id, data);
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
                HttpData adminData = httpMap.remove("admin-listener");	//$NON-NLS-1$
                
               adminServerPortNumber =""+(adminData != null ? adminData.getPort() : -1);	//$NON-NLS-1$
               SunAppSrvPlugin.logMessage("reading from domain.xml adminServerPortNumber="+adminServerPortNumber );	//$NON-NLS-1$
               
                
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
                serverPortNumber= ""+httpPort;	//$NON-NLS-1$
                SunAppSrvPlugin.logMessage("reading from domain.xml serverPortNumber="+serverPortNumber );	//$NON-NLS-1$
                /////ludo secure TODO   wi.setHttpsPort(httpsData != null ? httpsData.getPort() : -1);
                
                result = httpPort != -1;
            } catch(IllegalStateException ex) {
                SunAppSrvPlugin.logMessage("error IllegalStateException ",ex);	//$NON-NLS-1$
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
            return "{ " + id + ", " + port + ", " + secure + " }";	//$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
        }
        
    }

    public CommandFactory getCommandFactory() {

    		return new CommandFactory() {
    			public SetPropertyCommand getSetPropertyCommand(String name, String value) {
    				return new ServerCommand.SetPropertyCommand(name, value, "DEFAULT={0}={1}"); 
    			}
    		};
    	
    }
    
}       
    

