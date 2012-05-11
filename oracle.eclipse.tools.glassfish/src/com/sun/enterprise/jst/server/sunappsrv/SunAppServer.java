/*
 * Copyright (c) 1997-2011 Oracle and/or its affiliates. All rights reserved.
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
import java.net.InetAddress;
import java.net.ServerSocket;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

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
import org.glassfish.tools.ide.GlassFishIdeException;
import org.glassfish.tools.ide.admin.Command;
import org.glassfish.tools.ide.admin.CommandVersion;
import org.glassfish.tools.ide.admin.ResultString;
import org.glassfish.tools.ide.admin.ServerAdmin;
import org.glassfish.tools.ide.data.GlassFishAdminInterface;
import org.glassfish.tools.ide.data.GlassFishServer;
import org.glassfish.tools.ide.data.GlassFishVersion;
import org.glassfish.tools.ide.data.IdeContext;
import org.glassfish.tools.ide.server.ServerSupport;

import com.sun.enterprise.jst.server.sunappsrv.commands.CommandRunner;
import com.sun.enterprise.jst.server.sunappsrv.commands.Commands;
import com.sun.enterprise.jst.server.sunappsrv.commands.GlassfishModule.OperationState;
import com.sun.enterprise.jst.server.sunappsrv.commands.ServerCommand;
import com.sun.enterprise.jst.server.sunappsrv.commands.ServerCommand.SetPropertyCommand;
import com.sun.enterprise.jst.server.sunappsrv.derby.DerbyConfigurator;
import com.sun.enterprise.jst.server.sunappsrv.spi.HttpData;
import com.sun.enterprise.jst.server.sunappsrv.spi.HttpListenerReader;
import com.sun.enterprise.jst.server.sunappsrv.spi.JmxConnectorReader;
import com.sun.enterprise.jst.server.sunappsrv.spi.NetworkListenerReader;
import com.sun.enterprise.jst.server.sunappsrv.spi.TreeParser;




/* wrapper to get rw access to all the props defined in the serverdef files
 *
 **/

@SuppressWarnings("restriction")
public class SunAppServer extends GenericServer implements GlassFishServer {
    
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
    public static final String JARDEPLOY = "sunappserver.jarDeploy";	//$NON-NLS-1$
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
    	sunInitialize();

    }

    protected void sunInitialize(){
    	String domainDir = getDomainDir();
    	String domainName = getDomainName();
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
		if ((domainDir != null) && (!domainDir.startsWith("${"))) { //only if we are correctly setup...	//$NON-NLS-1$
			if (!isLocalServer()) {
				//no domain.xml to read
				getServerPort();// force an init if needed
				getAdminServerPort();// force an init if needed

			}else if (readServerConfiguration(new File(domainDir+File.separator+domainName+"/config/domain.xml"))) {	//$NON-NLS-1$
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
      
	  if (!isLocalServer()){
		  return null;		  
	  }
          
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
  
  public boolean isDomainValid() throws CoreException {
	  String domainValidationError = validateDomainExists(getDomainDir(), getDomainName());
	  if (domainValidationError != null) {
		  throw new CoreException(new Status(IStatus.ERROR,  SunAppSrvPlugin.SUNPLUGIN_ID, 
				  IJavaLaunchConfigurationConstants.ERR_INTERNAL_ERROR, domainValidationError, 
				  new RuntimeException(domainValidationError)));
	  }
	  return true;
  }
  
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
			map.put(ADDRESS, getServer().getHost());
			setAttribute(GenericServerRuntime.SERVER_INSTANCE_PROPERTIES, map);
			initializedCalled = true;
			return;

		}
		domdir = new File(val).getParentFile().getAbsolutePath();       		 		

		domainName = new File(val).getName();

		if ((domdir != null) && (!domdir.startsWith("${"))) { //only if we are correctly setup...	//$NON-NLS-1$
			domainValidationError = validateDomainExists(domdir, domainName);
			if (domainValidationError == null) {

			  sunInitialize();
			  map.put(ADMINSERVERPORT, adminServerPortNumber);
			  map.put(SERVERPORT, serverPortNumber);
		  }
	  }
	  setAttribute(GenericServerRuntime.SERVER_INSTANCE_PROPERTIES, map);
  }
  
  /* not yet, ui nor working well for generic validation
   */
  public IStatus validate() {
      
   	   SunAppSrvPlugin.logMessage("in SunAppServer -------validate"+getDomainDir()+"---"+getDomainName());
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
  
  /* return null is no need to keep sessions
   * or keepsession if server is older than 3.1
   * or keepstate for 3.1 and above servers
   */
  public  String computePreserveSessions() {
  	String ret=null;
      if ( !getKeepSessions().equals("true"))
      	return ret;
      if (SunAppSrvPlugin.is31OrAbove(getServer().getRuntime()))
      	ret = "keepstate";
      else 
      	ret = "keepSessions";
      return ret;
  }
    /* JAR deploy for v3
     * 
     */
    public String getJarDeploy() {
        String s = getProps().get(JARDEPLOY);
        if (s == null) {
            s = Boolean.FALSE.toString(); //by default, false
        }
        return s;
    }

    public void getJarDeploy(String value) {
        getProps().put(JARDEPLOY, value);
        /*  	if (value.equals (Boolean.TRUE.toString())){
        setAttribute(Server.PROP_AUTO_PUBLISH_SETTING, 2)'//2Server.AUTO_PUBLISH_OVERRIDE*
        setAttribute(Server.PROP_AUTO_PUBLISH_TIME, 0);
        
        }
        else{
        setAttribute(Server.PROP_AUTO_PUBLISH_SETTING, Server.AUTO_PUBLISH_DISABLE);
        
        }*/
    }  
  
  public String getUseAnonymousConnections() {
	  String s =getProps().get(USEANONYMOUSCONNECTIONS);
	  if (s==null){
		  String pass=getAdminPassword();
		  if ((s!=null)&&(!s.equals(""))){
			 return Boolean.FALSE.toString();
		  }
		  s = Boolean.TRUE.toString();
	  }
      return  s;
  }

  /*public void setUseAnonymousConnections(String value) {
  	getProps().put(USEANONYMOUSCONNECTIONS, value);
  }*/

  public void setV2ContextRoot(String value) {
	  	getProps().put(CONTEXTROOT, value);
  }

  public String getServerPort() {
		if (isLocalServer()) {
			return serverPortNumber;
		}
		else {
			serverPortNumber =getProps().get(SERVERPORT);
			  if (serverPortNumber==null){
				  serverPortNumber = "8080";				  
			  }
			  return serverPortNumber;
		}
			  
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
    	
	    return Integer.parseInt(getServerPort());
    }

	public String getAdminServerPort() {
		if (isLocalServer()) {
			return adminServerPortNumber;
		}
		else {//read it form the server props
			adminServerPortNumber =getProps().get(ADMINSERVERPORT);
			  if (adminServerPortNumber==null){
				  adminServerPortNumber = "4848";				  
			  }
			  return adminServerPortNumber;
		}
    }
    
    public String getAdminName() {
        return (String) getProps().get(ADMINNAME);
    }
    public void setAdminName(String value) {
    	getProps().put(ADMINNAME, value);
    }
        
    public String getDomainName() {
    	String val = getProps().get(DOMAINPATH); //new as of August 2010
    	if (val == null) {
    		// TODO throw some exception
    	}
    	if (val.startsWith("$")) { //not expanded yet
    		return "domain1";
    	}
    	else {
    		return new File(val).getName();
    	}
    }
     
    public String getDomainDir() {
    	String val = getProps().get(DOMAINPATH); //new as of August 2010
    	if (val == null) {
    		// TODO throw some exception
    	}
    	if (val.startsWith("$")) { //not expanded yet
          return val;
      	}
       	else {
       		return new File(val).getParentFile().getAbsolutePath();       		 		
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
        	sunInitialize(); 
            SunAppSrvPlugin.logMessage("in Save SunAppServer done" );	//$NON-NLS-1$
       }
     //   syncHostAndPortsValues();

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
	
	public boolean isLocalServer(){
		return getServer().getHost().equalsIgnoreCase("localhost");
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
         setDefaultPublishState();
        super.setDefaults(monitor);        
        
    }
    public void setDefaultPublishState(){
        if (isV3()){
    		setAttribute(Server.PROP_AUTO_PUBLISH_SETTING, 2/*Server.AUTO_PUBLISH_OVERRIDE*/);
           	setAttribute(Server.PROP_AUTO_PUBLISH_TIME, 0);
        }
        else{
           	setAttribute(Server.PROP_AUTO_PUBLISH_SETTING, Server.AUTO_PUBLISH_DISABLE);
                    	
        }    	
    }
    /* for both v3 or v3 prelude
     * 
     */
	public boolean isV3(){
		//test the server name to contain GlassFish v3 3.1 or 3.1.1
		return (
                        (this.getServer().getServerType().getId().equals("com.sun.enterprise.jst.server.sunappsrv92"))
                        ||
                        (this.getServer().getServerType().getId().equals("org.glassfish.jst.server.glassfish31"))
                        ||
                        (this.getServer().getServerType().getId().equals("org.glassfish.jst.server.glassfish311"))
                        ||
                        (this.getServer().getServerType().getId().equals("org.glassfish.jst.server.glassfish312"))
                        ||
                        (this.getServer().getServerType().getId().equals("org.glassfish.jst.server.glassfish40")));
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
     * Checks if the server is running.
     *  
     * @return true if we can ping to the server
     * @throws CoreException 
     */
    public  boolean isRunning() {
    	if (!isLocalServer()){// remote is for 3.0 or above, then we use get version http/s request to avoid
    		// proxy security issues with a simple socket usage...
    		try {
    			getVersionString();
    			return true;
    		} catch (GlassFishIdeException e) {
    			// something happened so we assume that the server is not running
    			return false;
    		}
    	}
    	// TODO find out where it occurs
    	if (adminServerPortNumber.equals("1114848")){	//$NON-NLS-1$
    		SunAppSrvPlugin.logMessage("catastrophic state where adminServerPortNumber is not initialized in SunAppServer.java");	//$NON-NLS-1$
    		SunAppSrvPlugin.logMessage("catastrophic Only thing to do is restart Eclipse");	//$NON-NLS-1$
    		initialize();
    		/*throw new CoreException(new Status(IStatus.ERROR,  SunAppSrvPlugin.SUNPLUGIN_ID, 
    				IJavaLaunchConfigurationConstants.ERR_INTERNAL_ERROR, 
    				"Error where adminServerPortNumber is not initialized and null in GlassFish Plugin. Restart Eclipse IDE", 
    				new RuntimeException ("Restart Eclipse. Internal State corrupted...")));
			*/

        }
    	return ServerSupport.isRunning(this);
    }

    public  boolean isPortAvailable(int port) {
        // if the port is not in the allowed range - return false
        if ((port < 0) && (port > 65535)) {
            return false;
        }
        // if the port is not in the restricted list, we'll try to open a server
        // socket on it, if we fail, then someone is already listening on this port
        // and it is occupied
        ServerSocket socket = null;
        try {
        	InetAddress s = InetAddress.getByName(getServer().getHost());
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
    
    public String getVersionString() throws GlassFishIdeException {
    	Command command = new CommandVersion();
    	IdeContext ide = new IdeContext();
    	Future<ResultString> future = ServerAdmin.exec(this, command, ide);
    	try {
    		ResultString result = future.get(30, TimeUnit.SECONDS);
    		return result.getValue();
    	} catch (InterruptedException e) {
    		throw new GlassFishIdeException("Exception by calling getVersionString", e);
    	} catch (ExecutionException e) {
    		throw new GlassFishIdeException("Exception by calling getVersionString", e);
    	} catch (TimeoutException e) {
    		throw new GlassFishIdeException("Timeout for getting version command exceeded", e);
    	}
    }
    
    /**
     * 
     * @return ServerStatus for possible V3 server. If server is not a V3 one, we will know
     *  If the server is a V3 server but with a different install location that this one, we can also detect 
     *  this
     */
    public ServerStatus getServerStatus() {

   	
    	   
           Commands.LocationCommand command = new Commands.LocationCommand();
           try {
                Future<OperationState> result = execute(command);
                OperationState res=result.get(30, TimeUnit.SECONDS);
                
                if(res == OperationState.RUNNING) { 
                	//let try one more time...it is possible to have running and then immediately completed..
        			SunAppSrvPlugin.logMessage("getV3ServerStatus trying one more time" );	//$NON-NLS-1$

                	res=result.get(15, TimeUnit.SECONDS);
                }
                if(res == OperationState.COMPLETED) {
                	String installRoot = this.getDomainDir()+File.separator+this.getDomainName();
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
                } else  if (res==OperationState.FAILED){
                	SunAppSrvPlugin.logMessage("apparently CREDENTIAL_ERROR" );	//$NON-NLS-1$
                	return ServerStatus.CREDENTIAL_ERROR;

                } else {
                	SunAppSrvPlugin.logMessage("Command Still running!!! error" );	//$NON-NLS-1$
                	return ServerStatus.CREDENTIAL_ERROR;              	
                }
            } catch(Exception ex) {
                SunAppSrvPlugin.logMessage("IsReady is failing=",ex );	//$NON-NLS-1$
                SunAppSrvPlugin.logMessage("getV3ServerStatus 2 CONNEXTION_ERROR" );	//$NON-NLS-1$
              	return ServerStatus.CONNEXTION_ERROR;
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
    		JmxConnectorReader jmxReader = new JmxConnectorReader();
    		HttpListenerReader httpListenerReader = new HttpListenerReader();
    		NetworkListenerReader networkListenerReader = new NetworkListenerReader();

    		try {
    			TreeParser.readXml(domainXml, jmxReader, httpListenerReader, networkListenerReader);
    			jmxPort = jmxReader.getResult();

    			httpMap.putAll(httpListenerReader.getResult());
    			httpMap.putAll(networkListenerReader.getResult());
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


    public CommandFactory getCommandFactory() {

    		return new CommandFactory() {
    			public SetPropertyCommand getSetPropertyCommand(String name, String value) {
    				return new ServerCommand.SetPropertyCommand(name, value, "DEFAULT={0}={1}"); 
    			}
    		};
    	
    }

/*
 * Implementation of adapter methods used by tooling SDK library.
 */
	@Override
	public int getAdminPort() {
		return Integer.parseInt(getAdminServerPort());
	}


	@Override
	public String getAdminUser() {
		return getAdminName();
	}


	@Override
	public String getDomainsFolder() {
		return getDomainDir();
	}


	@Override
	public String getHost() {
		return getServer().getHost();
	}


	@Override
	public String getName() {
		return getServer().getName();
	}


	@Override
	public int getPort() {
		return getHttpPort();
	}


	@Override
	public String getUrl() {
		return null;
	}


	@Override
	public GlassFishVersion getVersion() {
		return GlassFishVersion.GF_3_1_2;
	}


	@Override
	public GlassFishAdminInterface getAdminInterface() {
		return GlassFishAdminInterface.REST;
	}


	@Override
	public String getServerHome() {
		return new File(getServer().getRuntime().getLocation().toString()).getAbsolutePath();
	}
	

}       
    

