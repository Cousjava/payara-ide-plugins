package com.sun.enterprise.jst.server.sunappsrv;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants;
import org.eclipse.jst.server.generic.core.internal.GenericServer;
import org.eclipse.jst.server.generic.core.internal.GenericServerRuntime;
import org.glassfish.tools.ide.data.GlassFishAdminInterface;
import org.glassfish.tools.ide.data.GlassFishServer;
import org.glassfish.tools.ide.data.GlassFishVersion;

public abstract class GenericGlassfishServer extends GenericServer implements
		GlassFishServer {

	/*
	 * Static variables for getting server properties defined in server definition. 
	 */
	public static final String ROOTDIR = "sunappserver.rootdirectory";	//$NON-NLS-1$
    // This property does not come from serverdef, but is used there and in the ant files
    // so we set it by synchronizing it with the value from the generic server framework
    public static final String ADDRESS = "sunappserver.serveraddress";	//$NON-NLS-1$
    
    // now we read these props from domain.xml
    public static final String SERVERPORT = "sunappserver.serverportnumber";	//$NON-NLS-1$
    public static final String ADMINSERVERPORT = "sunappserver.adminserverportnumber";	//$NON-NLS-1$

    public static final String DOMAINPATH = "sunappserver.domainpath";	//$NON-NLS-1$
    public static final String ADMINNAME = "sunappserver.adminname";	//$NON-NLS-1$
    public static final String ADMINPASSWORD = "sunappserver.adminpassword";	//$NON-NLS-1$
    public static final String KEEPSESSIONS = "sunappserver.keepSessions";	//$NON-NLS-1$
    public static final String JARDEPLOY = "sunappserver.jarDeploy";	//$NON-NLS-1$
    public static final String USEANONYMOUSCONNECTIONS = "sunappserver.useAnonymousConnection";	//$NON-NLS-1$

    public static final String SAMPLEDBDIR = "sunappserver.sampledbdir";	//$NON-NLS-1$
    
    
    boolean initializedCalled = false;
    private String prevDomainDir, prevDomainName;
    private String domainValidationError = null;
    
    private List<PropertyChangeListener> propChangeListeners;
    
    public static final String DOMAINUPDATE = "domainupdate";	//$NON-NLS-1$
    
    
	/*
	 * Implementation of adapter methods used by tooling SDK library.
	 */
	@Override
	public int getAdminPort() {
		return Integer.parseInt(getProps().get(ADMINSERVERPORT));
	}


	@Override
	public String getAdminUser() {
		return (String) getProps().get(ADMINNAME);
	}


	@Override
	public String getDomainsFolder() {
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
	
	/*
	 * Specific implementation for Eclipse.
	 */
	
	@SuppressWarnings("unchecked")
	public Map<String, String> getProps(){
		  return getServerInstanceProperties();
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
	    return Integer.parseInt(getProps().get(SERVERPORT));
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

//  		if ((domdir != null) && (!domdir.startsWith("${"))) { //only if we are correctly setup...	//$NON-NLS-1$
//  			domainValidationError = validateDomainExists(domdir, domainName);
//  			if (domainValidationError == null) {
//
//  			  sunInitialize();
//  			  map.put(ADMINSERVERPORT, adminServerPortNumber);
//  			  map.put(SERVERPORT, serverPortNumber);
//  		  }
  	  //}
  	  //setAttribute(GenericServerRuntime.SERVER_INSTANCE_PROPERTIES, map);
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
    
    public boolean isDomainValid() throws CoreException {
  	  domainValidationError = validateDomainExists(getDomainsFolder(), getDomainName());
  	  if (domainValidationError != null) {
  		  throw new CoreException(new Status(IStatus.ERROR,  SunAppSrvPlugin.SUNPLUGIN_ID, 
  				  IJavaLaunchConfigurationConstants.ERR_INTERNAL_ERROR, domainValidationError, 
  				  new RuntimeException(domainValidationError)));
  	  }
  	  return true;
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
	
	private void syncHostAndPortsValues() {

	}
	
}
