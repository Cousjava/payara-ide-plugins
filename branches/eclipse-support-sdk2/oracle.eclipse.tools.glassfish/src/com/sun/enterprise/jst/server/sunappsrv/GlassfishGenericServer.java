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
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants;
import org.eclipse.jst.server.generic.core.internal.GenericServer;
import org.eclipse.wst.server.core.IServerWorkingCopy;
import org.eclipse.wst.server.core.ServerPort;
import org.eclipse.wst.server.core.internal.Server;
import org.eclipse.wst.server.core.util.SocketUtil;
import org.glassfish.tools.ide.data.GlassFishAdminInterface;
import org.glassfish.tools.ide.data.GlassFishServer;
import org.glassfish.tools.ide.data.GlassFishVersion;
import org.glassfish.tools.ide.server.parser.HttpData;
import org.glassfish.tools.ide.server.parser.HttpListenerReader;
import org.glassfish.tools.ide.server.parser.NetworkListenerReader;
import org.glassfish.tools.ide.server.parser.TreeParser;

import com.sun.enterprise.jst.server.sunappsrv.commands.ServerCommand;
import com.sun.enterprise.jst.server.sunappsrv.commands.ServerCommand.SetPropertyCommand;
import com.sun.enterprise.jst.server.sunappsrv.derby.DerbyConfigurator;

/* wrapper to get rw access to all the props defined in the serverdef files
 *
 **/

@SuppressWarnings("restriction")
public abstract class GlassfishGenericServer extends GenericServer implements
		GlassFishServer {

	public static final String ROOTDIR = "sunappserver.rootdirectory"; //$NON-NLS-1$
	// This property does not come from serverdef, but is used there and in the
	// ant files
	// so we set it by synchronizing it with the value from the generic server
	// framework
	public static final String ADDRESS = "sunappserver.serveraddress"; //$NON-NLS-1$

	// now we read these props from domain.xml
	public static final String SERVERPORT = "sunappserver.serverportnumber"; //$NON-NLS-1$
	public static final String ADMINSERVERPORT = "sunappserver.adminserverportnumber"; //$NON-NLS-1$
	public static final String USECUSTOMTARGET = "sunappserver.usecustomtarget";
	public static final String TARGET = "sunappserver.target";
	public static final String DOMAINPATH = "sunappserver.domainpath"; //$NON-NLS-1$
	public static final String ADMINNAME = "sunappserver.adminname"; //$NON-NLS-1$
	public static final String ADMINPASSWORD = "sunappserver.adminpassword"; //$NON-NLS-1$
	public static final String KEEPSESSIONS = "sunappserver.keepSessions"; //$NON-NLS-1$
	public static final String JARDEPLOY = "sunappserver.jarDeploy"; //$NON-NLS-1$
	public static final String USEANONYMOUSCONNECTIONS = "sunappserver.useAnonymousConnection"; //$NON-NLS-1$

	public static final String SAMPLEDBDIR = "sunappserver.sampledbdir"; //$NON-NLS-1$

	// used only for v2, populated from project properties or module name with
	// no space
	public static final String CONTEXTROOT = "sunappserver.contextroot"; //$NON-NLS-1$

	public static final String DOMAINUPDATE = "domainupdate"; //$NON-NLS-1$
	private List<PropertyChangeListener> propChangeListeners;

	public GlassfishGenericServer() {
		SunAppSrvPlugin.logMessage("in SunAppServer CTOR"); //$NON-NLS-1$
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.wst.server.core.model.ServerDelegate#initialize()
	 */
	@Override
	protected void initialize() {
		SunAppSrvPlugin
				.logMessage("in SunAppServer initialize" + this.getServer().getName()); //$NON-NLS-1$
		super.initialize();
		readDomainConfig();

	}

	public GlassfishGenericServerBehaviour getServerBehaviourAdapter() {
		GlassfishGenericServerBehaviour serverBehavior = (GlassfishGenericServerBehaviour) getServer()
				.getAdapter(GlassfishGenericServerBehaviour.class);
		if (serverBehavior == null) {
			serverBehavior = (GlassfishGenericServerBehaviour) getServer()
					.loadAdapter(GlassfishGenericServerBehaviour.class,
							new NullProgressMonitor());
		}
		return serverBehavior;
	}
	
	public boolean isRemote() {
		return (getServer().getServerType().supportsRemoteHosts() && !SocketUtil
				.isLocalhost(getServer().getHost()));
	}

	protected void readDomainConfig() {
		if (!isRemote()) {
			if (readServerConfiguration(new File(getDomainsFolder()
					+ File.separator + getDomainName() + "/config/domain.xml"))) { //$NON-NLS-1$
				SunAppSrvPlugin
						.logMessage("in SunAppServer initialize done readServerConfiguration"); //$NON-NLS-1$
				syncHostAndPortsValues();
				// this is mainly so serversection can listen and repopulate,
				// but it is not working as intended because the sunserver
				// instance to
				// which the prop change listener is attached is a different one
				// than is
				// seeing the changes. in fact, we have multiple instances of
				// this
				// object and the SunAppServerBehaviour object per server -
				// issue 140
				//firePropertyChangeEvent(DOMAINUPDATE, null, null);
			} else {
				SunAppSrvPlugin
						.logMessage("in SunAppServer could not readServerConfiguration - probably invalid domain"); //$NON-NLS-1$
			}
		}
	}

	@SuppressWarnings("unchecked")
	public Map<String, String> getProps() {
		return getServerInstanceProperties();
	}

	public String validateDomainExists(String domainPath) {

		if (isRemote()) {
			return null;
		}

		if ((domainPath != null) && (!domainPath.startsWith("${"))) { //only if we are correctly setup...	//$NON-NLS-1$
			File f = new File(domainPath);
			if (!f.exists()) {
				return MessageFormat.format(Messages.pathDoesNotExist,
						f.getAbsolutePath());
			}
			if (!f.isDirectory()) {
				return MessageFormat.format(Messages.pathNotDirectory,
						f.getAbsolutePath());
			}
			if (!f.canWrite()) {
				return MessageFormat.format(Messages.pathNotWritable,
						f.getAbsolutePath());
			}
			File configDir = new File(f, "config");
			if (!configDir.exists()) {
				return MessageFormat.format(Messages.pathDoesNotExist,
						configDir.getAbsolutePath());
			}
			if (!configDir.canWrite()) {
				return MessageFormat.format(Messages.pathNotWritable,
						configDir.getAbsolutePath());
			}
			File domain = new File(f, "config/domain.xml"); //$NON-NLS-1$
			if (!domain.exists()) {
				return MessageFormat.format(Messages.pathNotValidDomain,
						domain.getAbsolutePath());
			}
			return null;
		}
		return Messages.incompleteDomainSetup;
	}

	String domainValidationError = null;

	public boolean isDomainValid() throws CoreException {
		String domainValidationError = validateDomainExists(getDomainPath());
		if (domainValidationError != null) {
			throw new CoreException(new Status(IStatus.ERROR,
					SunAppSrvPlugin.SUNPLUGIN_ID,
					IJavaLaunchConfigurationConstants.ERR_INTERNAL_ERROR,
					domainValidationError, new RuntimeException(
							domainValidationError)));
		}
		return true;
	}

	/*
	 * not yet, ui nor working well for generic validation
	 */
	@Override
	public IStatus validate() {

		SunAppSrvPlugin.logMessage("in SunAppServer -------validate");// +getDomainDir()+"---"+getDomainName());
		IStatus s = null;
		// validate domain directory
		String domainValidation = validateDomainExists(getDomainPath());
		// File f= new File(getDomainDir()+File.separator+getdomainName());
		// if (!f.exists()){
		if (domainValidation != null) {
			return new Status(IStatus.ERROR, SunAppSrvPlugin.SUNPLUGIN_ID,
					domainValidation);
		}
		// reads ports from domain
		readDomainConfig();

		// validate ports
		try {
			//Integer.parseInt(getServerPort());
			Integer.parseInt(getAdminServerPort());
		} catch (NumberFormatException e) {
			return new Status(IStatus.ERROR, SunAppSrvPlugin.SUNPLUGIN_ID,
					Messages.invalidPortNumbers);
		}

		// validate target if needed
		if (useCustomTarget()) {
			String target = getTarget();
			if ((target == null) || target.isEmpty()) {
				return new Status(IStatus.ERROR, SunAppSrvPlugin.SUNPLUGIN_ID,
						Messages.emptyTargetMsg);
			}
		}
		
		return new Status(IStatus.OK, SunAppSrvPlugin.SUNPLUGIN_ID, 0, "", null); //$NON-NLS-1$
	}

	public String getKeepSessions() {
		return getProps().get(KEEPSESSIONS);
	}
	
	public boolean useCustomTarget() {
		return "true".equals(getProps().get(USECUSTOMTARGET));
	}
	
	public String getTarget() {
		return getProps().get(TARGET);
	}
	
	public boolean hasNonDefaultTarget() {
		return (getTarget() != null) && !getTarget().isEmpty();
	}

	/*
	 * return null is no need to keep sessions or keepsession if server is older
	 * than 3.1 or keepstate for 3.1 and above servers
	 */
	public String computePreserveSessions() {
		String ret = null;
		if (!getKeepSessions().equals("true"))
			return ret;
		if (SunAppSrvPlugin.is31OrAbove(getServer().getRuntime()))
			ret = "keepstate";
		else
			ret = "keepSessions";
		return ret;
	}

	/*
	 * JAR deploy for v3
	 */
	public String getJarDeploy() {
		String s = getProps().get(JARDEPLOY);
		if (s == null) {
			s = Boolean.FALSE.toString(); // by default, false
		}
		return s;
	}

	public void getJarDeploy(String value) {
		getProps().put(JARDEPLOY, value);
	}

	public String getUseAnonymousConnections() {
		String s = getProps().get(USEANONYMOUSCONNECTIONS);
		if (s == null) {
			String pass = getAdminPassword();
			if ((s != null) && (!s.equals(""))) {
				return Boolean.FALSE.toString();
			}
			s = Boolean.TRUE.toString();
		}
		return s;
	}

	public String getServerPort() {
		return getProps().get(SERVERPORT);
	}

	/*
	 * (non-Javadoc) Override this method to provide our server port for the
	 * http port Called by getModuleRootURL in the superclass, which returns
	 * 8080 if it doesn't find something else. The override of getServerPorts()
	 * implementation which provides our server port as an http port is not used
	 * by the superclass' method, so we must override this as well.
	 * 
	 * @see
	 * org.eclipse.jst.server.generic.core.internal.GenericServer#getHttpPort()
	 */
	@Override
	protected int getHttpPort() {
		return Integer.parseInt(getServerPort());
	}

	public String getAdminServerPort() {
		return getProps().get(ADMINSERVERPORT);
	}

	public String getAdminName() {
		return (String) getProps().get(ADMINNAME);
	}

	public void setAdminName(String value) {
		getProps().put(ADMINNAME, value);
	}

	public String getDomainPath() {
		return getProps().get(DOMAINPATH);
	}

	public String getSampleDatabaseDir() {
		// return (String) getProps().get(SAMPLEDBDIR);
		return DerbyConfigurator.getSampleDBLocation();
	}

	private void syncHostAndPortsValues() {
		Map<String, String> props = getProps();
		String currentHostValue = (String) props.get(ADDRESS);
		String genericHostValue = getServer().getHost();

		if ((currentHostValue == null)
				|| !currentHostValue.equals(genericHostValue)) {
			props.put(ADDRESS, genericHostValue);
		}
	}

	public void saveConfiguration(IProgressMonitor m) throws CoreException {
		SunAppSrvPlugin.logMessage("in Save SunAppServer"); //$NON-NLS-1$
		// if (initializedCalled==false){
		// readDomainConfig();
		//            SunAppSrvPlugin.logMessage("in Save SunAppServer done" );	//$NON-NLS-1$
		// }
		// syncHostAndPortsValues();

		super.saveConfiguration(m);
	}

	/**
	 * Add a property change listener to this server.
	 * 
	 * @param listener
	 *            java.beans.PropertyChangeListener
	 */
	public void addPropertyChangeListener(PropertyChangeListener listener) {
		if (propChangeListeners == null)
			propChangeListeners = new ArrayList<PropertyChangeListener>();
		propChangeListeners.add(listener);
	}

	/**
	 * Remove a property change listener from this server.
	 * 
	 * @param listener
	 *            java.beans.PropertyChangeListener
	 */
	public void removePropertyChangeListener(PropertyChangeListener listener) {
		if (propChangeListeners != null)
			propChangeListeners.remove(listener);
	}

	/**
	 * Fire a property change event.
	 * 
	 * @param propertyName
	 *            a property name
	 * @param oldValue
	 *            the old value
	 * @param newValue
	 *            the new value
	 */
	public void firePropertyChangeEvent(String propertyName, Object oldValue,
			Object newValue) {
		if (propChangeListeners == null)
			return;

		PropertyChangeEvent event = new PropertyChangeEvent(this, propertyName,
				oldValue, newValue);
		try {
			Iterator<PropertyChangeListener> iterator = propChangeListeners
					.iterator();
			while (iterator.hasNext()) {
				try {
					PropertyChangeListener listener = (PropertyChangeListener) iterator
							.next();
					listener.propertyChange(event);
				} catch (Exception e) {
					SunAppSrvPlugin.logMessage(
							"Error firing property change event", e); //$NON-NLS-1$
				}
			}
		} catch (Exception e) {
			SunAppSrvPlugin.logMessage("Error in property event", e); //$NON-NLS-1$
		}
	}

	public String getAdminPassword() {

		return (String) getProps().get(ADMINPASSWORD);
	}

	public void setAdminPassword(String value) {
		getProps().put(ADMINPASSWORD, value);
		try {
			// this.saveConfiguration(new NullProgressMonitor());
			this.configurationChanged();
		} catch (Exception ex) {
			SunAppSrvPlugin.logMessage("error =" + ex); //$NON-NLS-1$
		}
	}

	public static GlassfishGenericServer getSunAppServer(
			IServerWorkingCopy server) {
		GlassfishGenericServer sunserver = (GlassfishGenericServer) server
				.getOriginal().getAdapter(GlassfishGenericServer.class);
		if (sunserver == null) {
			sunserver = (GlassfishGenericServer) server.getOriginal()
					.loadAdapter(GlassfishGenericServer.class,
							new NullProgressMonitor());
		}
		return sunserver;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.jst.server.generic.core.internal.GenericServer#setDefaults
	 * (org.eclipse.core.runtime.IProgressMonitor) This implementation overrides
	 * the automatic publishing option to disable it by default.
	 */
	@Override
	public void setDefaults(IProgressMonitor monitor) {
		SunAppSrvPlugin
				.logMessage("In  setDefaults for " + this.getServer().getServerType().getName()); //$NON-NLS-1$
		setDefaultPublishState();
		super.setDefaults(monitor);

	}

	public void setDefaultPublishState() {
		setAttribute(Server.PROP_AUTO_PUBLISH_SETTING, 2/*
														 * Server.
														 * AUTO_PUBLISH_OVERRIDE
														 */);
		setAttribute(Server.PROP_AUTO_PUBLISH_TIME, 0);
	}

	public ServerPort[] getServerPorts() {
		try {
			ServerPort[] sp = new ServerPort[2];
			sp[0] = new ServerPort("adminserver", "Admin Server Port",
					Integer.parseInt(getAdminServerPort()), "HTTP");
			sp[1] = new ServerPort("server", "Server Port",
					Integer.parseInt(getServerPort()), "HTTP");

			return sp;
		} catch (Exception e) {
			return new ServerPort[0];
		}
	}

	public String getServerInstallationDirectory() {
		return getServer().getRuntime().getLocation().toString();
	}

	private boolean readServerConfiguration(File domainXml) {
		boolean result = false;
		final Map<String, HttpData> httpMap = new LinkedHashMap<String, HttpData>();

		if (domainXml.exists()) {
			//JmxConnectorReader jmxReader = new JmxConnectorReader();
			HttpListenerReader httpListenerReader = new HttpListenerReader();
			NetworkListenerReader networkListenerReader = new NetworkListenerReader();

			try {
				TreeParser.readXml(domainXml, httpListenerReader,
						networkListenerReader);
				//jmxPort = jmxReader.getResult();

				httpMap.putAll(httpListenerReader.getResult());
				httpMap.putAll(networkListenerReader.getResult());
				// !PW This probably more convoluted than it had to be, but
				// while
				// http-listeners are usually named "http-listener-1",
				// "http-listener-2", ...
				// technically they could be named anything.
				//
				// For now, the logic is as follows:
				// admin port is the one named "admin-listener"
				// http port is the first non-secure enabled port - typically
				// http-listener-1
				// https port is the first secure enabled port - typically
				// http-listener-2
				// disabled ports are ignored.
				//
				HttpData adminData = httpMap.remove("admin-listener"); //$NON-NLS-1$

				getProps().put(
						ADMINSERVERPORT,
						String.valueOf(adminData != null ? adminData.getPort()
								: -1)); //$NON-NLS-1$
				SunAppSrvPlugin
						.logMessage("reading from domain.xml adminServerPortNumber=" + getAdminServerPort()); //$NON-NLS-1$

				HttpData httpData = null;
				HttpData httpsData = null;

				for (HttpData data : httpMap.values()) {
					if (data.isSecure()) {
						if (httpsData == null) {
							httpsData = data;
						}
					} else {
						if (httpData == null) {
							httpData = data;
						}
					}
					if (httpData != null && httpsData != null) {
						break;
					}
				}

				int httpPort = httpData != null ? httpData.getPort() : -1;
				getProps().put(SERVERPORT, String.valueOf(httpPort));
				SunAppSrvPlugin
						.logMessage("reading from domain.xml serverPortNumber=" + getServerPort()); //$NON-NLS-1$
				// ///ludo secure TODO wi.setHttpsPort(httpsData != null ?
				// httpsData.getPort() : -1);

				result = httpPort != -1;
			} catch (IllegalStateException ex) {
				SunAppSrvPlugin.logMessage("error IllegalStateException ", ex); //$NON-NLS-1$
			}
		}
		return result;
	}

	public CommandFactory getCommandFactory() {

		return new CommandFactory() {
			public SetPropertyCommand getSetPropertyCommand(String name,
					String value) {
				return new ServerCommand.SetPropertyCommand(name, value,
						"DEFAULT={0}={1}");
			}
		};

	}

	public String getDomainConfigurationFilePath() {
		return getDomainPath().trim() + "/config/domain.xml";
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
		return new File(getDomainPath()).getParent();
	}

	@Override
	public String getDomainName() {
		return new File(getDomainPath()).getName();
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
		return GlassFishAdminInterface.HTTP;
	}

	@Override
	public String getServerHome() {
		return new File(getServer().getRuntime().getLocation().toString())
				.getAbsolutePath();
	}
	
	@Override
	public String getServerRoot() {
		return null;
	}
}
