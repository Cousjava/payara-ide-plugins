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

import java.io.File;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jst.server.core.IEnterpriseApplication;
import org.eclipse.jst.server.generic.core.internal.CorePlugin;
import org.eclipse.jst.server.generic.core.internal.GenericServerBehaviour;
import org.eclipse.jst.server.generic.core.internal.GenericServerCoreMessages;
import org.eclipse.jst.server.generic.core.internal.GenericServerRuntime;
import org.eclipse.osgi.util.NLS;
import org.eclipse.ui.PlatformUI;
import org.eclipse.wst.common.componentcore.internal.util.ComponentUtilities;
import org.eclipse.wst.server.core.IModule;
import org.eclipse.wst.server.core.IServer;
import org.eclipse.wst.server.core.ServerPort;
import org.eclipse.wst.server.core.model.ServerBehaviourDelegate;
import org.eclipse.wst.server.core.util.PublishHelper;
import org.eclipse.wst.server.core.util.SocketUtil;
import org.glassfish.tools.ide.GlassFishIdeException;
import org.glassfish.tools.ide.admin.Command;
import org.glassfish.tools.ide.admin.CommandAddResources;
import org.glassfish.tools.ide.admin.CommandDeploy;
import org.glassfish.tools.ide.admin.CommandGetProperty;
import org.glassfish.tools.ide.admin.CommandStopDAS;
import org.glassfish.tools.ide.admin.CommandUndeploy;
import org.glassfish.tools.ide.admin.CommandVersion;
import org.glassfish.tools.ide.admin.ResultMap;
import org.glassfish.tools.ide.admin.ResultString;
import org.glassfish.tools.ide.admin.ServerAdmin;
import org.glassfish.tools.ide.admin.TaskState;
import org.glassfish.tools.ide.data.IdeContext;
import org.glassfish.tools.ide.utils.ServerUtils;

import com.sun.enterprise.jst.server.sunappsrv.derby.DerbyConfigurator;
import com.sun.enterprise.jst.server.sunappsrv.log.GlassfishConsoleManager;
import com.sun.enterprise.jst.server.sunappsrv.serverview.Utils;
import com.sun.enterprise.jst.server.sunappsrv.sunresource.wizards.ResourceUtils;

/**
 *
 *
 */
@SuppressWarnings("restriction")
public abstract class GlassfishGenericServerBehaviour extends
		GenericServerBehaviour {
	private static final String DEFAULT_DOMAIN_DIR_NAME = "domains"; //$NON-NLS-N$
	private static final String DEFAULT_DOMAIN_NAME = "domain1"; //$NON-NLS-N$
	// not used yet private GlassFishV2DeployFacility gfv2depl=null;//lazy
	// initialized
	protected boolean needARedeploy = true; // by default, will be calculated..
	
	public enum ServerStatus {
		DOMAINDIR_MATCHING, DOMAINDIR_NOT_MATCHING, CONNEXTION_ERROR, CREDENTIAL_ERROR, MBEAN_ERROR, WRONG_SERVER_TYPE
	}

	/** Creates a new instance of SunAppServerBehaviour */
	public GlassfishGenericServerBehaviour() {
		SunAppSrvPlugin.logMessage("in SunAppServerBehaviour CTOR ");

	}

	protected void initialize(IProgressMonitor monitor) {
		super.initialize(monitor);
		SunAppSrvPlugin.logMessage("in SunAppServerBehaviour initialize");
		final GlassfishGenericServer sunserver = getSunAppServer();
		try {
			if (!sunserver.isRemote()) {
				DerbyConfigurator.configure(null,
						new File(sunserver.getServerInstallationDirectory()),
						sunserver.getDomainConfigurationFilePath());
			}
		} catch (CoreException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		if (isRunning()) {
			SunAppSrvPlugin
					.logMessage("in SunAppServerBehaviour initialize is running!!");

			if (sunserver.isRemote()) {
				setStartedState();
				return;
			}
			if (getServerStatus() == ServerStatus.DOMAINDIR_MATCHING) {
				SunAppSrvPlugin
						.logMessage("in SunAppServerBehaviour initialize V3 DOMAINDIR_MATCHING");
				setStartedState();
				return;
			} else {
				SunAppSrvPlugin
						.logMessage("***in SunAppServerBehaviour initialize V3 DOMAINDIR_NOT_MATCHING, will reset to stop shortly");
			}

		}

		try {
			sunserver.isDomainValid();
		} catch (CoreException e) {
			SunAppSrvPlugin
					.logMessage("in SunAppServerBehaviour initialize detected domain configuration problem...");
		}

		SunAppSrvPlugin
				.logMessage("in SunAppServerBehaviour initialize STOP by Default...");
		setServerState(IServer.STATE_STOPPED);
		resetStatus(IServer.STATE_STOPPED);
	}

	/*
	 * get the correct adapter for the GlassFish server
	 */
	public GlassfishGenericServer getSunAppServer() {
		// return (SunAppServer)getServer().getAdapter(SunAppServer.class);
		GlassfishGenericServer sunserver = (GlassfishGenericServer) getServer()
				.getAdapter(GlassfishGenericServer.class);
		if (sunserver == null) {
			sunserver = (GlassfishGenericServer) getServer().loadAdapter(
					GlassfishGenericServer.class, new NullProgressMonitor());
		}
		return sunserver;
	}

	/*
	 * stub this public method from parent since we are not using <start> class
	 * definition in the serverdef. This was called and created a NPE for our
	 * special case. Note that this workaround was here before (added in plugin
	 * version 1.0.3) but caused issue 33, so it was removed. It is now causing
	 * the NPE again sometimes, and testing issue 33 is successful, so it is
	 * being put back. We need to keep an eye on it, though. Not needed
	 * 
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jst.server.generic.core.internal.GenericServerBehaviour#
	 * setupLaunchConfiguration
	 * (org.eclipse.debug.core.ILaunchConfigurationWorkingCopy,
	 * org.eclipse.core.runtime.IProgressMonitor)
	 */
	public void setupLaunchConfiguration(
			ILaunchConfigurationWorkingCopy workingCopy,
			IProgressMonitor monitor) throws CoreException {
		// do nothing...to prevent a NPE in wtp2.0.2 winter edition
		// SunAppSrvPlugin.logMessage("in SunAppServerBehaviour setupLaunchConfiguration doing nothing");

	}

	protected void setupLaunch(ILaunch launch, String launchMode,
			IProgressMonitor monitor) throws CoreException {
		int state = getServer().getServerState();
		SunAppSrvPlugin
				.logMessage("in SunAppServerBehaviour setupLaunch state="
						+ state);

		if (getSunAppServer().isRemote()) {
			SunAppSrvPlugin
					.logMessage("in SunAppServerBehaviour CTOR after sunserver it is running!!!");
			setMode(launchMode); // ILaunchManager.RUN_MODE);
			setStartedState(launchMode);

		}
		state = getServer().getServerState();
		if (state != IServer.STATE_STARTED) {
			try {
				// super.setupLaunch(launch, launchMode, monitor);
				if (!"true".equals(launch.getLaunchConfiguration().getAttribute("stop-server", "false"))) {//$NON-NLS-1$ //$NON-NLS-2$

					String host = getServer().getHost();
					ServerPort[] ports = getServer().getServerPorts(null);
					ServerPort sp = null;
					if (SocketUtil.isLocalhost(host)) {
						for (int i = 0; i < ports.length; i++) {
							sp = ports[i];
							if (SocketUtil.isPortInUse(ports[i].getPort(), 5))
								throw new CoreException(
										new Status(
												IStatus.ERROR,
												CorePlugin.PLUGIN_ID,
												0,
												NLS.bind(
														GenericServerCoreMessages.errorPortInUse,
														Integer.toString(sp
																.getPort()), sp
																.getName()),
												null));
						}
					}
					setServerState(IServer.STATE_STARTING);
					setMode(launchMode);
				}
			} catch (CoreException ce) {

				SunAppSrvPlugin
						.logMessage("in SunAppServerBehaviour setupLaunch  ALREADY STARTED!!!!!!");
				setMode(launchMode); // ILaunchManager.RUN_MODE);
				setStartedState(launchMode);
				return;
			}
		}
		resetStatus(state);
	}

	public void setStartedState() {
		setStartedState(null);
	}

	public void setStartedState(String mode) {
		if (null != mode) {
			setMode(mode);
		}
		setServerState(IServer.STATE_STARTED);
		resetStatus(IServer.STATE_STARTED);
	}

	protected synchronized void setServerStarted() {
		setServerState(IServer.STATE_STARTED);
	}

	/*
	 * If the server state is unknown, reset the status to OK
	 */
	private void resetStatus(int state) {
		if (state == IServer.STATE_UNKNOWN) {
			setServerStatus(null);
		}
	}

	public GenericServerRuntime getRuntimeDelegate() {
		return (GenericServerRuntime) getServer().getRuntime().loadAdapter(
				GenericServerRuntime.class, null);
	}

	public String getVersion() throws GlassFishIdeException {
		Command command = new CommandVersion();
		IdeContext ide = new IdeContext();
		Future<ResultString> future = ServerAdmin.exec(getSunAppServer(),
				command, ide);
		try {
			ResultString result = future.get(30, TimeUnit.SECONDS);
			return result.getValue();
		} catch (InterruptedException e) {
			throw new GlassFishIdeException("Exception by calling getVersion",
					e);
		} catch (java.util.concurrent.ExecutionException e) {
			throw new GlassFishIdeException("Exception by calling getVersion",
					e);
		} catch (TimeoutException e) {
			throw new GlassFishIdeException(
					"Timeout for getting version command exceeded", e);
		}
	}

	/**
	 * Checks if the server is running.
	 * 
	 * @return true if we can ping to the server
	 * @throws CoreException
	 */
	public boolean isRunning() {
		GlassfishGenericServer server = getSunAppServer();
		// server port is not set yet
		try {
			server.getPort();
		} catch (NumberFormatException e) {
			if (ServerUtils.isDASRunning(server))
				updateHttpPort();
			else
				return false;
		}
		
		if (getSunAppServer().isRemote()) {// remote is for 3.0 or above, then we use get version
							// http/s request to avoid
			// proxy security issues with a simple socket usage...
			try {
				getVersion();
				return true;
			} catch (GlassFishIdeException e) {
				// something happened so we assume that the server is not
				// running
				return false;
			}
		}
		if (server.getAdminServerPort().equals("")) { //$NON-NLS-1$
			SunAppSrvPlugin
					.logMessage("catastrophic state where adminServerPortNumber is not initialized in SunAppServer.java"); //$NON-NLS-1$
			SunAppSrvPlugin
					.logMessage("catastrophic Only thing to do is restart Eclipse"); //$NON-NLS-1$
			server.initialize();
			/*
			 * throw new CoreException(new Status(IStatus.ERROR,
			 * SunAppSrvPlugin.SUNPLUGIN_ID,
			 * IJavaLaunchConfigurationConstants.ERR_INTERNAL_ERROR,
			 * "Error where adminServerPortNumber is not initialized and null in GlassFish Plugin. Restart Eclipse IDE"
			 * , new RuntimeException
			 * ("Restart Eclipse. Internal State corrupted...")));
			 */

		}
		return ServerUtils.isRunning(server);
	}

	public String getDomainName() {
		GlassfishGenericServer sunserver = getSunAppServer();
		String d = sunserver.getDomainName();
		if (isEmpty(d)) {
			d = DEFAULT_DOMAIN_NAME;
		}
		return d;
	}

	public String getDomainDir() {
		GlassfishGenericServer sunserver = getSunAppServer();
		String d = sunserver.getDomainsFolder();
		if (isEmpty(d)) {
			d = getDefaultDomainDir();
		}
		return d;
	}

	private String getDefaultDomainDir() {
		return getSunAppServer().getServerInstallationDirectory()
				+ File.separatorChar + DEFAULT_DOMAIN_DIR_NAME;
	}

	public String getSampleDatabaseDir() {
		return getSunAppServer().getSampleDatabaseDir();
	}

	private boolean isEmpty(String testString) {
		return ((testString == null) || (testString.trim().length() == 0));
	}

	public String getSunApplicationServerAdminPort() {
		// String port=
		// (String)getRuntimeDelegate().getServerInstanceProperties().get(SunAppServer.ADMINSERVERPORT);
		GlassfishGenericServer sunserver = getSunAppServer();
		// SunAppSrvPlugin.logMessage("sunappserver.adminserverportnumber we are looking for this prop value:"+sunserver.getAdminServerPort());
		return sunserver.getAdminServerPort();
	}

	public String getServerPort() {
		GlassfishGenericServer sunserver = getSunAppServer();
		// SunAppSrvPlugin.logMessage("sunappserver.adminserverportnumber we are looking for this prop value:"+sunserver.getServerPort());
		return sunserver.getServerPort();
	}

	@Override
	public void restart(final String launchMode) throws CoreException {
		SunAppSrvPlugin.logMessage("in SunAppServerBehaviour restart");
		setServerState(IServer.STATE_STOPPING);
		stopServer(false);
		setServerState(IServer.STATE_STOPPED);
		Thread thread = new Thread("Synchronous server start") {
			public void run() {
				try {
					//setServerState(IServer.STATE_STARTING);
					// SunAppSrvPlugin.logMessage("in !!!!!!!SunAppServerBehaviour restart");
					getServer().getLaunchConfiguration(true, null).launch(launchMode, new NullProgressMonitor());
					//getServer().start(launchMode, new NullProgressMonitor());
					SunAppSrvPlugin
							.logMessage("in SunAppServerBehaviour restart done");
					
				} catch (Exception e) {
					SunAppSrvPlugin.logMessage(
							"in SunAppServerBehaviour restart", e);
				}
			}
		};
		
		thread.setDaemon(true);
		thread.start();

	}

	@Override
	public void stop(boolean force) {
		SunAppSrvPlugin.logMessage("in SunAppServerBehaviour stop");
		

		stopServer(true);
		resetStatus(getServer().getServerState());
		setServerState(IServer.STATE_STOPPED);
	}

	/**
	 * 
	 * @stop GlassFish v3 or v3 prelude via http command
	 */
	protected void stopServer(boolean stopLogging) {
		final GlassfishGenericServer server = getSunAppServer();
		if (server.isRemote()) {
			PlatformUI.getWorkbench().getDisplay().syncExec(new Runnable() {
				public void run() {
					boolean answer = MessageDialog
							.openQuestion(
									PlatformUI.getWorkbench().getDisplay()
											.getActiveShell(),// getShell(),
									"Stopping a remote sever",
									"Are you sure you want to stop a remote domain? \nYou will not be able to start it again from this machine...");
					if (answer) {
						stopImpl(server);
					}
				}
			});

			return;
		}
		
		stopImpl(server);
		if (stopLogging)
			GlassfishConsoleManager.getConsole(server).stopLogging();
	}
	
	private void stopImpl(GlassfishGenericServer server) {
		ResultString result = CommandStopDAS.stopDAS(server);

		if (!TaskState.COMPLETED.equals(result.getState())) {
			SunAppSrvPlugin.logMessage("stop-domain v3 is failing. Reason: " + result.getValue()); //$NON-NLS-1$
		}
		
		// wait util is really stopped
		while (isRunning()) {
			try {
				Thread.sleep(1000);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
	}
	

	public String getDomainDirWithDomainName() {
		return getDomainDir().trim() + File.separatorChar + getDomainName();
	}

	protected String getConfigTypeID() {
		return SunAppSrvPlugin.SUNPLUGIN_ID
				+ ".SunAppServerLaunchConfigurationType";
	}

	/**
	 * Returns the String name of the stop launch configuration.
	 * 
	 * @return
	 */
	protected String getStopLaunchName() {
		return "ExternalLaunchStopper";
	}

	public void undeploy(String moduleName, IProgressMonitor monitor)
			throws CoreException {
		try {

			undeploy(moduleName);
			// Retrieve the IModule for the module name
			final List<IModule[]> moduleList = getAllModules();
			IModule[] module = null;
			for (IModule[] m : moduleList) {
				if (m.length == 1 && m[0].getName().equals(moduleName)) {
					module = m;
					break;
				}
			}
			// if we were able to map module name to IModule, set publish state
			// to Full to tell
			// a full deploy would be needed
			if (module != null) {
				setModulePublishState(module, IServer.PUBLISH_STATE_FULL);
			}

		} finally {
		}
	}

	private void undeploy(String moduleName) throws CoreException {
		CommandUndeploy cmd = new CommandUndeploy(moduleName, null);
		try {
			Future<ResultString> future =
                    ServerAdmin.<ResultString>exec(getSunAppServer(), cmd, new IdeContext());
                ResultString result = future.get(520, TimeUnit.SECONDS);
                if (!TaskState.COMPLETED.equals(result.getState())) {
                	SunAppSrvPlugin.logMessage("deploy is failing=" + result.getValue());
					throw new Exception("undeploy is failing=" + result.getValue());
                }
		} catch (Exception ex) {
			SunAppSrvPlugin.logMessage("Undeploy is failing=", ex);
			throw new CoreException(new Status(IStatus.ERROR,
					SunAppSrvPlugin.SUNPLUGIN_ID, 0, "cannot UnDeploy "
							+ moduleName, ex));
		}
	}

	/**
	 * Checks if the Ant publisher actually needs to publish. For ear modules it
	 * also checks if any of the children modules requires publishing.
	 * 
	 * @return true if ant publisher needs to publish.
	 */
	protected boolean publishNeeded(int kind, int deltaKind, IModule[] module) {
		if (kind != IServer.PUBLISH_INCREMENTAL && kind != IServer.PUBLISH_AUTO)
			return true;
		if (deltaKind != ServerBehaviourDelegate.NO_CHANGE)
			return true;
		if (AssembleModules.isModuleType(module[0], "jst.ear")) { //$NON-NLS-1$
			IEnterpriseApplication earModule = (IEnterpriseApplication) module[0]
					.loadAdapter(IEnterpriseApplication.class,
							new NullProgressMonitor());
			IModule[] childModules = earModule.getModules();
			for (int i = 0; i < childModules.length; i++) {
				IModule m = childModules[i];
				IModule[] modules = { module[0], m };
				if (IServer.PUBLISH_STATE_NONE != getSunAppServer().getServer()
						.getModulePublishState(modules))
					return true;
			}
		}
		return false;
	}
	
	protected void publishDeployedDirectory(int deltaKind, Properties p,
			IModule module[], IProgressMonitor monitor) throws CoreException {
		// ludo using PublishHelper now to control the temp area to be
		// in the same file system of the deployed apps so that the mv operation
		// Eclipse is doing sometimes can work.
		PublishHelper helper = new PublishHelper(new Path(
				getDomainDirWithDomainName() + "/eclipseAppsTmp").toFile());

		if (deltaKind == REMOVED) {
			String publishPath = (String) p.get(module[0].getId());
			SunAppSrvPlugin.logMessage("REMOVED in publishPath" + publishPath);
			String name = Utils.simplifyModuleID(module[0].getName());
			undeploy(name);

			if (publishPath != null) {
				try {
					File pub = new File(publishPath);
					if (pub.exists()) {
						SunAppSrvPlugin
								.logMessage("PublishUtil.deleteDirectory called");
						IStatus[] stat = PublishHelper.deleteDirectory(pub,
								monitor);
						analyseReturnedStatus(stat);
					}
				} catch (Exception e) {
					throw new CoreException(new Status(IStatus.WARNING,
							SunAppSrvPlugin.SUNPLUGIN_ID, 0, "cannot remove "
									+ module[0].getName(), e));
				}
			}
		} else {
			IPath path = new Path(getDomainDirWithDomainName()
					+ "/eclipseApps/" + module[0].getName());

			// IModuleResource[] moduleResource = getResources(module);
			// SunAppSrvPlugin.logMessage("IModuleResource len="+moduleResource.length);
			// for (int j=0;j<moduleResource.length ;j++
			// SunAppSrvPlugin.logMessage("IModuleResource n="+moduleResource[j].getName()+"-----"+moduleResource[j].getModuleRelativePath());

			String contextRoot = null;
			AssembleModules assembler = new AssembleModules(module[0], path,
					getSunAppServer(), helper);
			SunAppSrvPlugin.logMessage("PublishUtil.publishSmart called");

			// either ear or web.
			if (AssembleModules.isModuleType(module[0], "jst.web")) {
				SunAppSrvPlugin.logMessage("is WEB");
				assembler.assembleWebModule(monitor);

				needARedeploy = assembler.needsARedeployment();
				String projectContextRoot = ComponentUtilities
						.getServerContextRoot(module[0].getProject());
				contextRoot = (((projectContextRoot != null) && (projectContextRoot
						.length() > 0)) ? projectContextRoot : module[0]
						.getName());
			} else if (AssembleModules.isModuleType(module[0], "jst.ear")) {
				SunAppSrvPlugin.logMessage("is EAR");
				assembler.assembleDirDeployedEARModule(monitor);
				needARedeploy = assembler.needsARedeployment();

			} else {// default
				assembler.assembleNonWebOrNonEARModule(monitor);
				needARedeploy = assembler.needsARedeployment();

			}

			// deploy the sun resource file if there in path:
			registerSunResource(module, p, path);

			String spath = "" + path;
			// /BUG NEED ALSO to test if it has been deployed
			// once...isDeployed()
			if (needARedeploy) {
				String name = Utils.simplifyModuleID(module[0].getName());

				Map<String, String> properties = new HashMap<String, String>();
				File[] libraries = new File[0];
				CommandDeploy command = new CommandDeploy(name, null, new File(spath), contextRoot, properties, libraries);
				try {
					Future<ResultString> future =
		                    ServerAdmin.<ResultString>exec(getSunAppServer(), command, new IdeContext());
		                ResultString result = future.get(120, TimeUnit.SECONDS);
		                if (!TaskState.COMPLETED.equals(result.getState())) {
		                	SunAppSrvPlugin.logMessage("deploy is failing=" + result.getValue());
							throw new Exception("deploy is failing=" + result.getValue());
		                }
				} catch (Exception ex) {
					SunAppSrvPlugin.logMessage("deploy is failing=", ex);
					throw new CoreException(new Status(IStatus.ERROR,
							SunAppSrvPlugin.SUNPLUGIN_ID, 0, "cannot Deploy "
									+ name, ex));
				}
			} else {
				SunAppSrvPlugin
						.logMessage("optimal: NO NEED TO TO A REDEPLOYMENT, !!!");

			}
		}
	}
	
	public abstract ServerStatus getServerStatus();

	public void updateHttpPort() {
		GlassfishGenericServer server = getSunAppServer();
		CommandGetProperty cgp;
		cgp = new CommandGetProperty("*.server-config.*.http-listener-1.port");
		Future<ResultMap<String, String>> future = ServerAdmin.<ResultMap<String, String>>exec(getSunAppServer(), cgp, new IdeContext());
        ResultMap<String, String> result = null;
        try {
			result = future.get(20, TimeUnit.SECONDS);
		} catch (InterruptedException e) {
			SunAppSrvPlugin.logMessage("Unable to retrieve server http port for server ", e);
		} catch (java.util.concurrent.ExecutionException e) {
			SunAppSrvPlugin.logMessage("Unable to retrieve server http port for target ", e);
		} catch (TimeoutException e) {
			SunAppSrvPlugin.logMessage("Unable to retrieve server http port for target ", e);
		}
        
        boolean portSet = false;
        if ((result != null) && TaskState.COMPLETED.equals(result.getState())) {
        	for (Entry<String, String> entry : result.getValue().entrySet()) {
                String val = entry.getValue();
                try {
                    if (null != val && val.trim().length() > 0) {
                        Integer.parseInt(val);
                        server.getProps().put(GlassfishGenericServer.SERVERPORT, val);
                        portSet = true;
                        break;
                    }
                } catch (NumberFormatException nfe) {
                    // skip it quietly..
                }
            }
        }
        
        if (!portSet) {
        	server.getProps().put(GlassfishGenericServer.SERVERPORT, "28080");
        }
	}
	
	protected void registerSunResource(IModule module[], Properties p,
			IPath path) throws CoreException {
		// Get correct location for sun-resources.xml
		IProject project = module[0].getProject();
		String location = ResourceUtils.getRuntimeResourceLocation(project);
		if (location != null) {
			if (location.trim().length() > 0) {
				location = location + File.separatorChar
						+ ResourceUtils.RESOURCE_FILE_NAME;
			} else {
				location = ResourceUtils.RESOURCE_FILE_NAME;
			}
		}
		File sunResource = new File("" + path, location);
		if (sunResource.exists()) {
			ResourceUtils.checkUpdateServerResources(sunResource,
					getSunAppServer());
			CommandAddResources command = new CommandAddResources(sunResource, null);
			try {
				Future<ResultString> future =
	                    ServerAdmin.<ResultString>exec(getSunAppServer(), command, new IdeContext());
	                ResultString result = future.get(120, TimeUnit.SECONDS);
	                if (!TaskState.COMPLETED.equals(result.getState())) {
	                	SunAppSrvPlugin.logMessage("register resource is failing=" + result.getValue());
						throw new Exception("register resource is failing=" + result.getValue());
	                }
			} catch (Exception ex) {
				SunAppSrvPlugin.logMessage(
						"deploy of sun-resources is failing ", ex);
				throw new CoreException(new Status(IStatus.ERROR,
						SunAppSrvPlugin.SUNPLUGIN_ID, 0,
						"cannot register sun-resource.xml for "
								+ module[0].getName(), ex));
			}
		}
		p.put(module[0].getId(), path.toOSString());

	}

	protected void analyseReturnedStatus(IStatus[] status) throws CoreException {

		if (status == null || status.length == 0) {
			return;
		}
		/*
		 * if (status.length == 1) { throw new CoreException(status[0]); }
		 * String message = "GlassFish: Error Deploying"; MultiStatus ms = new
		 * MultiStatus(SunAppSrvPlugin.SUNPLUGIN_ID, 0, status, message, null);
		 * throw new CoreException(ms);
		 */
		for (IStatus s : status) {
			SunAppSrvPlugin.logMessage("analyseReturnedStatus: "
					+ s.getMessage());

		}
	}

}
