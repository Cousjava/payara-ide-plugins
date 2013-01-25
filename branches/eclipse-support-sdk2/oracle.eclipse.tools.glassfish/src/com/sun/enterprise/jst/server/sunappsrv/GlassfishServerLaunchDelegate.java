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
import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.tools.ant.taskdefs.Execute;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.debug.core.DebugEvent;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.IDebugEventSetListener;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.jdt.internal.debug.core.model.JDIDebugTarget;
import org.eclipse.jdt.launching.AbstractJavaLaunchConfigurationDelegate;
import org.eclipse.jdt.launching.AbstractVMInstall;
import org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants;
import org.eclipse.jdt.launching.IVMConnector;
import org.eclipse.jdt.launching.JavaRuntime;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.ui.PlatformUI;
import org.eclipse.wst.server.core.IServer;
import org.eclipse.wst.server.core.ServerUtil;
import org.eclipse.wst.server.core.model.ServerBehaviourDelegate;
import org.glassfish.tools.ide.admin.ResultProcess;
import org.glassfish.tools.ide.server.FetchLog;
import org.glassfish.tools.ide.server.ServerTasks;
import org.glassfish.tools.ide.server.ServerTasks.StartMode;
import org.glassfish.tools.ide.utils.ServerUtils;
import org.glassfish.tools.ide.utils.Utils;

import com.sun.enterprise.jst.server.sunappsrv.GlassfishGenericServerBehaviour.ServerStatus;
import com.sun.enterprise.jst.server.sunappsrv.log.GlassfishConsoleManager;
import com.sun.enterprise.jst.server.sunappsrv.log.IGlassFishConsole;
import com.sun.enterprise.jst.server.sunappsrv.preferences.PreferenceConstants;

@SuppressWarnings("restriction")
public class GlassfishServerLaunchDelegate extends AbstractJavaLaunchConfigurationDelegate {

	public static final String GFV3_MODULES_DIR_NAME = "modules"; //$NON-NLS-1$

	private static Pattern debugPortPattern = Pattern
			.compile("-Xrunjdwp:\\S*address=([0-9]+)");

	public GlassfishServerLaunchDelegate() {
		// SunAppSrvPlugin.logMessage("in SUN SunAppServerLaunch ctor");
	}

	protected void abort(String message, Throwable exception, int code)
			throws CoreException {
		throw new CoreException(new Status(IStatus.ERROR,
				SunAppSrvPlugin.SUNPLUGIN_ID, code, message, exception));
	}

	public void launch(ILaunchConfiguration configuration, String mode,
			ILaunch launch, IProgressMonitor monitor) throws CoreException {
		SunAppSrvPlugin.logMessage("in SUN SunAppServerLaunch launch"); //$NON-NLS-1$

		IServer server = ServerUtil.getServer(configuration);
		if (server == null) {
			abort("missing Server", null, IJavaLaunchConfigurationConstants.ERR_INTERNAL_ERROR); //$NON-NLS-1$
		}

		final GlassfishGenericServerBehaviour serverBehavior = (GlassfishGenericServerBehaviour) server
				.loadAdapter(ServerBehaviourDelegate.class, null);
		final GlassfishGenericServer serverAdapter = serverBehavior
				.getSunAppServer();

		// find out if our server is really running and ready
		boolean isReady = isServerReady(serverBehavior);
		if (isReady) {
			SunAppSrvPlugin.logMessage("server is already started!!!");
		}
		try {
			if (serverAdapter.isRemote()) {
				if (!isReady) {
					abort("GlassFish Remote Servers cannot be start from this machine.",
							null,
							IJavaLaunchConfigurationConstants.ERR_INTERNAL_ERROR);
				}
			} else {
				if (!isReady) {
					startDASAndTarget(serverAdapter, serverBehavior,
							configuration, launch, mode, monitor);
				}
			}
		} catch (InterruptedException e) {
			GlassfishConsoleManager.showConsole(serverAdapter).stopLogging();
			SunAppSrvPlugin.logMessage("Unable to start server on time.", e);
		}

		serverBehavior.setStartedState(mode);
	}

	private void startDASAndTarget(final GlassfishGenericServer serverAdapter,
			GlassfishGenericServerBehaviour serverBehavior,
			ILaunchConfiguration configuration, ILaunch launch, String mode,
			IProgressMonitor monitor) throws CoreException,
			InterruptedException {
		String domain = serverBehavior.getDomainName();
		String domainAbsolutePath = serverBehavior.getDomainDir()
				+ File.separator + domain;

		File bootstrapJar = ServerUtils.getJarName(
				serverAdapter.getServerInstallationDirectory(),
				ServerUtils.GFV3_JAR_MATCHER);
		if (bootstrapJar == null) {
			abort("bootstrap jar not found", null,
					IJavaLaunchConfigurationConstants.ERR_INTERNAL_ERROR);
		}

		// TODO which java to use? for now ignore the one from launch config
		AbstractVMInstall/* IVMInstall */vm = (AbstractVMInstall) serverBehavior
				.getRuntimeDelegate().getVMInstall();

		// IVMInstall vm2 = verifyVMInstall(configuration);

		StartupArgsImpl startArgs = new StartupArgsImpl();
		startArgs.setJavaHome(vm.getInstallLocation().getAbsolutePath());
		// Program & VM args
		String pgmArgs = getProgramArguments(configuration);
		String vmArgs = getVMArguments(configuration);
		startArgs.addJavaArgs(vmArgs);
		startArgs.addGlassfishArgs(pgmArgs);
		startArgs.addGlassfishArgs("--domain " + domain);
		startArgs.addGlassfishArgs("--domaindir "
				+ Utils.quote(domainAbsolutePath));
		StartMode startMode = StartMode.START;
		if ("debug".equals(mode)) {
			startMode = StartMode.DEBUG;
		}

		// String[] envp = getEnvironment(configuration);

		IPreferenceStore store = SunAppSrvPlugin.getInstance()
				.getPreferenceStore();
		// boolean verboseMode =
		// store.getBoolean(PreferenceConstants.ENABLE_START_VERBOSE);

		setDefaultSourceLocator(launch, configuration);

		startLogging(serverAdapter, serverBehavior);
		
		final ResultProcess process = ServerTasks.startServer(serverAdapter,
				startArgs, startMode);

		boolean javaDBStart = store
				.getBoolean(PreferenceConstants.ENABLE_START_JAVADB);
		if (javaDBStart) {
			String sampleDBDir = store
					.getString(PreferenceConstants.JAVA_DB_LOCATION);
			String[] command = ((sampleDBDir == null) ? new String[] {
					vm.getInstallLocation() + "/bin/java", //$NON-NLS-1$
					"-jar", //$NON-NLS-1$
					serverAdapter.getServerInstallationDirectory()
							+ "/modules/admin-cli.jar", "start-database" } //$NON-NLS-1$ //$NON-NLS-2$
					: new String[] {
							vm.getInstallLocation() + "/bin/java", //$NON-NLS-1$
							"-jar", //$NON-NLS-1$
							serverAdapter.getServerInstallationDirectory()
									+ "/modules/admin-cli.jar", //$NON-NLS-1$
							"start-database", "--dbhome", sampleDBDir //$NON-NLS-1$ //$NON-NLS-2$
					});
			// add also the stop on exit command:
			SunAppSrvPlugin.getInstance().addCommandToExecuteAtExit(
					new String[] {
							vm.getInstallLocation() + "/bin/java", //$NON-NLS-1$
							"-jar", //$NON-NLS-1$
							serverAdapter.getServerInstallationDirectory()
									+ "/modules/admin-cli.jar", //$NON-NLS-1$
							"stop-database" }); //$NON-NLS-1$
			try {
				Process process2 = Execute
						.launch(null,
								command,
								null,
								new File(serverAdapter
										.getServerInstallationDirectory()),
								true);
				DebugPlugin.newProcess(launch, process2, "Derby Database");
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}

		// wait (and update http port)
		waitUntilStarted(serverBehavior, ServerUtil.getServer(configuration)
				.getStartTimeout(), monitor);
		
		serverBehavior.updateHttpPort();

		// /////startPingingThread();
		setDefaultSourceLocator(launch, configuration);
		if (mode.equals("debug")) { //$NON-NLS-1$

			Map<String, String> arg = new HashMap<String, String>();
			Integer debugPort = null;
			try {
				debugPort = getDebugPort(process.getValue().getArguments());
			} catch (NumberFormatException e) {
				abort("Server run in debug mode but the debug port couldn't be determined!",
						e, IJavaLaunchConfigurationConstants.ERR_INTERNAL_ERROR);
			}
			if (debugPort == null) {
				abort("Server run in debug mode but the debug port couldn't be determined!",
						null,
						IJavaLaunchConfigurationConstants.ERR_INTERNAL_ERROR);
			}

			arg.put("hostname", "localhost"); //$NON-NLS-1$ //$NON-NLS-2$
			arg.put("port", debugPort.toString()); //$NON-NLS-1$ //$NON-NLS-2$
			arg.put("timeout", "25000"); //$NON-NLS-1$ //$NON-NLS-2$
			String connectorId = getVMConnectorId(configuration);
			IVMConnector connector = null;
			if (connectorId == null) {
				connector = JavaRuntime.getDefaultVMConnector();
			} else {
				connector = JavaRuntime.getVMConnector(connectorId);
			}
			// connect to VM
			connector.connect(arg, monitor, launch);

			DebugPlugin.getDefault().addDebugEventListener(
					new GlassfishServerDebugListener(serverBehavior,
							"localhost:" + debugPort));
		}

	}

	private void waitUntilStarted(
			GlassfishGenericServerBehaviour serverBehavior, int startTimeout,
			IProgressMonitor monitor) throws InterruptedException,
			CoreException {
		for (int i = 0; true; i++) {
			try {
				monitor.worked(10);
				Thread.sleep(1000);// 1 sec
				monitor.worked(1);
			} finally {
			}

			if (serverBehavior.isRunning()) {
				ServerStatus s = serverBehavior.getServerStatus();
				if (s == ServerStatus.CREDENTIAL_ERROR) {

					abort("Wrong user name or password.", null, IJavaLaunchConfigurationConstants.ERR_INTERNAL_ERROR); //$NON-NLS-1$
					return;
				}
				if (s != ServerStatus.DOMAINDIR_MATCHING) {

					SunAppSrvPlugin.logMessage("V3 not ready"); //$NON-NLS-1$
					continue;
				}
				return;
			}
		}
	}

	private boolean isServerReady(GlassfishGenericServerBehaviour serverBehavior)
			throws CoreException {
		ServerStatus status = ServerStatus.CONNEXTION_ERROR;
		GlassfishGenericServer serverAdapter = serverBehavior.getSunAppServer();
		serverAdapter.readDomainConfig(); // force reread of domain info if
											// necessary
		if (ServerUtils.isDASRunning(serverAdapter)) {
			status = serverBehavior.getServerStatus();
			if (status == ServerStatus.DOMAINDIR_MATCHING) {
				// // we are really to the server we know about, so that we can
				// // stop it and restart it to get the log file
				// serverBehavior.stop(true);
				// try {
				// Thread.sleep(2000);
				// } catch (InterruptedException e) {
				// // e.printStackTrace();
				// }
				// update http port
				serverBehavior.updateHttpPort();
				return true;
			} else if (status == ServerStatus.DOMAINDIR_NOT_MATCHING) {
				abort("Please, check the other GlassFish Application Server process and stop it.", //$NON-NLS-1$
						new RuntimeException(
								"A GlassFish Enterprise Server is running on this port, but with a different root installation..."),//$NON-NLS-1$
						IJavaLaunchConfigurationConstants.ERR_INTERNAL_ERROR);

			} else if (status == ServerStatus.CONNEXTION_ERROR) {
				abort("The Eclipse plugin cannot communicate with the GlassFish server....", //$NON-NLS-1$
						new RuntimeException(
								"The Eclipse plugin cannot communicate with the GlassFish server." //$NON-NLS-1$
										+ "Please, check for antivirus software blocking or monitoring this port, or firewall configuration, or VPN setup which might block some ports..."), //$NON-NLS-1$
						IJavaLaunchConfigurationConstants.ERR_INTERNAL_ERROR);

			} else {
				abort("The Eclipse plugin cannot communicate with the GlassFish server, status is :" + status, //$NON-NLS-1$
						new RuntimeException(
								"The Eclipse plugin cannot communicate with the GlassFish server." //$NON-NLS-1$
										+ "Status is :" + status), //$NON-NLS-1$
						IJavaLaunchConfigurationConstants.ERR_INTERNAL_ERROR);
			}
			return false;
		} else {
			return false;
		}
	}
	
	private void startLogging(final GlassfishGenericServer serverAdapter, final GlassfishGenericServerBehaviour serverBehavior) {
		try {
			PlatformUI.getWorkbench().getDisplay().asyncExec(new Runnable() {
				public void run() {
					File logFile = new File(serverBehavior.getDomainDirWithDomainName() + "/logs/server.log"); //$NON-NLS-1$
					try {
						logFile.createNewFile();
					} catch (IOException e) {
						// ignore, log file exists
					}
					// GlassFishConsole.showConsole(serverAdapter);
					IGlassFishConsole console = GlassfishConsoleManager
							.showConsole(serverAdapter);
					if (!console.isLogging())
						console.startLogging(FetchLog.create(serverAdapter, true));
				}
			});
		} catch (Exception e) {
			SunAppSrvPlugin.logMessage("page.showView", e); //$NON-NLS-1$
		}
	}

	private static Integer getDebugPort(String startArgs) {
		Matcher m = debugPortPattern.matcher(startArgs);
		if (m.find()) {
			return Integer.parseInt(m.group(1));
		}
		return null;
	}

	class GlassfishServerDebugListener implements IDebugEventSetListener {

		private GlassfishGenericServerBehaviour serverBehavior;
		private String debugTargetIdentifier;

		public GlassfishServerDebugListener(
				GlassfishGenericServerBehaviour serverBehavior,
				String debugTargetIdentifier) {
			this.serverBehavior = serverBehavior;
			this.debugTargetIdentifier = debugTargetIdentifier;
		}

		@Override
		public void handleDebugEvents(DebugEvent[] events) {
			if (events != null) {
				int size = events.length;
				for (int i = 0; i < size; i++) {
					if (events[i].getSource() instanceof JDIDebugTarget) {
						JDIDebugTarget dt = (JDIDebugTarget) events[i]
								.getSource();
						try {

							SunAppSrvPlugin
									.logMessage("JDIDebugTarget=" + dt.getName()); //$NON-NLS-1$
							if ((dt.getName().indexOf(debugTargetIdentifier) != -1)
									&& events[i].getKind() == DebugEvent.TERMINATE) { //$NON-NLS-1$
								DebugPlugin.getDefault()
										.removeDebugEventListener(this);
								if (!dt.isTerminated())
									serverBehavior.stop(true);
							}
						} catch (DebugException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						}
					}
				}
			}
		}
	}

}
