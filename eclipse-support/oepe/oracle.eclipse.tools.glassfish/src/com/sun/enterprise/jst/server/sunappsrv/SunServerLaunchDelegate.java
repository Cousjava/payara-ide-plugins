package com.sun.enterprise.jst.server.sunappsrv;

import java.io.File;
import java.io.IOException;
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
import org.eclipse.debug.core.model.RuntimeProcess;
import org.eclipse.jdt.internal.debug.core.model.JDIDebugTarget;
import org.eclipse.jdt.launching.AbstractJavaLaunchConfigurationDelegate;
import org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants;
import org.eclipse.jdt.launching.IVMConnector;
import org.eclipse.jdt.launching.IVMInstall;
import org.eclipse.jdt.launching.JavaRuntime;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.ui.PlatformUI;
import org.eclipse.wst.server.core.IServer;
import org.eclipse.wst.server.core.ServerUtil;
import org.eclipse.wst.server.core.model.ServerBehaviourDelegate;
import org.glassfish.tools.ide.server.FetchLog;
import org.glassfish.tools.ide.server.parser.JvmConfigReader;
import org.glassfish.tools.ide.server.parser.TreeParser;
import org.glassfish.tools.ide.utils.ServerUtils;

import com.sun.enterprise.jst.server.sunappsrv.GlassfishGenericServerBehaviour.ServerStatus;
import com.sun.enterprise.jst.server.sunappsrv.log.GlassfishConsoleManager;
import com.sun.enterprise.jst.server.sunappsrv.log.IGlassFishConsole;
import com.sun.enterprise.jst.server.sunappsrv.preferences.PreferenceConstants;

public class SunServerLaunchDelegate extends
		AbstractJavaLaunchConfigurationDelegate {

	public static final String GFV3_MODULES_DIR_NAME = "modules"; //$NON-NLS-1$

	public SunServerLaunchDelegate() {
		// SunAppSrvPlugin.logMessage("in SUN SunAppServerLaunch ctor");
	}

	protected void abort(String message, Throwable exception, int code)
			throws CoreException {
		throw new CoreException(new Status(IStatus.ERROR,
				SunAppSrvPlugin.SUNPLUGIN_ID, code, message, exception));
	}

	private String getScriptExtension() {
		String ret = ""; //$NON-NLS-1$
		if (File.separator.equals("\\")) {//$NON-NLS-1$
			ret = ".bat"; //$NON-NLS-1$
		}
		return ret;
	}

	public void launch(ILaunchConfiguration configuration, String mode,
			ILaunch launch, IProgressMonitor monitor) throws CoreException {
		SunAppSrvPlugin.logMessage("in SUN SunAppServerLaunch launch"); //$NON-NLS-1$

		IServer server = ServerUtil.getServer(configuration);
		if (server == null) {
			abort("missing Server", null, IJavaLaunchConfigurationConstants.ERR_INTERNAL_ERROR); //$NON-NLS-1$
		}

		final GlassfishV2ServerBehavior serverBehavior = (GlassfishV2ServerBehavior) server
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
			GlassfishV2ServerBehavior serverBehavior,
			ILaunchConfiguration configuration, ILaunch launch, String mode,
			IProgressMonitor monitor) throws CoreException,
			InterruptedException {

		IPreferenceStore store = SunAppSrvPlugin.getInstance()
				.getPreferenceStore();
		boolean verboseMode = store
				.getBoolean(PreferenceConstants.ENABLE_START_VERBOSE);
		String verboseFlag = "--verbose=" + verboseMode;

		String asadminCmd = serverAdapter.getServerInstallationDirectory()
				+ "/bin/asadmin" + getScriptExtension(); //$NON-NLS-1$
		String domain = serverBehavior.getDomainName();
		String debugFlag = "--debug=false"; //$NON-NLS-1$
		if (mode.equals("debug")) { //$NON-NLS-1$
			debugFlag = "--debug"; //$NON-NLS-1$
		}
		ProcessBuilder pb = null;
//		AbstractVMInstall/* IVMInstall */vm = (AbstractVMInstall) serverBehavior
//				.getRuntimeDelegate().getVMInstall();

		IVMInstall vm2 = verifyVMInstall(configuration);

		// String mainTypeName = tomcatServer.getRuntimeClass();

		setDefaultSourceLocator(launch, configuration);

		pb = new ProcessBuilder(asadminCmd,
				"start-domain", "--domaindir", serverBehavior.getDomainDir(), //$NON-NLS-1$ //$NON-NLS-2$
				debugFlag, verboseFlag, domain);
		SunAppSrvPlugin.getInstance().addCommandToExecuteAtExit(
				serverBehavior.getStopV2Command());

		startLogging(serverAdapter, serverBehavior);
		
		pb.directory(new File(serverAdapter.getServerInstallationDirectory()));
		try {
			Process p = pb.start();
			new RuntimeProcess(launch, p, "...", null);
		} catch (IOException e1) {
			abort("error Launching Executable", e1, IJavaLaunchConfigurationConstants.ERR_INTERNAL_ERROR); //$NON-NLS-1$
		}

		boolean javaDBStart = store
				.getBoolean(PreferenceConstants.ENABLE_START_JAVADB);
		if (javaDBStart) {
			String sampleDBDir = store
					.getString(PreferenceConstants.JAVA_DB_LOCATION);
			String[] command = ((sampleDBDir == null) ? new String[] {
					asadminCmd, "start-database" } : new String[] { asadminCmd, //$NON-NLS-1$
					"start-database", "--dbhome", sampleDBDir }); //$NON-NLS-1$ //$NON-NLS-2$
			// stop the db on exit of the IDE:
			SunAppSrvPlugin.getInstance().addCommandToExecuteAtExit(
					new String[] { asadminCmd, "stop-database" }); //$NON-NLS-1$
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
			File domainXml = new File(serverAdapter.getDomainsFolder()
					+ File.separator + serverAdapter.getDomainName()
					+ File.separator + "config" + File.separator + "domain.xml");
			Integer debugPort;
			if (!domainXml.exists()
					|| (debugPort = readDebugPort(domainXml)) == null) {
				debugPort = 9009;
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

	private Integer readDebugPort(File domainXml) {
		Integer result = null;
		JvmConfigReader jvmReader = new JvmConfigReader("server");
		TreeParser.readXml(domainXml, jvmReader);
		String debugOpts = jvmReader.getPropMap().get("debug-options");
		if (debugOpts != null) {
			Pattern p = Pattern.compile("address=([0-9]+)");
			Matcher m = p.matcher(debugOpts);
			if (m.find()) {
				String debugPort = m.group(1);
				try {
					result = Integer.parseInt(debugPort);
				} catch (NumberFormatException e) {
				}
			}

		}
		return result;
	}

	private void waitUntilStarted(
			GlassfishGenericServerBehaviour serverBehavior, int startTimeout,
			IProgressMonitor monitor) throws InterruptedException,
			CoreException {
		while (true) {
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
