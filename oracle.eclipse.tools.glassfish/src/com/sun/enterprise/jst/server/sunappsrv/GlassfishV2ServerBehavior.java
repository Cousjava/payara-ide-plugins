package com.sun.enterprise.jst.server.sunappsrv;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Properties;

import javax.management.MBeanServerConnection;
import javax.management.ObjectName;
import javax.management.remote.JMXConnector;
import javax.management.remote.JMXConnectorFactory;
import javax.management.remote.JMXServiceURL;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.osgi.util.NLS;
import org.eclipse.ui.PlatformUI;
import org.eclipse.wst.common.componentcore.internal.util.ComponentUtilities;
import org.eclipse.wst.server.core.IModule;
import org.eclipse.wst.server.core.util.PublishHelper;

import com.sun.enterprise.jst.server.sunappsrv.log.GlassfishConsoleManager;

public class GlassfishV2ServerBehavior extends GlassfishGenericServerBehaviour {

	@Override
	public ServerStatus getServerStatus() {
		GlassfishV2Server server = (GlassfishV2Server) getSunAppServer();
		JMXConnector jmxc = null;
		try {

			// Create an RMI connector client
			//
			JMXServiceURL url = new JMXServiceURL(
					"service:jmx:rmi:///jndi/rmi://" + getServer().getHost() + ":" + server.getJMXPort() + "/jmxrmi"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			HashMap<String, String[]> env = new HashMap<String, String[]>();
			env.put(JMXConnector.CREDENTIALS,
					new String[] { server.getAdminName(),
							server.getAdminPassword() });
			SunAppSrvPlugin
					.logMessage("service:jmx:rmi:///jndi/rmi://" + getServer().getHost() + ":" + server + "/jmxrmi"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			jmxc = JMXConnectorFactory.connect(url, env);
			SunAppSrvPlugin.logMessage("after JMXConnectorFactory"); //$NON-NLS-1$
			MBeanServerConnection mbsc = jmxc.getMBeanServerConnection();
			ObjectName on = new ObjectName(
					"com.sun.appserv:type=domain,category=config"); //$NON-NLS-1$

			Object o = mbsc.invoke(on, "getConfigDir", null, null); //$NON-NLS-1$
			SunAppSrvPlugin.logMessage("mbsc.invoke=" + o); //$NON-NLS-1$
			if (o != null) {
				File domainDir = new File("" + o).getParentFile(); //$NON-NLS-1$
				File knownDomainRoot = new File(server.getDomainsFolder()
						+ File.separator + server.getDomainName());
				if (domainDir.getCanonicalPath().equals(
						knownDomainRoot.getCanonicalPath())) {
					return ServerStatus.DOMAINDIR_MATCHING;
				} else {
					return ServerStatus.DOMAINDIR_NOT_MATCHING;

				}
			} else {
				SunAppSrvPlugin.logMessage("V2 not ready yet: o=null"); //$NON-NLS-1$
				return ServerStatus.MBEAN_ERROR;
			}

		} catch (Exception e) {
			SunAppSrvPlugin.logMessage("V2 not ready yet:", e); //$NON-NLS-1$
			return ServerStatus.CONNEXTION_ERROR;

		} finally {
			if (jmxc != null) {
				try {
					jmxc.close();
				} catch (IOException e) {
					// what can we do there

				}

			}
		}
	}

	@Override
	public void publishModule(int kind, int deltaKind, IModule[] module,
			IProgressMonitor monitor) throws CoreException {
		// first, test if the server is still existing
		File serverloc = new File(getSunAppServer()
				.getServerInstallationDirectory());
		if (!serverloc.exists()) {
			SunAppSrvPlugin.logMessage(
					NLS.bind(Messages.serverDirectoryGone,
							serverloc.getAbsolutePath()), null);
			return;

		}

		needARedeploy = true; // by default

		String projectContextRoot = ComponentUtilities
				.getServerContextRoot(module[0].getProject());
		GlassfishV2Server sunserver = (GlassfishV2Server) getSunAppServer();

		if ((projectContextRoot == null) || (projectContextRoot.length() <= 0)) {
			projectContextRoot = module[0].getName().replace(' ', '_');
		}
		sunserver.setContextRoot(projectContextRoot);
		super.publishModule(kind, deltaKind, module, monitor);
	}

	@Override
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
	
	

	@Override
	public void updateHttpPort() {
		// read domain config
		getSunAppServer().readDomainConfig();
	}

	private void stopImpl(GlassfishGenericServer server) {
		// set arguments to be passed to Runtime.exec

		String arr[] = getStopV2Command();
		// stop the SJSAS using Runtime.exec
		asyncExec(arr);
		int timeout = getServer().getStopTimeout();
		for (int i = 0; i < timeout; i++) {
			try {
				Thread.sleep(1000);
				SunAppSrvPlugin
						.logMessage("in SunAppServerBehaviour stopping...");

				if (isRunning() == false) {
					SunAppSrvPlugin
							.logMessage("in SunAppServerBehaviour really stopped");
					Thread.sleep(2000);// need an extra time to flush

					return;
				}
			} catch (Exception ex) {

				SunAppSrvPlugin.logMessage(
						"Error in SunAppServerBehaviour being stopped", ex);

			}
		}
	}

	private void asyncExec(final String[] arr) {
		new Thread(new Runnable() {
			public void run() {
				try {
					BufferedReader input = new BufferedReader(
							new InputStreamReader(Runtime.getRuntime()
									.exec(arr).getInputStream()));
					String line = null;
					while ((line = input.readLine()) != null)
						SunAppSrvPlugin.logMessage(">>> " + line);
					input.close();
				} catch (Exception ex) {
					SunAppSrvPlugin
							.logMessage("Error starting/stopping integrated SJSAS:\n"
									+ ex);
				}
			}
		}).start();
	}

	private String getScriptExtension() {
		String ret = "";
		if (File.separator.equals("\\")) {
			ret = ".bat"; // NOI18N
		}
		return ret;
	}

	public String[] getStopV2Command() {
		String asadminCmd = getSunAppServer().getServerInstallationDirectory()
				+ "/bin/asadmin" + getScriptExtension();

		String stop[] = { asadminCmd, "stop-domain", "--domaindir",
				getDomainDir(), getDomainName() };
		return stop;
	}

}
