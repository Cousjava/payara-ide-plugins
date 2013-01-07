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
	protected void publishDeployedDirectory(int deltaKind, Properties p,
			IModule[] module, IProgressMonitor monitor) throws CoreException {
		// ludo using PublishHelper now to control the temp area to be
		// in the same file system of the deployed apps so that the mv operation
		// Eclipse is doing sometimes can work.
		PublishHelper helper = new PublishHelper(new Path(
				getDomainDirWithDomainName() + "/eclipseAppsTmp").toFile());

		if (deltaKind == REMOVED) {
			String publishPath = (String) p.get(module[0].getId());
			SunAppSrvPlugin.logMessage("REMOVED in publishPath" + publishPath);

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
			if (!needARedeploy) {
				SunAppSrvPlugin
						.logMessage("optimal: NO NEED TO TO A REDEPLOYMENT, !!!");

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
