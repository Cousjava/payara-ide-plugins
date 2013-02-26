package com.sun.enterprise.jst.server.sunappsrv;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.osgi.util.NLS;
import org.eclipse.wst.common.componentcore.internal.util.ComponentUtilities;
import org.eclipse.wst.server.core.IModule;
import org.eclipse.wst.server.core.IServer;
import org.eclipse.wst.server.core.model.ServerBehaviourDelegate;
import org.glassfish.tools.ide.admin.CommandDeploy;
import org.glassfish.tools.ide.admin.CommandLocation;
import org.glassfish.tools.ide.admin.ResultMap;
import org.glassfish.tools.ide.admin.ResultString;
import org.glassfish.tools.ide.admin.ServerAdmin;
import org.glassfish.tools.ide.admin.TaskState;
import org.glassfish.tools.ide.data.IdeContext;

import com.sun.enterprise.jst.server.sunappsrv.serverview.Utils;

public class GlassfishV4ServerBehavior extends GlassfishGenericServerBehaviour {

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

		long t = System.currentTimeMillis();
		if (module.length > 1) {// only publish root modules, i.e web modules
			setModulePublishState(module, IServer.PUBLISH_STATE_NONE);
		} else {
			publishModuleForGlassFishV3(kind, deltaKind, module, monitor);
			SunAppSrvPlugin.logMessage("done publishModule in "
					+ (System.currentTimeMillis() - t) + " ms");
		}

	}
	
	/*
	 * Publishes for Web apps only in V3 prelude
	 */
	protected void publishModuleForGlassFishV3(int kind, int deltaKind,
			IModule[] module, IProgressMonitor monitor) throws CoreException {

		if (module.length > 1) {// only publish root modules, i.e web modules
			setModulePublishState(module, IServer.PUBLISH_STATE_NONE);
			return;
		}
		if (!publishNeeded(kind, deltaKind, module) || monitor.isCanceled()) {
			return;
		}
		IPath path = getTempDirectory().append("publish.txt");
		// SunAppSrvPlugin.logMessage("in PATH" +path +"module length======="
		// +module.length);

		FileInputStream fis = null;
		Properties prop = new Properties();
		try {
			fis = new FileInputStream(path.toFile());
			prop.load(fis);
		} catch (Exception e) {
		} finally {
			try {
				fis.close();
			} catch (Exception ex) {
			}
		}
		if ((!getSunAppServer().isRemote() && getSunAppServer().getJarDeploy().equalsIgnoreCase(
				"false"))) {
			publishDeployedDirectory(deltaKind, prop, module, monitor);
		} else {
			publishJarFile(kind, deltaKind, prop, module, monitor);

		}

		setModulePublishState(module, IServer.PUBLISH_STATE_NONE);
		FileOutputStream fos = null;
		try {
			prop.store(fos = new FileOutputStream(path.toFile()), "GlassFish 3");
		} catch (Exception e) {
			SunAppSrvPlugin.logMessage(" error in PUBLISH_STATE_NONE", e);
		} finally {
			if (fos != null)
				try {
					fos.close();
				} catch (IOException e) {
					// Auto-generated catch block
				}
		}

	}

	
	private void publishJarFile(int kind, int deltaKind, Properties p,
			IModule[] module, IProgressMonitor monitor) throws CoreException {
		// first try to see if we need to undeploy:

		if (deltaKind == ServerBehaviourDelegate.REMOVED) {
			// same logic as directory undeploy
			publishDeployedDirectory(deltaKind, p, module, monitor);

		} else {

			try {
				File archivePath = ExportJavaEEArchive.export(module[0],
						monitor);
				String name = Utils.simplifyModuleID(module[0].getName());
				String contextRoot = null;

				if (AssembleModules.isModuleType(module[0], "jst.web")) {
					String projectContextRoot = ComponentUtilities
							.getServerContextRoot(module[0].getProject());
					contextRoot = (((projectContextRoot != null) && (projectContextRoot
							.length() > 0)) ? projectContextRoot : module[0]
							.getName());
				}
				Map<String, String> properties = new HashMap<String, String>();
				File[] libraries = new File[0];
				
				CommandDeploy command = new CommandDeploy(name, null, archivePath, contextRoot, properties, libraries);
				
				try {
					Future<ResultString> future =
		                    ServerAdmin.<ResultString>exec(getSunAppServer(), command, new IdeContext());
		                ResultString result = future.get(520, TimeUnit.SECONDS);
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
			} catch (ExecutionException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}

		}
	}
	
	@Override
	public ServerStatus getServerStatus() {
		GlassfishGenericServer server = getSunAppServer();
		CommandLocation command = new CommandLocation();
		try {
			Future<ResultMap<String, String>> future = ServerAdmin
					.<ResultMap<String, String>> exec(server, command,
							new IdeContext());
			ResultMap<String, String> result = future.get(30, TimeUnit.SECONDS);

			if (result.getState() == TaskState.RUNNING) {
				// let try one more time...it is possible to have running and
				// then immediately completed..
				SunAppSrvPlugin
						.logMessage("getV3ServerStatus trying one more time"); //$NON-NLS-1$
				result = future.get(15, TimeUnit.SECONDS);
			}
			if (result.getState() == TaskState.COMPLETED) {
				String installRoot = server.getDomainsFolder() + File.separator
						+ server.getDomainName();
				String targetInstallRoot = result.getValue().get(
						"Domain-Root_value");
				// SunAppSrvPlugin.logMessage("IsReady is targetInstallRoot="+targetInstallRoot
				// );
				// SunAppSrvPlugin.logMessage("IsReady is installRoot="+installRoot
				// );
				if (installRoot != null && targetInstallRoot != null) {
					File installDir = new File(installRoot);
					File targetInstallDir = new File(targetInstallRoot);
					if (installDir.getCanonicalPath().equals(
							targetInstallDir.getCanonicalPath())) {
						SunAppSrvPlugin
								.logMessage("getV3ServerStatus DOMAINDIR_MATCHING"); //$NON-NLS-1$
						return ServerStatus.DOMAINDIR_MATCHING;
					} else {
						SunAppSrvPlugin
								.logMessage("getV3ServerStatus DOMAINDIR_NOT_MATCHING"); //$NON-NLS-1$
						return ServerStatus.DOMAINDIR_NOT_MATCHING;
					}
				} else {
					SunAppSrvPlugin
							.logMessage("getV3ServerStatus 3 DOMAINDIR_NOT_MATCHING"); //$NON-NLS-1$
					return ServerStatus.DOMAINDIR_NOT_MATCHING;
				}
			} else if (result.getState() == TaskState.FAILED) {
				SunAppSrvPlugin.logMessage("apparently CREDENTIAL_ERROR"); //$NON-NLS-1$
				return ServerStatus.CREDENTIAL_ERROR;

			} else {
				SunAppSrvPlugin.logMessage("Command Still running!!! error"); //$NON-NLS-1$
				return ServerStatus.CREDENTIAL_ERROR;
			}
		} catch (Exception ex) {
			SunAppSrvPlugin.logMessage("IsReady is failing=", ex); //$NON-NLS-1$
			SunAppSrvPlugin.logMessage("getV3ServerStatus 2 CONNEXTION_ERROR"); //$NON-NLS-1$
			return ServerStatus.CONNEXTION_ERROR;
		}
	}

}
