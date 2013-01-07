package com.sun.enterprise.jst.server.sunappsrv;

import java.io.File;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import org.glassfish.tools.ide.admin.CommandLocation;
import org.glassfish.tools.ide.admin.ResultMap;
import org.glassfish.tools.ide.admin.ServerAdmin;
import org.glassfish.tools.ide.admin.TaskState;
import org.glassfish.tools.ide.data.IdeContext;

import com.sun.enterprise.jst.server.sunappsrv.GlassfishGenericServerBehaviour.ServerStatus;

public class GlassfishV3ServerBehavior extends GlassfishGenericServerBehaviour {

	/**
	 * 
	 * @return ServerStatus for possible V3 server. If server is not a V3 one,
	 *         we will know If the server is a V3 server but with a different
	 *         install location that this one, we can also detect this
	 */
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
