/*
 * Copyright (c) 2011 Oracle and/or its affiliates. All rights reserved.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Oracle
 */

package oracle.eclipse.runtime.glassfish;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.jst.server.generic.core.internal.GenericServerRuntime;
import org.eclipse.wst.server.core.IRuntime;
import org.eclipse.wst.server.core.IRuntimeWorkingCopy;
import org.eclipse.wst.server.core.IServer;
import org.eclipse.wst.server.core.IServerType;
import org.eclipse.wst.server.core.IServerWorkingCopy;
import org.eclipse.wst.server.core.ServerCore;

import com.sun.enterprise.jst.server.sunappsrv.SunAppServer;
import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;

@SuppressWarnings("restriction")
public class V3Configurator {

	//
	private String serverID;

	private File serverLocation;

	/**
	 * @param serverID
	 */
	public V3Configurator(File serverLocation, String serverID) {
		this.serverID = serverID;
		this.serverLocation = serverLocation;
	}

	
	public void configure() throws CoreException {

		String glassfishLocation = new File(serverLocation, "glassfish")
				.getAbsolutePath();

		IRuntime alreadyThere = getRuntimeByLocation(glassfishLocation);
		if (alreadyThere != null) {
			SunAppSrvPlugin.logMessage("Already Registered: "
					+ glassfishLocation, null);
			return;
		}
		SunAppSrvPlugin.logMessage(
				"Not  Registered yet : " + glassfishLocation, null);

		deleteOldGlassFishInternalRuntimes(glassfishLocation);
		SunAppSrvPlugin.logMessage("done with deleting obsolete runtimes : ",
				null);

		IServerType st = ServerCore.findServerType(serverID);// v3
		IRuntime runtime = createRuntime(glassfishLocation);
		IServer[] servers = ServerCore.getServers();

		for (IServer server : servers) {
			if (server.getRuntime() == null) {
				server.delete();
			}
			if (runtime != null && server != null
					&& runtime.equals(server.getRuntime())) {
				// IRuntime ir = runtime.createWorkingCopy();
				// if (ir instanceof RuntimeWorkingCopy){
				// RuntimeWorkingCopy wc = (RuntimeWorkingCopy) ir;
				// wc.setLocation(new Path("/"));
				// wc.save(true, null);
				// }

				// / return ;
			}
		}

		IServerWorkingCopy wc = st.createServer(null, null, runtime, null);
		wc.setName("Internal " + runtime.getName());
		SunAppServer sunAppServer = (SunAppServer) wc
				.getAdapter(SunAppServer.class);

		String expectedDomainLocation = getDomainLocation();
		File dom = new File(expectedDomainLocation);
		if (!dom.exists()) {
			copyDirectory(
					new File(serverLocation, "/glassfish/domains/domain1"),
					new File(expectedDomainLocation));
		} else {// domain exists!! clean the osgi dir as a precaution, it might
				// have been used by another server
			deleteOSGICacheDirectory(new File(dom, "osgi-cache"));
		}

		Map<String, String> configuration = sunAppServer.getProps();
		configuration.put(SunAppServer.DOMAINPATH, expectedDomainLocation);

		sunAppServer.setServerInstanceProperties(configuration);

		wc.save(true, null);

		return;
	}

	private String getDomainLocation() {
		IWorkspace workspace = ResourcesPlugin.getWorkspace();
		IWorkspaceRoot root = workspace.getRoot();
		IPath location = root.getLocation();
		return "" + location + "/glassfish31eclipsedefaultdomain";
	}

	private IRuntime getRuntimeByLocation(String glassfishLocation) {

		IServerType st = ServerCore.findServerType(serverID);
		IRuntime[] runtimes = ServerCore.getRuntimes();
		ServerCore.getRuntimeTypes();
		ServerCore.getServers();
		for (IRuntime runtime : runtimes) {
			String currentlocation = "" + runtime.getLocation();
			if (runtime != null
					&& runtime.getRuntimeType().equals(st.getRuntimeType())) {
				if (currentlocation.equals(glassfishLocation))
					return runtime;
			}
		}
		return null;
	}

	private void deleteOldGlassFishInternalRuntimes(String newglassfishLocation) {

		// IServerType st = ServerCore.findServerType(serverID);
		IRuntime[] runtimes = ServerCore.getRuntimes();
		ServerCore.getRuntimeTypes();
		ServerCore.getServers();
		for (IRuntime runtime : runtimes) {
			String currentlocation = "" + runtime.getLocation();
			if (currentlocation.indexOf("oracle.eclipse.runtime.glassfish") != -1) {
				if (!currentlocation.equals(newglassfishLocation)) {
					// we can delete this old runtime defined from an old plugin
					// ServerCore.
					SunAppSrvPlugin.logMessage("Deleting Old registered : "
							+ currentlocation, null);

					try {
						runtime.delete();
					} catch (CoreException e) {
						SunAppSrvPlugin.logMessage(
								"Error Deleting Old registered : "
										+ currentlocation, e);

					}

					// delete the osgi cache as well
				}

			}
		}
	}

	static private boolean deleteOSGICacheDirectory(File osgicache) {
		if (osgicache.exists()) {
			File[] files = osgicache.listFiles();
			for (int i = 0; i < files.length; i++) {
				if (files[i].isDirectory()) {
					deleteOSGICacheDirectory(files[i]);
				} else {
					files[i].delete();
				}
			}
		}
		return (osgicache.delete());
	}

	private IRuntime createRuntime(String glassfishLocation) {
		try {
			IServerType st = ServerCore.findServerType(serverID);
			IRuntime[] runtimes = ServerCore.getRuntimes();
			ServerCore.getServers();
			for (IRuntime runtime : runtimes) {
				String fff = "" + runtime.getLocation();
				SunAppSrvPlugin.logMessage("loop in createRuntime : " + fff,
						null);
				if (runtime.getRuntimeType().equals(st.getRuntimeType())) {
					if (fff.equals(glassfishLocation)) {
						SunAppSrvPlugin.logMessage("ALREREEEEEDDDD : "
								+ glassfishLocation, null);
						return runtime;
					}
				}
			}

			IRuntimeWorkingCopy wc;
			SunAppSrvPlugin.logMessage("before Creating working copy : ", null);
			wc = st.getRuntimeType().createRuntime(null, null);
			SunAppSrvPlugin.logMessage("Creating working copy : " + wc, null);

			GenericServerRuntime gRun = (GenericServerRuntime) wc.loadAdapter(
					GenericServerRuntime.class, new NullProgressMonitor());
			SunAppSrvPlugin.logMessage("grun : " + gRun, null);

			HashMap<String, String> map = new HashMap<String, String>();
			SunAppSrvPlugin.logMessage("Creating NEW : " + glassfishLocation,
					null);

			map.put(SunAppServer.ROOTDIR, glassfishLocation);
			gRun.setServerDefinitionId(gRun.getRuntime().getRuntimeType()
					.getId());
			gRun.setServerInstanceProperties(map);

			wc.setLocation(new Path(glassfishLocation));
			SunAppSrvPlugin.logMessage("pre saving new runtime : "
					+ glassfishLocation, null);
			return wc.save(true, null);
		} catch (CoreException e) {
			SunAppSrvPlugin.logMessage("core exception : " + glassfishLocation,
					e);

		}
		return null;
	}

	private void copyDirectory(File sourceLocation, File targetLocation) {

		if (sourceLocation.isDirectory()) {
			if (!targetLocation.exists()) {
				targetLocation.mkdir();
			}

			String[] children = sourceLocation.list();
			for (int i = 0; i < children.length; i++) {
				copyDirectory(new File(sourceLocation, children[i]), new File(
						targetLocation, children[i]));
			}
		} else {
			InputStream in = null;
			OutputStream out = null;
			try {
				in = new FileInputStream(sourceLocation);
				out = new FileOutputStream(targetLocation);
				byte[] buf = new byte[10240];
				int len;
				while ((len = in.read(buf)) > 0) {
					out.write(buf, 0, len);
				}
			} catch (IOException e) {
			} finally {
				try {
					if (in != null)
						in.close();
				} catch (IOException e) {
				}
				try {
					if (out != null)
						out.close();
				} catch (IOException e) {
				}
			}

		}

	}

}
