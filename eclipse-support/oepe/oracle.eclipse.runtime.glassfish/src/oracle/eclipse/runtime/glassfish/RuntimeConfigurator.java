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

import oracle.eclipse.tools.glassfish.GlassfishGenericServer;
import oracle.eclipse.tools.glassfish.GlassfishToolsPlugin;

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

@SuppressWarnings("restriction")
public class RuntimeConfigurator {

	//
	private String serverID;

	private File serverLocation;

	/**
	 * @param serverID
	 */
	public RuntimeConfigurator(File serverLocation, String serverID) {
		this.serverID = serverID;
		this.serverLocation = serverLocation;
	}

	
	public void configure() throws CoreException {

		File glassfishLocation = new File(serverLocation, "glassfish");

		IRuntime alreadyThere = getRuntimeByLocation(glassfishLocation);
		if (alreadyThere != null) {
			GlassfishToolsPlugin.logMessage("Already Registered: "
					+ glassfishLocation, null);
			return;
		}
		GlassfishToolsPlugin.logMessage(
				"Not  Registered yet : " + glassfishLocation.getAbsolutePath(), null);

		//deleteOldGlassFishInternalRuntimes(glassfishLocation);
		GlassfishToolsPlugin.logMessage("done with deleting obsolete runtimes : ",
				null);

		IServerType st = ServerCore.findServerType(serverID);// v3
		IRuntime runtime = createRuntime(glassfishLocation.getAbsolutePath());
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
		GlassfishGenericServer sunAppServer = (GlassfishGenericServer) wc
				.getAdapter(GlassfishGenericServer.class);

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
		configuration.put(GlassfishGenericServer.DOMAINPATH, expectedDomainLocation);
		configuration.put(GlassfishGenericServer.USEANONYMOUSCONNECTIONS, "true");


		sunAppServer.setServerInstanceProperties(configuration);
		sunAppServer.setDefaultPublishState();
		wc.save(true, null);

		return;
	}

	private String getDomainLocation() {
		IWorkspace workspace = ResourcesPlugin.getWorkspace();
		IWorkspaceRoot root = workspace.getRoot();
		IPath location = root.getLocation();
		return "" + location + "/glassfish3122eclipsedefaultdomain";
	}

	private IRuntime getRuntimeByLocation(File glassfishLocation) {

		IServerType st = ServerCore.findServerType(serverID);
		IRuntime[] runtimes = ServerCore.getRuntimes();
		ServerCore.getRuntimeTypes();
		ServerCore.getServers();
		for (IRuntime runtime : runtimes) {
			File currentlocation = new File("" + runtime.getLocation());
			if (runtime != null
					&& runtime.getRuntimeType().equals(st.getRuntimeType())) {
				if (currentlocation.equals(glassfishLocation))
					return runtime;
			}
		}
		return null;
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
				GlassfishToolsPlugin.logMessage("loop in createRuntime : " + fff,
						null);
				if (runtime.getRuntimeType().equals(st.getRuntimeType())) {
					if (fff.equals(glassfishLocation)) {
						GlassfishToolsPlugin.logMessage("ALREREEEEEDDDD : "
								+ glassfishLocation, null);
						return runtime;
					}
				}
			}

			IRuntimeWorkingCopy wc;
			GlassfishToolsPlugin.logMessage("before Creating working copy : ", null);
			wc = st.getRuntimeType().createRuntime(null, null);
			wc.setName("GlassFish Plugin 3.1.2.2");
			GlassfishToolsPlugin.logMessage("Creating working copy : " + wc, null);

			GenericServerRuntime gRun = (GenericServerRuntime) wc.loadAdapter(
					GenericServerRuntime.class, new NullProgressMonitor());
			GlassfishToolsPlugin.logMessage("grun : " + gRun, null);

			HashMap<String, String> map = new HashMap<String, String>();
			GlassfishToolsPlugin.logMessage("Creating NEW : " + glassfishLocation,
					null);

			map.put(GlassfishGenericServer.ROOTDIR, glassfishLocation);
			gRun.setServerDefinitionId(gRun.getRuntime().getRuntimeType()
					.getId());
			gRun.setServerInstanceProperties(map);

			wc.setLocation(new Path(glassfishLocation));
			GlassfishToolsPlugin.logMessage("pre saving new runtime : "
					+ glassfishLocation, null);
			return wc.save(true, null);
		} catch (CoreException e) {
			GlassfishToolsPlugin.logMessage("core exception : " + glassfishLocation,
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
