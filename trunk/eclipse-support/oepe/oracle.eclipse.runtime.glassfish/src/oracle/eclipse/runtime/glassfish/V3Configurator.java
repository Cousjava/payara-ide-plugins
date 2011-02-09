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

@SuppressWarnings("restriction")
public class V3Configurator {

	//
	private String serverID;

	// for v3.1: "glassfish3"
	// private String serverRootDirName;

	private File serverLocation;

	/**
	 * @param serverID
	 * @param serverRootDirName
	 *            : for v3.1: "glassfish3"
	 */
	public V3Configurator(File serverLocation, String serverID,
			String serverRootDirName) {
		this.serverID = serverID;
		this.serverLocation = serverLocation;
		// this.serverRootDirName = serverRootDirName;

	}

	public void configure() throws CoreException {
		

		String glassfishLocation = new File(serverLocation, "glassfish")
				.getAbsolutePath();

		IServerType st = ServerCore.findServerType(serverID);// v3
		IRuntime runtime = createRuntime(glassfishLocation);
		IServer[] servers = ServerCore.getServers();

		for (IServer server : servers) {
			if (server.getRuntime() == null) {
				server.delete();
			}
			if (runtime != null && server != null
					&& runtime.equals(server.getRuntime())) {
				return ;
			}
		}

		IServerWorkingCopy wc = st.createServer(null, null, runtime, null);
		wc.setName("Internal " + runtime.getName());
		SunAppServer sunAppServer = (SunAppServer) wc
				.getAdapter(SunAppServer.class);

		IWorkspace workspace = ResourcesPlugin.getWorkspace();
		IWorkspaceRoot root = workspace.getRoot();
		IPath location = root.getLocation();
		String expectedDomainLocation = ""+location+"/glassfish31eclipsedefaultdomain";
		if (!new File(expectedDomainLocation).exists()){
			copyDirectory(new File(serverLocation,"/glassfish/domains/domain1"), 
					new File(expectedDomainLocation)) ;
		}		



		
		Map<String, String> configuration = sunAppServer.getProps();
		configuration
				.put(SunAppServer.DOMAINPATH, expectedDomainLocation);

		sunAppServer.setServerInstanceProperties(configuration);

		wc.save(true, null);

		return ;
	}

	private IRuntime createRuntime(String glassfishLocation) {
		try {
			IServerType st = ServerCore.findServerType(serverID);
			IRuntime[] runtimes = ServerCore.getRuntimes();
			ServerCore.getRuntimeTypes();
			ServerCore.getServers();
			for (IRuntime runtime : runtimes) {
				String fff = "" + runtime.getLocation();
				if (runtime != null
						&& runtime.getRuntimeType().equals(st.getRuntimeType())) {
					if (fff.equals(glassfishLocation))
						return runtime;
				}
			}

			IRuntimeWorkingCopy wc;
			wc = st.getRuntimeType().createRuntime(null, null);

			GenericServerRuntime gRun = (GenericServerRuntime) wc.loadAdapter(
					GenericServerRuntime.class, new NullProgressMonitor());

			HashMap<String, String> map = new HashMap<String, String>();

			map.put(SunAppServer.ROOTDIR, glassfishLocation);
			gRun.setServerDefinitionId(gRun.getRuntime().getRuntimeType()
					.getId());
			gRun.setServerInstanceProperties(map);

			wc.setLocation(new Path(glassfishLocation));
			return wc.save(true, null);
		} catch (CoreException e) {

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
					if (in!=null)
						in.close();
				} catch (IOException e) {
				}
				try {
					if (out!=null)
					out.close();
				} catch (IOException e) {
				}
				}

		}

	}

}
