/*DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.

Copyright 1997-2009 Sun Microsystems, Inc. All rights reserved.

The contents of this file are subject to the terms of either the GNU
General Public License Version 2 only ("GPL") or the Common Development
and Distribution License("CDDL") (collectively, the "License").  You
may not use this file except in compliance with the License. You can obtain
a copy of the License at https://glassfish.dev.java.net/public/CDDL+GPL.html
or glassfish/bootstrap/legal/LICENSE.txt.  See the License for the specific
language governing permissions and limitations under the License.

When distributing the software, include this License Header Notice in each
file and include the License file at glassfish/bootstrap/legal/LICENSE.txt.
Sun designates this particular file as subject to the "Classpath" exception
as provided by Sun in the GPL Version 2 section of the License file that
accompanied this code.  If applicable, add the following below the License
Header, with the fields enclosed by brackets [] replaced by your own
identifying information: "Portions Copyrighted [year]
[name of copyright owner]"

Contributor(s):

If you wish your version of this file to be governed by only the CDDL or
only the GPL Version 2, indicate your decision by adding "[Contributor]
elects to include this software in this distribution under the [CDDL or GPL
Version 2] license."  If you don't indicate a single choice of license, a
recipient has the option to distribute your version of this file under
either the CDDL, the GPL Version 2 or to extend the choice of license to
its licensees as provided above.  However, if you add GPL Version 2 code
and therefore, elected the GPL Version 2 license, then the option applies
only if the new code is made subject to such option by the copyright
holder.
 */
package com.sun.enterprise.jst.server.sunappsrv.configurator;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;

import org.apache.tools.ant.listener.TimestampedLogger;
import org.eclipse.ant.core.AntRunner;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
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
public class V2Configurator {

	public static void configure(IProgressMonitor progressMonitor) throws CoreException {

		String glassfishLoc = getGlassfishLocation();

		progressMonitor.subTask("Creating runtime ...");

		IServerType st = ServerCore.findServerType(Constants.SERVER_GLASSFISH_2_ID);
		IRuntime runtime = createRuntime(glassfishLoc);
		IServer[] servers = ServerCore.getServers();

		// Check if we already have a runtime, if we have, we don't need to
		// do anything.
		for (IServer server : servers) {
			if (server.getRuntime() == null) {
				server.delete();
			}
			if (runtime != null && server != null && runtime.equals(server.getRuntime())) {
				return;
			}
		}

		progressMonitor.subTask("Creating domain ...");
		IServerWorkingCopy wc = st.createServer(null, null, runtime, null);
		wc.setName("Bundled " + runtime.getName());

		SunAppServer sunAppServer = (SunAppServer) wc.getAdapter(SunAppServer.class);

		String domainLocation = Platform.getLocation().append(".metadata").append(".plugins").append(
				Constants.SERVER_GLASSFISH_2_ID).toOSString();

		Map<String, String> configuration = sunAppServer.getProps();
		configuration.put(SunAppServer.DOMAINDIR, domainLocation);
		configuration.put(SunAppServer.ADMINNAME, Constants.V2_USER);
		configuration.put(SunAppServer.ADMINPASSWORD, Constants.V2_PASS);
		configuration.put(SunAppServer.DOMAINNAME, createDomain(glassfishLoc));
		configuration.put(SunAppServer.SERVERPORT, Constants.V2_INSTANCE_PORT);
		configuration.put(SunAppServer.ADMINSERVERPORT, Constants.V2_ADMIN_PORT);
		sunAppServer.setServerInstanceProperties(configuration);

		wc.save(true, null);

	}

	private static String getGlassfishLocation() {
		String property = System.getProperty("gf2location");
		String glassfishLoc = null;
		if (property != null) {
			glassfishLoc = property;

		} else {
			try {
				// Get the eclipse installation location and from it V2
				// installation directory.
				glassfishLoc = new Path(Platform.getInstallLocation().getURL().getFile()).toPortableString()
						+ "/glassfishv2";

				Activator.getDefault().getLog().log(
						new Status(IStatus.INFO, Activator.PLUGIN_ID, "glassfishV2Loc =" + glassfishLoc));
			} catch (Exception e) {
				Activator.showErrorAndLog(new Status(IStatus.ERROR, Activator.PLUGIN_ID, e.getMessage(), e), e
						.getMessage(), "Exception occurred");
			}
		}
		return glassfishLoc;
	}

	private static String createDomain(String glassfishLoc) throws CoreException {
		int count = getDomainCount(glassfishLoc);
		String domainName = "v2domain" + count;

		increaseDomainCount(glassfishLoc, count);
		// We use ant for creating a domain for V2 installation as ant is
		// better suited for this task.
		AntRunner ant = new AntRunner();

		HashMap<String, String> map = configureDomain(glassfishLoc, domainName);

		try {
			URL xml = Platform.getBundle(Activator.PLUGIN_ID).getResource("ant/createDomain.xml");
			String antFile = FileLocator.toFileURL(xml).getFile();

			ant.setBuildFileLocation(antFile);
			ant.addUserProperties(map);
			ant.setArguments("-Dmessage=Building -verbose");
			ant.addBuildLogger(TimestampedLogger.class.getName());

			ant.run();
		} catch (IOException e) {
			Activator.showErrorAndLog(new Status(IStatus.ERROR, Activator.PLUGIN_ID,
					"Error in startup config for glassfish: " + e.getMessage(), e), e.getMessage(),
					"Exception occurred");
		}
		return domainName;
	}

	private static HashMap<String, String> configureDomain(String glassfishLoc, String domainName) {
		HashMap<String, String> map = new HashMap<String, String>();
		map.put(Constants.HTTPS_PORT, Constants.V2_HTTPS_PORT);
		map.put(Constants.ADMIN_PORT, Constants.V2_ADMIN_PORT);
		map.put(Constants.INSTANCE_PORT, Constants.V2_INSTANCE_PORT);
		map.put(Constants.IMQ_PORT, "7677");
		map.put(Constants.ORB_PORT, "3701");
		map.put(Constants.ADMIN_PASSWORD, Constants.V2_PASS);
		map.put(Constants.ADMIN_USERNAME, Constants.V2_USER);
		map.put(Constants.GLASSFISH_DIR, glassfishLoc);
		map.put(Constants.DOMAIN_NAME, domainName);
		map.put(Constants.DOMAIN_DIR, Platform.getLocation().append(".metadata").append(".plugins").append(
				Constants.SERVER_GLASSFISH_2_ID).toOSString());
		return map;
	}

	/**
	 * Read number of domains already created from file ".installed".
	 * 
	 * @param glassfishLoc
	 * @return
	 */
	private static int getDomainCount(String glassfishLoc) {

		int count = 0;
		if (new File(glassfishLoc + File.separator + ".installed").exists()) {
			try {
				BufferedReader in = new BufferedReader(new FileReader(glassfishLoc + File.separator + ".installed"));
				count = Integer.parseInt(in.readLine()) + 1;
				in.close();
			} catch (Exception e) {
				Activator.showErrorAndLog(new Status(IStatus.ERROR, Activator.PLUGIN_ID,
						"Error in startup config for glassfish: " + e.getMessage(), e), e.getMessage(),
						"Exception occurred");
			}
		}
		return count;
	}

	private static void increaseDomainCount(String glassfishLoc, int count) {
		// Increase the number of domains created in the file.
		try {
			BufferedWriter out = new BufferedWriter(new FileWriter(glassfishLoc + File.separator + ".installed"));
			out.write("" + count);
			out.close();
		} catch (Exception e) {
			Activator.showErrorAndLog(new Status(IStatus.ERROR, Activator.PLUGIN_ID,
					"Error in startup config for glassfish: " + e.getMessage(), e), e.getMessage(),
					"Exception occurred");
		}
	}

	@SuppressWarnings("unchecked")
	private static IRuntime createRuntime(String glassfishLocation) throws CoreException {
		IServerType st = ServerCore.findServerType(Constants.SERVER_GLASSFISH_2_ID);
		IRuntime[] runtimes = ServerCore.getRuntimes();
		for (IRuntime runtime : runtimes) {
			if (runtime != null && runtime.getRuntimeType().equals(st.getRuntimeType())) {
				return runtime;
			}
		}

		IRuntimeWorkingCopy wc;
		wc = st.getRuntimeType().createRuntime(null, null);

		GenericServerRuntime gRun = (GenericServerRuntime) wc.loadAdapter(GenericServerRuntime.class,
				new NullProgressMonitor());

		HashMap map = new HashMap();
		map.put(SunAppServer.ROOTDIR, glassfishLocation);
		gRun.setServerDefinitionId(gRun.getRuntime().getRuntimeType().getId());
		gRun.setServerInstanceProperties(map);

		// wc.setLocation(new Path(glassfishLocation));
		return wc.save(true, null);
	}
}
