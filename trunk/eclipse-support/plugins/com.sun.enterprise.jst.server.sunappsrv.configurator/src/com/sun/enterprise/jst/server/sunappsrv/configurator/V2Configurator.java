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

@SuppressWarnings("restriction")
public class V2Configurator {

	private static final String INSTANCE_PORT = "8081";
	private static final String ADMIN_PORT = "4849";
	private static final String HTTPS_PORT = "8182";
	private static final String AUSER = "admin";
	private static final String APASS = "adminadmin";

	public static void configure(IProgressMonitor progressMonitor) throws CoreException {
		String glassfishLoc = "";
		try {
			// Get the eclipse installation location and from it V2 installation
			// directory.
			glassfishLoc = FileLocator.toFileURL(Platform.getInstallLocation().getURL()).getFile() + File.pathSeparator
					+ "glassfishv2";
		} catch (IOException e1) {
			e1.printStackTrace();
		}
		// FIXME overriding for testing purposes. Installation directory while
		// running from workspace is target platforms location. It's better if
		// we use some other dir for V2.
		glassfishLoc = "/tmp/v2/glassfishv2";
		progressMonitor.subTask("Creating runtime ...");

		IServerType st = ServerCore.findServerType(Constants.SERVER_GLASSFISH_2_ID);// v3
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
		String domain = createDomain(glassfishLoc);

		progressMonitor.subTask("Configuring server access ...");
		IServerWorkingCopy wc = st.createServer(null, null, runtime, null);
		wc.setName("Bundled " + runtime.getName());

		SunAppServer sunAppServer = (SunAppServer) wc.getAdapter(SunAppServer.class);

		String domainLocation = Platform.getLocation().append(".metadata").append(".plugins").append(
				Constants.SERVER_GLASSFISH_2_ID).toOSString();
		Map<String, String> configuration = sunAppServer.getProps();
		configuration.put(SunAppServer.DOMAINDIR, domainLocation);
		configuration.put(SunAppServer.ADMINNAME, AUSER);
		configuration.put(SunAppServer.ADMINPASSWORD, APASS);
		configuration.put(SunAppServer.DOMAINNAME, domain);
		configuration.put(SunAppServer.SERVERPORT, INSTANCE_PORT);
		configuration.put(SunAppServer.ADMINSERVERPORT, ADMIN_PORT);
		sunAppServer.setServerInstanceProperties(configuration);

		wc.save(true, null);

		// startServer(sunAppServer);

	}

	private static String createDomain(String glassfishLoc) throws CoreException {
		// created domain count, we read that from the .installed file so that
		// we wouldn't create duplicate domains for different workspaces
		int count = 0;
		if (new File(glassfishLoc + File.separator + ".installed").exists()) {
			try {
				BufferedReader in = new BufferedReader(new FileReader(glassfishLoc + File.separator + ".installed"));
				count = Integer.parseInt(in.readLine()) + 1;
				in.close();
			} catch (IOException e) {
				e.printStackTrace();
			} catch (NumberFormatException e) {
				e.printStackTrace();
			}
		}

		// Read the how many domains have been created.
		try {
			BufferedWriter out = new BufferedWriter(new FileWriter(glassfishLoc + File.separator + ".installed"));
			out.write("" + count);
			out.close();
		} catch (IOException e) {
			e.printStackTrace();
		} catch (NumberFormatException e) {
			e.printStackTrace();
		}
		// We use ant for creating a domain for V2 installation as ant is
		// better suited for this task.
		AntRunner ant = new AntRunner();

		HashMap<String, String> map = new HashMap<String, String>();
		map.put(Constants.HTTPS_PORT, HTTPS_PORT);
		map.put(Constants.ADMIN_PORT, ADMIN_PORT);
		map.put(Constants.INSTANCE_PORT, INSTANCE_PORT);
		map.put(Constants.IMQ_PORT, "7677");
		map.put(Constants.ORB_PORT, "3701");
		map.put(Constants.ADMIN_PASSWORD, APASS);
		map.put(Constants.ADMIN_USERNAME, AUSER);
		map.put(Constants.GLASSFISH_DIR, glassfishLoc);
		String domainName = "bdomain" + count;
		map.put(Constants.DOMAIN_NAME, domainName);
		map.put(Constants.DOMAIN_DIR, Platform.getLocation().append(".metadata").append(".plugins").append(
				Constants.SERVER_GLASSFISH_2_ID).toOSString());

		try {
			URL xml = Platform.getBundle(Activator.PLUGIN_ID).getResource("ant/createDomain.xml");
			String antFile = FileLocator.toFileURL(xml).getFile();

			ant.setBuildFileLocation(antFile);
			ant.addUserProperties(map);
			// FIXME in production remove those lines, no need for the user to
			// see that much info
			// ant.setArguments("-Dmessage=Building -verbose");
			// ant.addBuildLogger(TimestampedLogger.class.getName());

			ant.run();
		} catch (IOException e) {
			throw new CoreException(new Status(IStatus.ERROR, Activator.PLUGIN_ID, e.getLocalizedMessage()));
		}
		return domainName;
	}

	@SuppressWarnings("unchecked")
	public static IRuntime createRuntime(String glassfishLocation) throws CoreException {
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

		wc.setLocation(new Path(glassfishLocation));
		return wc.save(true, null);
	}
}
