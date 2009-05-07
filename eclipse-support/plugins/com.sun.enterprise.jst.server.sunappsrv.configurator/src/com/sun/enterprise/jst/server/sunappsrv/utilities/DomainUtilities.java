package com.sun.enterprise.jst.server.sunappsrv.utilities;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.net.URL;
import java.text.MessageFormat;
import java.util.HashMap;

import org.apache.tools.ant.listener.TimestampedLogger;
import org.eclipse.ant.core.AntRunner;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;

import com.sun.enterprise.jst.server.sunappsrv.configurator.Activator;
import com.sun.enterprise.jst.server.sunappsrv.configurator.Messages;

public class DomainUtilities {

	private final static IPath baseDir = Platform.getLocation().append(".metadata").append(".plugins") //$NON-NLS-1$ //$NON-NLS-2$ 
			.append("com.sun.enterprise.jst.server"); //$NON-NLS-1$ //$NON-NLS-2$

	public static void ensureRuntimeHasCorrectDirectories(String glassfishLoc) {
		String dir = getInstallLocation();

		if (dir != null && !glassfishLoc.equals(dir)) {
			try {
				Activator.logMessage("changing: " + dir + " -> " + glassfishLoc, null, IStatus.INFO);

				String baseDir = Platform.getLocation().append(".metadata").append(".plugins") //$NON-NLS-1$ //$NON-NLS-2$ 
						.toOSString();
				replaceDirs(baseDir, dir, glassfishLoc);
			} catch (CoreException e) {
				Activator
						.showErrorAndLog(new Status(IStatus.ERROR, Activator.PLUGIN_ID, MessageFormat.format(
								Messages.ErrorInStartupConfig, e.getMessage()), e), e.getMessage(),
								Messages.EXCEPTION_OCCURRED);
			}
		}
		// }
	}

	private static void replaceDirs(String baseDir, String previousLoc, String newLoc) throws CoreException {
		try {
			HashMap<String, String> map = new HashMap<String, String>();

			map.put("previousDir", previousLoc);//$NON-NLS-1$
			map.put("newDir", newLoc);//$NON-NLS-1$
			map.put("baseDir", baseDir);//$NON-NLS-1$
			AntRunner ant = new AntRunner();
			URL xml = Platform.getBundle(Activator.PLUGIN_ID).getResource("ant/updateDomains.xml"); //$NON-NLS-1$
			String antFile = FileLocator.toFileURL(xml).getFile();

			ant.setBuildFileLocation(antFile);
			ant.addUserProperties(map);
			ant.setArguments("-Dmessage=Building -verbose"); //$NON-NLS-1$
			ant.addBuildLogger(TimestampedLogger.class.getName());

			ant.run();
		} catch (IOException e) {
			Activator.showErrorAndLog(new Status(IStatus.ERROR, Activator.PLUGIN_ID, MessageFormat.format(
					Messages.ErrorInStartupConfig, e.getMessage()), e), e.getMessage(), Messages.EXCEPTION_OCCURRED);
		}
	}

	public static String getInstallLocation() {

		if (!(new File(baseDir.append("installLocation.txt").toOSString()).exists())) {//$NON-NLS-1$
			return null;
		}

		BufferedReader in = null;
		try {
			in = new BufferedReader(new FileReader(baseDir.append("installLocation.txt").toOSString()));//$NON-NLS-1$
			try {
				return in.readLine();
			} finally {
				if (in != null)
					in.close();
			}
		} catch (Exception e) {
			Activator.showErrorAndLog(new Status(IStatus.ERROR, Activator.PLUGIN_ID, MessageFormat.format(
					Messages.ErrorInStartupConfig, e.getMessage()), e), e.getMessage(), Messages.EXCEPTION_OCCURRED);
			return null;
		}
	}

	public static void writeInstallLocation(String location) {
		try {
			new File(baseDir.toOSString()).mkdirs();
			BufferedWriter out = new BufferedWriter(new FileWriter(baseDir.append("installLocation.txt").toOSString())); //$NON-NLS-1$
			out.write(location);
			out.close();
		} catch (Exception e) {
			Activator.showErrorAndLog(new Status(IStatus.ERROR, Activator.PLUGIN_ID, MessageFormat.format(
					Messages.ErrorInStartupConfig, e.getMessage()), e), e.getMessage(), Messages.EXCEPTION_OCCURRED);
		}
	}
}
