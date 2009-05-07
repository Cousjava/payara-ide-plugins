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
