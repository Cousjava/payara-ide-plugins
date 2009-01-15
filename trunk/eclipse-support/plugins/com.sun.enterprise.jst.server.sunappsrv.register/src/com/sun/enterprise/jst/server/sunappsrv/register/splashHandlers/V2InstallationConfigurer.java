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
package com.sun.enterprise.jst.server.sunappsrv.register.splashHandlers;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.net.URL;
import java.util.HashMap;

import org.apache.tools.ant.listener.TimestampedLogger;
import org.eclipse.ant.core.AntRunner;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Platform;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import com.sun.enterprise.jst.server.sunappsrv.register.Activator;

public class V2InstallationConfigurer {

	public final static String GLASSFISH_INSTALL = "glassfish_dir";
	public final static String INSTALL_HOME = "install_home";
	public final static String INSTALL_HOME_F = "install_home_f";
	public final static String JAVA_HOME = "java_home";
	public final static String JAVA_HOME_F = "java_home_f";
	public final static String ADMIN_USERNAME = "admin_username";
	public final static String ADMIN_PASSWORD = "admin_password";

	public static void configureV2(String jdkFolder, String glassfishLoc) {
		// We use ant for replacing properties within V2 installation as ant is
		// better suited for this task.
		AntRunner ant = new AntRunner();

		HashMap<String, String> map = new HashMap<String, String>();

		// Get Java SDK installation directory from the user. We'll ignore the
		// Eclipse's conf as Eclipse usually takes JRE that doesn't suit our
		// needs.

		map.put(V2InstallationConfigurer.GLASSFISH_INSTALL, glassfishLoc);
		map.put(V2InstallationConfigurer.INSTALL_HOME, glassfishLoc);
		map.put(V2InstallationConfigurer.INSTALL_HOME_F, glassfishLoc + File.separator);
		map.put(V2InstallationConfigurer.ADMIN_USERNAME, "admin");
		map.put(V2InstallationConfigurer.ADMIN_PASSWORD, "adminadmin");

		map.put(V2InstallationConfigurer.JAVA_HOME, jdkFolder);
		map.put(V2InstallationConfigurer.JAVA_HOME_F, jdkFolder + File.separator);
		try {
			URL xml = Platform.getBundle(Activator.PLUGIN_ID).getResource("ant/postProcess.xml");
			String antFile = FileLocator.toFileURL(xml).getFile();

			ant.setBuildFileLocation(antFile);
			ant.addUserProperties(map);
			// FIXME in production remove those lines, no need for the user to
			// see that much info
			ant.setArguments("-Dmessage=Building -verbose");
			ant.addBuildLogger(TimestampedLogger.class.getName());

			ant.run();
		} catch (CoreException e) {
			Activator.logMessage("error",e);
			e.printStackTrace();
			return;
		} catch (IOException e) {
			Activator.logMessage("error",e);
			e.printStackTrace();
			return;
		}
		try {
			BufferedWriter out = new BufferedWriter(new FileWriter(glassfishLoc + File.separator + ".installed"));
			out.write("1");
			out.close();
		} catch (IOException e) {
			e.printStackTrace();
		}

	}

	public static String getJDKDir() {
		String lcOSName = System.getProperty("os.name").toLowerCase();
		boolean mac = lcOSName.startsWith("mac os x");
		if (mac){
			return System.getProperty("java.home");			
		}
		String file = "";
		String message = "";
		do {
			Shell shell = new Shell(Display.getDefault());
			DirectoryDialog dd = new DirectoryDialog(shell);
			dd.setText("Please select Java SDK installation " + "location for Glassfish V2.");
			dd.setFilterPath(file);
			dd.setMessage(message);
			file = dd.open();
			if (file == null) {
				System.err.println("Installation cancelled");
				System.exit(0);
			}
			shell.close();
			shell.dispose();
			File jarFile = new File("" + file + File.separator + "lib" + File.separator + "tools.jar");
			if (!jarFile.exists()) {
				message = "Directory \"" + file + "\" doesn't contain Java SDK installation";
			} else
				break;
		} while (true);
		return file;
	}

}
