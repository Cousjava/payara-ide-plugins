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
import java.net.URL;
import java.text.MessageFormat;
import java.util.HashMap;
import org.apache.tools.ant.listener.TimestampedLogger;
import org.eclipse.ant.core.AntRunner;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.osgi.framework.Bundle;

import com.sun.enterprise.jst.server.sunappsrv.register.Activator;
import com.sun.enterprise.jst.server.sunappsrv.register.Messages;

public class V3InstallationConfigurer {

	public final static String GLASSFISH_INSTALL = "glassfishv3_dir"; //$NON-NLS-1$

	public static void configureV3(String glassfishLoc) {
		// We use ant for replacing properties within V3 installation as ant is
		// better suited for this task.
		AntRunner ant = new AntRunner();

		HashMap<String, String> map = new HashMap<String, String>();

		map.put(V3InstallationConfigurer.GLASSFISH_INSTALL, glassfishLoc);

		try {
			Bundle bundle = Platform.getBundle(Activator.PLUGIN_ID);
			URL xml = bundle.getResource("ant/unzipSampleDB.xml"); //$NON-NLS-1$
			String antFile = FileLocator.toFileURL(xml).getFile();

			// even though we don't use this here in the code, we must access it to make sure
			// it is available in the ant file (if we don't, it is not copied into the 
			// relevant osgi directory with the other libs)
			URL dbURL = bundle.getResource("lib/derbysampledb.zip"); //$NON-NLS-1$
			String dbSampleFile = FileLocator.toFileURL(dbURL).getFile();
			// end required access

			ant.setBuildFileLocation(antFile);
			ant.addUserProperties(map);
			// FIXME in production remove those lines, no need for the user to
			// see that much info
			ant.setArguments("-Dmessage=Building -verbose"); //$NON-NLS-1$
			ant.addBuildLogger(TimestampedLogger.class.getName());

			ant.run();

		} catch (Exception e) {
			Activator.showErrorAndLog(new Status(IStatus.ERROR, Activator.PLUGIN_ID, e.getMessage(), e),
					MessageFormat.format(Messages.UNZIPPING_DERBY_SAMPLES_ENCOUNTERED_A_PROBLEM_0, e.getMessage()), Messages.EXCEPTION_OCCURRED);
		}

	}
}
