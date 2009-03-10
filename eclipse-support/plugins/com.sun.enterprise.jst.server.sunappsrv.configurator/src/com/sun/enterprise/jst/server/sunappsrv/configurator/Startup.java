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

import java.text.MessageFormat;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.wst.server.ui.internal.ServerUIPreferences;

@SuppressWarnings("restriction")
public class Startup {

	public static void mystartup(IProgressMonitor progressMonitor) {
		try {
			ServerUIPreferences preferences = new ServerUIPreferences();
			boolean showOnActivity = preferences.getShowOnActivity();
			preferences.setShowOnActivity(false);
			progressMonitor.setTaskName(Messages.CreatingGlassfishServerInstances);
			progressMonitor.worked(10);
			String sampleDB = GlassFishConfigurator.createSampleDerbyDB(progressMonitor);
			GlassFishConfigurator.createV2Server(progressMonitor, sampleDB);
            progressMonitor.worked(30);
			String domainXml = GlassFishConfigurator.createV3Server(progressMonitor, sampleDB);
            progressMonitor.worked(30);
			GlassFishConfigurator.createDerbyDB(progressMonitor, domainXml);
            progressMonitor.worked(30);
			preferences.setShowOnActivity(showOnActivity);
		} catch (CoreException e) {
			Activator.showErrorAndLog(new Status(IStatus.ERROR, Activator.PLUGIN_ID, MessageFormat.format(
					Messages.CreatingServerConfigurationsProblem, e.getMessage()), e), e.getMessage(),
					Messages.EXCEPTION_OCCURRED);
		}
	}

}
