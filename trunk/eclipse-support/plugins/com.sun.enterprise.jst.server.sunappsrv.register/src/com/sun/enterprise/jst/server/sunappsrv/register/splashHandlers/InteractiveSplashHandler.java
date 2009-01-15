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

import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.net.URISyntaxException;
import java.net.URL;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ProgressMonitorDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.splash.AbstractSplashHandler;

import com.sun.enterprise.jst.server.sunappsrv.register.Activator;

/**
 * @since 3.3
 * 
 */
public class InteractiveSplashHandler extends AbstractSplashHandler {

	// FIXME convert to property, so it could be given at runtime.
	private static final boolean workspace = false;

	public InteractiveSplashHandler() {
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.ui.splash.AbstractSplashHandler#init(org.eclipse.swt.widgets
	 * .Shell)
	 */
	public void init(final Shell splash) {
		super.init(splash);
		// showWizard(splash);

		final String glassfishLoc = getLocation();

		if (new File(glassfishLoc + File.separator + ".installed").exists())
			return;

		final String dir = V2InstallationConfigurer.getJDKDir();
		Display.getDefault().asyncExec(new Runnable() {

			public void run() {
				final Shell shell = new Shell(Display.getDefault());
				try {
					IRunnableWithProgress op = new IRunnableWithProgress() {
						public void run(IProgressMonitor progressMonitor) throws InvocationTargetException,
								InterruptedException {
							progressMonitor.setTaskName("Configuring Glassfish V2 installation ...");
							V2InstallationConfigurer.configureV2(dir, glassfishLoc);
						}
					};
					ProgressMonitorDialog pmd = new ProgressMonitorDialog(shell);
					pmd.run(true, false, op);
				} catch (Exception e) {
					Activator.logMessage("error",e);
					org.eclipse.jface.dialogs.ErrorDialog.openError(shell, "Exception occurred", e
							.getLocalizedMessage(), new Status(IStatus.ERROR, Activator.PLUGIN_ID, e
							.getLocalizedMessage()));
				}

			}
		});

	}

	private String getLocation() {
		if (workspace) {
			// Overriding for testing purposes. Installation directory while
			// running from workspace is target platforms location. It's better
			// if we use some other dir for V2. Also this path is
			// suitable only for *nix platforms.
			return "/tmp/v2/glassfishv2";
		} else {
			try {
				// Get the eclipse installation location and from it V2
				// installation
				// directory.
				URL url = FileLocator.toFileURL(Platform.getInstallLocation().getURL());
                File file = new File(url.toURI());
                return new File(file, "glassfishv2").getAbsolutePath();
			} catch (IOException e1) {
				Activator.logMessage("error",e1);
				e1.printStackTrace();
			} catch (URISyntaxException e) {
				Activator.logMessage("error",e);
				e.printStackTrace();
			}
		}
		return null;
	}

	/**
	 * Show registration dialog.
	 * 
	 * @param splash
	 */
	public static void showWizard(final Shell splash) {
		RegistrationWizard wizard = new RegistrationWizard();
		WizardDialog dialog = new WizardDialog(splash, wizard);
		dialog.open();
	}
}
