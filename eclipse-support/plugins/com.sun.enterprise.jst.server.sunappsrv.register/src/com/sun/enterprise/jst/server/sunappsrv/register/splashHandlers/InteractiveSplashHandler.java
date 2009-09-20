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
import java.text.MessageFormat;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.splash.AbstractSplashHandler;

import com.sun.enterprise.jst.server.sunappsrv.configurator.Constants;
import com.sun.enterprise.jst.server.sunappsrv.configurator.Startup;
import com.sun.enterprise.jst.server.sunappsrv.register.Activator;
import com.sun.enterprise.jst.server.sunappsrv.register.Messages;
import com.sun.enterprise.jst.server.sunappsrv.register.service.RegisterService;
import com.sun.enterprise.jst.server.sunappsrv.utilities.DomainUtilities;
import com.sun.enterprise.registration.RegistrationException;
import com.sun.enterprise.registration.RegistrationService.RegistrationReminder;

/**
 * @since 3.3
 * 
 */
public class InteractiveSplashHandler extends AbstractSplashHandler {

	private boolean skipInstall = false;
	private String JDKdir =null;
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

		Activator.logMessage(Messages.INITIALIZING_SPLASH_HANDLER, null, IStatus.INFO);

		showWizard(splash);

		final String glassfishLoc = getGlassfishv2Location();
		final File loc= new File(glassfishLoc);
		if (loc.exists()&&loc.isDirectory()) {  //Only if v2 is part of the bundle:
			File installedPlaceholder = new File(glassfishLoc + File.separator + ".installed"); //$NON-NLS-1$
			DomainUtilities.ensureRuntimeHasCorrectDirectories(glassfishLoc.substring(0, glassfishLoc.length()-Constants.GLASSFISHV2_1.length()));
			if (installedPlaceholder.exists()) {
				long installedTime = installedPlaceholder.lastModified();
				String asadminName = "asadmin" + (File.separator.equals("\\") ? ".bat" : "");
				long substitutionTime = new File(glassfishLoc + File.separator + "bin" + File.separator + asadminName).lastModified();
	
				// .installed file is written immediately after the substitution, so check for it being close in time
				// in elapsed minutes
				if ((Math.abs(installedTime - substitutionTime)/60000) <= 2) {
					skipInstall = true;
				}
			}
			Activator.logMessage("skip install is " + skipInstall, null, IStatus.INFO);
			
			JDKdir = V2InstallationConfigurer.getJDKDir();
			/* no need anymore for build 1.0.25 
			if (!skipInstall) {
				String v3RootDir = RegisterService.getv3PreludeLocation();
				ConfigureDefaultGlassFishJDK.modifyAsEnvScriptFile(v3RootDir, dir);
			}*/
		}
		Display.getDefault().asyncExec(new Runnable() {

			public void run() {
				final Shell shell = new Shell(Display.getDefault());
				try {

					Job job = new Job(Messages.SETUP_GLASSFISH) {
						@Override
						protected IStatus run(IProgressMonitor monitor) {
							monitor.beginTask(Messages.CONFIGURING_GLASSFISH_V2_1_INSTALLATION, 100);
							// execute the task ...

							if ((!skipInstall)&&(JDKdir!=null))
								V2InstallationConfigurer.configureV2(JDKdir, glassfishLoc);
							
							//then do the regular configuration for the default domains in the workspace directory
							Startup.mystartup(monitor);
							monitor.done();
							return Status.OK_STATUS;
						}
					};
					job.schedule();

				} catch (Exception e) {
					Activator.showErrorAndLog(new Status(IStatus.ERROR, Activator.PLUGIN_ID, e.getMessage(), e),
							MessageFormat.format(Messages.CONFIGURING_GLASSFISH_V2_1_ENCOUNTERED_A_PROBLEM_0, e
									.getMessage()), Messages.EXCEPTION_OCCURRED);
				}

			}
		});

	}

	private static String getGlassfishv2Location() {
		String property = System.getProperty("gf2location"); //$NON-NLS-1$
		String glassfishLoc = null;
		if (property != null) {
			glassfishLoc = property;
		} else {
			// Get the eclipse installation location and from it V2
			// installation directory.
			glassfishLoc = new Path(Platform.getInstallLocation().getURL().getFile()).toPortableString()
					+ Constants.GLASSFISHV2_1; //$NON-NLS-1$
			Activator.logMessage("glassfishLoc =" + glassfishLoc, null, IStatus.INFO); //$NON-NLS-1$
		}
		return glassfishLoc;
	}

	/**
	 * Show registration dialog.
	 * 
	 * @param splash
	 */
	private static void showWizard(final Shell splash) {
		try {
			RegistrationReminder reminder = RegisterService.getReminder();
			Activator.logMessage("Registration reminder = " + reminder, null, IStatus.INFO);//$NON-NLS-1$
			if (reminder.equals(RegistrationReminder.DONT_ASK_FOR_REGISTRATION))
				return;
			//
			// if (RegisterService.isRegistered(loc, null, 0))
			// return;
		} catch (RegistrationException e) {
			Activator.showErrorAndLog(new Status(IStatus.ERROR, Activator.PLUGIN_ID, e.getMessage(), e), MessageFormat
					.format(Messages.ERROR_GETTING_REGISTRATION_STATUS, e.getMessage()), Messages.EXCEPTION_OCCURRED);
		}

		RegistrationWizard wizard = new RegistrationWizard();
		WizardDialog dialog = new WizardDialog(splash, wizard);
		dialog.open();
	}

}
