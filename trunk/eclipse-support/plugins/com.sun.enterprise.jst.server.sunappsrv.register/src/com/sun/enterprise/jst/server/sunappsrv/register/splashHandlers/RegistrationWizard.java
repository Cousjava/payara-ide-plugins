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

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.plugin.AbstractUIPlugin;

import com.sun.enterprise.jst.server.sunappsrv.register.Activator;
import com.sun.enterprise.jst.server.sunappsrv.register.Messages;

public class RegistrationWizard extends Wizard {

	private RegistrationChoicePage choicePage;
	private RegisterAccountPage accountPage;
	private boolean success = false;

	public RegistrationWizard() {
		super();
		setWindowTitle(Messages.RegisterGlassfish);
		setNeedsProgressMonitor(true);
	}

	public void addPages() {
		choicePage = new RegistrationChoicePage(Messages.CHOOSE_REGISTRATION_METHOD);
		accountPage = new RegisterAccountPage(Messages.REGISTER_ACCOUNT);

		ImageDescriptor image = AbstractUIPlugin.imageDescriptorFromPlugin(
				"com.sun.enterprise.jst.server.sunappsrv.register", "icons/wizard75x66.png");
		choicePage.setImageDescriptor(image);
		accountPage.setImageDescriptor(image);

		addPage(choicePage);
		addPage(accountPage);

	}

	public boolean canFinish() {
		boolean choicePageComplete = choicePage.isPageComplete();
		boolean accountPageComplete = accountPage.isPageComplete();

		switch (choicePage.getRegistrationType()) {
		case RegistrationChoicePage.SKIP:
			return true;
		case RegistrationChoicePage.NO_ACCOUNT:
			return choicePageComplete && accountPageComplete;
		case RegistrationChoicePage.ACCOUNT:
			return choicePageComplete;
		default:
			return false;
		}
	}

	public boolean performFinish() {
		success = false;
		try {
			switch (choicePage.getRegistrationType()) {
			case RegistrationChoicePage.SKIP:
				success = choicePage.skipRegistration();
				break;
			case RegistrationChoicePage.NO_ACCOUNT:
				getContainer().run(false, true, new IRunnableWithProgress() {
					public void run(final IProgressMonitor monitor) throws InvocationTargetException,
							InterruptedException {
						monitor.setTaskName(Messages.PleaseWaitWhileRegistering);
						success = accountPage.registerUser();
						monitor.done();
					}
				});
				break;
			case RegistrationChoicePage.ACCOUNT:
				getContainer().run(false, true, new IRunnableWithProgress() {
					public void run(final IProgressMonitor monitor) throws InvocationTargetException,
							InterruptedException {
						monitor.setTaskName(Messages.PleaseWaitWhileRegistering);
						success = choicePage.register();
						monitor.done();
					}
				});
				break;
			}

		} catch (InvocationTargetException e) {
			Activator.logErrorMessage("Perform finish caused an exception", e); //$NON-NLS-1$
		} catch (InterruptedException e) {
			Activator.logErrorMessage("Perform finish caused an exception", e); //$NON-NLS-1$
		}
		return success;
	}
}
