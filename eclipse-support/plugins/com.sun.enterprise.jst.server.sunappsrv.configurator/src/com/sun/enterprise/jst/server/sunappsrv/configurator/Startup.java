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

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ProgressMonitorDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.wst.server.core.internal.IStartup;

@SuppressWarnings("restriction")
public class Startup implements IStartup {

	public void startup() {
		// In startup you have to explicitly get to UI thread.
		Display.getDefault().asyncExec(new Runnable() {

			public void run() {
				final Shell shell = new Shell(Display.getDefault());
				try {
					IRunnableWithProgress op = new IRunnableWithProgress() {
						public void run(IProgressMonitor progressMonitor) throws InvocationTargetException,
								InterruptedException {
							try {
								progressMonitor.setTaskName("Creating Glassfish servers instances");
								GlassFishConfigurator.createV2Server(progressMonitor);
								GlassFishConfigurator.createV3Server(progressMonitor);
							} catch (CoreException e) {
								org.eclipse.jface.dialogs.ErrorDialog.openError(shell, "Exception occurred", e
										.getLocalizedMessage(), new Status(IStatus.ERROR, Activator.PLUGIN_ID, e
										.getLocalizedMessage()));
							}
						}
					};
					ProgressMonitorDialog pmd = new ProgressMonitorDialog(shell);
					pmd.run(true, false, op);
				} catch (Exception e) {
					org.eclipse.jface.dialogs.ErrorDialog.openError(shell, "Exception occurred", e
							.getLocalizedMessage(), new Status(IStatus.ERROR, Activator.PLUGIN_ID, e
							.getLocalizedMessage()));
				}

			}
		});
	}

}
