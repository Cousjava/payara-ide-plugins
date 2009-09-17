// <editor-fold defaultstate="collapsed" desc="CDDL+GPL License">
/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 * 
 * Copyright 1997-2009 Sun Microsystems, Inc. All rights reserved.
 * 
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common Development
 * and Distribution License("CDDL") (collectively, the "License").  You
 * may not use this file except in compliance with the License. You can obtain
 * a copy of the License at https://glassfish.dev.java.net/public/CDDL+GPL.html
 * or glassfish/bootstrap/legal/LICENSE.txt.  See the License for the specific
 * language governing permissions and limitations under the License.
 * 
 * When distributing the software, include this License Header Notice in each
 * file and include the License file at glassfish/bootstrap/legal/LICENSE.txt.
 * Sun designates this particular file as subject to the "Classpath" exception
 * as provided by Sun in the GPL Version 2 section of the License file that
 * accompanied this code.  If applicable, add the following below the License
 * Header, with the fields enclosed by brackets [] replaced by your own
 * identifying information: "Portions Copyrighted [year]
 * [name of copyright owner]"
 * 
 * Contributor(s):
 * 
 * If you wish your version of this file to be governed by only the CDDL or
 * only the GPL Version 2, indicate your decision by adding "[Contributor]
 * elects to include this software in this distribution under the [CDDL or GPL
 * Version 2] license."  If you don't indicate a single choice of license, a
 * recipient has the option to distribute your version of this file under
 * either the CDDL, the GPL Version 2 or to extend the choice of license to
 * its licensees as provided above.  However, if you add GPL Version 2 code
 * and therefore, elected the GPL Version 2 license, then the option applies
 * only if the new code is made subject to such option by the copyright
 * holder.
 */
// </editor-fold>

package com.sun.enterprise.jst.server.sunappsrv.sunresource.wizards;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.osgi.util.NLS;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWizard;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;

import com.sun.enterprise.jst.server.sunappsrv.sunresource.MailInfo;

/**
 * This is a wizard that creates a new JavaMail Session resource.
 */

public class MailWizard extends Wizard implements INewWizard {
	private static final String RESOURCE_FILE_TEMPLATE = "templates/mail-resources-fragment-template.resource"; //$NON-NLS-1$
	
	private MailResourceWizardPage page;
	private ISelection selection;
	private String dirName;

	/**
	 * Constructor for MailWizard.
	 */
	public MailWizard() {
		super();
		setNeedsProgressMonitor(true);
	}

	/**
	 * Adding the page to the wizard.
	 */

	@Override
	public void addPages() {
		IContainer containerResource = getContainerResource();
		IProject selectedProject = ((containerResource != null) ? containerResource.getProject() : null);
		page = new MailResourceWizardPage(selectedProject);
		addPage(page);
	}

	/**
	 * This method is called when 'Finish' button is pressed in
	 * the wizard. We will create an operation and run it
	 * using wizard as execution context.
	 */
	@Override
	public boolean performFinish() {
		final String jndiName = page.getJNDIName();
		final MailInfo mailInfo = page.getMailInfo();
		final IProject selectedProject = page.getSelectedProject();
		
		IRunnableWithProgress op = new IRunnableWithProgress() {
			public void run(IProgressMonitor monitor) throws InvocationTargetException {
				try {
					doFinish(jndiName, mailInfo, selectedProject, monitor);
				} catch (CoreException e) {
					throw new InvocationTargetException(e);
				} finally {
					monitor.done();
				}
			}
		};
		try {
			getContainer().run(true, false, op);
		} catch (InterruptedException e) {
			return false;
		} catch (InvocationTargetException e) {
			Throwable realException = e.getTargetException();
			String message = realException.getMessage();
			if (message == null) {
				message = Messages.errorUnknown;
			}
			MessageDialog.openError(getShell(), Messages.ErrorTitle, message);
			return false;
		}
		return true;
	}
	
	/**
	 * The worker method. It will find the container, create the
	 * file and open the editor on the newly created file.  If the 
	 * file already exists, show an error
	 */

	private void doFinish(String jndiName, MailInfo mailInfo, IProject selectedProject, IProgressMonitor monitor) throws CoreException {
		//TODO - for now, just force WebContent/WEB-INF
		dirName = ResourceUtils.SETUP_DIR_NAME;
		
		IContainer containerResource = selectedProject;
		final IFolder folder = containerResource.getFolder(new Path(dirName));
		if (!folder.exists()) {
			IStatus status = new Status(IStatus.ERROR, "MailWizard", IStatus.OK, //$NON-NLS-1$
			NLS.bind(Messages.errorFolderMissing, dirName), null);
			throw new CoreException(status);
		}

		monitor.beginTask("Creating " + ResourceUtils.RESOURCE_FILE_NAME, 2);

		final IFile file = folder.getFile(new Path(ResourceUtils.RESOURCE_FILE_NAME));
		
		try {
			String fragment = createFragment(jndiName, mailInfo);
			InputStream stream = ResourceUtils.appendResource(file, fragment);
			if (!folder.exists()) {
				folder.create(true, true, monitor);
			}
			if (file.exists()) {
				file.setContents(stream, true, true, monitor);
			} else {
				file.create(stream, true, monitor);
			}
			stream.close();
		} catch (IOException e) {
		}
		monitor.worked(1);
		monitor.setTaskName("Opening file for editing...");
		getShell().getDisplay().asyncExec(new Runnable() {
			public void run() {
				IWorkbenchPage page =
					PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
				try {
					IDE.openEditor(page, file, true);
				} catch (PartInitException e) {
				}
			}
		});
		monitor.worked(1);
	}

	/**
	 * Initialize the file contents to contents of the given resource.
	 */
	public static String createFragment(String jndiName, MailInfo mailInfo)
		throws CoreException {

		/* We want to be truly OS-agnostic */
		final String newline = System.getProperty("line.separator"); //$NON-NLS-1$

		String line;
		StringBuffer sb = new StringBuffer();
		final String mailHost = mailInfo.getMailHost();
		final String mailUser = mailInfo.getMailUser();
		final String mailFrom = mailInfo.getMailFrom();
		
		boolean matchStart = false;
		boolean matchEnd = false;
		
		try {
			InputStream input = MailInfo.class.getResourceAsStream(RESOURCE_FILE_TEMPLATE);
			BufferedReader reader = new BufferedReader(new InputStreamReader(
					input));
			try {
				while ((line = reader.readLine()) != null) {
					if( line.indexOf("<mail-resource") != -1) {
						matchStart = true;
					}
					if ( (matchStart) && (! matchEnd) ) {
						line = line.replaceAll("\\$\\{jndiName\\}", jndiName); //$NON-NLS-1$
						line = line.replaceAll("\\$\\{mailHost\\}", mailHost); //$NON-NLS-1$
						line = line.replaceAll("\\$\\{mailUser\\}", mailUser); //$NON-NLS-1$
						line = line.replaceAll("\\$\\{mailFrom\\}", mailFrom); //$NON-NLS-1$

						if (line != null) {
							sb.append(line);
							sb.append(newline);
						}
						if(line.indexOf("</mail-resource>") != -1) {
							matchEnd = true;
						}
					}
				}
				
			} finally {
				reader.close();
			}
		} catch (IOException ioe) {
			IStatus status = new Status(IStatus.ERROR, "MailWizard", IStatus.OK, //$NON-NLS-1$
					ioe.getLocalizedMessage(), null);
			throw new CoreException(status);
		}

		return sb.toString();

	}

	private IContainer getContainerResource() {
		if (selection != null && selection.isEmpty() == false
				&& selection instanceof IStructuredSelection) {
			IStructuredSelection ssel = (IStructuredSelection) selection;
			if (ssel.size() > 1)
				return null;
			Object obj = ssel.getFirstElement();
			if (obj instanceof IResource) {
				IContainer containerResource;
				if (obj instanceof IContainer)
					containerResource = (IContainer) obj;
				else
					containerResource = ((IResource) obj).getParent();
				
				return ((containerResource != null) ? containerResource.getProject() : null);
			}
		}
		return null;
	}

	/**
	 * We will accept the selection in the workbench to see if
	 * we can initialize from it.
	 * @see IWorkbenchWizard#init(IWorkbench, IStructuredSelection)
	 */
	public void init(IWorkbench workbench, IStructuredSelection selection) {
		this.selection = selection;
	}
}