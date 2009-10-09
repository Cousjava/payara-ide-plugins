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

package com.sun.enterprise.jst.server.sunappsrv.sailfin.wizards;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.reflect.InvocationTargetException;
import java.util.List;
import java.util.regex.Pattern;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWizard;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;

/**
 * This is a wizard that creates a new AbstractSIPWizard.
 */

public abstract class AbstractSIPWizard extends Wizard implements INewWizard {
	private static final String SIP_PACKAGE_PREFIX = "javax.servlet.sip.";	//$NON-NLS-1$
	private static final String NEWLINE = System.getProperty("line.separator"); //$NON-NLS-1$

	private AbstractSIPWizardPage page;

	/**
	 * Constructor for AbstractSIPWizard.
	 */
	public AbstractSIPWizard() {
		super();
		setNeedsProgressMonitor(true);
	}

	abstract protected AbstractSIPWizardPage createWizardPage();
	abstract protected String getTemplateName();

	/**
	 * Adding the page to the wizard.
	 */
	public void addPages() {
		page = createWizardPage();
		addPage(page);
	}

	/**
	 * This method is called when 'Finish' button is pressed in
	 * the wizard. We will create an operation and run it
	 * using wizard as execution context.
	 */
	public boolean performFinish() {
		final String sourceFolderName = page.getSourceFolderName();
		final String packageName = page.getPackageName();
		final String className = page.getClassName();
		final List<String> interfaces = page.getInterfacesList();
		final IProject selectedProject = page.getSelectedProject();
		IRunnableWithProgress op = new IRunnableWithProgress() {
			public void run(IProgressMonitor monitor) throws InvocationTargetException {
				try {
					doFinish(sourceFolderName, packageName, className, interfaces, selectedProject, monitor);
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
			MessageDialog.openError(getShell(), Messages.ErrorTitle, realException.getMessage());
			return false;
		}
		return true;
	}
	
	/**
	 * The worker method. It will find the source folder, create the
	 * file and open the editor on the newly created file.
	 */

	private void doFinish(String sourceFolderName, String packageName,
			String className, List<String> interfaces, IProject selectedProject, 
			IProgressMonitor monitor) throws CoreException {
		String dirName = sourceFolderName + Path.SEPARATOR + packageName.replace('.', Path.SEPARATOR);
		String fileName = className + ".java"; //$NON-NLS-1$
		IContainer containerResource = selectedProject;
		final IFolder folder = containerResource.getFolder(new Path(dirName));

		// create the folder for the specified package if necessary
		if (!folder.exists()) {
			mkdirs(folder, monitor);
		}

		monitor.beginTask("Creating " + fileName, 2);

		final IFile file = folder.getFile(new Path(fileName));
		try {
			InputStream stream = openContentStream(packageName, className, interfaces);

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
	public InputStream openContentStream(String packageName, String className, List<String> interfaces)
		throws CoreException {
		String line;
		StringBuffer sb = new StringBuffer();

		try {
			InputStream input = AbstractSIPWizard.class.getResourceAsStream(getTemplateName());
			BufferedReader reader = new BufferedReader(new InputStreamReader(
					input));
			String interfacesString = ((interfaces == null) ? null : getCommaSeparatedInterfaces(interfaces));

			try {
				while ((line = reader.readLine()) != null) {
					if ((interfacesString != null) && line.startsWith("<#if")) {	//$NON-NLS-1$
						line = processBlock(line, reader, interfaces);
					} else {
						line = line.replaceAll("\\$\\{className\\}", className); //$NON-NLS-1$
						if (interfacesString != null) {
							line = line.replaceAll("\\$\\{interfaces\\}", getCommaSeparatedInterfaces(interfaces)); //$NON-NLS-1$
						}
						line = replaceOrRemove(line, "\\$\\{packageName\\}", packageName); //$NON-NLS-1$
					}
					if (line != null) {
						sb.append(line);
						sb.append(NEWLINE);
					}
				}

			} finally {
				reader.close();
			}

		} catch (IOException ioe) {
			IStatus status = new Status(IStatus.ERROR, getClass().getName(), IStatus.OK,
					ioe.getLocalizedMessage(), null);
			throw new CoreException(status);
		}

		return new ByteArrayInputStream(sb.toString().getBytes());

	}

	// find the interface name we are checking for of the form
	// <#if INTERFACENAME>
	private static String processBlock(String line, BufferedReader reader,
			List<String> interfaces) throws IOException {
		String interfaceName = line.substring(5, line.length() - 1);
		boolean hasThatInterface = interfaces.contains(interfaceName);
		StringBuilder builder = new StringBuilder(""); //$NON-NLS-1$

		// look for end of block, and return block if needed
		// need to iterate and read lines until the end of the block in 
		// either case so that block is processed and skipped if not needed
		while ((line = reader.readLine()) != null) {
			if (line.startsWith("</#if")) {	//$NON-NLS-1$
				return ((hasThatInterface) ? builder.toString() : null);
			}
			else if (hasThatInterface) {
				builder.append(line);
				builder.append(NEWLINE);
			}
		}

		return null;
	}

	private static String replaceOrRemove(String originalLine, String pattern, String value) {
		String containsPattern = ".*" + pattern + ".*"; //$NON-NLS-1$ //$NON-NLS-2$
		if ((originalLine != null) && Pattern.matches(containsPattern, originalLine)) {
			return (((value == null) || (value.length() == 0)) ? null : 
				originalLine.replaceAll(pattern, value));
		}
		return originalLine;
	}

	private static void mkdirs(IFolder folder, IProgressMonitor monitor)
		throws CoreException {
		if (folder != null && !folder.exists()) {
			IContainer parent = folder.getParent();
			if (parent != null && parent instanceof IFolder) {
				mkdirs((IFolder) parent, monitor);
			}

			folder.create(true, true, monitor);
		}
	}

	// validation doesn't allow an empty list, so there is at least one element
	// each element should have the "javax.servlet.sip." prefix added
	// for 2 or more elements, separate by commas and newlines
	private static String getCommaSeparatedInterfaces(List<String> interfacesList) {
		StringBuilder builder = new StringBuilder(SIP_PACKAGE_PREFIX);
		int count = interfacesList.size();

		builder.append(interfacesList.get(0));
		if (count > 1) {
			for (int i = 1; i < count; i++) {
				builder.append(',');
				builder.append(NEWLINE);
				builder.append("\t\t\t\t"); //$NON-NLS-1$
				builder.append(SIP_PACKAGE_PREFIX);
				builder.append(interfacesList.get(i));
			}
		}
		return builder.toString();
	}
	
	/**
	 * We will accept the selection in the workbench to see if
	 * we can initialize from it.
	 * @see IWorkbenchWizard#init(IWorkbench, IStructuredSelection)
	 */
	public void init(IWorkbench workbench, IStructuredSelection selection) {
	}

}