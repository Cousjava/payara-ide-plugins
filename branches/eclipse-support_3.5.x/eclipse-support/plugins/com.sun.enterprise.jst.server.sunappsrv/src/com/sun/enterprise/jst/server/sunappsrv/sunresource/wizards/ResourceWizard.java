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

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.jem.util.emf.workbench.ProjectUtilities;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jst.j2ee.internal.project.J2EEProjectUtilities;
import org.eclipse.osgi.util.NLS;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWizard;
import org.eclipse.wst.common.project.facet.core.FacetedProjectFramework;
import org.eclipse.wst.server.core.IRuntime;

/**
 * This is a superclass for wizards to create resources
 */

@SuppressWarnings("restriction")
public abstract class ResourceWizard extends Wizard implements INewWizard {
	private static final String GF_RUNTIME = "com.sun.enterprise.jst.server.runtime.sunappsrv"; //$NON-NLS-1$
	private static final String SAILFIN_RUNTIME = "com.sun.enterprise.jst.server.runtime.sailfin"; //$NON-NLS-1$

	protected ISelection selection;
	protected String dirName;
	protected IFolder folder;
	
	/**
	 * Constructor 
	 */
	public ResourceWizard() {
		super();
		setNeedsProgressMonitor(true);
	}

	protected void checkDir(IProject selectedProject) throws CoreException {
		dirName = ResourceUtils.getResourceLocation(selectedProject);
		if(dirName == null) {
			IStatus status = new Status(IStatus.ERROR, getClass().getName(), IStatus.OK, 
					NLS.bind(Messages.errorFolderNull, dirName), null);
			throw new CoreException(status);
		}
		IContainer containerResource = selectedProject;
		folder = containerResource.getFolder(new Path(dirName));
		if (!folder.exists()) {
			IStatus status = new Status(IStatus.ERROR, getClass().getName(), IStatus.OK, 
					NLS.bind(Messages.errorFolderMissing, dirName), null);
			throw new CoreException(status);
		}
	}

	
	protected static String replaceOrRemove(String originalLine, String pattern, String value) {
		String containsPattern = ".*" + pattern + ".*"; //$NON-NLS-1$ //$NON-NLS-2$
		if ((originalLine != null) && Pattern.matches(containsPattern, originalLine)) {
			return (((value == null) || (value.length() == 0)) ? null : 
				originalLine.replaceAll(pattern, value));
		}
		return originalLine;
	}

	protected IContainer getContainerResource() {
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

	protected List<IProject> getGlassFishAndSailfinProjects() {
		IProject[] allProjects = ProjectUtilities.getAllProjects();
		List<IProject> returnProjects = new ArrayList<IProject>();

		for (IProject project2 : allProjects) {
			try {
				if (FacetedProjectFramework.hasProjectFacet(project2, "sun.facet")) { //$NON-NLS-1$
					returnProjects.add(project2);
				} else {
					IRuntime runtime = J2EEProjectUtilities.getServerRuntime(project2);
					if (runtime != null) {
						String runtimeId = runtime.getRuntimeType().getId();

						if (runtimeId.startsWith(GF_RUNTIME) || runtimeId.startsWith(SAILFIN_RUNTIME)) {
							returnProjects.add(project2);
						}
					}
				}
			} catch (CoreException e) {
				// just skip from list
			}
		}
		return returnProjects;
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