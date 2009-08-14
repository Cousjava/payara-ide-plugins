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

import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jem.util.emf.workbench.ProjectUtilities;
import org.eclipse.jst.j2ee.internal.common.operations.INewJavaClassDataModelProperties;
import org.eclipse.jst.j2ee.internal.project.J2EEProjectUtilities;
import org.eclipse.jst.j2ee.internal.wizard.NewJavaClassWizardPage;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.wst.common.componentcore.internal.operation.IArtifactEditOperationDataModelProperties;
import org.eclipse.wst.common.frameworks.datamodel.IDataModel;
import org.eclipse.wst.common.project.facet.core.FacetedProjectFramework;

/**
 * This page makes use of the superclass (NewJavaClassWizardPage) controls and 
 * model for basic synchronization and validation but does not use it for the generation 
 * of the code.
 */

@SuppressWarnings("restriction")
public abstract class AbstractSIPWizardPage extends NewJavaClassWizardPage {

	public AbstractSIPWizardPage(IDataModel dataModel, String wizardDescription, String wizardTitle) {
		super(dataModel, "wizardPage1", wizardDescription, wizardTitle, J2EEProjectUtilities.DYNAMIC_WEB); //$NON-NLS-1$
	}

	abstract protected String getDefaultClassName();

	/* (non-Javadoc)
	 * @see org.eclipse.jst.j2ee.internal.wizard.NewJavaClassWizardPage#isProjectValid(org.eclipse.core.resources.IProject)
	 */
	@Override
	protected boolean isProjectValid(IProject project) {
		if (super.isProjectValid(project)) {
			try {
				return FacetedProjectFramework.hasProjectFacet(project, "sip.facet"); //$NON-NLS-1$
			} catch (CoreException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		return false;
	}

	protected Composite createTopLevelComposite(Composite parent) {
		Composite superComposite = super.createTopLevelComposite(parent);
		classText.setText(getDefaultClassName());
		projectNameLabel.setText(Messages.ProjectName);
		return superComposite;
	}

	protected String getPackageName() {
		//return packageText.getText();
		return getDataModel().getStringProperty(INewJavaClassDataModelProperties.JAVA_PACKAGE);
	}
	
	protected String getClassName() {
		//return classText.getText();
		return getDataModel().getStringProperty(INewJavaClassDataModelProperties.CLASS_NAME);
	}

	protected String getSourceFolderName() {
		String nameWithProject = getDataModel().getStringProperty(INewJavaClassDataModelProperties.SOURCE_FOLDER);
		String selectedProjectName = getSelectedProjectName();

		// strip off initial "/" and name of project
		return nameWithProject.substring(
				nameWithProject.indexOf(selectedProjectName) + selectedProjectName.length());
	}

	private String getSelectedProjectName() {
		return getDataModel().getStringProperty(IArtifactEditOperationDataModelProperties.PROJECT_NAME);		
	}

	protected IProject getSelectedProject() {
		return ProjectUtilities.getProject(getSelectedProjectName());
	}

	protected List<String> getInterfacesList() {
		return null;
	}
}