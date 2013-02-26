/*
 * Copyright (c) 1997-2010 Oracle and/or its affiliates. All rights reserved.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Sun Microsystems
 *     Oracle
 */

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
import org.eclipse.wst.server.core.IRuntime;

/**
 * This page makes use of the superclass (NewJavaClassWizardPage) controls and 
 * model for basic synchronization and validation but does not use it for the generation 
 * of the code.
 */

@SuppressWarnings("restriction")
public abstract class AbstractSIPWizardPage extends NewJavaClassWizardPage {

	private static final String SAILFIN1_RUNTIME = "com.sun.enterprise.jst.server.runtime.sailfin1"; //$NON-NLS-1$
	private static final String SAILFIN2_RUNTIME = "com.sun.enterprise.jst.server.runtime.sailfin2"; //$NON-NLS-1$

	public AbstractSIPWizardPage(IDataModel dataModel, String wizardDescription, String wizardTitle) {
		super(dataModel, "wizardPage1", wizardDescription, wizardTitle, J2EEProjectUtilities.DYNAMIC_WEB); //$NON-NLS-1$
	}

	abstract protected String getDefaultClassName();

	/* (non-Javadoc)
	 * @see org.eclipse.jst.j2ee.internal.wizard.NewJavaClassWizardPage#isProjectValid(org.eclipse.core.resources.IProject)
	 */
	@Override
	protected boolean isProjectValid(IProject project) {
		// returns true if super's check is true and the project either has the sip facet or 
		// is targeted to a sailfin v1 or v2 runtime
		return (super.isProjectValid(project) && isValidSailfinProject(project));
	}

	protected static boolean isValidSailfinProject(IProject project) {
		try {
			if (FacetedProjectFramework.hasProjectFacet(project, "sip.facet")) { //$NON-NLS-1$
				return true;
			}

			IRuntime runtime = J2EEProjectUtilities.getServerRuntime(project);

			if (runtime != null) {
				String runtimeType = runtime.getRuntimeType().getId();
				
				return (runtimeType.equals(SAILFIN1_RUNTIME) || 
						runtimeType.equals(SAILFIN2_RUNTIME));
				
			}
		} catch (CoreException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
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
