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

package com.sun.enterprise.jst.server.sunappsrv.v3.wizards;

import org.eclipse.core.resources.IProject;
import org.eclipse.jst.ejb.ui.internal.wizard.NewEnterpriseBeanClassWizardPage;
import org.eclipse.jst.jee.ui.internal.navigator.web.WebAppProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.wst.common.frameworks.datamodel.IDataModel;

@SuppressWarnings("restriction")
public class AddEjbTimerWizardPage extends NewEnterpriseBeanClassWizardPage {

	private Text scheduleText;

	public AddEjbTimerWizardPage(IDataModel model, String pageName,
			String pageDesc, String pageTitle, String moduleType) {
		super(model, pageName, pageDesc, pageTitle, moduleType);
	}

	protected Composite createTopLevelComposite(Composite parent) {
		Composite composite = super.createTopLevelComposite(parent);

		projectNameLabel.setText(Messages.ProjectName);
		addSeperator(composite, 3);
		
		new Label(composite, SWT.LEFT).setText(Messages.timerScheduleLabel);
		scheduleText = new Text(composite, SWT.MULTI | SWT.BORDER | SWT.WRAP /*| SWT.V_SCROLL*/);
		GridData layoutData = new GridData(GridData.FILL_BOTH);
		layoutData.verticalSpan = 2;
		initializeDialogUnits(scheduleText);
		layoutData.heightHint = convertHeightInCharsToPixels(2);
		scheduleText.setLayoutData(layoutData);
		synchHelper.synchText(scheduleText,
				AddEjbTimerDataModelProvider.SCHEDULE, null);

		return composite;
	}
	
	@Override
	protected String[] getValidationPropertyNames() {
		String[] base = super.getValidationPropertyNames();
		String[] result = new String[base.length + 1];
		System.arraycopy(base, 0, result, 0, base.length);
		result[base.length] = AddEjbTimerDataModelProvider.SCHEDULE;
		return result;
	}

	@Override
	protected void updateControls() {
		super.updateControls();
	}

	@Override
	protected boolean isProjectValid(IProject project) {
		// super's test for isProjectValid requires an ejb project and we don't 
		// want to do that, so result is basically a test of the grandsuper's isProjectValid
		// with the addition of allowing both ejb and web projects
		return WizardUtil.isWebOrEJBProjectWithGF3Runtime(project);
	}
	
	@Override
	protected IProject getExtendedSelectedProject(Object selection) {
		if (selection instanceof WebAppProvider) {
			return ((WebAppProvider) selection).getProject();
		}
		
		return super.getExtendedSelectedProject(selection);
	}
}
