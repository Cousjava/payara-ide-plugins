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

import org.eclipse.jst.ejb.ui.internal.wizard.NewEnterpriseBeanWizard;
import org.eclipse.jst.j2ee.internal.project.J2EEProjectUtilities;
import org.eclipse.wst.common.frameworks.datamodel.IDataModel;
import org.eclipse.wst.common.frameworks.datamodel.IDataModelProvider;

@SuppressWarnings("restriction")
public class AddEjbTimerWizard extends NewEnterpriseBeanWizard {

	public AddEjbTimerWizard() {
		this(null);
	}

	public AddEjbTimerWizard(IDataModel model) {
		super(model);
		setWindowTitle(Messages.timerWizardTitle);
		// setDefaultPageImageDescriptor(ImageDescriptor.createFromURL((URL)
		// J2EEPlugin.getDefault().getImage("timer_bean_wiz")));
	}

	@Override
	protected void doAddPages() {
		AddEjbTimerWizardPage page1 = new AddEjbTimerWizardPage(getDataModel(),
				"page1", Messages.timerWizardDescription,
				Messages.timerWizardTitle, J2EEProjectUtilities.EJB);
		// page1.setInfopopID(IEJBUIContextIds.EJB_MESSAGE_BEAN_WIZARD_ADD_MESSAGE_BEAN_PAGE_1);
		addPage(page1);
	}

	@Override
	protected IDataModelProvider getDefaultProvider() {
		return (IDataModelProvider) new AddEjbTimerDataModelProvider();
	}
}
