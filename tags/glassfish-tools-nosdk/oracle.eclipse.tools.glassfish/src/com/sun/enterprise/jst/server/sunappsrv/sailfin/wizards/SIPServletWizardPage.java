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

import static org.eclipse.jst.j2ee.internal.common.operations.INewJavaClassDataModelProperties.PROJECT;

import org.eclipse.jst.j2ee.internal.web.operations.NewServletClassDataModelProvider;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.wst.common.frameworks.datamodel.DataModelFactory;

/**
 * This page makes use of the and model (NewServletClassDataModelProvider) for basic 
 * synchronization and validation but does not use it for the generation of the code.
 */

@SuppressWarnings("restriction")
public class SIPServletWizardPage extends AbstractSIPWizardPage {
	private static final String SIP_SUPERCLASS_NAME = "javax.servlet.sip.SipServlet"; //$NON-NLS-1$
	private static final String DEFAULT_NAME = "NewSipServlet"; //$NON-NLS-1$

	public SIPServletWizardPage() {
		super(DataModelFactory.createDataModel(new NewServletClassDataModelProvider()), 
				Messages.ServletWizardDescription, Messages.ServletWizardTitle);
	}

	protected Composite createTopLevelComposite(Composite parent) {
		Composite superComposite = super.createTopLevelComposite(parent);

		// if there is no project which is valid, setting this text will cause NPEs in the model synch helper
		// so only set this if there is at least one valid project in the workspace
		if (model.getProperty(PROJECT) != null) {
			superText.setText(SIP_SUPERCLASS_NAME);
		}

		superText.setEnabled(false);
		superButton.setEnabled(false);
		return superComposite;
	}

	@Override
	protected String getDefaultClassName() {
		return DEFAULT_NAME;
	}
}
