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

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jst.j2ee.internal.common.operations.NewJavaClassDataModelProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.wst.common.frameworks.datamodel.DataModelFactory;

/**
 * This page makes use of the and model (NewJavaClassDataModelProvider) for basic 
 * synchronization and validation but does not use it for the generation of the code.
 */

@SuppressWarnings("restriction")
public class SIPListenerWizardPage extends AbstractSIPWizardPage {
	private static final String DEFAULT_NAME = "NewSipListener"; //$NON-NLS-1$
	private static final String[] INTERFACE_LIST = {"SipServletListener", //$NON-NLS-1$
		"SipErrorListener", 									//$NON-NLS-1$
		"TimerListener", 										//$NON-NLS-1$
		"SipApplicationSessionActivationListener", 				//$NON-NLS-1$
		"SipApplicationSessionAttributeListener",				//$NON-NLS-1$ 
		"SipApplicationSessionBindingListener",					//$NON-NLS-1$
		"SipApplicationSessionListener",						//$NON-NLS-1$
		"SipSessionActivationListener",							//$NON-NLS-1$
		"SipSessionAttributeListener",							//$NON-NLS-1$
		"SipSessionBindingListener",							//$NON-NLS-1$
		"SipSessionListener"};									//$NON-NLS-1$

	private CheckboxTableViewer checkTableViewer;

	public SIPListenerWizardPage() {
		super(DataModelFactory.createDataModel(new NewJavaClassDataModelProvider()), 
				Messages.ListenerWizardDescription, Messages.ListenerWizardTitle);
	}

	protected Composite createTopLevelComposite(Composite parent) {
		Composite superComposite = super.createTopLevelComposite(parent);

		createSuperInterfacesControls(superComposite);
		return superComposite;
	}

	protected void createSuperInterfacesControls(Composite composite) {
		Label label = new Label(composite, SWT.HORIZONTAL);
		label.setText(Messages.implementsLabel);
		checkTableViewer = CheckboxTableViewer.newCheckList(composite, SWT.CHECK);
		checkTableViewer.setContentProvider (new ArrayContentProvider());
		checkTableViewer.setInput(INTERFACE_LIST);
		checkTableViewer.addCheckStateListener(new ICheckStateListener() {
	           public void checkStateChanged(CheckStateChangedEvent e) {
	        	   validatePage();
	           }
		});
	}

	protected void validatePage(boolean showMessage) {
		boolean isSuperComplete;

		super.validatePage(showMessage);
		isSuperComplete = isPageComplete();
		if (isSuperComplete) {
			boolean hasInterfaces = (checkTableViewer.getCheckedElements().length > 0);

			if (showMessage) {
				setErrorMessage(hasInterfaces ? null : Messages.errorInterfacesMissing);
			}
			setPageComplete(hasInterfaces);
		}
	}

	@Override
	protected String getDefaultClassName() {
		return DEFAULT_NAME;
	}

	protected List<String> getInterfacesList() {
		List<String> interfacesSelected = new ArrayList<String>();
		Object[] selected = checkTableViewer.getCheckedElements();

		for (Object object : selected) {
			interfacesSelected.add(object.toString());
		}
	
		return interfacesSelected;
	}
}
