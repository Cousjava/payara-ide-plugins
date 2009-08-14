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