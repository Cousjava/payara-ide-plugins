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