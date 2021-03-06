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


/**
 * This is a wizard that creates a new SIPServlet.
 */

public class SIPServletWizard extends AbstractSIPWizard {
	/**
	 * location of the sip servlet creation template file
	 */
	private static final String RESOURCE_FILE_TEMPLATE = "sailfin/templates/SipServlet-java-template.resource"; //$NON-NLS-1$

	@Override
	protected AbstractSIPWizardPage createWizardPage() {
		return new SIPServletWizardPage();
	}

	@Override
	protected String getTemplateName() {
		return RESOURCE_FILE_TEMPLATE;
	}
}
