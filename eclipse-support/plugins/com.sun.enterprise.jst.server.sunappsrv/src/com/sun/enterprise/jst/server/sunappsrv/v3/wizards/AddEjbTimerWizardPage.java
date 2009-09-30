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

package com.sun.enterprise.jst.server.sunappsrv.v3.wizards;

import org.eclipse.core.resources.IProject;
import org.eclipse.jst.ejb.ui.internal.wizard.NewEnterpriseBeanClassWizardPage;
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
		scheduleText = new Text(composite, SWT.SINGLE | SWT.BORDER);
		scheduleText.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		synchHelper.synchText(scheduleText,
				AddEjbTimerDataModelProvider.SCHEDULE, null);

		return composite;
	}
	
	@Override
	protected String[] getValidationPropertyNames() {
		String[] base = super.getValidationPropertyNames();
		// String[] result = new String[base.length + 1];
		// System.arraycopy(base, 0, result, 0, base.length);
		// result[base.length] = ;
		// return result;
		return base;
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
}
