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
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.search.IJavaSearchConstants;
import org.eclipse.jdt.core.search.IJavaSearchScope;
import org.eclipse.jdt.internal.ui.dialogs.FilteredTypesSelectionDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.jst.j2ee.internal.common.operations.INewJavaClassDataModelProperties;
import org.eclipse.jst.j2ee.internal.dialogs.TypeSearchEngine;
import org.eclipse.jst.j2ee.internal.plugin.J2EEUIMessages;
import org.eclipse.jst.j2ee.internal.web.operations.INewWebClassDataModelProperties;
import org.eclipse.jst.jee.ui.internal.navigator.web.WebAppProvider;
import org.eclipse.jst.servlet.ui.internal.wizard.NewWebClassWizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.wst.common.frameworks.datamodel.IDataModel;

@SuppressWarnings("restriction")
public class AddGenericResourceWizardPage extends NewWebClassWizardPage {

	private Combo mimeTypeCombo;
	private Text repText;

	public AddGenericResourceWizardPage(IDataModel model, String pageName,
			String pageDesc, String pageTitle, String moduleType) {
		super(model, pageName, pageDesc, pageTitle, moduleType);
	}

	protected Composite createTopLevelComposite(Composite parent) {
		Composite composite = super.createTopLevelComposite(parent);
		Label mimeTypeLabel = new Label(composite, SWT.NONE);
		GridData data = new GridData();

		mimeTypeLabel.setText(Messages.mimeTypeLabel);
		mimeTypeLabel.setLayoutData(data);
		mimeTypeCombo = new Combo(composite, SWT.BORDER | SWT.READ_ONLY);
		data = new GridData(GridData.FILL_HORIZONTAL);
		data.widthHint = 300;
		data.horizontalSpan = 1;
		mimeTypeCombo.setLayoutData(data);
		synchHelper.synchCombo(mimeTypeCombo,
				AddGenericResourceDataModelProvider.MIME_TYPE, null);
		populateCombo();

		// placeholder so layout is correct
		new Label(composite, SWT.NONE);

		addRepresentationClassGroup(composite);

		// remove entire existing class section
		hideControl(existingClassButton);
		hideControl(existingClassLabel);
		hideControl(existingClassText);
		hideControl(existingButton);

		return composite;
	}

	/**
	 * Add superclass group to the composite
	 */
	private void addRepresentationClassGroup(Composite composite) {
		Label repLabel = new Label(composite, SWT.LEFT);
		repLabel.setText(Messages.representationClassLabel);
		repLabel.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_FILL));

		repText = new Text(composite, SWT.SINGLE | SWT.BORDER);
		repText.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		synchHelper.synchText(repText, AddGenericResourceDataModelProvider.REPRESENTATION_CLASS, null);

		Button repButton = new Button(composite, SWT.PUSH);
		repButton.setText(J2EEUIMessages.BROWSE_BUTTON_LABEL);
		repButton.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_FILL));
		repButton.addSelectionListener(new SelectionListener() {
			public void widgetSelected(SelectionEvent e) {
				handleRepButtonPressed();
			}

			public void widgetDefaultSelected(SelectionEvent e) {
				// Do nothing
			}
		});
	}
	protected void handleRepButtonPressed() {
		getControl().setCursor(new Cursor(getShell().getDisplay(), SWT.CURSOR_WAIT));
		IPackageFragmentRoot packRoot = (IPackageFragmentRoot) model.getProperty(INewJavaClassDataModelProperties.JAVA_PACKAGE_FRAGMENT_ROOT);
		if (packRoot == null)
			return;

		// this eliminates the non-exported classpath entries
		final IJavaSearchScope scope = TypeSearchEngine.createJavaSearchScopeForAProject(packRoot.getJavaProject(), true, true);

		FilteredTypesSelectionDialog dialog = new FilteredTypesSelectionDialog(getShell(),false, getWizard().getContainer(), scope, IJavaSearchConstants.CLASS);
		dialog.setTitle(Messages.representationClassDialogTitle);
		dialog.setMessage(Messages.representationClassDialogLabel);

		if (dialog.open() == Window.OK) {
			IType type = (IType) dialog.getFirstResult();
			String repClassFullPath = J2EEUIMessages.EMPTY_STRING;
			if (type != null) {
				repClassFullPath = type.getFullyQualifiedName();
			}
			repText.setText(repClassFullPath);
			getControl().setCursor(null);
			return;
		}
		getControl().setCursor(null);
	}

	private void populateCombo() {
		mimeTypeCombo.add(AddGenericResourceTemplateModel.TYPE_APP_JSON);
		mimeTypeCombo.add(AddGenericResourceTemplateModel.TYPE_APP_XML);
		mimeTypeCombo.add(AddGenericResourceTemplateModel.TYPE_TEXT_PLAIN);
		mimeTypeCombo.add(AddGenericResourceTemplateModel.TYPE_TEXT_HTML);
		mimeTypeCombo.select(0);
	}

	@Override
	protected String[] getValidationPropertyNames() {
		String[] base = super.getValidationPropertyNames();
		String[] result = new String[base.length + 2];
		System.arraycopy(base, 0, result, 0, base.length);
		result[base.length] = AddGenericResourceDataModelProvider.MIME_TYPE;
		result[base.length + 1] = AddGenericResourceDataModelProvider.REPRESENTATION_CLASS;
		return result;
	}

	@Override
	protected boolean isProjectValid(IProject project) {
		if (super.isProjectValid(project)) {
			return WizardUtil.hasGF3Runtime(project);
		}
		return false;
	}
	
	@Override
	protected IProject getExtendedSelectedProject(Object selection) {
		if (selection instanceof WebAppProvider) {
			return ((WebAppProvider) selection).getProject();
		}
		
		return super.getExtendedSelectedProject(selection);
	}

	@Override
	protected String getUseExistingCheckboxText() {
		// this is for the existing class browse button, which we have hidden
		return "Unused"; //$NON-NLS-1$
	}

	@Override
	protected String getUseExistingProperty() {
		// this is for the existing class browse button, which we have hidden
		return INewWebClassDataModelProperties.USE_EXISTING_CLASS;
	}

	@Override
	protected void handleClassButtonSelected() {
		// this is for the existing class browse button, which we have hidden
	}

	private void hideControl(Control control) {
		if (control != null) {
			control.setVisible(false);
			GridData data = new GridData();
		    data.exclude = true;
		    data.horizontalSpan = 2;
		    data.horizontalAlignment = SWT.FILL;
		    control.setLayoutData(data);
		}
	}
}
