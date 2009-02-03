/*DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.

Copyright 1997-2009 Sun Microsystems, Inc. All rights reserved.

The contents of this file are subject to the terms of either the GNU
General Public License Version 2 only ("GPL") or the Common Development
and Distribution License("CDDL") (collectively, the "License").  You
may not use this file except in compliance with the License. You can obtain
a copy of the License at https://glassfish.dev.java.net/public/CDDL+GPL.html
or glassfish/bootstrap/legal/LICENSE.txt.  See the License for the specific
language governing permissions and limitations under the License.

When distributing the software, include this License Header Notice in each
file and include the License file at glassfish/bootstrap/legal/LICENSE.txt.
Sun designates this particular file as subject to the "Classpath" exception
as provided by Sun in the GPL Version 2 section of the License file that
accompanied this code.  If applicable, add the following below the License
Header, with the fields enclosed by brackets [] replaced by your own
identifying information: "Portions Copyrighted [year]
[name of copyright owner]"

Contributor(s):

If you wish your version of this file to be governed by only the CDDL or
only the GPL Version 2, indicate your decision by adding "[Contributor]
elects to include this software in this distribution under the [CDDL or GPL
Version 2] license."  If you don't indicate a single choice of license, a
recipient has the option to distribute your version of this file under
either the CDDL, the GPL Version 2 or to extend the choice of license to
its licensees as provided above.  However, if you add GPL Version 2 code
and therefore, elected the GPL Version 2 license, then the option applies
only if the new code is made subject to such option by the copyright
holder.
 */
package com.sun.enterprise.jst.server.sunappsrv.register.splashHandlers;

import java.util.List;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

import com.sun.enterprise.jst.server.sunappsrv.register.Activator;
import com.sun.enterprise.jst.server.sunappsrv.register.service.RegisterService;
import com.sun.enterprise.registration.RegistrationException;

public class RegisterAccountPage extends WizardPage implements ModifyListener {

	private static final int FIELD_WIDTH = 150;
	private static final int LABEL_WITDH = 80;
	private Text tFirstName;
	private Text tLastName;
	private Text tEmail;
	private Text tPassword;
	private Text tConfirm;
	private Text tCompanyName;
	private Combo tCountry;
	private List countries;

	protected RegisterAccountPage(String pageName) {
		super(pageName);
		setTitle("Personal Information");
		setDescription("Please enter your personal information");
	}

	@SuppressWarnings("unchecked")
	public void createControl(Composite comp) {
		Composite composite = new Composite(comp, SWT.NONE);

		GridLayout layout = new GridLayout(2, false);
		layout.marginWidth = 2;
		layout.marginHeight = 2;
		composite.setLayout(layout);
		setControl(composite);

		tEmail = createTextComposite(composite, "Email address");
		tPassword = createTextComposite(composite, "Password", SWT.PASSWORD);
		tConfirm = createTextComposite(composite, "Retype password", SWT.PASSWORD);
		tFirstName= createTextComposite(composite, "Name");
		tLastName =createTextComposite(composite, "Last Name");
		tCompanyName =createTextComposite(composite, "Company Name");
		try {
			tCountry = createListComposite(composite, "Country", 0);
			countries = RegisterService.getCountries(null, 0);
			List dispList = (List) countries.get(1);
			for (Object c : dispList) {
				tCountry.add(c.toString());
			}
		} catch (RegistrationException e) {
			Activator.showErrorAndLog(new Status(IStatus.ERROR, Activator.PLUGIN_ID, "Error getting Countries list: "
					+ e.getMessage(), e), e.getMessage(), "Exception occurred");
		}
	}

	public Text createTextComposite(Composite c, String labelText) {
		return createTextComposite(c, labelText, 0);
	}

	public Text createTextComposite(Composite c, String labelText,  int i) {
		Label l1 = new Label(c, SWT.NONE);
		Text t = new Text(c, SWT.BORDER | i);
		GridData gridData = new GridData();
		gridData.widthHint = FIELD_WIDTH;
		t.setLayoutData(gridData);
		l1.setText(labelText);
		t.addModifyListener(this);
		return t;
	}

	public Combo createListComposite(Composite c, String labelText,  int i) {
		Label l1 = new Label(c, SWT.NONE);
		Combo co = new Combo(c, SWT.READ_ONLY);
		GridData gridData = new GridData();
		gridData.widthHint = FIELD_WIDTH;
		co.setLayoutData(gridData);
		co.addSelectionListener(new SelectionListener() {
			public void widgetDefaultSelected(SelectionEvent selectionevent) {
				updatePage();
			}

			public void widgetSelected(SelectionEvent selectionevent) {
				updatePage();
			}
		});

		l1.setText(labelText);
		return co;
	}
	
	public String getActualCountry(String country){
		List dispList = (List) countries.get(1);
		for (int i = 0; i < dispList.size(); i++) {
			if (dispList.get(i).toString().equals(country))
				return (String) ((List)countries.get(0)).get(i);
			
		}
		return null;
	}

	public boolean registerUser() {
		try {
			RegisterService.createSDNAccount(tEmail.getText(), tPassword.getText(), getActualCountry(tCountry.getText()), tFirstName.getText(), tLastName.getText(), tCompanyName.getText(), null, 0);
			return true;
		} catch (Exception e) {
			Activator.logErrorMessage("Creating an SDN account failed: ", e);
			setErrorMessage(e.getMessage());
			return false;
		}
	}

	public void modifyText(ModifyEvent arg0) {
		updatePage();
	}

	private void updatePage() {
		if (tEmail.getText().length() <=0){
			setErrorMessage("Please insert email adress");
			setPageComplete(false);
			return;
		}
		if (tPassword.getText().length() <=0){
			setErrorMessage("Please insert password");
			setPageComplete(false);
			return;
		}
		if (!tPassword.getText().equals(tConfirm.getText())){
			setErrorMessage("Passwords don't match");
			setPageComplete(false);
			return;
		}
		if (tFirstName.getText().length() <=0){
			setErrorMessage("Please insert your first name");
			setPageComplete(false);
			return;
		}
		if (tLastName.getText().length() <=0){
			setErrorMessage("Please insert your last name");
			setPageComplete(false);
			return;
		}
		if (tCompanyName.getText().length() <=0){
			setErrorMessage("Please insert your company name");
			setPageComplete(false);
			return;
		}
		if (tCountry.getText().length() <=0){
			setErrorMessage("Please insert your country");
			setPageComplete(false);
			return;
		}
		setErrorMessage(null);
		setMessage("Click finish to register your account and register Glassfish");
		setPageComplete(true);
	}
	

}
