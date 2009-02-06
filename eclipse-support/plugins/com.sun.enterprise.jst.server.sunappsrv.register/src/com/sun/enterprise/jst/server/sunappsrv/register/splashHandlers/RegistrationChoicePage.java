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

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.Bullet;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GlyphMetrics;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

import com.sun.enterprise.jst.server.sunappsrv.register.Activator;
import com.sun.enterprise.jst.server.sunappsrv.register.Messages;
import com.sun.enterprise.jst.server.sunappsrv.register.service.RegisterService;
import com.sun.enterprise.registration.RegistrationException;

public class RegistrationChoicePage extends WizardPage implements SelectionListener, ModifyListener {

	private static final int FIELD_WIDTH = 150;
	private static final int LABEL_WITDH = 80;
	public static final int NO_ACCOUNT = 0;
	public static final int ACCOUNT = 1;
	public static final int SKIP = 2;
	private StyledText styledText;
	private Button noAccount;
	private Button account;
	private Button skip;
	private boolean canFlip = true;
	private int type;
	private Text tPassword;
	private Text tUser;

	protected RegistrationChoicePage(String pageName) {
		super(pageName);
		setTitle(Messages.PERSONAL_INFORMATION);
		setPageComplete(false);
	}

	public void createControl(Composite comp) {
		Composite composite = new Composite(comp, SWT.NONE);

		FormLayout layout = new FormLayout();
		layout.marginWidth = 5;
		layout.marginHeight = 5;
		composite.setLayout(layout);

		setControl(composite);
		createText(composite);
		createOptions(composite);
	}

	private void createOptions(Composite composite) {
		noAccount = new Button(composite, SWT.RADIO);
		account = new Button(composite, SWT.RADIO);
		skip = new Button(composite, SWT.RADIO);

		noAccount.setText(Messages.I_DON_T_HAVE_A_SUN_ONLINE_ACCOUNT_SIGN_ME_UP);
		FormData formData = new FormData();
		formData.top = new FormAttachment(styledText);
		noAccount.setLayoutData(formData);
		noAccount.setSelection(true);
		noAccount.addSelectionListener(this);

		account.setText(Messages.I_ALREADY_HAVE_A_SUN_ONLINE_ACCOUNT);
		formData = new FormData();

		formData.top = new FormAttachment(noAccount);
		account.setLayoutData(formData);
		account.addSelectionListener(this);

		Composite userComposite = new Composite(composite, SWT.NONE);
		FormLayout layout1 = new FormLayout();
		userComposite.setLayout(layout1);

		formData = new FormData();
		formData.top = new FormAttachment(account);
		userComposite.setLayoutData(formData);

		Label l1 = new Label(userComposite, SWT.NONE);
		tUser = new Text(userComposite, SWT.BORDER);
		tUser.addModifyListener(this);

		l1.setText(Messages.USER_NAME);
		formData = new FormData();
		formData.width = LABEL_WITDH;
		l1.setLayoutData(formData);

		formData = new FormData();
		formData.width = FIELD_WIDTH;
		formData.left = new FormAttachment(l1, 0, SWT.DEFAULT);
		formData.top = new FormAttachment(l1, 0, SWT.TOP);
		tUser.setLayoutData(formData);

		Composite passComposite = new Composite(composite, SWT.NONE);
		FormLayout layout2 = new FormLayout();
		passComposite.setLayout(layout2);

		formData = new FormData();
		formData.top = new FormAttachment(userComposite);
		passComposite.setLayoutData(formData);

		Label l2 = new Label(passComposite, SWT.NONE);
		tPassword = new Text(passComposite, SWT.BORDER | SWT.PASSWORD);
		tPassword.addModifyListener(this);

		l2.setText(Messages.PASSWORD);
		formData = new FormData();
		formData.width = LABEL_WITDH;
		l2.setLayoutData(formData);

		formData = new FormData();
		formData.width = FIELD_WIDTH;
		formData.left = new FormAttachment(l2, 0, SWT.DEFAULT);
		formData.top = new FormAttachment(l2, 0, SWT.TOP);
		tPassword.setLayoutData(formData);

		skip.setText(Messages.SKIP_REGISTRATION);
		formData = new FormData();
		formData.top = new FormAttachment(passComposite);
		skip.setLayoutData(formData);
		skip.addSelectionListener(this);
	}

	private void createText(Composite composite) {
		Color background = composite.getBackground();
		styledText = new StyledText(composite, SWT.FULL_SELECTION | SWT.WRAP);
		styledText.setBackground(background);
		FormData formData = new FormData();
		styledText.setLayoutData(formData);

		StringBuffer text = new StringBuffer();
		text.append(Messages.WHY_REGISTER_GAIN_CONVENIENT_ACCESS_TO_BENEFITS_SUCH_AS);
		text.append(Messages.PATCH_INFORMATION_AND_BUG_UPDATES);
		text.append(Messages.SCREENCASTS_AND_TUTORIALS);
		text.append(Messages.SUPPORT_AND_TRAINING_OFFERING);

		styledText.setText(text.toString());
		try {
			StyleRange style = new StyleRange();
			style.metrics = new GlyphMetrics(0, 0, 40);
			Bullet bullet = new Bullet(style);

			styledText.setLineBullet(2, 3, bullet);
		} catch (Exception e) {
			styledText.setText(e.getMessage());
		}
	}

	public void widgetDefaultSelected(SelectionEvent arg0) {
		// TODO Auto-generated method stub

	}

	public boolean canFlipToNextPage() {
		return canFlip;
	}

	public void widgetSelected(SelectionEvent se) {
		Object src = se.getSource();
		if (src.equals(noAccount)) {
			Button bt = (Button) src;
			if (bt.getSelection()) {
				type = NO_ACCOUNT;
				canFlip = true;
				setMessage(null);
				setErrorMessage(null);
				setPageComplete(true);
			}
		}
		if (src.equals(account)) {
			Button bt = (Button) src;
			if (bt.getSelection()) {
				type = ACCOUNT;
				canFlip = false;
				updateFields();
			}

		}
		if (src.equals(skip)) {
			Button bt = (Button) src;
			if (bt.getSelection()) {
				type = SKIP;
				canFlip = false;
				setMessage(null);
				setErrorMessage(null);
				setPageComplete(true);
			}
		}
	}

	public int getRegistrationType() {
		return type;
	}

	public boolean skipRegistration() {
		try {
			RegisterService.skipRegister(null, 0);
			return true;
		} catch (RegistrationException e) {
			Activator.logErrorMessage("Skiping registration failed", e); //$NON-NLS-1$
			setErrorMessage(e.getMessage());
		}
		return false;

	}

	public boolean register() {
		try {
			RegisterService.validateAccountAndRegister(tUser.getText(), tPassword.getText(), null, 0);
			return true;
		} catch (Exception e) {
			Activator.logErrorMessage("Registration failed", e); //$NON-NLS-1$
			setErrorMessage(e.getMessage());
		}

		return false;
	}

	public void modifyText(ModifyEvent arg0) {
		updateFields();
	}

	private void updateFields() {
		if (tUser.getText().length() > 0 && tPassword.getText().length() > 0) {
			setPageComplete(true);
			setErrorMessage(null);
			setMessage(Messages.CLICK_FINISH_TO_REGISTER_GLASSFISH_SERVER);
		} else{
			setPageComplete(false);
			setErrorMessage(Messages.PLEASE_INSERT_USERNAME_AND_PASSWORD);
		}
	}

}
