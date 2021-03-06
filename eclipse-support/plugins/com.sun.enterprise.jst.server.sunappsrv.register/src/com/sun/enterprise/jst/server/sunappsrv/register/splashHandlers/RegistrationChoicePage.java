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

import java.lang.reflect.InvocationTargetException;
import java.net.ConnectException;
import java.net.UnknownHostException;
import java.text.MessageFormat;

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
	private Text tHost;
	private Text tPort;

	protected RegistrationChoicePage(String pageName) {
		super(pageName);
		setTitle(pageName);
		setPageComplete(true);
	}

	public void createControl(Composite comp) {
		Composite composite = new Composite(comp, SWT.NONE);

		FormLayout layout = new FormLayout();
		layout.marginWidth = 8;
		layout.marginHeight = 8;
		composite.setLayout(layout);

		setControl(composite);
		createText(composite);
		createOptions(composite);
		setMessage(Messages.SelectRegistrationMethod);
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

		Composite userComposite = createUser(composite);
		Composite passComposite = createPass(composite, userComposite);
		Composite hostComposite = createHost(composite, passComposite);
		Composite portComposite = createPort(composite, hostComposite);

		skip.setText(Messages.SKIP_REGISTRATION);
		formData = new FormData();
		formData.top = new FormAttachment(portComposite);
		skip.setLayoutData(formData);
		skip.addSelectionListener(this);

	}

	private Composite createPort(Composite composite, Composite userComposite) {
		FormData formData;
		Composite passComposite = new Composite(composite, SWT.NONE);
		FormLayout layout2 = new FormLayout();
		passComposite.setLayout(layout2);

		formData = new FormData();
		formData.top = new FormAttachment(userComposite);
		passComposite.setLayoutData(formData);

		Label label = new Label(passComposite, SWT.NONE);
		tPort = new Text(passComposite, SWT.BORDER);
		tPort.setTextLimit(5);
		tPort.addModifyListener(this);

		label.setText(Messages.ProxyPort);
		formData = new FormData();
		formData.width = LABEL_WITDH;
		formData.left = new FormAttachment(0, 35);
		formData.top = new FormAttachment(0, 4);
		label.setLayoutData(formData);

		formData = new FormData();
		formData.width = FIELD_WIDTH;
		formData.left = new FormAttachment(label, 0, SWT.DEFAULT);
		formData.top = new FormAttachment(label, 0, SWT.TOP);
		tPort.setLayoutData(formData);
		tPort.setEnabled(false);
		return passComposite;
	}

	private Composite createHost(Composite composite, Composite userComposite) {
		FormData formData;
		Composite passComposite = new Composite(composite, SWT.NONE);
		FormLayout layout2 = new FormLayout();
		passComposite.setLayout(layout2);

		formData = new FormData();
		formData.top = new FormAttachment(userComposite);
		passComposite.setLayoutData(formData);

		Label label = new Label(passComposite, SWT.NONE);
		tHost = new Text(passComposite, SWT.BORDER);
		tHost.addModifyListener(this);

		label.setText(Messages.ProxyHost);
		formData = new FormData();
		formData.width = LABEL_WITDH;
		formData.left = new FormAttachment(0, 35);
		formData.top = new FormAttachment(0, 4);
		label.setLayoutData(formData);

		formData = new FormData();
		formData.width = FIELD_WIDTH;
		formData.left = new FormAttachment(label, 0, SWT.DEFAULT);
		formData.top = new FormAttachment(label, 0, SWT.TOP);
		tHost.setLayoutData(formData);
		tHost.setEnabled(false);
		return passComposite;
	}

	private Composite createPass(Composite composite, Composite userComposite) {
		FormData formData;
		Composite passComposite = new Composite(composite, SWT.NONE);
		FormLayout layout2 = new FormLayout();
		passComposite.setLayout(layout2);

		formData = new FormData();
		formData.top = new FormAttachment(userComposite);
		passComposite.setLayoutData(formData);

		Label lPass = new Label(passComposite, SWT.NONE);
		tPassword = new Text(passComposite, SWT.BORDER | SWT.PASSWORD);
		tPassword.addModifyListener(this);

		lPass.setText(Messages.PASSWORD);
		formData = new FormData();
		formData.width = LABEL_WITDH;
		formData.left = new FormAttachment(0, 35);
		formData.top = new FormAttachment(0, 4);
		lPass.setLayoutData(formData);

		formData = new FormData();
		formData.width = FIELD_WIDTH;
		formData.left = new FormAttachment(lPass, 0, SWT.DEFAULT);
		formData.top = new FormAttachment(lPass, 0, SWT.TOP);
		tPassword.setLayoutData(formData);
		tPassword.setEnabled(false);
		return passComposite;
	}

	private Composite createUser(Composite composite) {
		FormData formData;
		Composite userComposite = new Composite(composite, SWT.NONE);
		FormLayout layout1 = new FormLayout();
		userComposite.setLayout(layout1);

		formData = new FormData();
		formData.top = new FormAttachment(account);
		userComposite.setLayoutData(formData);

		Label lUser = new Label(userComposite, SWT.NONE);
		tUser = new Text(userComposite, SWT.BORDER);
		tUser.addModifyListener(this);

		lUser.setText(Messages.USER_NAME);
		formData = new FormData();
		formData.width = LABEL_WITDH;
		formData.left = new FormAttachment(0, 35);
		formData.top = new FormAttachment(0, 4);
		lUser.setLayoutData(formData);

		formData = new FormData();
		formData.width = FIELD_WIDTH;
		formData.left = new FormAttachment(lUser, 0, SWT.DEFAULT);
		formData.top = new FormAttachment(lUser, 0, SWT.TOP);
		tUser.setLayoutData(formData);
		tUser.setEnabled(false);
		return userComposite;
	}

	private void createText(Composite composite) {
		Color background = composite.getBackground();
		styledText = new StyledText(composite, SWT.FULL_SELECTION | SWT.WRAP);
		styledText.setBackground(background);
		styledText.setEditable(false);
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
				setMessage(Messages.SelectRegistrationMethod);
				setErrorMessage(null);
				tUser.setEnabled(false);
				tPassword.setEnabled(false);
				tHost.setEnabled(false);
				tPort.setEnabled(false);
				setPageComplete(true);
			}
			return;
		}
		if (src.equals(account)) {
			Button bt = (Button) src;
			if (bt.getSelection()) {
				type = ACCOUNT;
				canFlip = false;
				tUser.setEnabled(true);
				tPassword.setEnabled(true);
				tHost.setEnabled(true);
				tPort.setEnabled(true);
				setMessage(Messages.PLEASE_INSERT_USERNAME_AND_PASSWORD);
				updateFields();
			}
			return;

		}
		if (src.equals(skip)) {
			Button bt = (Button) src;
			if (bt.getSelection()) {
				type = SKIP;
				canFlip = false;
				setMessage(Messages.SelectRegistrationMethod);
				setErrorMessage(null);
				tUser.setEnabled(false);
				tPassword.setEnabled(false);
				tHost.setEnabled(false);
				tPort.setEnabled(false);
				setPageComplete(true);
			}
			return;
		}
	}

	public int getRegistrationType() {
		return type;
	}

	public boolean skipRegistration() {
		try {
			RegisterService.skipRegister();
			return true;
		} catch (RegistrationException e) {
			Activator.logErrorMessage("Registration failed", e); //$NON-NLS-1$
			setErrorMessage(MessageFormat.format(Messages.RegistrationException, e.getMessage()));
		}

		return false;

	}

	public boolean register() {
		try {
			if (!tPort.getText().equals(""))
				RegisterService.setProxy(tHost.getText(), Integer.parseInt(tPort.getText()));
			RegisterService.validateAccountAndRegister(tUser.getText(), tPassword.getText());
			return true;
		} catch (UnknownHostException e) {
			Activator.logErrorMessage("Registration failed", e); //$NON-NLS-1$
			setErrorMessage(MessageFormat.format(Messages.HostNotFound, e.getMessage()));
		} catch (ConnectException e) {
			Activator.logErrorMessage("Registration failed", e); //$NON-NLS-1$
			setErrorMessage(MessageFormat.format(Messages.ConnectionError, e.getMessage()));
		} catch (ClassNotFoundException e) {
			Activator.logErrorMessage("Registration failed", e); //$NON-NLS-1$
			setErrorMessage(MessageFormat.format(Messages.ClassNotFound, e.getMessage()));
		} catch (NoSuchMethodException e) {
			Activator.logErrorMessage("Registration failed", e); //$NON-NLS-1$
			setErrorMessage(MessageFormat.format(Messages.NoSuchMethod, e.getMessage()));
		} catch (IllegalAccessException e) {
			Activator.logErrorMessage("Registration failed", e); //$NON-NLS-1$
			setErrorMessage(MessageFormat.format(Messages.IllegalAccessException, e.getMessage()));
		} catch (InstantiationException e) {
			Activator.logErrorMessage("Registration failed", e); //$NON-NLS-1$
			setErrorMessage(MessageFormat.format(Messages.InstantiationException, e.getMessage()));
		} catch (InvocationTargetException e) {
			Activator.logErrorMessage("Registration failed", e); //$NON-NLS-1$
			setErrorMessage(MessageFormat.format(Messages.InvocationException, e.getMessage()));
		} catch (RegistrationException e) {
			Activator.logErrorMessage("Registration failed", e); //$NON-NLS-1$
			setErrorMessage(MessageFormat.format(Messages.RegistrationException, e.getMessage()));
		}

		return false;
	}

	public void modifyText(ModifyEvent arg0) {
		updateFields();
	}

	private void updateFields() {
		String error = null;
		if (tUser.getText().length() > 0 && tPassword.getText().length() > 0) {
			setPageComplete(true);
		} else {
			setPageComplete(false);
		}
		try {
			if (tPort.getText().length() > 0)
				Integer.parseInt(tPort.getText());
		} catch (NumberFormatException e) {
			error = Messages.PleaseInsertCorrectPortNumber;
		}
		setErrorMessage(error);
	}

}
