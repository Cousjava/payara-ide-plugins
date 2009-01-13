package com.sun.enterprise.jst.server.sunappsrv.register.splashHandlers;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

public class RegisterAccountPage extends WizardPage {

	private static final int FIELD_WIDTH = 150;
	private static final int LABEL_WITDH = 80;
	private Text tName;
	private Text tSurname;
	private Text tEmail;
	private Text tPassword;
	private Text tConfirm;
	private Text tCompanyName;
	private Text tCountry;

	protected RegisterAccountPage(String pageName) {
		super(pageName);
		setTitle("Personal Information");
		setDescription("Please enter your personal information");
	}

	public void createControl(Composite comp) {
		Composite composite = new Composite(comp, SWT.NONE);

		GridLayout layout = new GridLayout(2, false);
		layout.marginWidth = 2;
		layout.marginHeight = 2;
		composite.setLayout(layout);
		setControl(composite);

		createTextComposite(composite, "Email address", tEmail);
		createTextComposite(composite, "Password", tPassword, SWT.PASSWORD);
		createTextComposite(composite, "Retype password", tConfirm, SWT.PASSWORD);
		createTextComposite(composite, "Name", tName);
		createTextComposite(composite, "Last Name", tSurname);
		createTextComposite(composite, "Company Name", tCompanyName);
		createTextComposite(composite, "Country", tCountry);
	}

	public void createTextComposite(Composite c, String labelText, Text text) {
		createTextComposite(c, labelText, text, 0);
	}

	public void createTextComposite(Composite c, String labelText, Text text, int i) {
		Label l1 = new Label(c, SWT.NONE);
		Text t = new Text(c, SWT.BORDER | i);
		GridData gridData = new GridData();
		gridData.widthHint = FIELD_WIDTH;
		t.setLayoutData(gridData);
		l1.setText(labelText);
		text = t;
	}
}
