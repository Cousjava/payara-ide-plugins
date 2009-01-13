package com.sun.enterprise.jst.server.sunappsrv.register.splashHandlers;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.Bullet;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.custom.StyledText;
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

public class RegistrationChoicePage extends WizardPage implements SelectionListener {

	private static final int FIELD_WIDTH = 150;
	private static final int LABEL_WITDH = 80;
	private StyledText styledText;
	private Button noAccount;
	private Button account;
	private Button skip;
	private boolean canFlip = true;

	protected RegistrationChoicePage(String pageName) {
		super(pageName);
		setTitle("Personal Information");
		setDescription("Please enter your personal information");
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

		noAccount.setText("I don't have a Sun Online Account. Sign me up.");
		FormData formData = new FormData();
		formData.top = new FormAttachment(styledText);
		noAccount.setLayoutData(formData);
		noAccount.setSelection(true);
		noAccount.addSelectionListener(this);

		account.setText("I already have a Sun Online Account.");
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
		Text t = new Text(userComposite, SWT.BORDER);

		l1.setText("User Name");
		formData = new FormData();
		formData.width = LABEL_WITDH;
		l1.setLayoutData(formData);

		formData = new FormData();
		formData.width = FIELD_WIDTH;
		formData.left = new FormAttachment(l1, 0, SWT.DEFAULT);
		formData.top = new FormAttachment(l1, 0, SWT.TOP);
		t.setLayoutData(formData);

		Composite passComposite = new Composite(composite, SWT.NONE);
		FormLayout layout2 = new FormLayout();
		passComposite.setLayout(layout2);

		formData = new FormData();
		formData.top = new FormAttachment(userComposite);
		passComposite.setLayoutData(formData);

		Label l2 = new Label(passComposite, SWT.NONE);
		Text t2 = new Text(passComposite, SWT.BORDER | SWT.PASSWORD);

		l2.setText("Password");
		formData = new FormData();
		formData.width = LABEL_WITDH;
		l2.setLayoutData(formData);

		formData = new FormData();
		formData.width = FIELD_WIDTH;
		formData.left = new FormAttachment(l2, 0, SWT.DEFAULT);
		formData.top = new FormAttachment(l2, 0, SWT.TOP);
		t2.setLayoutData(formData);

		skip.setText("Skip registration.");
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
		text.append("Why register? Gain convenient access " + "to benefits such as:\n\n");
		text.append("Patch information and bug updates\n");
		text.append("Screencasts and tutorials\n");
		text.append("Support and training offering\n");

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

	//
	// @Override
	// public boolean canFlipToNextPage() {
	// return canFlip ;
	// }

	public void widgetSelected(SelectionEvent se) {
		Object src = se.getSource();
		if (src.equals(noAccount)) {
			Button bt = (Button) src;
			if (bt.getSelection()) {
				System.out.println("User doesn't have an account");
				canFlip = true;
			}
		}
		if (src.equals(account)) {
			Button bt = (Button) src;
			if (bt.getSelection()) {
				System.out.println("User has account");
				canFlip = false;
			}

		}
		if (src.equals(skip)) {
			Button bt = (Button) src;
			if (bt.getSelection()) {
				System.out.println("User decided to skip registration");
				canFlip = false;
			}
		}

	}

}
