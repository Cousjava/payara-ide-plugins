package com.sun.enterprise.jst.server.sunappsrv.register.splashHandlers;

import org.eclipse.jface.wizard.Wizard;

public class RegistrationWizard extends Wizard {

	@Override
	public void addPages() {
		addPage(new RegistrationChoicePage("Choose registration method"));
		addPage(new RegisterAccountPage("Register account"));
	}

	@Override
	public boolean performFinish() {
		return false;
	}

}
