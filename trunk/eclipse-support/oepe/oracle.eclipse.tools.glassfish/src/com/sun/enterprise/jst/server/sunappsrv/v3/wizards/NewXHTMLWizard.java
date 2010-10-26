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


package com.sun.enterprise.jst.server.sunappsrv.v3.wizards;

import java.lang.reflect.Field;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.dialogs.WizardNewFileCreationPage;
import org.eclipse.wst.html.ui.internal.wizard.NewHTMLTemplatesWizardPage;
import org.eclipse.wst.html.ui.internal.wizard.NewHTMLWizard;

@SuppressWarnings("restriction")
public class NewXHTMLWizard extends NewHTMLWizard {

	public void init(IWorkbench aWorkbench, IStructuredSelection aSelection) {
		super.init(aWorkbench, aSelection);
    	String title = getWindowTitle();
    	title = title.replace("HTML ", "XHTML ");
    	title = title.replace(" HTML", " XHTML");
    	setWindowTitle(title);
	}

    public void addPage(IWizardPage page) {
    	if (page instanceof WizardNewFileCreationPage) {
    		((WizardNewFileCreationPage) page).setFileExtension("xhtml");
    	}
    	
    	if (page instanceof NewHTMLTemplatesWizardPage) {
    		page = new NewXHTMLTemplatesWizardPage();
    		try {
				Field declaredField = getClass().getSuperclass().getDeclaredField("fNewFileTemplatesPage");
				declaredField.setAccessible(true);
				declaredField.set(this, page);
			} catch (SecurityException e) {
				e.printStackTrace();
			} catch (NoSuchFieldException e) {
				e.printStackTrace();
			} catch (IllegalArgumentException e) {
				e.printStackTrace();
			} catch (IllegalAccessException e) {
				e.printStackTrace();
			}
    	}
    	
    	String title = page.getTitle();
    	title = title.replace("HTML ", "XHTML ");
    	title = title.replace(" HTML", " XHTML");
    	page.setTitle(title);
    	
    	String desc = page.getDescription();
    	desc = desc.replace("HTML ", "XHTML ");
    	desc = desc.replace(" HTML", " XHTML");
    	page.setDescription(desc);
    	
    	super.addPage(page);
    }
}
