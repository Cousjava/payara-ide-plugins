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


package com.sun.enterprise.jst.server.sunappsrv.actions;

import java.net.URL;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IViewActionDelegate;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.browser.IWebBrowser;
import org.eclipse.ui.browser.IWorkbenchBrowserSupport;

import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;

/**
 *
 * @author Ludovic.Champenois@Sun.COM
 */
public class ShowURLAction implements IObjectActionDelegate ,IViewActionDelegate ,IWorkbenchWindowActionDelegate {

	private String url;

    public ShowURLAction(String url) {
        this.url = url;
   }
 

	public void setActivePart(IAction arg0, IWorkbenchPart arg1) {
		
	}

	public void run(IAction arg0) {
		try {

			
			IWorkbenchBrowserSupport browserSupport = PlatformUI.getWorkbench().getBrowserSupport();
			IWebBrowser browser = browserSupport.createBrowser(IWorkbenchBrowserSupport.LOCATION_BAR | IWorkbenchBrowserSupport.NAVIGATION_BAR, null, null, null);
			browser.openURL(new URL(url));
		} catch (Exception e) {
	           SunAppSrvPlugin.logMessage("Error opening browser: "+e.getMessage());

		}		
	}

	public void selectionChanged(IAction arg0, ISelection arg1) {		
	}

	public void init(IViewPart arg0) {
				
	}

	public void dispose() {		
	}

	public void init(IWorkbenchWindow arg0) {		
	}

}
