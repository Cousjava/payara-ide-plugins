/*
 * Copyright (c) 2011 Oracle and/or its affiliates. All rights reserved.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Oracle
 */

package com.sun.enterprise.jst.server.sunappsrv.serverview;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.navigator.ICommonActionExtensionSite;
import org.eclipse.ui.navigator.ICommonViewerSite;
import org.eclipse.ui.navigator.ICommonViewerWorkbenchSite;

import com.sun.enterprise.jst.server.sunappsrv.serverview.actions.OpenInBrowserAction;
import com.sun.enterprise.jst.server.sunappsrv.serverview.actions.TestWebServiceAction;
import com.sun.enterprise.jst.server.sunappsrv.serverview.actions.UndeployAction;
import com.sun.enterprise.jst.server.sunappsrv.serverview.actions.WSDLInfoWebServiceAction;

public class ServerViewActionProvider extends GenericActionProvider {

	
	// used in plugin.xml as an ID.!!!!
	private static final String SERVERVIEW_EXTENTION_ID = "glassfish31.serverview.contentprovider";
	private ICommonActionExtensionSite actionSite;



	@Override
	protected String getExtensionId() {
		return SERVERVIEW_EXTENTION_ID;
	}

	public void init(ICommonActionExtensionSite s) {
		super.init(s);
		this.actionSite = s;
	}

	@Override
	protected void refresh(Object selection) {
		super.refresh(selection);
		if (selection instanceof DeployedApplicationsNode) {
			DeployedApplicationsNode root = (DeployedApplicationsNode) selection;
			root.refresh();
		}
	}
	
	@Override
	public void fillActionBars(IActionBars o) {
		super.fillActionBars(o);
	}



	@Override
	public void fillContextMenu(IMenuManager menu) {
		super.fillContextMenu(menu);
		ICommonViewerSite site = actionSite.getViewSite();
		IStructuredSelection selection = null;
		if (site instanceof ICommonViewerWorkbenchSite) {
			ICommonViewerWorkbenchSite wsSite = (ICommonViewerWorkbenchSite) site;
			selection = (IStructuredSelection) wsSite.getSelectionProvider()
					.getSelection();

			if (selection instanceof TreeSelection) {
				TreeSelection ts = (TreeSelection) selection;
				Object obj = ts.getFirstElement();
				if (obj instanceof DeployedApplicationsNode) {

				} else if (obj instanceof ResourcesNode) {
					// no-op
				} else if (obj instanceof ApplicationNode) {
					menu.add(new Separator());
					// Add undeploy action
					Action undeployAction = new UndeployAction(selection , actionSite);
					menu.add(undeployAction);
					menu.add(new OpenInBrowserAction(selection));
				} else if (obj instanceof WebServiceNode) {
					menu.add(new TestWebServiceAction(selection));
					menu.add(new WSDLInfoWebServiceAction(selection));
					
				}else if (obj instanceof TreeNode) {

				}
				
				
			}
		}

	}



}
