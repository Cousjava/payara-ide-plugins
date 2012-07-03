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
package com.sun.enterprise.jst.server.sunappsrv.serverview.actions;

import java.net.URL;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.swt.widgets.Event;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.browser.IWebBrowser;
import org.eclipse.ui.browser.IWorkbenchBrowserSupport;

import com.sun.enterprise.jst.server.sunappsrv.GlassfishGenericServer;
import com.sun.enterprise.jst.server.sunappsrv.GlassfishGenericServerBehaviour;
import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;
import com.sun.enterprise.jst.server.sunappsrv.commands.Utils;
import com.sun.enterprise.jst.server.sunappsrv.serverview.DeployedWebServicesNode;
import com.sun.enterprise.jst.server.sunappsrv.serverview.WebServiceNode;

public 	class WSDLInfoWebServiceAction extends Action {
	ISelection selection;

	public WSDLInfoWebServiceAction(ISelection selection) {
		setText("Show WDSL content in Browser");


		this.selection = selection;
	}

	public void runWithEvent(Event event) {
		if (selection instanceof TreeSelection) {
			TreeSelection ts = (TreeSelection) selection;
			Object obj = ts.getFirstElement();
			if (obj instanceof WebServiceNode) {
				final WebServiceNode module = (WebServiceNode) obj;
				final DeployedWebServicesNode target = (DeployedWebServicesNode) module
						.getParent();

				try {
					final GlassfishGenericServerBehaviour be = target
							.getServerBehavior();

					IWorkbenchBrowserSupport browserSupport = PlatformUI
							.getWorkbench().getBrowserSupport();
					IWebBrowser browser = browserSupport
							.createBrowser(
									IWorkbenchBrowserSupport.LOCATION_BAR
											| IWorkbenchBrowserSupport.NAVIGATION_BAR,
									null, null, null);
					GlassfishGenericServer server= be.getSunAppServer();
			         String host = server.getServer().getHost();
			         int port = Integer.parseInt(server.getServerPort());

			         String url = Utils.getHttpListenerProtocol(host,port)+"://"+ host+":"+port+ "/"+module.getWSInfo().getName();
					browser.openURL(new URL(url));

				} catch (Exception e) {
					SunAppSrvPlugin.logMessage("Error opening browser: "
							+ e.getMessage());
				}
			}
			super.run();
		}
	}

	@Override
	public void run() {
		this.runWithEvent(null);
	}

}
