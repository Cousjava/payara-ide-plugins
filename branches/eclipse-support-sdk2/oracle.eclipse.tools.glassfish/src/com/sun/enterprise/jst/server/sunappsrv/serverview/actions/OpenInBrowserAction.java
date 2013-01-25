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

import java.net.URI;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.swt.widgets.Event;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.browser.IWebBrowser;
import org.eclipse.ui.browser.IWorkbenchBrowserSupport;
import org.eclipse.ui.texteditor.IWorkbenchActionDefinitionIds;

import com.sun.enterprise.jst.server.sunappsrv.GlassfishGenericServer;
import com.sun.enterprise.jst.server.sunappsrv.GlassfishGenericServerBehaviour;
import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;
import com.sun.enterprise.jst.server.sunappsrv.serverview.DeployedApplicationsNode;
import com.sun.enterprise.jst.server.sunappsrv.serverview.TreeNode;
import com.sun.enterprise.jst.server.sunappsrv.serverview.Utils;

public 	class OpenInBrowserAction extends Action {
	ISelection selection;

	public OpenInBrowserAction(ISelection selection) {
		setText("Open in Browser");
		ISharedImages sharedImages = PlatformUI.getWorkbench()
				.getSharedImages();
		setImageDescriptor(sharedImages
				.getImageDescriptor(ISharedImages.IMG_TOOL_FORWARD));
		setDisabledImageDescriptor(sharedImages
				.getImageDescriptor(ISharedImages.IMG_TOOL_FORWARD_DISABLED));
		setActionDefinitionId(IWorkbenchActionDefinitionIds.PRINT);

		this.selection = selection;
	}

	public void runWithEvent(Event event) {
		if (selection instanceof TreeSelection) {
			TreeSelection ts = (TreeSelection) selection;
			Object obj = ts.getFirstElement();
			if (obj instanceof TreeNode) {
				final TreeNode module = (TreeNode) obj;
				final DeployedApplicationsNode target = (DeployedApplicationsNode) module
						.getParent();

				try {
					final GlassfishGenericServerBehaviour be = target.getServer()
							.getServerBehaviourAdapter();

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

			         URI uri  = new URI(Utils.getHttpListenerProtocol(host,port), null, host, port, "/"+module.getName(), null, null); // NOI18N
					browser.openURL(uri.toURL());

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
