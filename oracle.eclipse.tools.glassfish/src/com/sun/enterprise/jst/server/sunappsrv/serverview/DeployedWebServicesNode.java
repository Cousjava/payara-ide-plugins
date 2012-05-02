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

import java.util.ArrayList;
import java.util.List;

import org.eclipse.wst.server.core.IServer;

import com.sun.enterprise.jst.server.sunappsrv.SunAppServerBehaviour;
import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;
import com.sun.enterprise.jst.server.sunappsrv.commands.CommandRunner;
import com.sun.enterprise.jst.server.sunappsrv.commands.WSDesc;

public class DeployedWebServicesNode extends TreeNode {

	IServer server = null;
	WebServiceNode[] deployedapps = null;

	public DeployedWebServicesNode(IServer server) {
		super("Deployed Web Services", null, null);
		this.server = server;

	}

	public IServer getServer() {
		return this.server;
	}

	public SunAppServerBehaviour getServerBehavior() {
		return (SunAppServerBehaviour) server.loadAdapter(
				SunAppServerBehaviour.class, null);
	}

	public Object[] getChildren() {

		ArrayList<WebServiceNode> appsList = new ArrayList<WebServiceNode>();
		if (this.deployedapps == null) {

			final SunAppServerBehaviour behaviour = getServerBehavior();
			try {
				if (behaviour == null) {
					this.deployedapps = appsList
							.toArray(new WebServiceNode[appsList
									.size()]);
					return this.deployedapps;
				}

				try {
					CommandRunner mgr = new CommandRunner(
							behaviour.getSunAppServer());
					List<WSDesc> wss = mgr.getWebServices();
							

						for (WSDesc app : wss) {
							WebServiceNode t = new WebServiceNode(this, server,
									app);

							appsList.add(t);
						}
				
				} catch (Exception ex) {
					SunAppSrvPlugin.logMessage("get Applications is failing=", ex); //$NON-NLS-1$

				}
			} catch (Exception e) {
			}
			this.deployedapps = appsList
					.toArray(new WebServiceNode[appsList.size()]);
		}

		return this.deployedapps;
	}

	public void refresh() {
		this.deployedapps = null;
	}



}
