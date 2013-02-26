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
import java.util.Map.Entry;

import org.eclipse.wst.server.core.IServer;

import com.sun.enterprise.jst.server.sunappsrv.SunAppServerBehaviour;
import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;
import com.sun.enterprise.jst.server.sunappsrv.commands.AppDesc;
import com.sun.enterprise.jst.server.sunappsrv.commands.CommandRunner;

public class DeployedApplicationsNode extends TreeNode {

	IServer server = null;
	ApplicationNode[] deployedapps = null;

	public DeployedApplicationsNode(IServer server) {
		super("Deployed Applications", null, null);
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

		ArrayList<ApplicationNode> appsList = new ArrayList<ApplicationNode>();
		if (this.deployedapps == null) {

			final SunAppServerBehaviour behaviour = getServerBehavior();
			try {
				if (behaviour == null) {
					this.deployedapps = appsList
							.toArray(new ApplicationNode[appsList
									.size()]);
					return this.deployedapps;
				}

				try {
					CommandRunner mgr = new CommandRunner(
							behaviour.getSunAppServer());
					java.util.Map<String, List<AppDesc>> appMap = mgr
							.getApplications(null);
					for (Entry<String, List<AppDesc>> entry : appMap.entrySet()) {
						List<AppDesc> apps = entry.getValue();
						for (AppDesc app : apps) {
							ApplicationNode t = new ApplicationNode(this, server,
									app);

							appsList.add(t);
						}
					}
				} catch (Exception ex) {
					SunAppSrvPlugin.logMessage("get Applications is failing=", ex); //$NON-NLS-1$

				}
			} catch (Exception e) {
			}
			this.deployedapps = appsList
					.toArray(new ApplicationNode[appsList.size()]);
		}

		return this.deployedapps;
	}

	public void refresh() {
		this.deployedapps = null;
	}



}
