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

import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.ProgressMonitorDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.navigator.ICommonActionExtensionSite;
import org.eclipse.ui.texteditor.IWorkbenchActionDefinitionIds;
import org.glassfish.tools.ide.admin.CommandDeleteResource;
import org.glassfish.tools.ide.admin.ResultString;
import org.glassfish.tools.ide.admin.ServerAdmin;
import org.glassfish.tools.ide.admin.TaskState;
import org.glassfish.tools.ide.data.IdeContext;

import com.sun.enterprise.jst.server.sunappsrv.GlassfishGenericServer;
import com.sun.enterprise.jst.server.sunappsrv.GlassfishGenericServerBehaviour;
import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;
import com.sun.enterprise.jst.server.sunappsrv.serverview.NodeTypes;
import com.sun.enterprise.jst.server.sunappsrv.serverview.ResourcesNode;

public class UnregisterResourceAction extends Action {
	ISelection selection;
	ICommonActionExtensionSite actionSite;

	public UnregisterResourceAction(ISelection selection,
			ICommonActionExtensionSite actionSite) {
		setText("Unregister");
		ISharedImages sharedImages = PlatformUI.getWorkbench()
				.getSharedImages();
		setImageDescriptor(sharedImages
				.getImageDescriptor(ISharedImages.IMG_TOOL_DELETE));
		setDisabledImageDescriptor(sharedImages
				.getImageDescriptor(ISharedImages.IMG_TOOL_DELETE_DISABLED));
		setActionDefinitionId(IWorkbenchActionDefinitionIds.DELETE);

		this.selection = selection;
		this.actionSite = actionSite;
	}

	public void runWithEvent(Event event) {
		if (selection instanceof TreeSelection) {
			TreeSelection ts = (TreeSelection) selection;
			Object obj = ts.getFirstElement();
			if (obj instanceof ResourcesNode) {
				final ResourcesNode currentResource = (ResourcesNode) obj;
				if (currentResource.getResource() == null) {
					return;
				}

				try {
					final GlassfishGenericServerBehaviour be = currentResource
							.getServer().getServerBehaviourAdapter();
					IRunnableWithProgress op = new IRunnableWithProgress() {
						public void run(IProgressMonitor monitor) {
							GlassfishGenericServer server = be
									.getSunAppServer();

							String propName = "";
							boolean cascadeDelete = false;
							String type = currentResource.getType();
							if (type.equals(NodeTypes.JDBC_RESOURCE)) {
								propName = "jdbc_resource_name";
							} else if (type
									.equals(NodeTypes.JDBC_CONNECTION_POOL)) {
								propName = "jdbc_connection_pool_id";
								cascadeDelete = true;
							}

							else if (type.equals(NodeTypes.CONN_RESOURCE)) {
								propName = "connector_resource_pool_id";
							}

							else if (type
									.equals(NodeTypes.CONN_CONNECTION_POOL)) {
								propName = "poolname";
							}

							else if (type
									.equals(NodeTypes.ADMINOBJECT_RESOURCE)) {
								propName = "jndi_name";
							}

							else if (type.equals(NodeTypes.JAVAMAIL_RESOURCE)) {
								propName = "jndi_name";
							}

							String resourceName = currentResource.getResource()
									.getName();
							CommandDeleteResource command = new CommandDeleteResource(
									resourceName, currentResource.getResource()
											.getCommandSuffix(), propName,
									cascadeDelete);
							Future<ResultString> future = ServerAdmin
									.<ResultString> exec(server, command,
											new IdeContext());
							ResultString result;
							try {
								result = future.get(30, TimeUnit.SECONDS);
								if (!TaskState.COMPLETED.equals(result
										.getState())) {
									SunAppSrvPlugin
											.logMessage("Unable to delete resource "
													+ resourceName
													+ ". Message: "
													+ result.getValue());
								}
							} catch (InterruptedException e) {
								SunAppSrvPlugin.logMessage(
										"Unable to delete resource "
												+ resourceName, e);
							} catch (ExecutionException e) {
								SunAppSrvPlugin.logMessage(
										"Unable to delete resource "
												+ resourceName, e);
							} catch (TimeoutException e) {
								SunAppSrvPlugin.logMessage(
										"Unable to delete resource "
												+ resourceName, e);
							}

						}
					};
					Shell shell = Display.getDefault().getActiveShell();
					if (shell != null) {
						new ProgressMonitorDialog(shell).run(true, false, op);
					}
					// //?? currentResource.getParent().refresh();
					StructuredViewer view = actionSite.getStructuredViewer();
					view.refresh(currentResource.getParent());

				} catch (Exception e) {
				}
			}
		}
		super.run();
	}

	@Override
	public void run() {
		this.runWithEvent(null);
	}

}
