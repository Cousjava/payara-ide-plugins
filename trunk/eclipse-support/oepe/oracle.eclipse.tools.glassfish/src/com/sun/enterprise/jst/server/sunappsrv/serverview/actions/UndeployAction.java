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

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
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
import org.eclipse.wst.server.core.IModule;
import org.eclipse.wst.server.core.IServer;
import org.eclipse.wst.server.core.IServerWorkingCopy;
import org.eclipse.wst.server.core.internal.Server;
import org.eclipse.wst.server.ui.internal.ServerUIPlugin;

import com.sun.enterprise.jst.server.sunappsrv.GlassfishGenericServerBehaviour;
import com.sun.enterprise.jst.server.sunappsrv.serverview.DeployedApplicationsNode;
import com.sun.enterprise.jst.server.sunappsrv.serverview.TreeNode;

public 	class UndeployAction extends Action {
	ISelection selection;
	ICommonActionExtensionSite actionSite;

	public UndeployAction(ISelection selection, ICommonActionExtensionSite actionSite) {
		setText("Undeploy");
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

	@SuppressWarnings("restriction")
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
					IRunnableWithProgress op = new IRunnableWithProgress() {
						public void run(IProgressMonitor monitor) {
							try {

								IServer server = be.getServer();

								IModule[] im = server.getModules();
								IModule imodule = null;
								for (int i = 0; i < im.length; i++) {
									if (im[i].getName().equals(
											module.getName())) {
										imodule = im[i];

									}
								}
								if (imodule == null) {
									return;
								}
								try {
									IServerWorkingCopy wc = server
											.createWorkingCopy();
									wc.modifyModules(null,
											new IModule[] { imodule },
											monitor);
									server = wc.save(true, monitor);

								} catch (CoreException e) {

									e.printStackTrace();
								}
								if (server.getServerState() != IServer.STATE_STOPPED
										&& ServerUIPlugin
												.getPreferences()
												.getPublishOnAddRemoveModule()) {
									final IAdaptable info = new IAdaptable() {
										public Object getAdapter(
												Class adapter) {
											if (Shell.class.equals(adapter))
												return Display.getDefault()
														.getActiveShell();
											return null;
										}
									};
									server.publish(
											IServer.PUBLISH_INCREMENTAL,
											null, info, null);
								}

							} catch (Exception e) {
								e.printStackTrace();

							}
						}
					};
					Shell shell = Display.getDefault().getActiveShell();
					if (shell != null) {
						new ProgressMonitorDialog(shell).run(true, false,
								op);
					}
					target.refresh();
					StructuredViewer view = actionSite
							.getStructuredViewer();
					view.refresh(target);

					// set to FULL to tell the system a full deploy is
					// needed.
					Server server = (Server) be.getServer();
					server.setModulePublishState(server.getModules(),
							IServer.PUBLISH_STATE_FULL);

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

