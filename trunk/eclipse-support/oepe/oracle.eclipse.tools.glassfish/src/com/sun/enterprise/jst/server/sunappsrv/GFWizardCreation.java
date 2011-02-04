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

package com.sun.enterprise.jst.server.sunappsrv;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jst.server.generic.ui.internal.SWTUtil;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Text;
import org.eclipse.wst.server.core.IServerWorkingCopy;
import org.eclipse.wst.server.core.TaskModel;
import org.eclipse.wst.server.ui.wizard.IWizardHandle;
import org.eclipse.wst.server.ui.wizard.WizardFragment;

public class GFWizardCreation extends WizardFragment {

	private Helper helper;

	public Composite createComposite(Composite parent, IWizardHandle handle) {

		Composite container = new Composite(parent, SWT.NONE);
		GridLayout grid = new GridLayout(1, false);
		grid.marginWidth = 0;
		container.setLayout(grid);
		container.setLayoutData(new GridData(GridData.FILL_BOTH));
		handle.setImageDescriptor(ImageDescriptor.createFromURL(SunAppSrvPlugin
				.getInstance().getBundle().getEntry("/icons/wizard75x66.png")) //$NON-NLS-1$
		);
		handle.setTitle("GlassFish Application server");
		handle.setDescription("Enter the configuration parameters for this server...");
		IServerWorkingCopy server = getServer();
		SunAppServer dl = (SunAppServer) server.loadAdapter(SunAppServer.class,
				null);
		helper = new Helper(handle, dl);

		Composite cp = new Composite(container, SWT.NONE);
		GridLayout layout = new GridLayout(3, false);
		cp.setLayout(layout);
		cp.setLayoutData(new GridData(GridData.FILL_BOTH));
		helper.decorate(cp);

		return container;
	}
	
	@Override
	public boolean hasComposite() {
		return true;
	}

	/**
	 * @return
	 */
	private IServerWorkingCopy getServer() {
		IServerWorkingCopy server = (IServerWorkingCopy) getTaskModel()
				.getObject(TaskModel.TASK_SERVER);
		return server;
	}

	@Override
	public void enter() {

		// if (helper.validate())// failed do not continue
		// return;

	}

	@Override
	public void exit() {
		if (helper == null)
			return;
		if (helper.validate())// failed do not continue
			return;

	}


	static class Helper {

		private SunAppServer fServer;

		protected String fLastMessage;
		protected IWizardHandle fWizard;
		private List fPropertyControls = new ArrayList();

		private final class PathModifyListener implements ModifyListener {
			public void modifyText(ModifyEvent e) {
				String path = ((Text) e.widget).getText();

				if (path.length() < 1) {
					fLastMessage = "Specify a valid path";
					fWizard.setMessage(fLastMessage, IMessageProvider.ERROR);
				} else if (!pathExist(path)) {
					fLastMessage = "invalid path: " + path;
					fWizard.setMessage(fLastMessage, IMessageProvider.ERROR);
				} else {
					if (fLastMessage != null
							&& fLastMessage.equals(fWizard.getMessage())) {
						fLastMessage = null;
						fWizard.setMessage(null, IMessageProvider.NONE);
					}
					validate();
				}
			}

			private boolean pathExist(String path) {
				File f = new File(path);
				return f.exists();
			}
		}

		public Helper(IWizardHandle handle, SunAppServer server) {
			super();
			fServer = server;
			fWizard = handle;
		}

		public void decorate(Composite parent) {

			Text path = SWTUtil.createLabeledFile("domain Directory", "/",
					parent);
			path.setData(SunAppServer.DOMAINPATH);
			path.addModifyListener(new PathModifyListener());
			fPropertyControls.add(path);

			Text adminName = SWTUtil.createLabeledText("Admin name", "admin",
					parent);
			adminName.setData(SunAppServer.ADMINNAME);
			fPropertyControls.add(adminName);
			Text password = SWTUtil.createLabeledText("Admin password", "",
					parent);
			password.setData(SunAppServer.ADMINPASSWORD);
			fPropertyControls.add(adminName);
			Button sessions = SWTUtil.createLabeledCheck(
					"Preserve Session Across redeploy", true, parent);
			sessions.setData(SunAppServer.KEEPSESSIONS);
			fPropertyControls.add(sessions);
			Dialog.applyDialogFont(parent);
		}

		/**
		 * Returns the property name/value pairs.
		 * 
		 * @return Map containing the values collected from the user
		 */
		public Map getValues() {
			Map propertyMap = new HashMap();
			for (int i = 0; i < fPropertyControls.size(); i++) {
				String prop = (String) ((Control) fPropertyControls.get(i))
						.getData();
				if (fPropertyControls.get(i) instanceof Button) {
					Button button = (Button) fPropertyControls.get(i);
					propertyMap.put(prop,
							Boolean.toString(button.getSelection()));
				} else if (fPropertyControls.get(i) instanceof Combo) {
					Combo combo = (Combo) fPropertyControls.get(i);
					int index = combo.getSelectionIndex();
					if (index > 0) {// is there a selection?
						propertyMap.put(prop, combo.getItem(index));
					} else {
						propertyMap.put(prop, combo.getText());
					}

				} else {
					Text text = (Text) fPropertyControls.get(i);
					propertyMap.put(prop, text.getText());
				}
			}
			return propertyMap;
		}

		public boolean validate() {
			IStatus status = null;
			if (fServer != null) {
				fServer.setServerInstanceProperties(getValues());
				status = fServer.validate();
			}
			if (status == null || status.isOK()) {
				fWizard.setMessage(null, IMessageProvider.NONE);
				fWizard.update();
			} else {
				fWizard.setMessage(status.getMessage(), IMessageProvider.ERROR);
				fWizard.update();
				return true;
			}
			return false;
		}
	}

}
