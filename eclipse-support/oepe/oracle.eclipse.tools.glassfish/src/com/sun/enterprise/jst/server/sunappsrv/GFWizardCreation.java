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
import java.io.IOException;
import java.net.ConnectException;
import java.net.SocketTimeoutException;
import java.net.UnknownHostException;
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
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.wst.server.core.IRuntime;
import org.eclipse.wst.server.core.IRuntimeWorkingCopy;
import org.eclipse.wst.server.core.IServerWorkingCopy;
import org.eclipse.wst.server.core.TaskModel;
import org.eclipse.wst.server.ui.wizard.IWizardHandle;
import org.eclipse.wst.server.ui.wizard.WizardFragment;

import com.sun.enterprise.jst.server.sunappsrv.commands.Utils;

public class GFWizardCreation extends WizardFragment {

	private Helper helper;
	boolean isValid =false;
	private IWizardHandle handle;
	public Composite createComposite(Composite parent, IWizardHandle handle) {
		this.handle = handle;
		Composite container = new Composite(parent, SWT.NONE);
		GridLayout grid = new GridLayout(1, false);
		grid.marginWidth = 0;
		container.setLayout(grid);
		container.setLayoutData(new GridData(GridData.FILL_BOTH));
		handle.setImageDescriptor(ImageDescriptor.createFromURL(SunAppSrvPlugin
				.getInstance().getBundle().getEntry("/icons/wizard75x66.png")) //$NON-NLS-1$
		);
		handle.setTitle("GlassFish Application Server");
		handle.setDescription("Enter the configuration parameters for this GlassFish domain...");
		IServerWorkingCopy server = getServer();
		SunAppServer gf = (SunAppServer) server.loadAdapter(SunAppServer.class,
				null);
		helper = new Helper(handle, gf, server);

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
		
		IRuntime runtime = (IRuntime) getTaskModel().getObject(TaskModel.TASK_RUNTIME);
  
		if (runtime != null){
			String s =runtime.getName();
			s =""+runtime.getName();
		}
		
		IServerWorkingCopy server = (IServerWorkingCopy) getTaskModel()
				.getObject(TaskModel.TASK_SERVER);
		return server;
	}

	@Override
	public void enter() {
		IServerWorkingCopy server = getServer();
		SunAppServer gf = (SunAppServer) server.loadAdapter(SunAppServer.class,
				null);
		helper.updateServer(gf, server);

		exit();

	}

	@Override
	public void exit() {
		if (helper == null)
			return;
		if (isValid = !helper.validate())// failed do not continue
			return;

	}
	@Override
	public boolean isComplete(){
		return isValid;
	}
	static class Helper {

		private SunAppServer glassfish;
		private IServerWorkingCopy serverCopy;
		protected String fLastMessage;
		protected IWizardHandle fWizard;
		private List fPropertyControls = new ArrayList();
		private Text path=null;
		private Text adminport = null;
		private Text serverport =null;
		private Label domainDirLabel=null;
		private Label adminportLabel=null;
		private Label serverportLabel=null;

		public void updateServer(SunAppServer server,
				IServerWorkingCopy scopy){
			this.glassfish = server;
			this.serverCopy = scopy;
			if (glassfish.isLocalServer()) {
				
				String defaultLocation =""+serverCopy.getRuntime().getLocation();// getSunApplicationServerInstallationDirectory();
				File f= new File (defaultLocation,"/domains/domain1");
				defaultLocation = f.getAbsolutePath();
				path.setText(defaultLocation);
				path.setVisible(true);
				domainDirLabel.setVisible(true);
				adminport.setVisible(false);
				serverport.setVisible(false);
				adminportLabel.setVisible(false);
				serverportLabel.setVisible(false);
				}
			else {
				path.setVisible(false);
				domainDirLabel.setVisible(false);
				adminport.setVisible(true);
				serverport.setVisible(true);				
				adminportLabel.setVisible(true);
				serverportLabel.setVisible(true);			}
		}
		private final class PathModifyListener implements ModifyListener {
			public void modifyText(ModifyEvent e) {
				String path = ((Text) e.widget).getText();

				if (path.length() < 1) {
					fLastMessage = "Specify a valid GlassFish Domain directory";
					fWizard.setMessage(fLastMessage, IMessageProvider.ERROR);
				} else if (!validDomainDir(path)) {
					fLastMessage = "invalid GlassFish domain: " + path;
					fWizard.setMessage(fLastMessage, IMessageProvider.ERROR);
				} else {
					//if (fLastMessage != null
					//		&& fLastMessage.equals(fWizard.getMessage())) {
					//	fLastMessage = null;
						fWizard.setMessage(null, IMessageProvider.NONE);
					//}
					validate();
				}
			}

			private boolean validDomainDir(String path) {
				File f = new File(path,"/config/domain.xml");
				if  (!f.exists()){
					return false;
				}
				if  (!f.isFile()){
					return false;
				}				
				return true;
			}
		}

		public Helper(IWizardHandle handle, SunAppServer server,
				IServerWorkingCopy scopy) {
			super();
			glassfish = server;
			fWizard = handle;
			serverCopy = scopy;
		}

	/*	private GenericServerRuntime getRuntimeDelegate() {
			GenericServerRuntime g=  (GenericServerRuntime) serverCopy.getRuntime().loadAdapter(
					GenericServerRuntime.class, null);
			return g;
		}

		private String getSunApplicationServerInstallationDirectory() {
			GenericServerRuntime g = getRuntimeDelegate();
			String path = (String) g
					.getServerInstanceProperties().get(SunAppServer.ROOTDIR);
			return path;
		}*/

		public void decorate(Composite parent) {
			
			boolean local = glassfish.isLocalServer();

				String defaultLocation =""+serverCopy.getRuntime().getLocation();// getSunApplicationServerInstallationDirectory();
				File f= new File (defaultLocation,"/domains/domain1");
				defaultLocation = f.getAbsolutePath();

				
				domainDirLabel = new Label(parent, SWT.NONE);
				domainDirLabel.setText("domain Directory");
				path = new Text(parent, SWT.SHADOW_IN | SWT.BORDER);
				GridData gridData = new GridData(SWT.FILL,SWT.BEGINNING,true,false);
				gridData.horizontalSpan = 2;
				path.setLayoutData(gridData);
				path.setText(defaultLocation);				
				path.setData(SunAppServer.DOMAINPATH);
				path.addModifyListener(new PathModifyListener());
				fPropertyControls.add(path);
				path.setVisible(local);



			Text adminName = SWTUtil.createLabeledText("Admin name", "admin",
					parent);
			adminName.setData(SunAppServer.ADMINNAME);
			fPropertyControls.add(adminName);

			Text password = SWTUtil.createLabeledText("Admin password", "",
					parent);
			password.setEchoChar('*');
			password.setData(SunAppServer.ADMINPASSWORD);
			fPropertyControls.add(password);

			Button sessions = SWTUtil.createLabeledCheck(
					"Preserve Session Across redeploy", true, parent);
			sessions.setData(SunAppServer.KEEPSESSIONS);
			fPropertyControls.add(sessions);
			Dialog.applyDialogFont(parent);
			
			
			adminportLabel = new Label(parent, SWT.NONE);
			adminportLabel.setText("Admin port");
			adminport = new Text(parent, SWT.SHADOW_IN | SWT.BORDER);
			GridData gridData2 = new GridData(SWT.FILL,SWT.BEGINNING,true,false);
			gridData2.horizontalSpan = 2;
			adminport.setLayoutData(gridData2);
			adminport.setText("4848");
			adminport.setData(SunAppServer.ADMINSERVERPORT);
			
			fPropertyControls.add(adminport);
			adminport.addModifyListener(new ModifyListener() {
				public void modifyText(ModifyEvent e) {
					try {
					Integer.parseInt(adminport.getText());
					}catch (NumberFormatException ex){
						fWizard.setMessage("Not an integer value: "+ ex.getMessage() , IMessageProvider.ERROR);
						fWizard.update();
						return;
					}
					validate();
				}
			});
			adminport.setVisible(!local);
			adminportLabel.setVisible(!local);

			serverportLabel = new Label(parent, SWT.NONE);
			serverportLabel.setText("Server Port");
			serverport = new Text(parent, SWT.SHADOW_IN | SWT.BORDER);
			GridData gridData3 = new GridData(SWT.FILL,SWT.BEGINNING,true,false);
			gridData3.horizontalSpan = 2;
			serverport.setLayoutData(gridData3);
			serverport.setText("8080");				
			
			serverport.setData(SunAppServer.SERVERPORT);
			fPropertyControls.add(serverport);
			serverport.addModifyListener(new ModifyListener() {
				public void modifyText(ModifyEvent e) {
					try {
						Integer.parseInt(serverport.getText());
						}catch (NumberFormatException ex){
							fWizard.setMessage("Not an integer value: "+ ex.getMessage() , IMessageProvider.ERROR);
							fWizard.update();
							return;
						}
						validate();
						}
			});
			serverport.setVisible(!local);
			serverportLabel.setVisible(!local);

		}

		/**
		 * Returns the property name/value pairs.
		 * 
		 * @return Map containing the values collected from the user
		 */
		public Map getValues() {
			Map propertyMap = new HashMap();
			for (int i = 0; i < fPropertyControls.size(); i++) {
				Control c = (Control) fPropertyControls.get(i);
				if (!c.isVisible()){
					continue;
				}
				String prop = (String) (c).getData();
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
			if (glassfish != null) {
				glassfish.setServerInstanceProperties(getValues());
				status = glassfish.validate();
			}
			try {
				if ((!glassfish.isLocalServer())&&(!Utils.isSecurePort(glassfish.getServer().getHost(), Integer.parseInt(glassfish.getAdminServerPort())))){
					fWizard.setMessage("Cannot access a non secure remote server. (Hint: run asadmin enable-secure-admin)", IMessageProvider.ERROR);
					fWizard.update();
					return true;					
				}
			} catch (UnknownHostException e) {
				fWizard.setMessage("Unknown Host Exception for host name: "+ e.getMessage() , IMessageProvider.ERROR);
				fWizard.update();
				return true;
				}
			catch (Exception e) {
				fWizard.setMessage("error testing remote server: "+ e.getMessage() , IMessageProvider.ERROR);
				fWizard.update();
				return true;
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
