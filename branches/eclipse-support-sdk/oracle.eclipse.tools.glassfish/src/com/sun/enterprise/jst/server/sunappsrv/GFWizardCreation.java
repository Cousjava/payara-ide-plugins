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
import org.eclipse.jst.server.generic.servertype.definition.Property;
import org.eclipse.jst.server.generic.servertype.definition.ServerRuntime;
import org.eclipse.jst.server.generic.ui.internal.SWTUtil;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
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
	}

	
	@Override
	public boolean isComplete() {
		return helper.validate();
	}
	
	class Helper {

		private SunAppServer glassfish;
		private IServerWorkingCopy serverCopy;
		private ArrayList<Control> fPropertyControls = null;
		protected String fLastMessage;
		protected IWizardHandle fWizard;
		private Text path=null;
		private Text adminName = null;
		private Text adminPassword = null;
		private Text adminport = null;
		private Text serverport =null;
		private Text target = null;
		private Button preserveSessions = null;
		private Label domainDirLabel=null;
		private Label adminportLabel=null;
		private Label serverportLabel=null;
		private Button pingButton=null;
		private Button domainBrowseButton = null;

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
				domainBrowseButton.setVisible(true);
				domainDirLabel.setVisible(true);
				adminport.setVisible(false);
				serverport.setVisible(false);
				adminportLabel.setVisible(false);
				serverportLabel.setVisible(false);
				pingButton.setVisible(false);
				}
			else {
				path.setVisible(false);
				domainBrowseButton.setVisible(false);
				domainDirLabel.setVisible(false);
				adminport.setVisible(true);
				serverport.setVisible(true);				
				adminportLabel.setVisible(true);
				serverportLabel.setVisible(true);
				pingButton.setVisible(true);
			}
		}
		private final class PathModifyListener implements ModifyListener {
			public void modifyText(ModifyEvent e) {
				String path = ((Text) e.widget).getText();

				if (path.length() < 1) {
					fLastMessage = "Specify a valid GlassFish Domain directory";
					fWizard.setMessage(fLastMessage, IMessageProvider.ERROR);
					fWizard.update();
				} else {
					validate();
				}
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
			fPropertyControls = new ArrayList<Control>();
			// get properties from server definition
			Map<String, Property> serverProps = getServerProperties(glassfish.getServerDefinition());
			Property prop = null;
				String defaultLocation =""+serverCopy.getRuntime().getLocation();// getSunApplicationServerInstallationDirectory();
				File f= new File (defaultLocation,"/domains/domain1");
				defaultLocation = f.getAbsolutePath();
				
				// domain path and browse button
				prop = serverProps.get(SunAppServer.DOMAINPATH);
				domainDirLabel = new Label(parent, SWT.NONE);
				domainDirLabel.setText(prop.getLabel());
				GridData gridData = new GridData(SWT.FILL,SWT.BEGINNING,true,false);
				gridData.horizontalSpan = 1;
				path = new Text(parent, SWT.SHADOW_IN | SWT.BORDER);
				path.setLayoutData(gridData);
				path.setText(prop.getDefault());
				path.setData(SunAppServer.DOMAINPATH);
				fPropertyControls.add(path);
				domainBrowseButton = SWTUtil.createButton(parent,"...");
				path.addModifyListener(new PathModifyListener());
				final Shell s =  domainBrowseButton.getShell();
				domainBrowseButton.addSelectionListener(new SelectionAdapter() {
					public void widgetSelected(SelectionEvent e) {
						DirectoryDialog dlg = new DirectoryDialog(s);
						dlg.setText(path.getText().replace('\\', '/'));
						dlg.setMessage("Select a GlassFish Domain Directory");
						String res = dlg.open();
						if (res != null) {
							path.setText(res.replace('\\', '/'));
						}
					}

					public void widgetDefaultSelected(SelectionEvent e) {
						widgetSelected(e);
					}

				});

				// target field
				prop = serverProps.get(SunAppServer.TARGET);
				target = SWTUtil.createLabeledText(prop.getLabel(), prop.getDefault(), parent);
				target.setData(SunAppServer.TARGET);
				fPropertyControls.add(target);

				// admin name
				prop = serverProps.get(SunAppServer.ADMINNAME);
			adminName = SWTUtil.createLabeledText(prop.getLabel(), prop.getDefault(), parent);
			adminName.setData(SunAppServer.ADMINNAME);
			fPropertyControls.add(adminName);

			// admin pass
			prop = serverProps.get(SunAppServer.ADMINPASSWORD);
			adminPassword = SWTUtil.createLabeledText(prop.getLabel(), prop.getDefault(), parent);
			adminPassword.setEchoChar('*');
			adminPassword.setData(SunAppServer.ADMINPASSWORD);
			fPropertyControls.add(adminPassword);

			// session preservation
			prop = serverProps.get(SunAppServer.KEEPSESSIONS);
			preserveSessions = SWTUtil.createLabeledCheck(prop.getLabel(), Boolean.parseBoolean(prop.getDefault()), parent);
			preserveSessions.setData(SunAppServer.KEEPSESSIONS);
			fPropertyControls.add(preserveSessions);
			
			// admin port
			prop = serverProps.get(SunAppServer.ADMINSERVERPORT);
			GridData gridData2 = new GridData(SWT.FILL,SWT.BEGINNING,true,false);
			gridData2.horizontalSpan = 2;
			adminportLabel = new Label(parent, SWT.NONE);
			adminportLabel.setText(prop.getLabel());
			adminport = new Text(parent, SWT.SHADOW_IN | SWT.BORDER);
			adminport.setText(prop.getDefault());
			adminport.setLayoutData(gridData2);
			adminport.setData(SunAppServer.ADMINSERVERPORT);
			adminport.addModifyListener(new ModifyListener() {
				public void modifyText(ModifyEvent e) {
					validate();
				}
			});
			fPropertyControls.add(adminport);

			// server port
			prop = serverProps.get(SunAppServer.SERVERPORT);
			GridData gridData3 = new GridData(SWT.FILL,SWT.BEGINNING,true,false);
			gridData3.horizontalSpan = 2;
			serverportLabel = new Label(parent, SWT.NONE);
			serverportLabel.setText(prop.getLabel());
			serverport = new Text(parent, SWT.SHADOW_IN | SWT.BORDER);
			serverport.setText(prop.getDefault());
			serverport.setLayoutData(gridData3);
			serverport.setData(SunAppServer.SERVERPORT);
			serverport.addModifyListener(new ModifyListener() {
				public void modifyText(ModifyEvent e) {
					validate();
				}
			});
			fPropertyControls.add(serverport);
			
			// ping button
			pingButton = SWTUtil.createButton(parent, "Ping Server...");
		    Listener listener = new Listener() {
		        public void handleEvent(Event event) {
		        	validate();
		        }
		      };
		      pingButton.addListener(SWT.Selection, listener);
		      
		      Dialog.applyDialogFont(parent);
		      
		      //updateServer(glassfish, serverCopy);
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
				String prop = (String) (c).getData();
				if (prop==null){
					continue;
				}
				if ((!c.isVisible()/*&&(!prop.equals(SunAppServer.DOMAINPATH)))*/)){
					continue;
				}
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
		
		private Map<String, Property> getServerProperties(ServerRuntime serverDef) {
			HashMap<String, Property> propertiesMap = new HashMap<String, Property>(serverDef.getProperty().size());
			for (int i = 0; i < serverDef.getProperty().size(); i++) {
				Property p = (Property) serverDef.getProperty().get(i);
				propertiesMap.put(p.getId(), p);
			}
			return propertiesMap;
		}
		
		public boolean validate() {
			IStatus status = null;
			if (glassfish != null) {
				glassfish.setServerInstanceProperties(getValues());
				status = glassfish.validate();
			}
			
			if (!glassfish.isLocalServer()) {
				String version = glassfish.getVersionString();
				if (version.equals("")) {
					// (!Utils.isSecurePort(glassfish.getServer().getHost(),
					// Integer.parseInt(glassfish.getAdminServerPort())))){
					fWizard.setMessage(
							"Cannot communicate with "
									+ glassfish.getServer().getHost()
									+ ":"
									+ glassfish.getAdminServerPort()
									+ " remote server. Is it up? Is it secure? (Hint: run asadmin enable-secure-admin)",
							IMessageProvider.ERROR);
					fWizard.update();
					return false;
				}
			}

			if (status == null || status.isOK()) {
				fWizard.setMessage(null, IMessageProvider.NONE);
				fWizard.update();
			} else {
				fWizard.setMessage(status.getMessage(), IMessageProvider.ERROR);
				fWizard.update();
				return false;
			}
			return true;
		}
	}

}
