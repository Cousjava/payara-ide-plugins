/*
 * Copyright (c) 1997-2010 Oracle and/or its affiliates. All rights reserved.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Sun Microsystems
 *     Oracle
 */


package com.sun.enterprise.jst.server.sunappsrv.editorsections;



import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.forms.IFormColors;
import org.eclipse.ui.forms.widgets.ExpandableComposite;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.wst.server.core.model.ServerBehaviourDelegate;
import org.eclipse.wst.server.ui.editor.ServerEditorSection;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.layout.GridDataFactory;
import org.eclipse.jst.server.generic.ui.internal.SWTUtil;

import com.sun.enterprise.jst.server.sunappsrv.Messages;
import com.sun.enterprise.jst.server.sunappsrv.SunAppServer;
import com.sun.enterprise.jst.server.sunappsrv.SunAppServerBehaviour;
import com.sun.enterprise.jst.server.sunappsrv.SunAppServerCommands;



/**
 *
 * @author ludo
 */
@SuppressWarnings("restriction")
public class ServerSection extends ServerEditorSection implements PropertyChangeListener {


	SunAppServer sunserver;
	Text username = null;
	Text password = null;
	Text adminServerPortNumber = null;
	Text serverPortNumber = null;

	/**
	 *
	 */
	public ServerSection() {
	}


	public void init(IEditorSite site, IEditorInput input) {
		super.init(site, input);
		sunserver = SunAppServer.getSunAppServer(server);
		sunserver.addPropertyChangeListener(this);
	}


	/* (non-Javadoc)
	 * @see org.eclipse.wst.server.ui.editor.ServerEditorSection#dispose()
	 */
	@Override
	public void dispose() {
		sunserver.removePropertyChangeListener(this);
		super.dispose();
	}


	public void createSection(Composite parent) {
		super.createSection(parent);

		FormToolkit toolkit = getFormToolkit(parent.getDisplay());

		Section section = toolkit.createSection(parent,
				ExpandableComposite.TITLE_BAR
				| Section.DESCRIPTION
				| ExpandableComposite.TWISTIE
				| ExpandableComposite.EXPANDED
				| ExpandableComposite.FOCUS_TITLE);

		section.setText(Messages.wizardSectionTitle);
		/// String loc = sunserver.getRootDir();

		//SunAppServerBehaviour serverBehavior = (SunAppServerBehaviour) server.loadAdapter(ServerBehaviourDelegate.class, null);
		//     String loc =  serverBehavior.getSunApplicationServerInstallationDirectory();
		// this is not used, so comment it out until it is
		// should probably use formatters instead of concat as well
		// String loc="";
		//section.setDescription(Messages.wizardSectionDescription +" ("+ loc+")");
		section.setDescription(Messages.wizardSectionDescription);
		section.setLayoutData(new GridData(SWT.FILL, SWT.FILL, false, false));

		Composite comp = toolkit.createComposite(section);
		GridLayout gl = new GridLayout();
		gl.numColumns = 3;
		gl.verticalSpacing = 5;
		gl.marginWidth = 10;
		gl.marginHeight = 5;
		comp.setLayout(gl);
		comp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, false, false));
		section.setClient(comp);
		GridDataFactory txtGDF = GridDataFactory.fillDefaults().grab(true, false).span(2, 1).hint(50, SWT.DEFAULT);





		final Text domaindir = SWTUtil.createLabeledPath(Messages.DomainDirectory, sunserver.getDomainDir()+File.separator+sunserver.getdomainName(), comp, toolkit);
		txtGDF.align(SWT.FILL, SWT.CENTER).span(1, 1).applyTo(domaindir);
		domaindir.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {

				execute(new SunAppServerCommands(server, domaindir.getText(),SunAppServer.DOMAINPATH));

			}
		});
		domaindir.selectAll();
		domaindir.setFocus();
		createLabel(comp, Messages.AdminName, toolkit);

		username = toolkit.createText(comp, sunserver.getAdminName(), SWT.BORDER);
		txtGDF.align(SWT.FILL, SWT.CENTER).span(2, 1).applyTo(username);
		username.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				execute(new SunAppServerCommands(server, username.getText(),SunAppServer.ADMINNAME));
			}
		});

		createLabel(comp, Messages.AdminPassword, toolkit);

		password = toolkit.createText(comp,sunserver.getAdminPassword(), SWT.BORDER);
		password.setEchoChar('*');
		txtGDF.applyTo(password);
		password.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				execute(new SunAppServerCommands(server, password.getText(),SunAppServer.ADMINPASSWORD));
			}
		});

		createLabel(comp, Messages.ServerPortNumber, toolkit);

		serverPortNumber = toolkit.createText(comp, sunserver.getServerPort(), SWT.BORDER);

		txtGDF.applyTo(serverPortNumber);
		if (sunserver.isLocalServer()){
			serverPortNumber.setEditable(false);
			serverPortNumber.setEnabled(false);
		}
		serverPortNumber.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				execute(new SunAppServerCommands(server, serverPortNumber.getText(),SunAppServer.SERVERPORT));
			}
		});

		createLabel(comp, Messages.AdminServerPortNumber, toolkit);

		adminServerPortNumber = toolkit.createText(comp, sunserver.getAdminServerPort(), SWT.BORDER);
		txtGDF.applyTo(adminServerPortNumber);
		if (sunserver.isLocalServer()){
			adminServerPortNumber.setEditable(false);
			adminServerPortNumber.setEnabled(false);
		}
		adminServerPortNumber.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				execute(new SunAppServerCommands(server, adminServerPortNumber.getText(),SunAppServer.ADMINSERVERPORT));
			}
		});


		if (sunserver.isV3()) {
			final Button useAnonymousConnection = new Button(comp, SWT.CHECK);
			useAnonymousConnection.setText(Messages.UseAnonymousConnection);
			boolean useAnon=sunserver.getUseAnonymousConnections().equals("true");
			useAnonymousConnection.setSelection(useAnon);
			username.setEnabled(!useAnon);
			password.setEnabled(!useAnon);
			txtGDF.span(3, 1).indent(45, 0).applyTo(useAnonymousConnection);
			useAnonymousConnection.addSelectionListener(new org.eclipse.swt.events.SelectionAdapter() {
				public void widgetSelected(org.eclipse.swt.events.SelectionEvent e) {
					//Determines if the checkBox is checked or not
					boolean selected = useAnonymousConnection.getSelection();
					username.setEnabled(!selected);
					password.setEnabled(!selected);
					execute(new SunAppServerCommands(server, ""+selected,SunAppServer.USEANONYMOUSCONNECTIONS));
				}
			});

			final Button keepSessions = new Button(comp, SWT.CHECK);
			keepSessions.setText(Messages.keepSessions);
			keepSessions.setSelection(sunserver.getKeepSessions().equals("true"));
			txtGDF.applyTo(keepSessions);
			keepSessions.addSelectionListener(new org.eclipse.swt.events.SelectionAdapter() {
				public void widgetSelected(org.eclipse.swt.events.SelectionEvent e) {
					//Determines if the checkBox is checked or not
					boolean selected = keepSessions.getSelection();
					execute(new SunAppServerCommands(server, ""+selected,SunAppServer.KEEPSESSIONS));
				}
			});
			
			if (sunserver.isLocalServer()){
				final Button jarDeploy = new Button(comp, SWT.CHECK);
				jarDeploy.setText(Messages.jarDeploy);
				jarDeploy.setSelection(sunserver.getJarDeploy().equals("true"));
				txtGDF.applyTo(jarDeploy);
				jarDeploy.addSelectionListener(new org.eclipse.swt.events.SelectionAdapter() {
					public void widgetSelected(org.eclipse.swt.events.SelectionEvent e) {
						//Determines if the checkBox is checked or not
						boolean selected = jarDeploy.getSelection();
						execute(new SunAppServerCommands(server, ""+selected,SunAppServer.JARDEPLOY));
					}
				});
			}
		}
	}

	/* (non-Javadoc)
	 * @see org.eclipse.wst.server.ui.editor.ServerEditorSection#getErrorMessage()
	 */
	@Override
	public String getErrorMessage() {
		String domainValidationError = sunserver.validateDomainExists(sunserver.getDomainDir(), sunserver.getdomainName());
		if (domainValidationError != null) {
			return domainValidationError;
		}
		return super.getErrorMessage();
	}


	public IStatus[] getSaveStatus(){
		String domainValidationError = sunserver.validateDomainExists(sunserver.getDomainDir(), sunserver.getdomainName());
		if (domainValidationError != null) {
			IStatus[] i= new IStatus[1];
			i[0] = new Status(IStatus.ERROR,  "Glassfish",domainValidationError);
			return i;
		}
		
		return new IStatus[0];
	}
	protected Label createLabel(Composite parent, String text, FormToolkit toolkit) {
		Label label = toolkit.createLabel(parent, text);
		label.setForeground(toolkit.getColors().getColor(IFormColors.TITLE));
		label.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false));
		return label;
	}

	protected Spinner createSpinner(Composite parent, String text, FormToolkit toolkit) {
		Spinner autoPublishTime = new Spinner(parent, SWT.BORDER);
		autoPublishTime.setMinimum(0);
		autoPublishTime.setMaximum(120);
		autoPublishTime.setSelection(6789);
		GridData data = new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING);
		data.widthHint = 60;
		autoPublishTime.setLayoutData(data);



		return autoPublishTime;
	}


	// note that this is currently not working due to issue 140
	public void propertyChange(PropertyChangeEvent evt) {
		if (SunAppServer.DOMAINUPDATE == evt.getPropertyName()) {
			username.setText(sunserver.getAdminName());
			password.setText(sunserver.getAdminPassword());
			adminServerPortNumber.setText(sunserver.getAdminServerPort());
			serverPortNumber.setText(sunserver.getServerPort());
		}
	}



}