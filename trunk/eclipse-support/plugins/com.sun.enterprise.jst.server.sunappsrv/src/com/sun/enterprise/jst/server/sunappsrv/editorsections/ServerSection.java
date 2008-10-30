// <editor-fold defaultstate="collapsed" desc="CDDL Licence">
/*
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * glassfishplugins/www/license/CDDLv1.0.txt or
 * https://glassfish.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * glassfishplugins/www/license/CDDLv1.0.txt.  If applicable,
 * add the following below this CDDL HEADER, with the
 * fields enclosed by brackets "[]" replaced with your
 * own identifying information: Portions Copyright [yyyy]
 * [name of copyright owner]
 */
// </editor-fold>

package com.sun.enterprise.jst.server.sunappsrv.editorsections;



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
import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;



/**
 *
 * @author ludo
 */
public class ServerSection extends ServerEditorSection {


	SunAppServer sunserver;

	/**
	 *
	 */
	public ServerSection() {
	}


	public void init(IEditorSite site, IEditorInput input) {
		super.init(site, input);
		sunserver = SunAppServer.getSunAppServer(server);
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

		SunAppServerBehaviour serverBehavior = (SunAppServerBehaviour) server.loadAdapter(ServerBehaviourDelegate.class, null);
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

		createLabel(comp, Messages.DomainName, toolkit);

		final Text domainname = toolkit.createText(comp, sunserver.getdomainName(), SWT.BORDER);
		txtGDF.applyTo(domainname);
		domainname.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {

				execute(new SunAppServerCommands(server, domainname.getText(),SunAppServer.DOMAINNAME));


			}
		});

		final Text domaindir = SWTUtil.createLabeledPath(Messages.DomainDirectory, sunserver.getDomainDir(), comp, toolkit);
		txtGDF.align(SWT.FILL, SWT.CENTER).span(1, 1).applyTo(domaindir);
		domaindir.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {

				execute(new SunAppServerCommands(server, domaindir.getText(),SunAppServer.DOMAINDIR));

			}
		});

		createLabel(comp, Messages.AdminName, toolkit);

		final Text username = toolkit.createText(comp, sunserver.getAdminName(), SWT.BORDER);
		txtGDF.align(SWT.FILL, SWT.CENTER).span(2, 1).applyTo(username);
		username.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				execute(new SunAppServerCommands(server, username.getText(),SunAppServer.ADMINNAME));
			}
		});

		createLabel(comp, Messages.AdminPassword, toolkit);

		final Text password = toolkit.createText(comp,sunserver.getAdminPassword(), SWT.BORDER);
		password.setEchoChar('*');
		txtGDF.applyTo(password);
		password.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				execute(new SunAppServerCommands(server, password.getText(),SunAppServer.ADMINPASSWORD));
			}
		});

		createLabel(comp, Messages.ServerPortNumber, toolkit);

		final Text serverPortNumber = toolkit.createText(comp, sunserver.getServerPort(), SWT.BORDER);

		txtGDF.applyTo(serverPortNumber);
		serverPortNumber.setEditable(false);
		serverPortNumber.setEnabled(false);


		createLabel(comp, Messages.AdminServerPortNumber, toolkit);

		final Text adminServerPortNumber = toolkit.createText(comp, sunserver.getAdminServerPort(), SWT.BORDER);
		txtGDF.applyTo(adminServerPortNumber);
		adminServerPortNumber.setEditable(false);
		adminServerPortNumber.setEnabled(false);



		if (serverBehavior.isV3()) {
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
		}
	}
	public IStatus[] getSaveStatus(){
		File f= new File(sunserver.getDomainDir()+File.separator+sunserver.getdomainName());
		if (!f.exists()){
			IStatus[] i= new IStatus[1];
			i[0] = new Status(IStatus.ERROR,  "Glassfish",f.getAbsolutePath()+" does not exist" );
			return i;
		}
		if (!f.isDirectory()){
			IStatus[] i= new IStatus[1];
			i[0] = new Status(IStatus.ERROR,  "Glassfish",f.getAbsolutePath()+" is not a directory" );
			return i;
		}
		if (!f.canWrite()){
			IStatus[] i= new IStatus[1];
			i[0] = new Status(IStatus.ERROR,  "Glassfish",f.getAbsolutePath()+" is not writable" );
			return i;
		}
		File domain= new File(f,"config/domain.xml");
		if (!domain.exists()){
			IStatus[] i= new IStatus[1];
			i[0] = new Status(IStatus.ERROR,  "Glassfish",domain.getAbsolutePath()+" is not a valid domain (no domain.xml)" );
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



}