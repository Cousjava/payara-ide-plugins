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



import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Button;
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
        gl.numColumns = 2;
        gl.verticalSpacing = 5;
        gl.marginWidth = 10;
        gl.horizontalSpacing = 15;
        gl.marginHeight = 5;
        comp.setLayout(gl);
        comp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, false, false));
        section.setClient(comp);
        
        createLabel(comp, Messages.DomainName, toolkit);
        
        final Text domainname = toolkit.createText(comp, sunserver.getdomainName(), SWT.BORDER);
        domainname.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
        domainname.addModifyListener(new ModifyListener() {
            public void modifyText(ModifyEvent e) {
                execute(new SunAppServerCommands(server, domainname.getText(),SunAppServer.DOMAINNAME));
            }
        });
        
        createLabel(comp, Messages.DomainDirectory, toolkit);
        final Text domaindir = toolkit.createText(comp, sunserver.getDomainDir(), SWT.BORDER);
        domaindir.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
        // TODO - figure out how to allow scrolling to see this whole value and copy/paste
        // according to docs, setEditabled(false) is supposed to do that, but does not (on Mac)
        // setEnabled(false) grays it out, but does not accomplish that either
        domaindir.setEditable(false);
        domaindir.setEnabled(false);
 
        createLabel(comp, Messages.AdminName, toolkit);
        
        final Text username = toolkit.createText(comp, sunserver.getAdminName(), SWT.BORDER);
        username.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
        username.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
                execute(new SunAppServerCommands(server, username.getText(),SunAppServer.ADMINNAME));
            }
        });
        
        createLabel(comp, Messages.AdminPassword, toolkit);
        
        final Text password = toolkit.createText(comp,sunserver.getAdminPassword(), SWT.BORDER);
        password.setEchoChar('*');
        password.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false));
        password.addModifyListener(new ModifyListener() {
            public void modifyText(ModifyEvent e) {
                execute(new SunAppServerCommands(server, password.getText(),SunAppServer.ADMINPASSWORD));
            }
        });
        
       createLabel(comp, Messages.ServerPortNumber, toolkit);
        
        final Text serverPortNumber = toolkit.createText(comp, sunserver.getServerPort(), SWT.BORDER);
        
        serverPortNumber.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
        serverPortNumber.setEditable(false);
        serverPortNumber.setEnabled(false);

        
        createLabel(comp, Messages.AdminServerPortNumber, toolkit);
        
        final Text adminServerPortNumber = toolkit.createText(comp, sunserver.getAdminServerPort(), SWT.BORDER);
        adminServerPortNumber.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false));
        adminServerPortNumber.setEditable(false);
        adminServerPortNumber.setEnabled(false);

        

        if (serverBehavior.isV3()) {
            createLabel(comp, "   ", toolkit);
	        final Button useAnonymousConnection = new Button(comp, SWT.CHECK);
	        useAnonymousConnection.setText(Messages.UseAnonymousConnection);
	        useAnonymousConnection.setSelection(sunserver.getUseAnonymousConnections().equals("true"));
	        useAnonymousConnection.setLayoutData(new GridData(SWT.FILL, SWT.RIGHT, false, false));
	        useAnonymousConnection.addSelectionListener(new org.eclipse.swt.events.SelectionAdapter() {
	    	   public void widgetSelected(org.eclipse.swt.events.SelectionEvent e) {
	    	   //Determines if the checkBox is checked or not
	    	   boolean selected = useAnonymousConnection.getSelection();
	           execute(new SunAppServerCommands(server, ""+selected,SunAppServer.USEANONYMOUSCONNECTIONS));
	    	   }
	    	});

	        createLabel(comp, "   ", toolkit);
	        final Button keepSessions = new Button(comp, SWT.CHECK);
	        keepSessions.setText(Messages.keepSessions);
	        keepSessions.setSelection(sunserver.getKeepSessions().equals("true"));
	        keepSessions.setLayoutData(new GridData(SWT.FILL, SWT.RIGHT, false, false));
	        keepSessions.addSelectionListener(new org.eclipse.swt.events.SelectionAdapter() {
	    	   public void widgetSelected(org.eclipse.swt.events.SelectionEvent e) {
	    	   //Determines if the checkBox is checked or not
	    	   boolean selected = keepSessions.getSelection();
	           execute(new SunAppServerCommands(server, ""+selected,SunAppServer.KEEPSESSIONS));
	    	   }
	    	});
        }
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