// <editor-fold defaultstate="collapsed" desc="CDDL+GPL License">
/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2009 Sun Microsystems, Inc. All rights reserved.
 *
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common Development
 * and Distribution License("CDDL") (collectively, the "License").  You
 * may not use this file except in compliance with the License. You can obtain
 * a copy of the License at https://glassfish.dev.java.net/public/CDDL+GPL.html
 * or glassfish/bootstrap/legal/LICENSE.txt.  See the License for the specific
 * language governing permissions and limitations under the License.
 *
 * When distributing the software, include this License Header Notice in each
 * file and include the License file at glassfish/bootstrap/legal/LICENSE.txt.
 * Sun designates this particular file as subject to the "Classpath" exception
 * as provided by Sun in the GPL Version 2 section of the License file that
 * accompanied this code.  If applicable, add the following below the License
 * Header, with the fields enclosed by brackets [] replaced by your own
 * identifying information: "Portions Copyrighted [year]
 * [name of copyright owner]"
 *
 * Contributor(s):
 *
 * If you wish your version of this file to be governed by only the CDDL or
 * only the GPL Version 2, indicate your decision by adding "[Contributor]
 * elects to include this software in this distribution under the [CDDL or GPL
 * Version 2] license."  If you don't indicate a single choice of license, a
 * recipient has the option to distribute your version of this file under
 * either the CDDL, the GPL Version 2 or to extend the choice of license to
 * its licensees as provided above.  However, if you add GPL Version 2 code
 * and therefore, elected the GPL Version 2 license, then the option applies
 * only if the new code is made subject to such option by the copyright
 * holder.
 */
// </editor-fold>

package com.sun.enterprise.jst.server.sunappsrv.sunresource.wizards;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jem.util.emf.workbench.ProjectUtilities;
import org.eclipse.jface.dialogs.IDialogPage;
import org.eclipse.jface.layout.GridDataFactory;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.wst.common.project.facet.core.FacetedProjectFramework;

import com.sun.enterprise.jst.server.sunappsrv.sunresource.JMSInfo;

/**
 * @author Nitya Doraisamy
 */

public class JMSResourceWizardPage extends WizardPage {
	
	private Text jndiText;
	private Button queueRButton;
	private Button topicRButton;
	private Button queueConnectionRButton;
	private Button topicConnectionRButton;
	private Button connectionRButton;
		
	private IProject selectedProject;
	private JMSInfo jmsInfo;

	private Combo projectNameCombo;

	private List<String> resources = new ArrayList<String>();
	private String defaultJndiName = "jms/myQueue"; //$NON-NLS-1$
	
	/**
	 * Constructor for JMSResourceWizardPage.
	 *
	 * @param selection
	 */
	public JMSResourceWizardPage(IProject project) {
		super("wizardPage"); //$NON-NLS-1$
		setTitle(Messages.jmsWizardTitle);
		setDescription(Messages.jmsWizardDescription);
		selectedProject = project;
	}

	/**
	 * @see IDialogPage#createControl(Composite)
	 */
	public void createControl(Composite parent) {
		Composite container = new Composite(parent, SWT.NULL);
		GridLayout layout = new GridLayout();
		layout.numColumns = 3;
		container.setLayout(layout);
		Label label = new Label(container, SWT.NULL);
		label.setText(Messages.ProjectName);

		projectNameCombo = new Combo(container, SWT.READ_ONLY | SWT.SINGLE);
		GridDataFactory.defaultsFor(projectNameCombo).span(2, 1).applyTo(projectNameCombo);
		projectNameCombo.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetDefaultSelected( SelectionEvent e) {
				widgetSelected(e);
			}

			@Override
			public void widgetSelected( SelectionEvent e) {
				String newSelection = projectNameCombo.getText();
				if (newSelection != null) {
					selectedProject = ProjectUtilities.getProject(newSelection);
                                        resources = ResourceUtils.getResources(ResourceUtils.TYPE_JDBC, selectedProject);
					dialogChanged();
				}
			}
		});

		label = new Label(container, SWT.NULL);
		label.setText(Messages.JNDIName);
				
		jndiText = new Text(container, SWT.BORDER | SWT.SINGLE);
		GridDataFactory.defaultsFor(jndiText).span(2, 1).applyTo(jndiText);
		jndiText.setText(defaultJndiName); 
		jndiText.addModifyListener(new ModifyListener() {
            public void modifyText(ModifyEvent e) {
                dialogChanged();
            }
        });

		//Dummy Label for spacing
		label = new Label(container, SWT.NULL);
		GridDataFactory.defaultsFor(label).span(3, 1).applyTo(label);
		
		label = new Label(container, SWT.NULL);
		label.setText(Messages.lblChooseType);
		GridDataFactory.defaultsFor(label).span(3, 1).applyTo(label);
		
		//Dummy Label for spacing
		label = new Label(container, SWT.NULL);
		GridDataFactory.defaultsFor(label).span(3, 1).applyTo(label);
		
		label = new Label(container, SWT.NULL);
		label.setText(Messages.lblAdminObject);
		GridDataFactory.defaultsFor(label).span(3, 1).applyTo(label);		
		
		GridData gridData = new GridData();
		gridData.horizontalIndent = 40;
		gridData.horizontalSpan = 3;
		gridData.horizontalAlignment = GridData.FILL;
 				
		queueRButton = new Button(container, SWT.RADIO);
		queueRButton.setText(Messages.lblQueue);
		queueRButton.setLayoutData(gridData);
		queueRButton.setSelection(true);
				
		topicRButton = new Button(container, SWT.RADIO);
		topicRButton.setText(Messages.lblTopic);
		topicRButton.setLayoutData(gridData);
		
		//Dummy Label for spacing
		label = new Label(container, SWT.NULL);
		GridDataFactory.defaultsFor(label).span(3, 1).applyTo(label);
		
		label = new Label(container, SWT.NULL);
		label.setText(Messages.lblConnector);
		GridDataFactory.defaultsFor(label).span(3, 1).applyTo(label);
		
		queueConnectionRButton = new Button(container, SWT.RADIO);
		queueConnectionRButton.setText(Messages.lblQueueConnectionFactory);
		queueConnectionRButton.setLayoutData(gridData);
				
		topicConnectionRButton = new Button(container, SWT.RADIO);
		topicConnectionRButton.setText(Messages.lblTopicConnectionFactory);
		topicConnectionRButton.setLayoutData(gridData);
				
		connectionRButton = new Button(container, SWT.RADIO);
		connectionRButton.setText(Messages.lblConnectionFactory);
		connectionRButton.setLayoutData(gridData);
		
		initialize();
		dialogChanged();
		setControl(container);
	}

	private void initialize() {
		resources = ResourceUtils.getResources(ResourceUtils.TYPE_CONNECTOR, selectedProject);
		if(resources.contains(defaultJndiName)){
			String jndiName = ResourceUtils.getUniqueResourceName(defaultJndiName, resources);
			jndiText.setText(jndiName);
		}
		populateCombos();
		dialogChanged();
	}

	public String getJNDIName() {
		return jndiText.getText();
	}

	public String getResourceType(){
		String resourceType = null;
		if(queueRButton.getSelection()){
			resourceType = JMSInfo.QUEUE;
		}else if(topicRButton.getSelection()){
			resourceType = JMSInfo.TOPIC;
		}else if(queueConnectionRButton.getSelection()){
			resourceType = JMSInfo.QUEUE_CONNECTION;
		}else if(topicConnectionRButton.getSelection()){
			resourceType = JMSInfo.TOPIC_CONNECTION;
		}else if(connectionRButton.getSelection()){
			resourceType = JMSInfo.CONNECTION_FACTORY;
		}
		return resourceType;
	}

	public IProject getSelectedProject() {
		return selectedProject;
	}

	private void dialogChanged() {
		setPageComplete(false);
		boolean hasProject = (projectNameCombo.getSelectionIndex() != -1);
		if (!hasProject) {
			setErrorMessage(Messages.errorProjectMissing);
			return;
		}
		String jndiName = getJNDIName();
		if (jndiName.length() == 0 ) {
			setErrorMessage(Messages.errorJndiNameMissing);
			return;
		} else {
			if(ResourceUtils.isDuplicate(jndiName, resources)) {
				setErrorMessage(NLS.bind(Messages.errorDuplicateName, jndiName));
				return;
			}
		}
		
		if ((getResourceType() == null) || (getResourceType().length() == 0 )) {
			setErrorMessage(Messages.errorResourceTypeMissing);
			return;
		}
		setPageComplete(true);
		setErrorMessage(null);
	}

	private List<IProject> getSunFacetIProjects() {
		IProject[] allProjects = ProjectUtilities.getAllProjects();
		List<IProject> returnProjects = new ArrayList<IProject>();

		for (IProject project2 : allProjects) {
			try {
				if (FacetedProjectFramework.hasProjectFacet(project2, "sun.facet")) { //$NON-NLS-1$
					returnProjects.add(project2);
				}
			} catch (CoreException e) {
				// just skip from list
			}
		}
		return returnProjects;
	}


	private void populateCombos() {
		projectNameCombo.removeAll();
		List<IProject> validProjects = getSunFacetIProjects();
		String selectProjectName = ((selectedProject != null) ? selectedProject.getName() : null);
		int selectionIndex = -1;
		for (int i = 0; i < validProjects.size(); i++) {
			IProject nextProject = validProjects.get(i);
			String projectName = nextProject.getName();
			projectNameCombo.add(projectName);
			if (projectName.equals(selectProjectName)) {
				selectionIndex = i;
			}
		}
		if ((selectionIndex != -1) && (projectNameCombo.getItemCount() > 0)) {
			projectNameCombo.select(selectionIndex);
		} else { // selectedProject is not a valid candidate project
			selectedProject = null;
		}
	}

	public JMSInfo getJMSInfo() {
		jmsInfo = new JMSInfo();
		jmsInfo.setJndiName(getJNDIName());
		jmsInfo.setResourceType(getResourceType());
		return jmsInfo;
	}

}