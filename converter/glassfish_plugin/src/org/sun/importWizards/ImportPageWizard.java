package org.sun.importWizards;


import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.preference.DirectoryFieldEditor;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

import org.sun.gf.nbecl.factory.DotProjectFactory;

public class ImportPageWizard extends WizardPage implements IWizardPage {
	private Label locationLabel;
	private Text locationVal;
	IPath path;
	
	public ImportPageWizard(String pageName) {
		super(pageName);
		setTitle("Import Glassfish Project");
		setDescription("Import a Glassfish project from Netbeans.");
	}

	public void createControl(Composite parent) {
		// Create basic layout for the page
		Composite composite = new Composite(parent, SWT.NONE);
		composite.setLayout(new GridLayout(2, false));
		setControl(composite);
		
		// Project Data
		locationLabel = new Label(composite, SWT.NONE);
		locationLabel.setText("Project &Location:");
		locationLabel.setLayoutData(new GridData());
		locationVal = new Text(composite, SWT.BORDER);
		locationVal.setText("RobotTemplate");
		locationVal.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		
		Composite fileSelectionArea = new Composite(parent, SWT.NONE);
		GridData fileSelectionData = new GridData(GridData.GRAB_HORIZONTAL
				| GridData.FILL_HORIZONTAL);
		fileSelectionArea.setLayoutData(fileSelectionData);

		GridLayout fileSelectionLayout = new GridLayout();
		fileSelectionLayout.numColumns = 3;
		fileSelectionLayout.makeColumnsEqualWidth = false;
		fileSelectionLayout.marginWidth = 0;
		fileSelectionLayout.marginHeight = 0;
		fileSelectionArea.setLayout(fileSelectionLayout);
		
		final DirectoryFieldEditor editor = new DirectoryFieldEditor("folderSelect","Select Folder: ", fileSelectionArea); //NON-NLS-1 //NON-NLS-2
		editor.getTextControl(fileSelectionArea).addModifyListener(new ModifyListener(){
			public void modifyText(ModifyEvent e) {
				path = new Path(editor.getStringValue());
				try {
					
					String projectName = org.sun.gf.nbecl.factory.DotProjectFactory.getProjectName(path.toString());
					//canFinish = true;
				} catch (Exception e1) {
					//canFinish = false;
					// TODO Auto-generated catch block
					e1.printStackTrace();
				} 
			}
		});
		fileSelectionArea.moveAbove(null);

	}

	public String getLocation() {
		return locationVal.getText();
	}
	
	public boolean canFlipToNextPage(){
		return false;
	}
	
	public boolean canFinish(){
		return locationVal.getText().length() > 0;
	}

}
