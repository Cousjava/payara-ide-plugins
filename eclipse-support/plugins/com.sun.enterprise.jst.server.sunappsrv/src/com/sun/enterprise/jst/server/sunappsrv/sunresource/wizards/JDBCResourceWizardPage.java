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

package com.sun.enterprise.jst.server.sunappsrv.sunresource.wizards;

import org.eclipse.datatools.connectivity.ICategory;
import org.eclipse.datatools.connectivity.IConnectionProfile;
import org.eclipse.datatools.connectivity.IProfileListener;
import org.eclipse.datatools.connectivity.ProfileManager;
import org.eclipse.datatools.connectivity.internal.ConnectionProfileManager;
import org.eclipse.datatools.connectivity.internal.ui.wizards.CPWizardNode;
import org.eclipse.datatools.connectivity.internal.ui.wizards.NewCPWizard;
import org.eclipse.datatools.connectivity.internal.ui.wizards.ProfileWizardProvider;
import org.eclipse.datatools.connectivity.ui.wizards.IWizardCategoryProvider;
import org.eclipse.jface.dialogs.IDialogPage;
import org.eclipse.jface.layout.GridDataFactory;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.sun.enterprise.jst.server.sunappsrv.sunresource.JDBCInfo;

/**
 * 
 */

public class JDBCResourceWizardPage extends WizardPage {
	private static final String DATABASE_CATEGORY_ID = "org.eclipse.datatools.connectivity.db.category"; //$NON-NLS-1$

	private Text jndiText;
	
	private IConnectionProfile connectionProfile;

	private Combo combo;

	private NewCPWizard wizard;

	private WizardDialog wizardDialog;

	/**
	 * Constructor for JDBCResourceWizardPage.
	 * 
	 * @param selection
	 */
	public JDBCResourceWizardPage() {
		super("wizardPage"); //$NON-NLS-1$
		setTitle(Messages.wizardTitle);
		setDescription(Messages.wizardDescription);
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
		label.setText(Messages.JNDIName);

		jndiText = new Text(container, SWT.BORDER | SWT.SINGLE);
		GridDataFactory.defaultsFor(jndiText).span(2, 1).applyTo(jndiText);
		jndiText.setText("jdbc/myDatasource");

		label = new Label(container, SWT.NULL);
		label.setText(Messages.Connection);

		combo = new Combo(container, SWT.READ_ONLY | SWT.SINGLE);
		combo.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetDefaultSelected( SelectionEvent e) {
				widgetSelected(e);
			}

			@Override
			public void widgetSelected( SelectionEvent e) {
				String newSelection = combo.getText();
				if (newSelection != null) {
					connectionProfile = ProfileManager.getInstance().
						getProfileByName(newSelection);
				}
			}
		});
		GridDataFactory.defaultsFor(combo).align(SWT.FILL, SWT.CENTER).applyTo(combo);

		Button button = new Button(container, SWT.PUSH);
		button.setText(Messages.Create);
		button.addSelectionListener(new SelectionListener() {
			public void widgetDefaultSelected(SelectionEvent e) {
				// do nothing
			}
		
			public void widgetSelected(SelectionEvent e) {
				connectionProfile = showCPWizard();
				if (connectionProfile != null) {
					String newName = connectionProfile.getName();

					combo.add(newName);
					combo.select(combo.indexOf(newName));
					updateStatus();
					combo.pack();
				}
			}
		});
		GridDataFactory.defaultsFor(button).align(SWT.END, SWT.CENTER).applyTo(button);

		initialize();
		setControl(container);
	}

	private void initialize() {
		populateCombo();
		updateStatus();
	}

	private void updateStatus() {
		boolean hasConnection = (combo.getSelectionIndex() != -1);
		setPageComplete(hasConnection);
		setErrorMessage(hasConnection ? null: "need to specify connection");
		
	}

	public String getJNDIName() {
		return jndiText.getText();
	}

	private IConnectionProfile showCPWizard () {
		// Filter datasource category
	  	ViewerFilter viewerFilter = new ViewerFilter() {
	
			@Override
			public boolean select( Viewer viewer, Object parentElement, Object element) {
				
				CPWizardNode wizardNode = ( CPWizardNode) element;
				if( !( wizardNode.getProvider() instanceof IWizardCategoryProvider)) {
					ICategory cat = ConnectionProfileManager.getInstance().getProvider(
									(( ProfileWizardProvider) wizardNode.getProvider()).getProfile()).getCategory();
					
					// Only display wizards belong to database category
					while( cat != null) {
						if( cat.getId().equals(DATABASE_CATEGORY_ID))
							return true;
						cat = cat.getParent();
					}
				}
				return false;
			}
		};
		wizard = new NewCPWizard( viewerFilter, null);
		Shell currentShell = Display.getCurrent().getActiveShell();
		wizardDialog = new WizardDialog( currentShell, wizard);
		wizardDialog.setBlockOnOpen( true);
		
		ProfileManager profileManager = ProfileManager.getInstance();
		ProfileListener listener = new ProfileListener();
		profileManager.addProfileListener(listener);
		int result = wizardDialog.open();
		profileManager.removeProfileListener(listener);
		
		return ((result != Window.CANCEL) ? listener.newProfile : null);
	}

	private IConnectionProfile[] getConnectionProfiles () {
		return ProfileManager.getInstance().getProfilesByCategory(DATABASE_CATEGORY_ID);
	}

	private void populateCombo() {
		combo.removeAll();
		for (IConnectionProfile profile : getConnectionProfiles()) {
			if (connectionProfile == null) {
				connectionProfile = profile;
			}
			combo.add(profile.getName());
		}

		if (combo.getItemCount() > 0) {
			combo.select(0);
		}
	}

	JDBCInfo getJDBCInfo() {
		return new JDBCInfo(connectionProfile);
	}

	static class ProfileListener implements IProfileListener {
		IConnectionProfile newProfile;
		
		public void profileAdded( IConnectionProfile profile) {
			newProfile = profile;
		}
	
		public void profileChanged( IConnectionProfile profile) {
			// do nothing
		}
	
		public void profileDeleted( IConnectionProfile profile) {
			// do nothing
		}
	}
}