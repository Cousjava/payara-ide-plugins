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
package com.sun.enterprise.jst.server.sunappsrv.preferences;

import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;



/**
 * This class represents a preference page that
 * is contributed to the Preferences dialog. By 
 * subclassing <samp>FieldEditorPreferencePage</samp>, we
 * can use the field support built into JFace that allows
 * us to create a page that is small and knows how to 
 * save, restore and apply itself.
 * <p>
 * This page is used to modify preferences only. They
 * are stored in the preference store that belongs to
 * the main plug-in class. That way, preferences can
 * be accessed directly via the preference store.
 * @author  Ludovic Champenois
 */

public class GlassFishPreferencePage extends FieldEditorPreferencePage
	implements IWorkbenchPreferencePage {

	public GlassFishPreferencePage() {
		super(GRID);
		setPreferenceStore(SunAppSrvPlugin.getInstance().getPreferenceStore());
		setDescription("You can configure GlassFish Enterprise Server Plugin global preferences");
	}
	
	/**
	 * Creates the field editors. Field editors are abstractions of
	 * the common GUI blocks needed to manipulate various types
	 * of preferences. Each field editor knows how to save and
	 * restore itself.
	 */
	public void createFieldEditors() {
	//	addField(new DirectoryFieldEditor(PreferenceConstants.P_PATH, 
	//			"&Directory preference:", getFieldEditorParent()));
		
		addField(
				new BooleanFieldEditor(
					PreferenceConstants.ENABLE_LOG,
					"&Enable GlassFish Plugin Log information in IDE log file",
					getFieldEditorParent()));
		addField(
				new BooleanFieldEditor(
					PreferenceConstants.ENABLE_START_JAVADB,
					"&Start the JavaDB database process when Starting GlassFish Server",
					getFieldEditorParent()));
		addField(
				new BooleanFieldEditor(
					PreferenceConstants.ENABLE_START_VERBOSE,
					"&Start the GlassFish Enterprise Server in verbose mode (Eclipse console can be used)",
					getFieldEditorParent()));

	/*	addField(new RadioGroupFieldEditor(
				PreferenceConstants.P_CHOICE,
			"An example of a multiple-choice preference",
			1,
			new String[][] { { "&Choice 1", "choice1" }, {
				"C&hoice 2", "choice2" }
		}, getFieldEditorParent()));
		addField(
			new StringFieldEditor(PreferenceConstants.P_STRING, "A &text preference:", getFieldEditorParent()));
	*/
		}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
	 */
	public void init(IWorkbench workbench) {
	}
	
}
