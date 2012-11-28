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

package com.sun.enterprise.jst.server.sunappsrv.preferences;

import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.DirectoryFieldEditor;
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

		addField(
				new BooleanFieldEditor(
					PreferenceConstants.ENABLE_COLORS_CONSOLE,
					"Enable colored rendering in the GlassFish Log Viewer",
					getFieldEditorParent()));

		addField(
				new DirectoryFieldEditor(
					PreferenceConstants.JAVA_DB_LOCATION,
					"Java DB Database Location",
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
