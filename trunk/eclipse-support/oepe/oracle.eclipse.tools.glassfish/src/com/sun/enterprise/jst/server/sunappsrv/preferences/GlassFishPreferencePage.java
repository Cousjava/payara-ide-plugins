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
