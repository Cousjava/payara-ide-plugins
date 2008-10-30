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

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;

import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;

/**
 * Class used to initialize default preference values.
 * 
 * @author  Ludovic Champenois
 */
public class PreferenceInitializer extends AbstractPreferenceInitializer {

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer#initializeDefaultPreferences()
	 */
	public void initializeDefaultPreferences() {
		IPreferenceStore store = SunAppSrvPlugin.getInstance().getPreferenceStore();
		store.setDefault(PreferenceConstants.ENABLE_LOG, false);
		store.setDefault(PreferenceConstants.ENABLE_START_JAVADB, false);
		store.setDefault(PreferenceConstants.ENABLE_START_VERBOSE, false);
		store.setDefault(PreferenceConstants.ENABLE_COLORS_CONSOLE, true);
			//	store.setDefault(PreferenceConstants.P_CHOICE, "choice2");
	//	store.setDefault(PreferenceConstants.P_STRING,
		//		"Default value");
	}

}
