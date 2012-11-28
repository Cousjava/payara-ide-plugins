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

import java.io.File;

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;

import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;
import com.sun.enterprise.jst.server.sunappsrv.derby.DerbyConfigurator;

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
		String loc = DerbyConfigurator.getSampleDBLocation();

		store.setDefault(PreferenceConstants.JAVA_DB_LOCATION, loc);
		File f = new File(loc);
		if (!f.exists()){
			f.mkdirs();
		}
	}

}
