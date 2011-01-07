/*
 * Copyright (c) 2011 Oracle and/or its affiliates. All rights reserved.
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

package com.sun.enterprise.jst.server.sunappsrv.installation;

import java.io.File;

import org.eclipse.core.runtime.CoreException;

import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.wst.server.ui.internal.ServerUIPreferences;

@SuppressWarnings("restriction")
public class Startup implements org.eclipse.wst.server.core.internal.IStartup {

	@Override
	public void startup() {
		ServerUIPreferences preferences = ServerUIPreferences.getInstance();
		boolean showOnActivity = preferences.getShowOnActivity();
		preferences.setShowOnActivity(false);
		// progressMonitor.setTaskName(Messages.CreatingGlassfishv3Configuration);
		// progressMonitor.worked(10);

		File glassfishv3Location = new File(new Path(Platform
				.getInstallLocation().getURL().getFile()).toPortableString()
				+ "glassfish3");
		if (glassfishv3Location.exists() && glassfishv3Location.isDirectory()) {

			try {
				new V3Configurator(glassfishv3Location,
						"org.glassfish.jst.server.glassfish31", "glassfish3")
						.configure();
			} catch (CoreException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		preferences.setShowOnActivity(showOnActivity);

	}

}
