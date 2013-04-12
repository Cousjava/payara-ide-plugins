/*
 * Copyright (c) 2011 Oracle and/or its affiliates. All rights reserved.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Oracle
 */

package oracle.eclipse.runtime.glassfish;

import java.io.File;
import java.net.URL;

import oracle.eclipse.tools.glassfish.RuntimeConfigurator;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Platform;
import org.eclipse.wst.server.ui.internal.ServerUIPreferences;
import org.osgi.framework.Bundle;

@SuppressWarnings("restriction")
public class Startup implements org.eclipse.wst.server.core.internal.IStartup {

	public void startup() {
		ServerUIPreferences preferences = ServerUIPreferences.getInstance();
		boolean showOnActivity = preferences.getShowOnActivity();
		preferences.setShowOnActivity(false);
		Bundle bundle = //Activator.getDefault().getBundle();
				Platform.getBundle("oracle.eclipse.runtime.glassfish.build3122");
		try {
			URL url = FileLocator.resolve(bundle.getEntry("/"));

			File glassfishvLocation = new File(url.getFile(), "glassfish3");

		//	File glassfishv3Location = new File(
		//			new Path(Platform.getInstallLocation().getURL().getFile())
		//					.toPortableString() + "glassfish3");
			if (glassfishvLocation.exists()
					&& glassfishvLocation.isDirectory()) {

				new RuntimeConfigurator(glassfishvLocation,
						"org.glassfish.jst.server.glassfish31", "Internal GlassFish 3.1.2.2",
						"glassfish3122eclipsedefaultdomain")
						.configure();

			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		preferences.setShowOnActivity(showOnActivity);

	}

}
