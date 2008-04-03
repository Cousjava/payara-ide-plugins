/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 * 
 * Copyright 1997-2008 Sun Microsystems, Inc. All rights reserved.
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

package org.glassfish.tools.eclipse;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle
 * 
 * @author Ludovic.Champenois@Sun.COM
 */
public class Activator extends AbstractUIPlugin implements BundleActivator {

	// CHANGE ME
	private final static String LOC = "/Users/ludo/glassfish-10.0-SNAPSHOT/glassfish";

	// The plug-in ID
	public static final String PLUGIN_ID = "Eclipse GlassFish V3 Embedded";
	// The shared instance
	private static Activator plugin;
	Bundle gfMgmtBundle;
	private final static String DEFAULT_DOMAINS_DIR_PROPNAME = "AS_DEF_DOMAINS_PATH";
	private final static String INSTANCE_ROOT_PROP_NAME = "com.sun.aas.instanceRoot";
	private final static String INSTALL_ROOT_PROP_NAME = "com.sun.aas.installRoot";

	/*
	 * message shown for debugging purposed in the action
	 * 
	 */
	public static String message = "";

	/*
	 * The constructor
	 */

	public Activator() {
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext)
	 */
	public void start(BundleContext context) throws Exception {
		super.start(context);
		plugin = this;
		String domainDir = LOC + "/domains/domain1";

		System.setProperty(INSTALL_ROOT_PROP_NAME, LOC);
		System.setProperty(INSTANCE_ROOT_PROP_NAME, domainDir);
		System.setProperty(DEFAULT_DOMAINS_DIR_PROPNAME, LOC + "/domains");
		System.setProperty(INSTANCE_ROOT_PROP_NAME, LOC);
		System.setProperty("com.sun.aas.installRootURI", "file://" + domainDir);
		System.setProperty("org.jvnet.hk2.osgiadapter.contextrootdir", LOC
				+ "/modules");
		String aa = System.getProperty("org.osgi.framework.system.packages");
		logMessage("Setting system properties");
		System.setProperty("org.osgi.framework.system.packages", "sun.misc;"
				+ aa);

		String[] hk2BundleLocations = {
				"file://" + LOC + "/modules/auto-depends-0.2-SNAPSHOT.jar",
				"file://" + LOC + "/modules/config-0.2-SNAPSHOT.jar",
				"file://" + LOC + "/modules/hk2-core-0.2-SNAPSHOT.jar",
				// "file://"+LOC+"/modules/hk2-0.2-SNAPSHOT.jar",
				"file://" + LOC + "/modules/osgi-adapter-0.2-SNAPSHOT.jar", };
		
		Bundle b = null;

		for (String l : hk2BundleLocations) {
			b = context.installBundle(l);
			// b.start();

			logMessage("install Bundle =" + l + "\n");
		}
		gfMgmtBundle = b; // b is osgi-adapter since this was the last one in
							// our array
		gfMgmtBundle.start();
		logMessage("gfMgmtBundle started =" + b);

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext)
	 */
	public void stop(BundleContext context) throws Exception {
		plugin = null;
		super.stop(context);
		gfMgmtBundle.stop(); // this will stop every module that was started
								// as part of GlassFish
	}

	/**
	 * Returns the shared instance
	 * 
	 * @return the shared instance
	 */
	public static Activator getDefault() {
		return plugin;
	}

	/**
	 * Returns an image descriptor for the image file at the given plug-in
	 * relative path
	 * 
	 * @param path
	 *            the path
	 * @return the image descriptor
	 */
	public static ImageDescriptor getImageDescriptor(String path) {
		return imageDescriptorFromPlugin(PLUGIN_ID, path);
	}

	public static void logMessage(String mess) {
		final Status status = new Status(IStatus.INFO, PLUGIN_ID, 1,
				"GlassFish: " + mess, null);
		getDefault().getLog().log(status);
		message += mess;
	}

}
