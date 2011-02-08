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

import org.eclipse.ui.IStartup;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

public class Activator  extends AbstractUIPlugin implements IStartup {

	private static BundleContext context;
    // The shared instance
    private static Activator plugin;
	static BundleContext getContext() {
		return context;
	}

	/*
	 * (non-Javadoc)
	 * @see org.osgi.framework.BundleActivator#start(org.osgi.framework.BundleContext)
	 */

    public void start(BundleContext context) throws Exception {
        super.start(context);
       Activator.context = context;
       Activator.plugin = this;
    }
	/*
	 * (non-Javadoc)
	 * @see org.osgi.framework.BundleActivator#stop(org.osgi.framework.BundleContext)
	 */
	public void stop(BundleContext bundleContext) throws Exception {
		Activator.context = null;
	       Activator.plugin = null;
	}
    public static Activator getDefault() {
        return plugin;
    }

	@Override
	public void earlyStartup() {
		// TODO Auto-generated method stub
		
	}
}
