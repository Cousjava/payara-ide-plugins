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


package com.sun.enterprise.jst.server.sunappsrv.actions;

import org.eclipse.wst.server.core.IServer;

/**
 *
 * @author Ludovic.Champenois@Sun.COM
 */
public class PreludeRegistrationAction extends OpenBrowserEditorAction  {

    /**
     * The constructor.
     */
    public PreludeRegistrationAction() {
    	super ("Register your GlassFish Enterprise Server...",getImageDescriptorFromlocalImage("icons/obj16/registration.png"));
    }

	@Override
 	public void perform (IServer server) {
 		// for now, do registration through url whether v2 or v3
 		// once registration plugin is ready, consider using it (eclipse gui) instead
 		// if we do, we will need a plugin dependency on that module or to do some refactoring here
 		// if we do not, and continue to use code which is called v3 for both v2 & v3, 
 		// we should think about renaming this action and moving Register class out of v3 package
      	     	
		if (accept(server)==false){
			showMessageDialog();
			return;
		}
		showPageInBrowser(server);
    }

	protected String getEditorClassName() { return "com.sun.enterprise.jst.server.sunappsrv.v3.Register"; }

 	protected String getIconName() { return "icons/obj16/sunappsrvs.gif"; }

 	protected String getURL() { return new com.sun.enterprise.jst.server.sunappsrv.v3.Register().getURL(); }
 
	@Override
	public boolean accept(IServer server) {
		return acceptIfServerRunning(server);
	}
}
