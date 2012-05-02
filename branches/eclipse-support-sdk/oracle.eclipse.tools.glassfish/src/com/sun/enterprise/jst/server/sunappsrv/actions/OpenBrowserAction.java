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
import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;
import com.sun.enterprise.jst.server.sunappsrv.AdminURLHelper;

public class OpenBrowserAction extends AppServerContextAction {

	public OpenBrowserAction() {
		super("Open GlassFish Admin Console...",getImageDescriptorFromlocalImage("icons/obj16/glassfishserver.gif"));
	}

	@Override
	public void perform(IServer server) {
		
		if (accept(server)==false){
			showMessageDialog();
			return;
		}
		try {
			showPageInDefaultBrowser(AdminURLHelper.getURL("", server));
		} catch (Exception e) {
	           SunAppSrvPlugin.logMessage("Error opening browser: "+e.getMessage());

		}
	}

	@Override
	public boolean accept(IServer server) {
		return acceptIfServerRunning(server);
	}
}
