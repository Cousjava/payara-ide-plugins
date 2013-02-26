package com.sun.enterprise.jst.server.sunappsrv.handlers;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.wst.server.core.IServer;

import com.sun.enterprise.jst.server.sunappsrv.AdminURLHelper;
import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;

public class ViewAdminConsoleHandler extends AbstractGlassfishSelectionHandler {

	@Override
	public void processSelection(IStructuredSelection selection) {
		IServer server = (IServer) selection.getFirstElement();
		try {
			showPageInDefaultBrowser(AdminURLHelper.getURL("", server));
		} catch (Exception e) {
	           SunAppSrvPlugin.logMessage("Error opening browser: "+e.getMessage());

		}
	}

}
