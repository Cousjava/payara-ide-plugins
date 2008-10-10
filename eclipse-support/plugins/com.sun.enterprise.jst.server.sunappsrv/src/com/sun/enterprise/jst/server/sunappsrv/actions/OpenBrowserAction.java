package com.sun.enterprise.jst.server.sunappsrv.actions;

import java.net.URL;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.browser.IWebBrowser;
import org.eclipse.ui.browser.IWorkbenchBrowserSupport;
import org.eclipse.wst.server.ui.internal.ServerUIPlugin;

import com.sun.enterprise.jst.server.sunappsrv.SunAppServerBehaviour;
import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;

public class OpenBrowserAction extends AppServerContextAction implements IObjectActionDelegate {

	public OpenBrowserAction() {
		super();
	}


	public void dispose() {
		// do nothing
	}


	public void init(IWorkbenchWindow window) {
		// do nothing
	}

	public void run(IAction action) {
		//super.run(action);
		try {
	           SunAppSrvPlugin.logMessage(" opening browser: "+selectedServer);
	        SunAppServerBehaviour sab = (SunAppServerBehaviour) selectedServer.loadAdapter(
	                SunAppServerBehaviour.class, null);
	           SunAppSrvPlugin.logMessage(" opening browser: "+sab);
			
			
			IWorkbenchBrowserSupport browserSupport = ServerUIPlugin.getInstance().getWorkbench().getBrowserSupport();
			IWebBrowser browser = browserSupport.createBrowser(IWorkbenchBrowserSupport.LOCATION_BAR | IWorkbenchBrowserSupport.NAVIGATION_BAR, null, null, null);
			browser.openURL(new URL("http://localhost:"+sab.getSunAppServer().getAdminServerPort()));
		} catch (Exception e) {
	           SunAppSrvPlugin.logMessage("Error opening browser: "+e.getMessage());

		}
	}

}
