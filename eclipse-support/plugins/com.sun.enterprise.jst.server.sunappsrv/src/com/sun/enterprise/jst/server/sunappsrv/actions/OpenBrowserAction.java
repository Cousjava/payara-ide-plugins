package com.sun.enterprise.jst.server.sunappsrv.actions;

import java.net.URL;

import org.eclipse.ui.browser.IWebBrowser;
import org.eclipse.ui.browser.IWorkbenchBrowserSupport;
import org.eclipse.wst.server.core.IServer;
import org.eclipse.wst.server.ui.internal.ServerUIPlugin;

import com.sun.enterprise.jst.server.sunappsrv.SunAppServerBehaviour;
import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;

public class OpenBrowserAction extends AppServerContextAction {

	public OpenBrowserAction() {
		super("Open GlassFish Admin Console...",getImageDescriptorFromlocalImage("icons/obj16/glassfishserver.gif"));
	}


	public void execute(IServer server) {
		
		if (accept(server)==false){
			showMessageDialog();
			return;
		}
		try {
	        SunAppServerBehaviour sab = (SunAppServerBehaviour) server.loadAdapter(
	                SunAppServerBehaviour.class, null);
			
			
			IWorkbenchBrowserSupport browserSupport = ServerUIPlugin.getInstance().getWorkbench().getBrowserSupport();
			IWebBrowser browser = browserSupport.createBrowser(IWorkbenchBrowserSupport.LOCATION_BAR | IWorkbenchBrowserSupport.NAVIGATION_BAR, null, null, null);
			browser.openURL(new URL("http://"+sab.getSunAppServer().getAdminServerPort()+":"+sab.getSunAppServer().getAdminServerPort()));
		} catch (Exception e) {
	           SunAppSrvPlugin.logMessage("Error opening browser: "+e.getMessage());

		}
	}
	public boolean accept(IServer server) {
        SunAppServerBehaviour sab = (SunAppServerBehaviour) server.loadAdapter( SunAppServerBehaviour.class, null);
		return sab.getSunAppServer().isRunning();
	}
}
