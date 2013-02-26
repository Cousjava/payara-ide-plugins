package com.sun.enterprise.jst.server.sunappsrv.handlers;

import java.net.URL;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.browser.IWebBrowser;
import org.eclipse.ui.browser.IWorkbenchBrowserSupport;

import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;

public class ShowURLHandler extends AbstractHandler {

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		String url = event.getParameter("oracle.eclipse.tools.glassfish.commands.urlParam");
		// this should not happen
		if (url == null)
			return null;
		try {
			IWorkbenchBrowserSupport browserSupport = PlatformUI.getWorkbench()
					.getBrowserSupport();
			IWebBrowser browser = browserSupport.createBrowser(
					IWorkbenchBrowserSupport.LOCATION_BAR
							| IWorkbenchBrowserSupport.NAVIGATION_BAR, null,
					null, null);
			browser.openURL(new URL(url));
		} catch (Exception e) {
			SunAppSrvPlugin.logMessage("Error opening browser: "
					+ e.getMessage());

		}
		return null;
	}

}
