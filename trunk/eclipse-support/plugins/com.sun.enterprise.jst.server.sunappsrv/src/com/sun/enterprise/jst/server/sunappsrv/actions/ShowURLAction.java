
// <editor-fold defaultstate="collapsed" desc="CDDL Licence">
/*
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * glassfishplugins/www/license/CDDLv1.0.txt or
 * https://glassfish.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * glassfishplugins/www/license/CDDLv1.0.txt.  If applicable,
 * add the following below this CDDL HEADER, with the
 * fields enclosed by brackets "[]" replaced with your
 * own identifying information: Portions Copyright [yyyy]
 * [name of copyright owner]
 */
// </editor-fold>

package com.sun.enterprise.jst.server.sunappsrv.actions;

import java.net.URL;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IViewActionDelegate;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.browser.IWebBrowser;
import org.eclipse.ui.browser.IWorkbenchBrowserSupport;
import org.eclipse.wst.server.core.IServer;
import org.eclipse.wst.server.ui.internal.ServerUIPlugin;

import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;

/**
 *
 * @author Ludovic.Champenois@Sun.COM
 */
public class ShowURLAction implements IObjectActionDelegate ,IViewActionDelegate ,IWorkbenchWindowActionDelegate {

	private String url;

    public ShowURLAction(String url) {
        this.url = url;
   }
 

	public void setActivePart(IAction arg0, IWorkbenchPart arg1) {
		
	}

	public void run(IAction arg0) {
		try {

			
			IWorkbenchBrowserSupport browserSupport = ServerUIPlugin.getInstance().getWorkbench().getBrowserSupport();
			IWebBrowser browser = browserSupport.createBrowser(IWorkbenchBrowserSupport.LOCATION_BAR | IWorkbenchBrowserSupport.NAVIGATION_BAR, null, null, null);
			browser.openURL(new URL(url));
		} catch (Exception e) {
	           SunAppSrvPlugin.logMessage("Error opening browser: "+e.getMessage());

		}		
	}

	public void selectionChanged(IAction arg0, ISelection arg1) {		
	}

	public void init(IViewPart arg0) {
				
	}

	public void dispose() {		
	}

	public void init(IWorkbenchWindow arg0) {		
	}

}