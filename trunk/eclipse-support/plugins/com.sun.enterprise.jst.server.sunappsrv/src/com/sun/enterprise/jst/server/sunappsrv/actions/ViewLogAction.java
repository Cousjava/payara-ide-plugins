
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

import java.io.File;

import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.wst.server.core.IServer;

import com.sun.enterprise.jst.server.sunappsrv.SunAppServerBehaviour;
import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;
import com.sun.enterprise.jst.server.sunappsrv.log.LogView;
/**
 * @author: ludovic champenois
 */

public class ViewLogAction extends AppServerContextAction{
	public ViewLogAction() {
		super("View Log File",getImageDescriptorFromlocalImage("icons/obj16/logfile.gif"));
	}


	public void execute(IServer server) {
		

		try {
	        SunAppServerBehaviour sab = (SunAppServerBehaviour) server.loadAdapter(
	                SunAppServerBehaviour.class, null);
	        IWorkbench iw = SunAppSrvPlugin.getInstance().getWorkbench();
    		IWorkbenchPage page  = iw.getActiveWorkbenchWindow()
    		.getActivePage();
    		LogView lv = (LogView) page.showView("com.sun.enterprise.jst.server.sunappsrv.log.LogView");			
		   	String logFile =  sab.getDomainDirWithDomainName()+"/logs/server.log";	        		    
			lv.init(new File(logFile));
			
		} catch (Exception e) {
	           SunAppSrvPlugin.logMessage("Error opening browser: "+e.getMessage());

		}
	}
	public boolean accept(IServer server) {
        return true;
	}
}