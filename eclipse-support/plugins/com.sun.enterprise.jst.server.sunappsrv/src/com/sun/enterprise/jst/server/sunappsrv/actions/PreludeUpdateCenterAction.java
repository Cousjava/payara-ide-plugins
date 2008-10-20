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

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Path;

import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.wst.server.core.IServer;

import com.sun.enterprise.jst.server.sunappsrv.SunAppServerBehaviour;
import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;

/**
 * small action to test if the module is functional. Need te be removed later, when we are done
 * debugging
 *
 * @author Ludovic.Champenois@Sun.COM
 */
public class PreludeUpdateCenterAction extends AppServerContextAction {

    /**
     * The constructor.
     */
    public PreludeUpdateCenterAction() {
    	super ("GlassFish Update Center...",getImageDescriptorFromlocalImage("icons/obj16/updateCenter.png"));
    }


    public void execute(IServer server) {
    	
		if (accept(server)==false){
			showMessageDialog();
			return;
		}
        IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
        try {
            SunAppSrvPlugin.logMessage("PreludeUpdateCenterAction run for " +server);
            IFile file = ResourcesPlugin.getWorkspace().getRoot().getFile(new Path("icons/obj16/sunappsrv.gif"));
            page.openEditor(new FileEditorInput(file), "com.sun.enterprise.jst.server.sunappsrv.v3.UpdateCenter");
        } catch (Exception e) {
            SunAppSrvPlugin.logMessage("PreludeUpdateCenterAction " + e);
            try {
                page.openEditor(null, "com.sun.enterprise.jst.server.sunappsrv.v3.UpdateCenter");
            } catch (PartInitException e1) {
                // TODO Auto-generated catch block
                e1.printStackTrace();
            }
            e.printStackTrace();
        }

    }
    public boolean accept(IServer server) {
    	SunAppServerBehaviour sab = (SunAppServerBehaviour) server.loadAdapter( SunAppServerBehaviour.class, null);
    	try {
    		return sab.getSunAppServer().isRunning();
    	} catch (CoreException e) {
    		return false;
    	}
    }
}
