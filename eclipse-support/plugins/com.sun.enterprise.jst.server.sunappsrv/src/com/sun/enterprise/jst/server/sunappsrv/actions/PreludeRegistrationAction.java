
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
// </editor-fold>package com.sun.enterprise.jst.server.sunappsrv.actions;
package com.sun.enterprise.jst.server.sunappsrv.actions;

import java.io.File;

import org.apache.tools.ant.taskdefs.Execute;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.action.IAction;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.wst.server.core.IServer;

import com.sun.enterprise.jst.server.sunappsrv.SunAppServerBehaviour;
import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;

/**
 *
 * @author Ludovic.Champenois@Sun.COM
 */
public class PreludeRegistrationAction extends AppServerContextAction  {

    /**
     * The constructor.
     */
    public PreludeRegistrationAction() {
    	super ("Register your GlassFish Enterprise Server...",getImageDescriptorFromlocalImage("icons/obj16/registration.png"));
    }

    public void execute (IServer server) {
    	    	
      	
    	SunAppServerBehaviour sab = (SunAppServerBehaviour) server.loadAdapter( SunAppServerBehaviour.class, null);
    	if (sab.isV3()==false){//V2 only
    		String[] command = new String[]{
    				sab.getSunAppServer().getDomainDir()+"/../../updatetool/bin/updatetool"
    		}; 
    		try {
    			Execute.launch(null, command, null, new File(sab.getSunApplicationServerInstallationDirectory()), true);
    			return;
    		} catch (Exception ioe) {
    			SunAppSrvPlugin.logMessage("error Launching Executable", ioe);
    		} 
    	}
      	     	
		if (accept(server)==false){
			showMessageDialog();
			return;
		}
        IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
        try {
            SunAppSrvPlugin.logMessage("PreludeRegistrationAction run"+server);
            IFile file = ResourcesPlugin.getWorkspace().getRoot().getFile(new Path("icons/obj16/sunappsrvs.gif"));
            page.openEditor(new FileEditorInput(file), "com.sun.enterprise.jst.server.sunappsrv.v3.Register");
        } catch (Exception e) {
            SunAppSrvPlugin.logMessage("PreludeRegistrationAction " + e);
            try {
                page.openEditor(null, "com.sun.enterprise.jst.server.sunappsrv.v3.Register");
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
		}	}
}
