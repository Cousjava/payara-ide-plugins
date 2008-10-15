package com.sun.enterprise.jst.server.sunappsrv.actions;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.ResourcesPlugin;
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
		return sab.getSunAppServer().isRunning();
	}
}
