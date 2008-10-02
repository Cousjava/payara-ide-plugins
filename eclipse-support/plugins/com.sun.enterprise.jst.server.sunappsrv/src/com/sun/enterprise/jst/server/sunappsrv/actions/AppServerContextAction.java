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

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.wst.server.core.IModule;
import org.eclipse.wst.server.core.IServer;
import org.eclipse.wst.server.ui.internal.view.servers.ModuleServer;

import com.sun.enterprise.jst.server.sunappsrv.SunAppServerBehaviour;
import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;

/**
 * @author Ludo
 *
 */
public class AppServerContextAction implements IObjectActionDelegate {

    private IWorkbenchPart targetPart;
    private IServer selectedServer;
    private IModule selectedModule;

    /**
     * Constructor for CleanWorkDirAction.
     */
    public AppServerContextAction() {
        super();
    }

    /**
     * @see IObjectActionDelegate#setActivePart(IAction, IWorkbenchPart)
     */
    public void setActivePart(IAction action, IWorkbenchPart targetPart) {
        this.targetPart = targetPart;
    }

    /**
     * @see org.eclipse.ui.IActionDelegate#run(IAction)
     */
    public void run(IAction action) {
        Shell s = targetPart.getSite().getShell();


        SunAppServerBehaviour sab = (SunAppServerBehaviour) selectedServer.loadAdapter(
                SunAppServerBehaviour.class, null);

        SunAppSrvPlugin.logMessage(
                "Message is:" + action + sab.getDomainDir() + selectedServer + selectedModule);//+Activator.message+"'");
    }

    /**
     * @see org.eclipse.ui.IActionDelegate#selectionChanged(IAction, ISelection)
     */
    public void selectionChanged(IAction action, ISelection selection) {
        selectedServer = null;
        selectedModule = null;
        if (!selection.isEmpty()) {
            if (selection instanceof IStructuredSelection) {
                Object obj = ((IStructuredSelection) selection).getFirstElement();
                if (obj instanceof IServer) {
                    selectedServer = (IServer) obj;
                } else if (obj instanceof ModuleServer) {
                    ModuleServer ms = (ModuleServer) obj;
                    selectedModule = ms.module[ms.module.length - 1];
                    if (selectedModule != null) {
                        selectedServer = ms.server;
                    }
                }
            }
        }
    }
}
