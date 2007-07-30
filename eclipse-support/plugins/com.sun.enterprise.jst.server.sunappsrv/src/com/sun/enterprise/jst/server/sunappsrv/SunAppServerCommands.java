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

package com.sun.enterprise.jst.server.sunappsrv;

import org.eclipse.wst.server.core.IServerWorkingCopy;
import org.eclipse.core.commands.operations.AbstractOperation;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;

/* set all the different app server props using the generic do/undo framework
 *
 **/

public class SunAppServerCommands  extends AbstractOperation {
    
    protected String value;
    protected String oldValue;
    protected String commandName;
    
    SunAppServer sunServer;
    
    public SunAppServerCommands(IServerWorkingCopy server, String newValue, String commandName) {
        super( commandName);
        sunServer = SunAppServer.getSunAppServer(server);
        this.value = newValue;
        this.commandName = commandName;
    }
    
     

    
	public IStatus execute(IProgressMonitor monitor, IAdaptable adapt) {
        oldValue=  (String) sunServer.getProps().get(commandName);
        sunServer.getProps().put(commandName, value);
        return null;
	}



	public IStatus undo(IProgressMonitor monitor, IAdaptable adapt) {
        if (sunServer != null) {
            sunServer.getProps().put(commandName, oldValue);
        }
        return null;
	}
	
	public IStatus redo(IProgressMonitor monitor, IAdaptable adapt) {
		return execute(monitor, adapt);
	}    
    
    
}