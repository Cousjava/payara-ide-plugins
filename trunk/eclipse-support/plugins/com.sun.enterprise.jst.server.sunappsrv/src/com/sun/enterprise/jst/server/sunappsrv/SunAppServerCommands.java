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
import org.eclipse.wst.server.ui.internal.command.ServerCommand;

/* set all the different app server props using the generic do/undo framework
 *
 **/

public class SunAppServerCommands extends ServerCommand {
    
    protected String value;
    protected String oldValue;
    protected String commandName;
    
    SunAppServer sunServer;
    
    public SunAppServerCommands(IServerWorkingCopy server, String newValue, String commandName) {
        super(server, commandName);
        this.value = newValue;
        this.commandName = commandName;
    }
    
    
    public void execute() {
        
        sunServer = SunAppServer.getSunAppServer(server);
        oldValue=  (String) sunServer.getServerInstanceProperties().get(commandName);
        sunServer.getServerInstanceProperties().put(commandName, value);
        
    }
    
    
    public void undo() {
        if (sunServer != null) {
            sunServer.getServerInstanceProperties().put(commandName, oldValue);
        }
    }
    
    
    
    
}