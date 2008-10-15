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
package com.sun.enterprise.jst.server.sunappsrv.v3;


import org.eclipse.core.resources.ResourcesPlugin;

import com.sun.enterprise.jst.server.sunappsrv.SunAppServerBehaviour;
import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;
import com.sun.enterprise.jst.server.sunappsrv.actions.AppServerContextAction;


/**
 * @author Ludo
 *
 */

public class UpdateCenter extends Register {

    public UpdateCenter() {
        super();
        SunAppSrvPlugin.logMessage("Update Center Action...with "+AppServerContextAction.selectedServer);

        ResourcesPlugin.getWorkspace().addResourceChangeListener(this);
        if (AppServerContextAction.selectedServer!=null){
    		SunAppServerBehaviour sab = (SunAppServerBehaviour) AppServerContextAction.selectedServer.loadAdapter(
    				SunAppServerBehaviour.class, null);
    		String hostName = sab.getSunAppServer().getServer().getHost();
    		URLtoShow ="http://"+hostName+":"+sab.getSunAppServer().getAdminServerPort()+"/updateCenter/addOn.jsf";     
        
        }else{
        	URLtoShow = "http://localhost:4848/updateCenter/addOn.jsf";
        
        }
    }
}
