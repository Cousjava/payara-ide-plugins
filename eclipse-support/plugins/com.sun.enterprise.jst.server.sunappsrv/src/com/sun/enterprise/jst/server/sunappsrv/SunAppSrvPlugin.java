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

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Plugin;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.ui.plugin.AbstractUIPlugin;

import com.sun.enterprise.jst.server.sunappsrv.preferences.PreferenceConstants;


/**
 *Sun Servers Plugin
 */
public class SunAppSrvPlugin extends AbstractUIPlugin {
    
    protected static final String SUNPLUGIN_ID = "com.sun.enterprise.jst.server.sunappsrv";
    private static SunAppSrvPlugin singleton;
    
    public SunAppSrvPlugin() {
        singleton = this; 


        
    }
    
    
    public static SunAppSrvPlugin getInstance() {
        return singleton;
    }
    
    public static void logMessage(String mess){

    	IPreferenceStore store = getInstance().getPreferenceStore();
    	boolean trace= store.getDefaultBoolean(PreferenceConstants.ENABLE_LOG);
    	if(trace){
    		Status status = new Status(IStatus.INFO, SUNPLUGIN_ID, 1,"GlassFish: "+mess,null);        
    		getInstance().getLog().log(status);
    	}
    }
    public static void logMessage(String mess, Exception e){
        final Status status = new Status(IStatus.ERROR, SUNPLUGIN_ID, 1,"GlassFish: "+mess,e);        
        getInstance().getLog().log(status);
    }
}
