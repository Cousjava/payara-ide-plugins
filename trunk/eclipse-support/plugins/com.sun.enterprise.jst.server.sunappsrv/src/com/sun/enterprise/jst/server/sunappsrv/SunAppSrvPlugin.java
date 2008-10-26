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

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.HashSet;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

import com.sun.enterprise.jst.server.sunappsrv.preferences.PreferenceConstants;


/**
 *Sun Servers Plugin
 */
public class SunAppSrvPlugin extends AbstractUIPlugin {
    
    protected static final String SUNPLUGIN_ID = "com.sun.enterprise.jst.server.sunappsrv";
    private static SunAppSrvPlugin singleton;
    private static HashSet<String[]> commandsToExecuteAtExit = new HashSet<String[]>();
    public SunAppSrvPlugin() {
        singleton = this; 
    }

    public void stop(BundleContext v) throws Exception {
    	logMessage("STOP IS CALLED!!!!!!!!!!!!!!!!");
    	for (String[] command: commandsToExecuteAtExit){
    		try {
    			BufferedReader input = new BufferedReader(new InputStreamReader(Runtime.getRuntime().exec(command).getInputStream()));
    			String line = null;
    			while ((line = input.readLine()) != null) logMessage(">>> " + line);
    			input.close();
    		} catch (Exception ex) {
    			logMessage("Error executing process:\n" + ex);
    		}
    		}
    	super.stop(v);

    }

    
    
    public static SunAppSrvPlugin getInstance() {
        return singleton;
    }
    public void addCommandToExecuteAtExit(String command[]){
       	for (String[] com: commandsToExecuteAtExit){
    		if ( Arrays.equals(com, command)){
    	    	logMessage("Command already there");
   			
    			return;
    		}
       	}
       	commandsToExecuteAtExit.add(command);
    	logMessage("addCommandToExecuteAtExit size="+commandsToExecuteAtExit.size());
    }
    public static void logMessage(String mess){

    	IPreferenceStore store = getInstance().getPreferenceStore();
    	boolean trace= store.getBoolean(PreferenceConstants.ENABLE_LOG);
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
