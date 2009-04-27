// <editor-fold defaultstate="collapsed" desc="CDDL+GPL License">
/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 * 
 * Copyright 1997-2009 Sun Microsystems, Inc. All rights reserved.
 * 
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common Development
 * and Distribution License("CDDL") (collectively, the "License").  You
 * may not use this file except in compliance with the License. You can obtain
 * a copy of the License at https://glassfish.dev.java.net/public/CDDL+GPL.html
 * or glassfish/bootstrap/legal/LICENSE.txt.  See the License for the specific
 * language governing permissions and limitations under the License.
 * 
 * When distributing the software, include this License Header Notice in each
 * file and include the License file at glassfish/bootstrap/legal/LICENSE.txt.
 * Sun designates this particular file as subject to the "Classpath" exception
 * as provided by Sun in the GPL Version 2 section of the License file that
 * accompanied this code.  If applicable, add the following below the License
 * Header, with the fields enclosed by brackets [] replaced by your own
 * identifying information: "Portions Copyrighted [year]
 * [name of copyright owner]"
 * 
 * Contributor(s):
 * 
 * If you wish your version of this file to be governed by only the CDDL or
 * only the GPL Version 2, indicate your decision by adding "[Contributor]
 * elects to include this software in this distribution under the [CDDL or GPL
 * Version 2] license."  If you don't indicate a single choice of license, a
 * recipient has the option to distribute your version of this file under
 * either the CDDL, the GPL Version 2 or to extend the choice of license to
 * its licensees as provided above.  However, if you add GPL Version 2 code
 * and therefore, elected the GPL Version 2 license, then the option applies
 * only if the new code is made subject to such option by the copyright
 * holder.
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
    			logMessage(">>> " + command[0],null);
    			BufferedReader input = new BufferedReader(new InputStreamReader(Runtime.getRuntime().exec(command).getInputStream()));
    			String line = null;
    			while ((line = input.readLine()) != null) logMessage(">>> " + line,null);
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
