/*
 * Copyright (c) 1997-2010 Oracle and/or its affiliates. All rights reserved.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Sun Microsystems
 *     Oracle
 */


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
    
    public static final String SUNPLUGIN_ID = "oracle.eclipse.tools.glassfish";
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
    
    public static IStatus createErrorStatus(String mess, Exception e) {
        return new Status(IStatus.ERROR, SUNPLUGIN_ID, -1, mess, e);        
    }
    
}
