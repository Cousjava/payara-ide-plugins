/*
 * Copyright (c) 1997-2011 Oracle and/or its affiliates. All rights reserved.
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
import org.eclipse.wst.server.core.IRuntime;
import org.osgi.framework.BundleContext;

import com.sun.enterprise.jst.server.sunappsrv.preferences.PreferenceConstants;


/**
 *Sun Servers Plugin
 */
public class SunAppSrvPlugin extends AbstractUIPlugin {
	public static final String V3_RUNTIME = "com.sun.enterprise.jst.server.runtime.sunappsrv92"; //$NON-NLS-1$
	public static final String V31_RUNTIME = "org.glassfish.jst.server.runtime.glassfish31"; //$NON-NLS-1$
	public static final String V311_RUNTIME = "org.glassfish.jst.server.runtime.glassfish311"; //$NON-NLS-1$
	public static final String V312_RUNTIME = "org.glassfish.jst.server.runtime.glassfish312"; //$NON-NLS-1$
	public static final String V3122_RUNTIME = "org.glassfish.jst.server.runtime.glassfish3122"; //$NON-NLS-1$
	public static final String V4_RUNTIME = "org.glassfish.jst.server.runtime.glassfish40"; //$NON-NLS-1$
    
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
    
    public static boolean is31OrAbove(IRuntime runtime){      
        if (runtime.getRuntimeType().getId().equals(V31_RUNTIME))
            return true;
        if (runtime.getRuntimeType().getId().equals(V311_RUNTIME))
            return true;
        if (runtime.getRuntimeType().getId().equals(V312_RUNTIME))
            return true;
        if (runtime.getRuntimeType().getId().equals(V3122_RUNTIME))
            return true;
        if (runtime.getRuntimeType().getId().equals(V4_RUNTIME))
        	return true;
        return false;      
                    
    }
    public static boolean is3OrAbove(IRuntime runtime){      
        if (runtime.getRuntimeType().getId().equals(V3_RUNTIME))
            return true;

        return is31OrAbove(runtime);      
                    
    }
    
}
