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



package com.sun.enterprise.jst.server.sunappsrv.actions;

import java.io.File;
import java.io.IOException;

import org.apache.tools.ant.taskdefs.Execute;
import org.eclipse.wst.server.core.IServer;

import com.sun.enterprise.jst.server.sunappsrv.SunAppServerBehaviour;
import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;

/**
 * small action to test if the module is functional. Need te be removed later, when we are done
 * debugging
 *
 * @author Ludovic.Champenois@Sun.COM
 */
public class PreludeUpdateCenterAction extends OpenBrowserEditorAction {

    /**
     * The constructor.
     */
    public PreludeUpdateCenterAction() {
    	super ("GlassFish Update Center...",getImageDescriptorFromlocalImage("icons/obj16/updateCenter.png"));
    }


	@Override
    public void perform(IServer server) {
    	SunAppServerBehaviour sab = (SunAppServerBehaviour) server.loadAdapter( SunAppServerBehaviour.class, null);
    	if (!sab.isV3()) { // V2 only
			String loc=sab.getSunApplicationServerInstallationDirectory()+"/updatecenter/bin/updatetool";    	
       	    if (File.separator.equals("\\")) {
       			loc = loc + ".bat"; //NOI18N
       	    }
    		String[] command = new String[]{
    				loc    		}; 
    		try {
    			File appinstallDir = new File(sab.getSunApplicationServerInstallationDirectory());
 
    			// need to test this separately because in this case the launch below doesn't work, but
    			// the exit code is not a failure
    			if (!appinstallDir.canWrite()) {
    				throw new IOException("Cannot write to directory " + appinstallDir);
    			}

    			Process process = Execute.launch(null, command, null, appinstallDir, true);
    			try {
    				int exitValue = process.exitValue();

    				// this test works in debug with breakpoints, but not regular run for certain cases - could
    				// be a timing issue because same "failure" case exits with 1 or 0 depending on timing
    				if (Execute.isFailure(exitValue)) {
    					// can get fancy here and try to get info out of the process error stream
    					// on why it failed, but it is not straightforward and this is a corner case
    					throw new IOException();
    				}
    			} catch (IllegalThreadStateException e) {
    				// if it gets here, the ui came up and is still running, we don't want to 
    				// block - this is actually the "success" case
    			}
    			return;
    		} catch (Exception ioe) {
    			String exceptionMessage = ioe.getMessage();
    			String message = ((exceptionMessage != null) ? "Error launching updatetool executable: " + exceptionMessage :
    				"Error launching updatetool executable");
    			showMessageDialog(message);
    			SunAppSrvPlugin.logMessage("error Launching Executable", ioe);
    		}
    	} else { // V3
			if (accept(server)==false){
				showMessageDialog();
				return;
			}
			showPageInBrowser(server);
    	}
    }

	protected String getEditorClassName() { return "com.sun.enterprise.jst.server.sunappsrv.v3.UpdateCenter"; }

 	protected String getIconName() { return "icons/obj16/sunappsrv.gif"; }

 	protected String getURL() { return new com.sun.enterprise.jst.server.sunappsrv.v3.UpdateCenter().getURL(); }

    @Override
    public boolean accept(IServer server) {
        return acceptIfServerRunning(server);
    }
}
