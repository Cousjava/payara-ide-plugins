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

import org.eclipse.wst.server.core.IServer;
import org.glassfish.tools.ide.server.FetchLog;

import com.sun.enterprise.jst.server.sunappsrv.SunAppServerBehaviour;
import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;
import com.sun.enterprise.jst.server.sunappsrv.log.GlassfishConsoleManager;
import com.sun.enterprise.jst.server.sunappsrv.log.GlassFishConsole;
import com.sun.enterprise.jst.server.sunappsrv.log.IGlassFishConsole;
import com.sun.enterprise.jst.server.sunappsrv.log.RemoteGlassFishConsole;
/**
 * @author: ludovic champenois
 */

public class ViewLogAction extends AppServerContextAction{
	public ViewLogAction() {
		super("View Log File",getImageDescriptorFromlocalImage("icons/obj16/logfile.gif"));
	}


	@Override
	public void perform(IServer server) {
		

		try {
	        SunAppServerBehaviour sab = (SunAppServerBehaviour) server.loadAdapter(
	                SunAppServerBehaviour.class, null);
	        if (sab.getSunAppServer().isLocalServer()){
	        	IGlassFishConsole console = GlassfishConsoleManager.getConsole(sab.getSunAppServer());
	        	console.startLogging(FetchLog.create(sab.getSunAppServer(), true));
	        }else {
	    		if (!sab.getSunAppServer().isRunning()){
	    			showMessageDialog();
	    			return;
	    		}
	        	RemoteGlassFishConsole.showConsole(sab.getSunAppServer());
	        	
	        }
			
		} catch (Exception e) {
	           SunAppSrvPlugin.logMessage("Error opening log: "+e.getMessage());

		}
	}
	public boolean accept(IServer server) {
        return true;
	}
}
