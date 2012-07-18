package com.sun.enterprise.jst.server.sunappsrv.handlers;

import java.io.File;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.wst.server.core.IServer;

import com.sun.enterprise.jst.server.sunappsrv.SunAppServerBehaviour;
import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;
import com.sun.enterprise.jst.server.sunappsrv.log.GlassFishConsole;
import com.sun.enterprise.jst.server.sunappsrv.log.RemoteGlassFishConsole;

public class ViewLogHandler extends AbstractGlassfishSelectionHandler {

	@Override
	public void processSelection(IStructuredSelection selection) {
		IServer server = (IServer) selection.getFirstElement();
		try {
	        SunAppServerBehaviour sab = (SunAppServerBehaviour) server.loadAdapter(
	                SunAppServerBehaviour.class, null);
	        if (sab.getSunAppServer().isLocalServer()){
	        	String logFile =  sab.getDomainDirWithDomainName()+"/logs/server.log";	        		    
	        	GlassFishConsole.showConsole(new File(logFile));
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

}
