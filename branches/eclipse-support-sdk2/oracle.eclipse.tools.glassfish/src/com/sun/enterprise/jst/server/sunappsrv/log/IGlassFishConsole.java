package com.sun.enterprise.jst.server.sunappsrv.log;

import org.eclipse.ui.console.IConsole;
import org.glassfish.tools.ide.server.FetchLog;


public interface IGlassFishConsole extends IConsole {
	
	public void startLogging(FetchLog... logFetchers);
	
	public void stopLogging();
	
	public boolean isLogging();

}
