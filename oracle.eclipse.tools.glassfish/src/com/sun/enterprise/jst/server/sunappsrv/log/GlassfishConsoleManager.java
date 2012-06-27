package com.sun.enterprise.jst.server.sunappsrv.log;

import java.io.File;

import org.eclipse.ui.console.ConsolePlugin;
import org.eclipse.ui.console.IConsole;
import org.eclipse.ui.console.IConsoleListener;
import org.eclipse.ui.console.IConsoleManager;

import com.sun.enterprise.jst.server.sunappsrv.SunAppServer;

public class GlassfishConsoleManager {
	
	//private static Map<String, IGlassFishConsole> consoles = new HashMap<String, IGlassFishConsole>();
	private static IConsoleManager manager = ConsolePlugin.getDefault().getConsoleManager();
	
	static {
		manager.addConsoleListener(new IConsoleListener() {
			
			@Override
			public void consolesRemoved(IConsole[] consoles) {
				System.out.println("console removed");
				
			}
			
			@Override
			public void consolesAdded(IConsole[] consoles) {
				// TODO Auto-generated method stub
				
			}
		});
	}
	
	public static IGlassFishConsole getConsole(SunAppServer server) {
		String consoleID = getConsoleID(server);
		IGlassFishConsole gfConsole = findConsole(consoleID);
		//IGlassFishConsole gfConsole = consoles.get(consoleID);
		if (gfConsole == null) {
			//no console found, so create a new one
			gfConsole = new GFConsole(consoleID);
		    manager.addConsoles(new IConsole[]{gfConsole});
		    //consoles.put(consoleID, gfConsole);
		}
		manager.showConsoleView(gfConsole);
		return gfConsole;
	}
	
	private static IGlassFishConsole findConsole(String name) {
	      IConsole[] existing = manager.getConsoles();
	      IGlassFishConsole myConsole = null;

	      for (int i = 0; i < existing.length; i++) {
	         if (name.equals(existing[i].getName())) {
	        	 myConsole = (IGlassFishConsole) existing[i];
	        	 return myConsole;
	         }
	      }
	      return null;
	}
	
	private static String getConsoleID(SunAppServer server) {
		if (server.isLocalServer())
			return server.getDomainsFolder() + File.separator + 
					server.getDomainName() + File.separator + "logs" +
			File.separator + "server.log";
		else
			return server.getHost()+":"+server.getAdminPort();
	}

}
