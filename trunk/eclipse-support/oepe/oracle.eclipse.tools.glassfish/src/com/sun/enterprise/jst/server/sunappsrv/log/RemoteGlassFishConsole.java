/*
 * Copyright (c) 2011 Oracle and/or its affiliates. All rights reserved.
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


package com.sun.enterprise.jst.server.sunappsrv.log;

import java.io.IOException;

import org.eclipse.jface.text.IDocument;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.console.ConsolePlugin;
import org.eclipse.ui.console.IConsole;
import org.eclipse.ui.console.IConsoleManager;
import org.eclipse.ui.console.MessageConsole;
import org.eclipse.ui.console.MessageConsoleStream;

import com.sun.enterprise.jst.server.sunappsrv.SunAppServer;
import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;
import com.sun.enterprise.jst.server.sunappsrv.handlers.AppServerContextAction;


/**
 * 
 * Simple MessageConsole subclass so we can enable our coloring in org.eclipse.ui.console.consolePageParticipants
 * for our log console without affecting other consoles.  There are also some 
 * convenience methods for initializing from a file and showing the console page.
 * 
 * @author Ludovic Champenois
 *
 */
public class RemoteGlassFishConsole extends MessageConsole {
	private static int interval = 6000;
	private static int numLines = 1000;
	private RemoteLogThread thread;
	private SunAppServer server;
	
	public RemoteGlassFishConsole(SunAppServer s) {
		super(s.getServer().getHost()+":"+s.getAdminServerPort(), AppServerContextAction.getImageDescriptorFromlocalImage("icons/obj16/glassfishserver.gif"));
		server = s;
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.console.AbstractConsole#init()
	 */
	@Override
	protected void init() {
		super.init();
		final IDocument document = getDocument();

		try {
			thread = new RemoteLogThread(server, interval, numLines);			
		}
		catch (Exception e) {
			e.printStackTrace();
			return;
		}

		addListener(document, thread);
		thread.start();
	}

	public void dispose(){
		SunAppSrvPlugin.logMessage("Console Dispose is CALLED...");
		if (thread!=null){
			thread.halt();
			thread=null;
		}
		super.dispose();
	}
	
	private static MessageConsole getConsole(SunAppServer s) {
	      ConsolePlugin plugin = ConsolePlugin.getDefault();
	      IConsoleManager manager = plugin.getConsoleManager();
	      IConsole[] existing = manager.getConsoles();
	      String name = s.getServer().getHost()+":"+s.getAdminServerPort();
	      MessageConsole myConsole = null;

	      for (int i = 0; i < existing.length; i++) {
	         if (name.equals(existing[i].getName())) {
	        	 myConsole = (MessageConsole) existing[i];
	        	 return myConsole;
	         }
	      }
	      //no console found, so create a new one
	      myConsole = new RemoteGlassFishConsole(s);
	      manager.addConsoles(new IConsole[]{myConsole});
	      return myConsole;
	}

	public static void showConsole(SunAppServer s) {
		MessageConsole myConsole = getConsole(s);
		if (myConsole != null) {
			ConsolePlugin.getDefault().getConsoleManager().showConsoleView(myConsole);
		}
	}

	private RemoteLogThread.LogListener addListener(final IDocument doc, RemoteLogThread th){
		final Display display = Display.getCurrent();
		return th.addListener(new RemoteLogThread.LogListener()	{
			public void logChanged(final Ring list){
				if (!display.isDisposed()) {
					display.asyncExec(new Runnable(){
						public void run(){
							// it is best to split this into multiple lines instead
							// of nesting it like this: out.println(list.getCompleteDocument());
							// so that we have the chance to see the output for debugging purposes
							// if desired
							MessageConsoleStream out = newMessageStream();
							String output = list.getCompleteDocument();
							
							out.println(output);
							try {
								out.close();
							} catch (IOException e) {
								e.printStackTrace();
							}
						}
					});
				}
			}
		});
	}
}
