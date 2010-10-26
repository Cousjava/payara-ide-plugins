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


package com.sun.enterprise.jst.server.sunappsrv.log;

import java.io.File;
import java.io.IOException;

import org.eclipse.jface.text.IDocument;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.console.ConsolePlugin;
import org.eclipse.ui.console.IConsole;
import org.eclipse.ui.console.IConsoleManager;
import org.eclipse.ui.console.MessageConsole;
import org.eclipse.ui.console.MessageConsoleStream;

import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;


/**
 * 
 * Simple MessageConsole subclass so we can enable our coloring in org.eclipse.ui.console.consolePageParticipants
 * for our log console without affecting other consoles.  There are also some 
 * convenience methods for initializing from a file and showing the console page.
 * 
 * @author Rochelle Raccah
 *
 */
public class GlassFishConsole extends MessageConsole {
	private static int interval = 500;
	private static int numLines = 1000;
	private LogThread thread;
	private File file;
	
	public GlassFishConsole(File f) {
		super(f.getAbsolutePath(), null);
		file = f;
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.console.AbstractConsole#init()
	 */
	@Override
	protected void init() {
		super.init();
		final IDocument document = getDocument();

		try {
			thread = new LogThread(file, interval, numLines);			
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
		file = null;
		super.dispose();
	}
	
	private static MessageConsole getConsole(File f) {
	      ConsolePlugin plugin = ConsolePlugin.getDefault();
	      IConsoleManager manager = plugin.getConsoleManager();
	      IConsole[] existing = manager.getConsoles();
	      String name = f.getAbsolutePath();
	      MessageConsole myConsole = null;

	      for (int i = 0; i < existing.length; i++) {
	         if (name.equals(existing[i].getName())) {
	        	 myConsole = (MessageConsole) existing[i];
	        	 return myConsole;
	         }
	      }
	      //no console found, so create a new one
	      myConsole = new GlassFishConsole(f);
	      manager.addConsoles(new IConsole[]{myConsole});
	      return myConsole;
	}

	public static void showConsole(File f) {
		MessageConsole myConsole = getConsole(f);
		if (myConsole != null) {
			ConsolePlugin.getDefault().getConsoleManager().showConsoleView(myConsole);
		}
	}

	private LogThread.LogListener addListener(final IDocument doc, LogThread th){
		final Display display = Display.getCurrent();
		return th.addListener(new LogThread.LogListener()	{
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
