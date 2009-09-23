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
