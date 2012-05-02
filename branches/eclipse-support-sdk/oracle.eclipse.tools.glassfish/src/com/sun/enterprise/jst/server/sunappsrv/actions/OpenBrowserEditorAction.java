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

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.wst.server.core.IServer;

import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;

public abstract class OpenBrowserEditorAction extends AppServerContextAction {
	public OpenBrowserEditorAction(String name, ImageDescriptor image) {
		super(name, image);
	}

	abstract protected String getEditorClassName();
	abstract protected String getIconName();
	abstract protected String getURL();

 	protected void showPageInBrowser(IServer server) {
	    IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
	    String className = getClass().getName();
	    String editorClassName = getEditorClassName();
	    try {
	        SunAppSrvPlugin.logMessage(className + " run "+server);
	        // workaround for eclipse bug 77217
	        // OpenSolaris doesn't have internal browser set up properly
	        if ("SunOS".equals(System.getProperty("os.name"))) {
	        	showPageInDefaultBrowser(getURL());
	        } else { // end workaround
	        IFile file = ResourcesPlugin.getWorkspace().getRoot().getFile(new Path(getIconName()));
	        page.openEditor(new FileEditorInput(file), editorClassName);
	        }
	    } catch (Exception e) {
	        SunAppSrvPlugin.logMessage(className + " " + e);
	        try {
	            page.openEditor(null, editorClassName);
	        } catch (PartInitException e1) {
	            // TODO Auto-generated catch block
	            e1.printStackTrace();
	        }
	        e.printStackTrace();
	    }
    }
}
