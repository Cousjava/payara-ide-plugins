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
