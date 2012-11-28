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

package com.sun.enterprise.jst.server.sunappsrv.v3;

import java.net.URL;

import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.ControlListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.browser.IWebBrowser;
import org.eclipse.ui.browser.IWorkbenchBrowserSupport;
import org.eclipse.ui.part.MultiPageEditorPart;

import com.sun.enterprise.jst.server.sunappsrv.AdminURLHelper;
import com.sun.enterprise.jst.server.sunappsrv.Messages;
import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;
import com.sun.enterprise.jst.server.sunappsrv.handlers.AppServerContextAction;

/**
 * @author Ludo
 *
 */
/**
 * V3 register admin gui page
 *
 */
public class Register extends MultiPageEditorPart implements IResourceChangeListener {

    private Browser browser;
    Composite parent;
    String URLtoShow;
    String pageTextTitle;

    /**
     *
     */
    public Register() {
        this("/sysnet/registration.jsf", Messages.register);
    }

    protected Register(String urlSuffix, String pageText) {
        super();
        ResourcesPlugin.getWorkspace().addResourceChangeListener(this);
        SunAppSrvPlugin.logMessage(this.getClass() + " Action...with "+AppServerContextAction.selectedServer);
        URLtoShow = AdminURLHelper.getURL(urlSuffix, AppServerContextAction.selectedServer);
        pageTextTitle = pageText;
    }

    public String getURL() {
    	return URLtoShow;
    }

    /**
     * Creates page 1 of the multi-page editor,
     * which allows you to change the font used in page 2.
     */
    void createPage1() {
        parent = getContainer();
        // workaround for eclipse bug 77217
        // OpenSolaris doesn't have internal browser set up properly
        if ("SunOS".equals(System.getProperty("os.name"))) {
    		IWorkbenchBrowserSupport browserSupport = PlatformUI.getWorkbench().getBrowserSupport();
    		IWebBrowser externalBrowser;
			try {
				externalBrowser = browserSupport.createBrowser(IWorkbenchBrowserSupport.LOCATION_BAR | IWorkbenchBrowserSupport.NAVIGATION_BAR, null, null, null);
		   		externalBrowser.openURL(new URL(URLtoShow));
		   	 } catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
        } else { // end workaround

        browser = new Browser(getContainer(), SWT.NONE);
        browser.setUrl(URLtoShow);

        parent.addControlListener(new ControlListener() {

            public void controlMoved(ControlEvent arg0) {
                Point c = parent.computeSize(SWT.DEFAULT, SWT.DEFAULT);
                browser.setSize(c.x, c.y);
            }

            public void controlResized(ControlEvent arg0) {
                Point c = parent.computeSize(SWT.DEFAULT, SWT.DEFAULT);
                browser.setSize(c.x, c.y);
            }
        });
        int index = addPage(browser);
        setPageText(index, pageTextTitle);
        }
    }

    /**
     * Creates the pages of the multi-page editor.
     */
    protected void createPages() {
        createPage1();
    }

    /**
     * The <code>MultiPageEditorPart</code> implementation of this
     * <code>IWorkbenchPart</code> method disposes all nested editors.
     * Subclasses may extend.
     */
    public void dispose() {
        ResourcesPlugin.getWorkspace().removeResourceChangeListener(this);
        super.dispose();
    }

    /**
     * The <code>MultiPageEditorExample</code> implementation of this method
     * checks that the input is an instance of <code>IFileEditorInput</code>.
     */
    public void init(IEditorSite site, IEditorInput editorInput)
            throws PartInitException {
        if (!(editorInput instanceof IFileEditorInput)) {
            throw new PartInitException("Invalid Input: Must be IFileEditorInput");
        }
        super.init(site, editorInput);
    }

    public boolean isSaveAsAllowed() {
        return false;
    }

    @Override
    public void doSave(IProgressMonitor arg0) {
        // TODO Auto-generated method stub
    }

    @Override
    public void doSaveAs() {
        // TODO Auto-generated method stub
    }

    public void resourceChanged(IResourceChangeEvent arg0) {
        // TODO Auto-generated method stub
    }
}
