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

package com.sun.enterprise.jst.server.sunappsrv.editorsections;

import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.ControlListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.wst.server.core.IServer;
import org.eclipse.wst.server.core.IServerListener;
import org.eclipse.wst.server.core.ServerEvent;
import org.eclipse.wst.server.ui.editor.ServerEditorPart;
import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;
import com.sun.enterprise.jst.server.sunappsrv.AdminURLHelper;

/**
 *
 * @author ludo
 */
public class UpdateCenterEditorPage extends ServerEditorPart {

    String URLtoShow;
    Browser browser;

    /**
     *
     */
    public UpdateCenterEditorPage() {
    }

    IServerListener listener = new IServerListener() {
        public void serverChanged(ServerEvent event) {
            int eventKind = event.getKind();
            IServer eventServer = event.getServer();
            if (eventKind == (ServerEvent.SERVER_CHANGE | ServerEvent.STATE_CHANGE)) {
                int eventState = eventServer.getServerState();
                if (eventState == IServer.STATE_STARTED) {
                    refreshBrowser(true);
                } else if (eventState == IServer.STATE_STOPPED) {
                    refreshBrowser(false);
                }
            }
        }
    };

    private void refreshBrowser(final boolean isRunning) {
        Display.getDefault().asyncExec(new Runnable() {
            public void run() {
                if (browser != null) {
                    if (isRunning) {
                        SunAppSrvPlugin.logMessage("refreshing browser to " + URLtoShow);
                        browser.setUrl(URLtoShow);
                        browser.refresh();
                    } else { // was running before, now is not
                        // TODO - show blank page or a message here or disable
                        SunAppSrvPlugin.logMessage("server is stopped - could refresh browser non UC url");
                    }
                }
            }
        });
    }

    public void init(IEditorSite site, IEditorInput input) {
        super.init(site, input);
        IServer iserver = server.getOriginal();
        iserver.addServerListener(listener);
        URLtoShow = AdminURLHelper.getURL("/updateCenter/addOn.jsf", iserver);
    }

    @Override
    public void createPartControl(Composite arg0) {
        SunAppSrvPlugin.logMessage("createPartControl ");
        // workaround for eclipse bug 77217
        // OpenSolaris doesn't have internal browser set up properly, but it doesn't make sense
        // to show an editor tab in the external browser like for actions
        if ("SunOS".equals(System.getProperty("os.name"))) {
            SunAppSrvPlugin.logMessage("Can't show UC in tab for Open Solaris");
    		/*IWorkbenchBrowserSupport browserSupport = ServerUIPlugin.getInstance().getWorkbench().getBrowserSupport();
    		IWebBrowser externalBrowser;
			try {
				externalBrowser = browserSupport.createBrowser(IWorkbenchBrowserSupport.LOCATION_BAR | IWorkbenchBrowserSupport.NAVIGATION_BAR, null, null, null);
		   		externalBrowser.openURL(new URL(URLtoShow));
		   	 } catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}*/
        } else { // end workaround
        browser = new Browser(arg0, SWT.NONE);
        browser.setUrl(URLtoShow);
        final Composite parent = arg0;
        arg0.addControlListener(new ControlListener() {

            public void controlMoved(ControlEvent arg0) {
                Point c = parent.computeSize(SWT.DEFAULT, SWT.DEFAULT);
                browser.setSize(c.x, c.y);
            }

            public void controlResized(ControlEvent arg0) {
                Point c = parent.computeSize(SWT.DEFAULT, SWT.DEFAULT);
                browser.setSize(c.x, c.y);
            }
        });
        }
    }

    /* (non-Javadoc)
     * @see org.eclipse.wst.server.ui.editor.ServerEditorPart#dispose()
     */
    @Override
    public void dispose() {
        server.getOriginal().removeServerListener(listener);
        super.dispose();
    }

    @Override
    public void setFocus() {
        SunAppSrvPlugin.logMessage("setFocus ");
    }
}
