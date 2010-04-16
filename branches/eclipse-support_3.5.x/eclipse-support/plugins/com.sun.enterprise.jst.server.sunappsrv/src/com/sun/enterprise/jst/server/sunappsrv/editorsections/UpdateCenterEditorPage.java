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
