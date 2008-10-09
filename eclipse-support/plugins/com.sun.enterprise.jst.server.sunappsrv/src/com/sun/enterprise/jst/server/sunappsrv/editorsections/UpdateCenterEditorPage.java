// <editor-fold defaultstate="collapsed" desc="CDDL Licence">
/*
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * glassfishplugins/www/license/CDDLv1.0.txt or
 * https://glassfish.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * glassfishplugins/www/license/CDDLv1.0.txt.  If applicable,
 * add the following below this CDDL HEADER, with the
 * fields enclosed by brackets "[]" replaced with your
 * own identifying information: Portions Copyright [yyyy]
 * [name of copyright owner]
 */
// </editor-fold>
package com.sun.enterprise.jst.server.sunappsrv.editorsections;

import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.ControlListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.wst.server.ui.editor.ServerEditorPart;

import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;

/**
 *
 * @author ludo
 */
public class UpdateCenterEditorPage extends ServerEditorPart {

    String URLtoShow = "http://localhost:4848/updateCenter/addOn.jsf";
    Browser browser;

    @Override
    public void createPartControl(Composite arg0) {
        SunAppSrvPlugin.logMessage("createPartControl ");
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

    @Override
    public void setFocus() {
        SunAppSrvPlugin.logMessage("createPartControl ");

    }
}
