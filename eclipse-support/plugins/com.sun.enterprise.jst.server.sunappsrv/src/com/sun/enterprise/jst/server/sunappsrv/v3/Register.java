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
package com.sun.enterprise.jst.server.sunappsrv.v3;

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
import org.eclipse.ui.part.MultiPageEditorPart;

import com.sun.enterprise.jst.server.sunappsrv.Messages;
import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;
import com.sun.enterprise.jst.server.sunappsrv.AdminURLHelper;
import com.sun.enterprise.jst.server.sunappsrv.actions.AppServerContextAction;

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

    /**
     * Creates page 1 of the multi-page editor,
     * which allows you to change the font used in page 2.
     */
    void createPage1() {
        parent = getContainer();

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
