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

import org.eclipse.jface.action.IAction;

import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.ide.IDEActionFactory;
import org.eclipse.ui.part.MultiPageEditorActionBarContributor;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.ITextEditorActionConstants;


/**
 * @author Ludo
 *
 */


public class BrowserEditorContributor extends MultiPageEditorActionBarContributor {

    private IEditorPart activeEditorPart;

    /**
     * Creates a multi-page contributor.
     */
    public BrowserEditorContributor() {
        super();
    }

    /**
     * Returns the action registered with the given text editor.
     * @return IAction or null if editor is null.
     */
    protected IAction getAction(ITextEditor editor, String actionID) {
        return (editor == null ? null : editor.getAction(actionID));
    }

    public void setActivePage(IEditorPart part) {
        if (activeEditorPart == part) {
            return;
        }

        activeEditorPart = part;

        IActionBars actionBars = getActionBars();
        if (actionBars != null) {

            ITextEditor editor = (part instanceof ITextEditor) ? (ITextEditor) part : null;

            actionBars.setGlobalActionHandler(
                    ActionFactory.DELETE.getId(),
                    getAction(editor, ITextEditorActionConstants.DELETE));
            actionBars.setGlobalActionHandler(
                    ActionFactory.UNDO.getId(),
                    getAction(editor, ITextEditorActionConstants.UNDO));
            actionBars.setGlobalActionHandler(
                    ActionFactory.REDO.getId(),
                    getAction(editor, ITextEditorActionConstants.REDO));
            actionBars.setGlobalActionHandler(
                    ActionFactory.CUT.getId(),
                    getAction(editor, ITextEditorActionConstants.CUT));
            actionBars.setGlobalActionHandler(
                    ActionFactory.COPY.getId(),
                    getAction(editor, ITextEditorActionConstants.COPY));
            actionBars.setGlobalActionHandler(
                    ActionFactory.PASTE.getId(),
                    getAction(editor, ITextEditorActionConstants.PASTE));
            actionBars.setGlobalActionHandler(
                    ActionFactory.SELECT_ALL.getId(),
                    getAction(editor, ITextEditorActionConstants.SELECT_ALL));
            actionBars.setGlobalActionHandler(
                    ActionFactory.FIND.getId(),
                    getAction(editor, ITextEditorActionConstants.FIND));
            actionBars.setGlobalActionHandler(
                    IDEActionFactory.BOOKMARK.getId(),
                    getAction(editor, IDEActionFactory.BOOKMARK.getId()));
            actionBars.updateActionBars();
        }
    }
}
