/*
 * Copyright (c) 2011 Oracle and/or its affiliates. All rights reserved.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Oracle
 */

package com.sun.enterprise.jst.server.sunappsrv.serverview;


import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.swt.widgets.Event;
import org.eclipse.ui.navigator.CommonActionProvider;
import org.eclipse.ui.navigator.CommonViewer;
import org.eclipse.ui.navigator.ICommonActionExtensionSite;
import org.eclipse.ui.navigator.ICommonViewerSite;
import org.eclipse.ui.navigator.ICommonViewerWorkbenchSite;

public abstract class GenericActionProvider extends CommonActionProvider{
	Action refreshAction ;
	protected ICommonActionExtensionSite actionSite;
	
	public GenericActionProvider() {
	}

	public void init(ICommonActionExtensionSite aSite) {
		super.init(aSite);
		this.actionSite = aSite;
		ICommonViewerSite site = aSite.getViewSite();
		if( site instanceof ICommonViewerWorkbenchSite ) {
			StructuredViewer v = aSite.getStructuredViewer();
			if( v instanceof CommonViewer ) {
				CommonViewer cv = (CommonViewer)v;
				ICommonViewerWorkbenchSite wsSite = (ICommonViewerWorkbenchSite)site;
				makeActions(cv, wsSite.getSelectionProvider());
			}
		}
	}
	
	private void makeActions(CommonViewer cv,
			ISelectionProvider selectionProvider) {
		ISelection sel =  selectionProvider.getSelection();
		refreshAction = new RefreshAction( sel );		
	}



	@Override
	public void fillContextMenu(IMenuManager menu) {
		super.fillContextMenu(menu);
	//	menu.removeAll();
		
		ICommonViewerSite site = actionSite.getViewSite();
		IStructuredSelection selection = null;
		if( site instanceof ICommonViewerWorkbenchSite ) {
			ICommonViewerWorkbenchSite wsSite = (ICommonViewerWorkbenchSite)site;
			selection = (IStructuredSelection) wsSite.getSelectionProvider().getSelection();
			refreshAction = new RefreshAction( selection );		
			menu.add(refreshAction);
			menu.add(new Separator());
		}
	}

	/**
	 * @return the ID of the common navigator content extension 
	 */
	abstract protected String getExtensionId() ;
	
	protected void refresh( Object selection ){
		
	}
	
	


	class RefreshAction extends Action{
		ISelection selection ;
		public RefreshAction(ISelection selection ) {
					setText("Refresh");
	//ludo		setImageDescriptor(...todo);
			this.selection = selection ;
		}

		public void runWithEvent(Event event) {
			if( selection instanceof TreeSelection ){
				TreeSelection ts = (TreeSelection)selection;
				Object obj = ts.getFirstElement();
				refresh( obj );
				StructuredViewer view = actionSite.getStructuredViewer();
				view.refresh(obj);
			}
			super.run();
		}		
		
		@Override
		public void run() {
			this.runWithEvent(null);
		}

	}
	

	

	
	
}
