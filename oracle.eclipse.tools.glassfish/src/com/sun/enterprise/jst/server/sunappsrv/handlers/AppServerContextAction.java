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


package com.sun.enterprise.jst.server.sunappsrv.handlers;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Iterator;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IViewActionDelegate;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.SelectionProviderAction;
import org.eclipse.ui.browser.IWebBrowser;
import org.eclipse.ui.browser.IWorkbenchBrowserSupport;
import org.eclipse.wst.server.core.IServer;

import com.sun.enterprise.jst.server.sunappsrv.SunAppServerBehaviour;
import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;

/**
 * @author Ludo
 *
 */
public class AppServerContextAction extends SelectionProviderAction implements IObjectActionDelegate ,IViewActionDelegate,
IWorkbenchWindowActionDelegate {

	private IWorkbenchPart targetPart;
	private IViewPart viewPArt;
	public static IServer selectedServer;
	public Shell shell;
	public AppServerContextAction() {
		this("",null);
		
	}
		public AppServerContextAction(String name, ImageDescriptor image) {
		super(getSelectionProvider("org.eclipse.wst.server.ui.ServersView"), name);

		viewPArt = getView("org.eclipse.wst.server.ui.ServersView");
		if (viewPArt==null){
			return;
		}
			IViewSite w= viewPArt.getViewSite();
			IActionBars a= w.getActionBars();
			IToolBarManager tb = a.getToolBarManager();
			///SunAppSrvPlugin.logMessage("IToolBarManager i " +tb);
		/*	IContributionItem [] ici =tb.getItems();
			for (IContributionItem item :ici){
				///SunAppSrvPlugin.logMessage("item i " +item.getId());
			}*/

		this.shell = getView("org.eclipse.wst.server.ui.ServersView").getViewSite().getShell();
		setEnabled(true);
		
		if (image==null){
			image = getImageDescriptorFromlocalImage("icons/obj16/sunappsrv.gif");
		}

		this.setImageDescriptor(image);	
		tb.add(this);
		tb.update(true);

	}
		
		public static ImageDescriptor getImageDescriptorFromlocalImage(String localPath){
			URL url = null;
			try {
				url = new URL(SunAppSrvPlugin.getInstance().getDescriptor().getInstallURL(),
						localPath);
			} catch (MalformedURLException e) {
			}

			return ImageDescriptor.createFromURL(url);			
		}
		
		
		
	public AppServerContextAction(Shell shell, ISelectionProvider selectionProvider, String text) {
		super(selectionProvider, text);
		this.shell = getView("org.eclipse.wst.server.ui.ServersView").getViewSite().getShell();
		this.setImageDescriptor(getImageDescriptorFromlocalImage("icons/obj16/sunappsrv.gif"));	
		
	}


	public static ISelectionProvider getSelectionProvider(String id) {
		IViewPart vp =getView( id);
		if (vp!=null){
			return vp.getViewSite().getSelectionProvider();
		}
		return null;
	
	}
		public static IViewPart getView(String id) {
		try{
		IViewReference viewReferences[] = PlatformUI.getWorkbench()
		.getActiveWorkbenchWindow().getActivePage().getViewReferences();
		for (int i = 0; i < viewReferences.length; i++) {
			if (id.equals(viewReferences[i].getId())) {
				return viewReferences[i].getView(false);
			}
		}
		}
		catch (Exception e)
		{
			return null;
		}
		return null;
	}	

	public void setActivePart(IAction action, IWorkbenchPart targetPart) {
		this.targetPart = targetPart;
	}

	/**
	 * @see org.eclipse.ui.IActionDelegate#run(IAction)
	 */
	public void run(IAction action) {
		if (targetPart==null){
			targetPart = viewPArt;
		}

		SunAppServerBehaviour sab = (SunAppServerBehaviour) selectedServer.loadAdapter(
				SunAppServerBehaviour.class, null);
		SunAppSrvPlugin.logMessage("Message is:" + action + sab.getDomainDir() + selectedServer );
		perform(selectedServer);
	}


	/**
	 * Return true if this server can currently be acted on.
	 *
	 * @return boolean
	 * @param server org.eclipse.wst.server.core.IServer
	 */
	public boolean accept(IServer server) {
		return true;
	}

	protected boolean acceptIfServerRunning(IServer server) {
		SunAppServerBehaviour sab = (SunAppServerBehaviour) server.loadAdapter(
				SunAppServerBehaviour.class, null);
		if (sab != null) {
			try {
				return sab.getSunAppServer().isRunning();
			} catch (CoreException e) {
				return false;
			}
		}
		return false;
	}

	/**
	 * Perform action on this server.
	 * @param server org.eclipse.wst.server.core.IServer
	 */
	public void perform(IServer server) {
		
		SunAppSrvPlugin.logMessage(">>>>>>execute(IServer server) called on " +server);

	}

	public void run() {
		Iterator iterator = getStructuredSelection().iterator();
		Object obj = iterator.next();
		if (obj instanceof IServer) {
			IServer server = (IServer) obj;
			if (accept(server))
				perform(server);
			selectionChanged(getStructuredSelection());
		}
	}

	/**
	 * @see org.eclipse.ui.IActionDelegate#selectionChanged(IAction, ISelection)
	 */
	public void selectionChanged(IAction action, ISelection selection) {
		//selectedServer = null;
		if (!selection.isEmpty()) {
			if (selection instanceof IStructuredSelection) {
				Object obj = ((IStructuredSelection) selection).getFirstElement();
				if (obj instanceof IServer) {
					selectedServer = (IServer) obj;
					boolean isActionAccepted = accept(selectedServer);
					setEnabled(isActionAccepted);
					action.setEnabled(isActionAccepted);
					return;
				}
			}
		}
		setEnabled(false);

	}	
	/**
	 * Update the enable state.
	 * 
	 * @param sel a selection
	 */
	public void selectionChanged(IStructuredSelection sel) {
		if (sel.isEmpty()) {
			setEnabled(false);
			return;
		}
		boolean enabled = false;
		Iterator iterator = sel.iterator();
		while (iterator.hasNext()) {
			Object obj = iterator.next();
			if (obj instanceof IServer) {
				IServer server = (IServer) obj;
				if (accept(server))
					enabled = true;
			} else {
				setEnabled(false);
				return;
			}
		}
		setEnabled(enabled);	
	}

	protected void showMessageDialog(){
		showMessageDialog("GlassFish Server has to be up and running...\nPlease start the server.");
	}

	protected void showMessageDialog(String msg){
		MessageDialog message;
		Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
		String labels[] = new String[1];
		labels[0] = "OK";
		message = new MessageDialog(shell, "Cannot Execute this action", null,
				msg, 2, labels, 1);
		message.open();
	}
	public void init(IViewPart arg0) {
		//selectionChanged(getSelectionProvider().getSelection());
		
	}
	public void init(IWorkbenchWindow arg0) {
		//selectionChanged(getSelectionProvider().getSelection());
	}

 	protected void showPageInDefaultBrowser(String url) throws PartInitException, MalformedURLException {
		IWorkbenchBrowserSupport browserSupport = PlatformUI.getWorkbench().getBrowserSupport();
		IWebBrowser browser = browserSupport.createBrowser(IWorkbenchBrowserSupport.LOCATION_BAR | IWorkbenchBrowserSupport.NAVIGATION_BAR, null, null, null);
		browser.openURL(new URL(url));
	}

}
