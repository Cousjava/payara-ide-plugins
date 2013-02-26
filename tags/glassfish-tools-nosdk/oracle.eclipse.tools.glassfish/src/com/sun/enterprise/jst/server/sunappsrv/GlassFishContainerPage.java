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


package com.sun.enterprise.jst.server.sunappsrv;

import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.ui.wizards.IClasspathContainerPage;
import org.eclipse.jdt.ui.wizards.IClasspathContainerPageExtension;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.wst.server.core.IRuntime;
import org.eclipse.wst.server.core.IServer;
import org.eclipse.wst.server.core.ServerCore;


public class GlassFishContainerPage extends WizardPage implements
IClasspathContainerPage, IClasspathContainerPageExtension{
	String s="";
	public GlassFishContainerPage() {
		super("GlassFishLibraries");


		setTitle("GlassFish  Libraries");
		setDescription("GlassFish Libraries edition page");

		
		IRuntime[] runtimes = ServerCore.getRuntimes();
		int size = runtimes.length;
		for (int i = 0; i < size; i++) {
			if (runtimes[i].getRuntimeType() != null) {
				
		        SunAppServer sab1 = (SunAppServer) runtimes[i].loadAdapter(
		                SunAppServer.class, null);
		        
			    SunAppSrvPlugin.logMessage("in sab1  "+sab1);

		        SunAppServerBehaviour sab = (SunAppServerBehaviour) runtimes[i].loadAdapter(
		                SunAppServerBehaviour.class, null);
			    SunAppSrvPlugin.logMessage("in sab  "+sab);
		        
				s=s+ "--"+runtimes[i].getLocation()+runtimes[i].getId();
				
				/*
				RuntimeClasspathProviderWrapper rcpw = JavaServerPlugin.findRuntimeClasspathProvider(runtimes[i].getRuntimeType());
				if (rcpw != null) {
					IPath serverContainerPath = new Path(RuntimeClasspathContainer.SERVER_CONTAINER)
							.append(rcpw.getId()).append(runtimes[i].getId());
					runtimeMap.put(runtimes[i], JavaCore.newContainerEntry(serverContainerPath));
					
					
					*/
				IServer[] ss=ServerCore.getServers();
				int aa = ss.length;
				for (int ii = 0; ii < aa; ii++) {
			         sab1 = (SunAppServer) ss[ii].loadAdapter(
			                SunAppServer.class, null);
			        
				    SunAppSrvPlugin.logMessage("777in sab1  "+sab1);

			         sab = (SunAppServerBehaviour) ss[ii].loadAdapter(
			                SunAppServerBehaviour.class, null);
				    SunAppSrvPlugin.logMessage("777in sab  "+sab);					
				}
			}
		}		
		
		
		
	}

	public void createControl(Composite arg0) {
		Composite top = new Composite(arg0, SWT.NONE);
		GridLayout layout = new GridLayout(1, true);
		top.setLayout(layout);

		Label lbl = new Label(top, SWT.NONE);
		lbl.setText(s);
		GridData gd = new GridData(GridData.FILL_HORIZONTAL);
		lbl.setLayoutData(gd);

		this.setControl(top);

	}

	public boolean finish() {
		// TODO Auto-generated method stub
		return true;
	}

	public IClasspathEntry getSelection() {

		IClasspathEntry cpe =JavaCore.newContainerEntry(
			    new Path("GlassFishLibraries"), //
			    false); //not exported			
		return cpe;
	}

	public void setSelection(IClasspathEntry arg0) {

	}

	public void initialize(IJavaProject arg0, IClasspathEntry[] arg1) {
	
	}

}
