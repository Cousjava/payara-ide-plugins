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
