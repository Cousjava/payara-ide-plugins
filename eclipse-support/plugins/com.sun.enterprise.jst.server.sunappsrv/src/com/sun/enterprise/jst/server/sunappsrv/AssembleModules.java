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

			
import java.io.File;
import java.io.IOException;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.jst.server.core.IEnterpriseApplication;
import org.eclipse.jst.server.core.IJ2EEModule;
import org.eclipse.jst.server.core.IWebModule;
import org.eclipse.jst.server.generic.core.internal.CorePlugin;
import org.eclipse.wst.server.core.IModule;
import org.eclipse.wst.server.core.internal.Server;
import org.eclipse.wst.server.core.internal.ServerPlugin;
import org.eclipse.wst.server.core.model.IModuleFolder;
import org.eclipse.wst.server.core.model.IModuleResource;
import org.eclipse.wst.server.core.model.IModuleResourceDelta;
import org.eclipse.wst.server.core.util.ProjectModule;
import org.eclipse.wst.server.core.util.PublishHelper;
import org.eclipse.jst.server.generic.core.internal.publishers.ModulePackager;

/* assemble modules (i.e if a web app depends on a utility lib, we need to create the jar file for this utility and
 * put it in the web-inf/lib area of the web app.
 * Later for v3 we need to also handle ear files
 */
public  class AssembleModules {

	protected IModule module; 
	protected IPath assembleRoot;
	protected PublishHelper publishHelper;
	protected SunAppServer server;

	
	protected AssembleModules(IModule module, IPath assembleRoot, SunAppServer server)
	{
		this.module=module;
		this.assembleRoot = assembleRoot;
		this.server =server;
		publishHelper = new PublishHelper(null);
        SunAppSrvPlugin.logMessage("AssembleModules assembleRoot="+assembleRoot);
/*		if(isModuleType(module, "jst.web")) {
			
		}
		if(isModuleType(module, "jst.ear")) {
			
		}
		*/

	}
	

	public IPath assembleWebModule(IProgressMonitor monitor) throws CoreException{
		IPath parent =copyModule(module,monitor);
		IWebModule webModule = (IWebModule)module.loadAdapter(IWebModule.class, monitor);
		IModule[] childModules = webModule.getModules();
		for (int i = 0; i < childModules.length; i++) {
			IModule module = childModules[i];
			packModule(module, webModule.getURI(module), parent);
		}
		return parent;
	}
	
		
	public static boolean isModuleType(IModule module, String moduleTypeId){	
			if(module.getModuleType()!=null && moduleTypeId.equals(module.getModuleType().getId())){
				return true;
			}
			return false;
	}
	
	
	protected void packModule(IModule module, String deploymentUnitName, IPath destination)throws CoreException {
		
		String dest = destination.append(deploymentUnitName).toString();
        SunAppSrvPlugin.logMessage("AssembleModules dest="+dest);

		ModulePackager packager = null;
		try {
			packager = new ModulePackager(dest,false);
			ProjectModule pm = (ProjectModule) module.loadAdapter(ProjectModule.class, null);
			IModuleResource[] resources = pm.members();
			for (int i = 0; i < resources.length; i++) {
		        SunAppSrvPlugin.logMessage("AssembleModules resources="+resources[i]);

				doPackModule(resources[i], packager);
			}
		} catch (IOException e) {
			IStatus status = new Status(IStatus.ERROR, SunAppSrvPlugin.SUNPLUGIN_ID, 0,
					"unable to assemble module", e); //$NON-NLS-1$
			throw new CoreException(status);
		}
		finally{
			try{
				packager.finished();
			}
			catch(Exception e){
			}
		}
	}

	private void doPackModule(IModuleResource resource, ModulePackager packager) throws CoreException, IOException{
			if (resource instanceof IModuleFolder) {
				IModuleFolder mFolder = (IModuleFolder)resource;
				IModuleResource[] resources = mFolder.members();
		        SunAppSrvPlugin.logMessage("AssembleModules IModuleFolder="+mFolder);

				packager.writeFolder(resource.getModuleRelativePath().append(resource.getName()).toPortableString());

				for (int i = 0; resources!= null && i < resources.length; i++) {
			        SunAppSrvPlugin.logMessage("AssembleModules resources[i]="+resources[i]);

					doPackModule(resources[i], packager);
				}
			} else {
				String destination = resource.getModuleRelativePath().append(resource.getName()).toPortableString();
				IFile file = (IFile) resource.getAdapter(IFile.class);
				if (file != null){
					packager.write(file, destination);
				}
				else {
					File file2 = (File) resource.getAdapter(File.class);
					packager.write(file2, destination);
				}
			}
	}

	protected IPath copyModule(IModule module, IProgressMonitor monitor) throws CoreException {
		ProjectModule pm =(ProjectModule)module.loadAdapter(ProjectModule.class, monitor);
		IStatus[] status = publishHelper.publishSmart(pm.members(), assembleRoot, monitor);
		if (status != null && status.length > 0)
			throw new CoreException(status[0]);
		return assembleRoot;
	}
	
	/* not used for now... Would be ejb module when v3 has them
	 * 
	 */
	public IPath assembleNonWebOrNonEARModule(IProgressMonitor monitor) throws CoreException {
		return copyModule(module,monitor);		
	}
	
	public IPath assembleEARModule(IProgressMonitor monitor) throws CoreException{
	/*	//copy ear root to the temporary assembly directory
		IPath parent =copyModule(module,monitor);
		IEnterpriseApplication earModule = (IEnterpriseApplication)module.loadAdapter(IEnterpriseApplication.class, monitor);
		IModule[] childModules = earModule.getModules();
		for (int i = 0; i < childModules.length; i++) {
			IModule module = childModules[i];
			String uri = earModule.getURI(module);
			if(uri==null){ 
				IStatus status = new Status(IStatus.ERROR, CorePlugin.PLUGIN_ID, 0,	"unable to assemble module null uri",null ); //$NON-NLS-1$
				throw new CoreException(status);
			}
			IJ2EEModule jeeModule = (IJ2EEModule) module.loadAdapter(IJ2EEModule.class,monitor);
			if( jeeModule != null && jeeModule.isBinary() ){//Binary module just copy
				ProjectModule pm = (ProjectModule) module.loadAdapter(ProjectModule.class, null);
				IModuleResource[] resources = pm.members();
				publishHelper.publishFull(resources, parent, monitor);
				continue;//done! no need to go further
			}
			if( shouldRepack( module ) ){	
				packModuleEARModule(module,uri, parent);
            }
		}
		return parent;
		*/
		return null;
	}
	/**
     * Checks if there has been a change in the published resources.
     * @param module
     * @return module changed
	 */
	private boolean shouldRepack( IModule module ) {
        final Server _server = (Server) server.getServer();
        final IModule[] modules ={module}; 
        IModuleResourceDelta[] deltas = _server.getPublishedResourceDelta( modules );
        return deltas.length > 0;
    }

   protected void packModuleEARModule(IModule module, String deploymentUnitName, IPath destination) throws CoreException {
	/*	if(module.getModuleType().getId().equals("jst.web")) //$NON-NLS-1$
		{
			AbstractModuleAssembler assembler= AbstractModuleAssembler.Factory.getModuleAssembler(module, server);
			IPath webAppPath = assembler.assemble(new NullProgressMonitor());
			String realDestination = destination.append(deploymentUnitName).toString();
			ModulePackager packager=null;
			try {
				packager =new ModulePackager(realDestination);
				packager.pack(webAppPath.toFile(),webAppPath.toOSString());
			
			} catch (IOException e) {
				IStatus status = new Status(IStatus.ERROR, CorePlugin.PLUGIN_ID, 0,
						"unable to assemble module", e); //$NON-NLS-1$
				throw new CoreException(status);
			}
			finally{
				if(packager!=null) {					
					try {
						packager.finished();
					} catch (IOException e) {
					}
				}				
			}			
			
		}
		else
		{
			super.packModule(module, deploymentUnitName, destination);
		}
		*/
	}
}
