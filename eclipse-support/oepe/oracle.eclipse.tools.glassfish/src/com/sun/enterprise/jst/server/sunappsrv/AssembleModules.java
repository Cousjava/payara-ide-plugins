/*
 * Copyright (c) 1997-2011 Oracle and/or its affiliates. All rights reserved.
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


import java.io.File;
import java.io.IOException;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.jst.server.core.IEnterpriseApplication;
import org.eclipse.jst.server.core.IJ2EEModule;
import org.eclipse.jst.server.core.IWebModule;
import org.eclipse.jst.server.generic.core.internal.CorePlugin;
import org.eclipse.jst.server.generic.core.internal.publishers.ModulePackager;
import org.eclipse.wst.server.core.IModule;
import org.eclipse.wst.server.core.IServer;
import org.eclipse.wst.server.core.internal.Server;
import org.eclipse.wst.server.core.model.IModuleFolder;
import org.eclipse.wst.server.core.model.IModuleResource;
import org.eclipse.wst.server.core.model.IModuleResourceDelta;
import org.eclipse.wst.server.core.util.ProjectModule;
import org.eclipse.wst.server.core.util.PublishHelper;

/* assemble modules (i.e if a web app depends on a utility lib, we need to create the jar file for this utility and
 * put it in the web-inf/lib area of the web app.
 * Later for v3 we need to also handle ear files
 */
@SuppressWarnings("restriction")
public  class AssembleModules {

	protected IModule module;
	protected IPath assembleRoot;
	protected PublishHelper publishHelper;
	protected SunAppServer server;
	protected boolean childNeedsARedeployment=false;

	protected AssembleModules(IModule module, IPath assembleRoot, SunAppServer server , PublishHelper helper)
	{
		this.module=module;
		this.assembleRoot = assembleRoot;
		this.server =server;
		this.publishHelper = helper;
        SunAppSrvPlugin.logMessage("AssembleModules assembleRoot="+assembleRoot);


	}


	public IPath assembleWebModule(IProgressMonitor monitor) throws CoreException{

		IPath parent = assembleRoot;

		final IModule[] rootMod = { module };
		boolean shouldCopy = (IServer.PUBLISH_STATE_NONE != server.getServer().getModulePublishState(rootMod));
		if (shouldCopy)
			copyModule(module, monitor);
		
		
		IWebModule webModule = (IWebModule)module.loadAdapter(IWebModule.class, monitor);
		IModule[] childModules = webModule.getModules();
		for (int i = 0; i < childModules.length; i++) {
			IModule module = childModules[i];
			//packModule(module, webModule.getURI(module), parent);
			String uri = webModule.getURI(module);
			if (uri == null) { // The bad memories of WTP 1.0
				IStatus status = new Status(IStatus.ERROR, CorePlugin.PLUGIN_ID, 0, "unable to assemble module null uri", null); //$NON-NLS-1$
				throw new CoreException(status);
			}
			IJ2EEModule jeeModule = (IJ2EEModule) module.loadAdapter(IJ2EEModule.class, monitor);
			if (jeeModule != null && jeeModule.isBinary()) { // Binary module
				ProjectModule pm = (ProjectModule) module.loadAdapter(ProjectModule.class, null);
				IModuleResource[] resources = pm.members();
				publishHelper.publishToPath(resources, parent.append(uri), monitor);
			}
			else { // Project module
			//ludo 2010	packModule(module, uri, parent);
				String version= server.getVersion3Only();
				if (version.indexOf(" 3.1")==-1){
					packModule(module, uri, parent);
				}else{				

					if( shouldRepack( module ) ){
						AssembleModules assembler= new AssembleModules(module, assembleRoot.append(uri),server , publishHelper);
						childNeedsARedeployment = (childNeedsARedeployment||assembler.needsARedeployment());
						assembler.copyModule(module,monitor);
					}	
				}				
			}
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
		        SunAppSrvPlugin.logMessage("AssembleModules  doPackModule IModuleFolder="+mFolder);
		        SunAppSrvPlugin.logMessage("AssembleModules  doPackModule resource.getModuleRelativePath()="+resource.getModuleRelativePath());
		        SunAppSrvPlugin.logMessage("AssembleModules  resource.getModuleRelativePath().append(resource.getName()).toPortableString()="+resource.getModuleRelativePath().append(resource.getName()).toPortableString());

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
        //SunAppSrvPlugin.logMessage("AssembleModules copyModule ProjectModule is="+pm);
		IPath [] jarPaths = null;
		if(module.getModuleType().getId().equals("jst.web")) {//$NON-NLS-1$
			//IModuleResource[] mr = getResources(module);
			IWebModule webModule = (IWebModule)module.loadAdapter(IWebModule.class, monitor);
			IModule [] childModules = webModule.getModules();
			if (childModules != null && childModules.length > 0) {
				jarPaths = new IPath[childModules.length];
				for (int i = 0; i < childModules.length; i++) {
					if (webModule != null) {
						jarPaths[i] = new Path(webModule.getURI(childModules[i]));
					}
					else {
						IJ2EEModule childModule = (IJ2EEModule)childModules[i].loadAdapter(IJ2EEModule.class, monitor);
						if (childModule != null && childModule.isBinary()) {
							jarPaths[i] = new Path("WEB-INF/lib").append(childModules[i].getName());
						}
						else {
							jarPaths[i] = new Path("WEB-INF/lib").append(childModules[i].getName() + ".jar");
						}
					}
				}
			}			
			
		}
		IStatus[] status = publishHelper.publishSmart(pm.members(), assembleRoot,jarPaths, monitor);
		if (status != null && status.length > 0){
			// no need to emit an error like CoreException(status[0]); just log in the entry
			// see https://glassfishplugins.dev.java.net/issues/show_bug.cgi?id=268
			for (int i=0;i<status.length;i++){
				SunAppSrvPlugin.logMessage("warning copying module: "+status[i].getMessage());
			}
		}

		return assembleRoot;
	}

	/* not used for now... Would be ejb module when v3 has them
	 *
	 */
	public IPath assembleNonWebOrNonEARModule(IProgressMonitor monitor) throws CoreException {
		return copyModule(module,monitor);
	}

	public IPath assembleEARModule(IProgressMonitor monitor) throws CoreException{
		//copy ear root to the temporary assembly directory
		IPath parent = assembleRoot;

		final IModule[] rootMod = { module };
		boolean shouldCopy = (IServer.PUBLISH_STATE_NONE != server.getServer().getModulePublishState(rootMod));
		if (shouldCopy)
			copyModule(module, monitor);
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
				//ludo nbew publishHelper.publishFull(resources, parent, monitor);
				//
				//
				//
				publishHelper.publishToPath(resources, parent.append(uri), monitor);
				//
				//
				//
				//


				continue;//done! no need to go further
			}
			if( shouldRepack( module ) ){
				packModuleEARModule(module,uri, parent);
            }
		}
		return parent;


	}
	/**
     * Checks if there has been a change in the published resources.
     * @param module
     * @return module changed
	 */
	/*private boolean shouldRepack( IModule module ) {
        final Server _server = (Server) server.getServer();
        final IModule[] modules ={module};
        IModuleResourceDelta[] deltas = _server.getPublishedResourceDelta( modules );

        return deltas.length > 0;
    }*/
	private boolean shouldRepack(IModule lmodule) {
		final IModule[] rootMod = { module };
		final IModule[] modules = { module, lmodule };
		boolean repack = (IServer.PUBLISH_STATE_NONE != server.getServer().getModulePublishState(modules));
		repack |= (IServer.PUBLISH_STATE_NONE != server.getServer().getModulePublishState(rootMod));
		return repack;
	}

	/* returns true is a deploy command has to be run.
	 *  for example a simple JSP change does not need a redeployment as the file is already been copied by the assembly in the
	 *  correct directory
	 */
	public boolean needsARedeployment(  ) {
        final Server _server = (Server) server.getServer();
        final IModule[] modules ={module};
        IModuleResourceDelta[] deltas = _server.getPublishedResourceDelta( modules );
        return (childNeedsARedeployment|| criticalResourceChangeThatNeedsARedeploy(deltas));
    }
	/*return true is a module resource change requires a redeploy command
	 * for example, web.xml or a .class file change needs a redepploy.
	 * a jsp or html change just needs a file copy not a redeploy command.
	 */
	private boolean criticalResourceChangeThatNeedsARedeploy(IModuleResourceDelta[] deltas){
		if (deltas==null){
			return false;
		}

        for (int i=0;i<deltas.length;i++){
            if (deltas[i].getModuleResource().getName().endsWith (".class")){//class file
                SunAppSrvPlugin.logMessage("Class Changed in AssembleModules criticalResourceChangeThatNeedsARedeploy DELTA IS="+deltas[i].getKind()+deltas[i].getModuleResource().getName());
           	return true;
            }
            if (deltas[i].getModuleResource().getName().endsWith (".properties")){//properties file
            	return true;
            }
            if (deltas[i].getModuleResource().getName().endsWith (".xml")){//all XML files, including DD files or config files
                SunAppSrvPlugin.logMessage("XML Changed in AssembleModules criticalResourceChangeThatNeedsARedeploy DELTA IS="+deltas[i].getKind()+deltas[i].getModuleResource().getName());
           	return true;
            }
            if (deltas[i].getModuleResource().getName().equalsIgnoreCase("manifest.mf") ){
                SunAppSrvPlugin.logMessage("MANIFEST FIle  Changed in AssembleModules criticalResourceChangeThatNeedsARedeploy DELTA IS="+deltas[i].getKind()+deltas[i].getModuleResource().getName());
           	return true;
            }
            SunAppSrvPlugin.logMessage("AssembleModules neither class manifest or xml file");

            IModuleResourceDelta[] childrenDeltas= deltas[i].getAffectedChildren();
	        if ( criticalResourceChangeThatNeedsARedeploy(childrenDeltas)){
	        	return true;
	        }
        }

		return false;

	}

   protected void packModuleEARModule(IModule module, String deploymentUnitName, IPath destination) throws CoreException {
       SunAppSrvPlugin.logMessage("AssembleModules packModuleEARModule="+module.getId()+" "+module.getName());
       SunAppSrvPlugin.logMessage("AssembleModules deploymentUnitName="+deploymentUnitName); //ie foo.war or myejbs.jar
       //need to replace the , with_ ie _war or _jar as the dirname for dir deploy
       SunAppSrvPlugin.logMessage("AssembleModules destination="+destination);
		if(module.getModuleType().getId().equals("jst.web")) {//$NON-NLS-1$

			AssembleModules assembler= new AssembleModules(module, assembleRoot,server , publishHelper);
			IPath webAppPath = assembler.assembleWebModule(new NullProgressMonitor());
			String realDestination = destination.append(deploymentUnitName).toString();
	        SunAppSrvPlugin.logMessage("AssembleModules realDestination="+realDestination);
			ModulePackager packager=null;
			try {
				packager =new ModulePackager(realDestination,false);
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
		else {
			/*ludo super.*/packModule(module, deploymentUnitName, destination);
		}

	}


	public IPath assembleDirDeployedEARModule(IProgressMonitor monitor) throws CoreException{
		//copy ear root to the temporary assembly directory
		IPath parent = assembleRoot;

		final IModule[] rootMod = { module };
		boolean shouldCopy = (IServer.PUBLISH_STATE_NONE != server.getServer().getModulePublishState(rootMod));
		if (shouldCopy)
			copyModule(module, monitor);
		IEnterpriseApplication earModule = (IEnterpriseApplication)module.loadAdapter(IEnterpriseApplication.class, monitor);
		IModule[] childModules = earModule.getModules();
		SunAppSrvPlugin.logMessage("assembleDirDeployedEARModule childModules.length="+childModules.length);
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
				publishHelper.publishToPath(resources, parent.append(uri), monitor);
				//was publishHelper.publishSmart(resources, parent, monitor);
				continue;//done! no need to go further
			}
			if(!module.getModuleType().getId().equals("jst.utility")) {//$NON-NLS-1$	see bug https://glassfishplugins.dev.java.net/issues/show_bug.cgi?id=251
				if (uri.endsWith(".war")){
					uri = uri.substring(0,uri.length()-4)+"_war";					
				} else if (uri.endsWith(".jar")){
					uri = uri.substring(0,uri.length()-4)+"_jar";
				}
			}

			if( shouldRepack( module ) ){
//				packModuleEARModule(module,uri, parent);
				if(module.getModuleType().getId().equals("jst.web")) {//$NON-NLS-1$
					AssembleModules assembler = new AssembleModules(module, assembleRoot.append(uri),server, publishHelper);
					childNeedsARedeployment = (childNeedsARedeployment||assembler.needsARedeployment());
					IPath webAppPath = assembler.assembleWebModule(new NullProgressMonitor());
				}
				else {
				//	/*ludo super.*/packModule(module, uri, parent);
					AssembleModules assembler= new AssembleModules(module, assembleRoot.append(uri),server , publishHelper);
					childNeedsARedeployment = (childNeedsARedeployment||assembler.needsARedeployment());
					assembler.copyModule(module,monitor);
				}

            }

		}
		return parent;


	}
}
