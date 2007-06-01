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
package com.sun.enterprise.jst.server.sunappsrv.ejbjar;

import java.io.ByteArrayInputStream;

import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jst.server.core.FacetUtil;
import org.eclipse.wst.common.componentcore.ComponentCore;
import org.eclipse.wst.common.componentcore.datamodel.properties.IFacetDataModelProperties;
import org.eclipse.wst.common.componentcore.datamodel.properties.IFacetProjectCreationDataModelProperties;
import org.eclipse.wst.common.componentcore.resources.IVirtualComponent;
import org.eclipse.wst.common.frameworks.datamodel.AbstractDataModelOperation;
import org.eclipse.wst.common.frameworks.datamodel.IDataModel;
import org.eclipse.wst.common.project.facet.core.runtime.IRuntime;

public class SunEjbJarXmlCreate extends AbstractDataModelOperation  {
    
    public SunEjbJarXmlCreate() {
    }
    
    public SunEjbJarXmlCreate(IDataModel model) {
        super(model);
    }
    
    public IStatus execute(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
        
      ////  String runtimeID = model.getStringProperty(J2EEComponentCreationDataModelProvider.RUNTIME_TARGET_ID);
        //       if (runtimeID != null && runtimeID.startsWith("GlassFish")){
        execute();
        //       }
        
        return Status.OK_STATUS;
    }
    
    public void execute() {
        IVirtualComponent comp = ComponentCore.createComponent(getProject());

        createDeploymentPlan(getSunEjbJarFile(comp));

    }
    
    public static IFile getSunEjbJarFile(IVirtualComponent comp) {
        IPath deployPlanPath = comp.getRootFolder().getUnderlyingFolder()
        .getProjectRelativePath().append("META-INF").append("sun-ejb-jar.xml");
        return comp.getProject().getFile(deployPlanPath);
    }
    
    
    private  void createDeploymentPlan(IFile deployPlanFile) {
    	ByteArrayInputStream is=null;
        try {
            
            is = new ByteArrayInputStream(getSunEjbXml().getBytes());
           deployPlanFile.create(is, false, null);
        } catch (Exception e) {
            
        } finally {
            try {
                if (is!=null)
                    is.close();
            } catch (Exception e) {
                // do nothing
            }
        }
        

    }
    
 private String getSunEjbXml   (){
	 return "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"+
"<!DOCTYPE sun-ejb-jar PUBLIC \"-//Sun Microsystems, Inc.//DTD Application Server 9.0 EJB 3.0//EN\" \"http://www.sun.com/software/appserver/dtds/sun-ejb-jar_3_0-0.dtd\">\n"+
"<sun-ejb-jar>\n"+
"  <enterprise-beans/>\n"+
"</sun-ejb-jar>\n";
 }
    
    public IStatus redo(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
        return null;
    }
    
    
    public IStatus undo(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
        return null;
    }
    
 
    
	public org.eclipse.wst.server.core.IRuntime getRuntime() {
		IRuntime runtime = (IRuntime) model.getProperty(IFacetProjectCreationDataModelProperties.FACET_RUNTIME);
		if (runtime != null) {
			return FacetUtil.getRuntime(runtime);
		}
		return null;
	}

	public IProject getProject() {
		String projectName = model.getProperty(IFacetDataModelProperties.FACET_PROJECT_NAME).toString();
		if (projectName != null) {
			return ResourcesPlugin.getWorkspace().getRoot().getProject(projectName);
		}
		return null;
	}
}
