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

import java.io.InputStream;
import org.eclipse.core.runtime.Status;
import org.eclipse.wst.common.componentcore.ComponentCore;
import org.eclipse.wst.common.componentcore.resources.IVirtualComponent;

import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jst.j2ee.application.internal.operations.J2EEComponentCreationDataModelProvider;
import org.eclipse.wst.common.componentcore.datamodel.properties.IComponentCreationDataModelProperties;
import org.eclipse.wst.common.frameworks.datamodel.AbstractDataModelOperation;
import org.eclipse.wst.common.frameworks.datamodel.IDataModel;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IPath;

public class SunEjbJarXmlCreate extends AbstractDataModelOperation  {
    
    public SunEjbJarXmlCreate() {
    }
    
    public SunEjbJarXmlCreate(IDataModel model) {
        super(model);
    }
    
    public IStatus execute(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
        
        String runtimeID = model.getStringProperty(J2EEComponentCreationDataModelProvider.RUNTIME_TARGET_ID);
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
    
    
    private  SunEjbJarDeploymentPlan createDeploymentPlan(IFile deployPlanFile) {
        SunEjbJarDeploymentPlan plan=null;
        InputStream is=null;
        try {
            
            plan=new SunEjbJarDeploymentPlan();
            is=plan.getInputStream();
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
        
        return plan;
    }
    
    
    
    public IStatus redo(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
        return null;
    }
    
    
    public IStatus undo(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
        return null;
    }
    
    
    
    public String getComponentName() {
        return model.getProperty(
                IComponentCreationDataModelProperties.COMPONENT_NAME).toString();
    }
    
    public IProject getProject() {
        String projectName = model.getProperty(
                IComponentCreationDataModelProperties.PROJECT_NAME).toString();
        if (projectName != null) {
            return ResourcesPlugin.getWorkspace().getRoot().getProject( projectName);
        }
        return null;
    }
}
