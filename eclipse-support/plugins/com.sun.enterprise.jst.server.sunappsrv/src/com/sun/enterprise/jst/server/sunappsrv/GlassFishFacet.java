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

package com.sun.enterprise.jst.server.sunappsrv;

/**
 *
 * @author ludo
 */
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jst.common.project.facet.JavaProjectFacetCreationDataModelProvider;
import org.eclipse.jst.j2ee.internal.project.J2EEProjectUtilities;
import org.eclipse.wst.common.componentcore.datamodel.properties.IFacetDataModelProperties;
import org.eclipse.wst.common.componentcore.internal.util.IModuleConstants;
import org.eclipse.wst.common.frameworks.datamodel.DataModelFactory;
import org.eclipse.wst.common.frameworks.datamodel.IDataModel;
import org.eclipse.wst.common.project.facet.core.IDelegate;
import org.eclipse.wst.common.project.facet.core.IProjectFacetVersion;
import org.eclipse.core.commands.ExecutionException;

import com.sun.enterprise.jst.server.sunappsrv.ejbjar.SunEjbJarXmlCreate;
import com.sun.enterprise.jst.server.sunappsrv.web.IndexJSPCreate;
import com.sun.enterprise.jst.server.sunappsrv.web.SunWebXmlCreate;

public class GlassFishFacet implements IDelegate {
    
    
    
    public void execute(IProject prj, IProjectFacetVersion fv,Object config, IProgressMonitor monitor) throws CoreException {
        
        //IVirtualComponent virtualC = ComponentCore.createComponent(prj);
		IDataModel model = DataModelFactory.createDataModel(new JavaProjectFacetCreationDataModelProvider());
		model.setStringProperty(IFacetDataModelProperties.FACET_PROJECT_NAME, prj.getName());

        
 ///       IDataModel model = DataModelFactory.createDataModel(new JavaComponentCreationDataModelProvider());
///        model.setStringProperty(IComponentCreationDataModelProperties.COMPONENT_NAME, virtualC.getName());
///        model.setStringProperty(IComponentCreationDataModelProperties.PROJECT_NAME, prj.getName());
        String type = J2EEProjectUtilities.getJ2EEProjectType(getProject(model ));
        try{
        if (IModuleConstants.JST_WEB_MODULE.equals(type)) {
            SunWebXmlCreate swa= new SunWebXmlCreate(model,"9.x");
            swa.execute(monitor,null);
            IndexJSPCreate i= new IndexJSPCreate(model);
            i.execute(monitor,null);
            
        } else if (IModuleConstants.JST_EJB_MODULE.equals(type)) {
            SunEjbJarXmlCreate sej= new SunEjbJarXmlCreate(model,"9.x");
            sej.execute(monitor,null);
        } else if (IModuleConstants.JST_EAR_MODULE.equals(type)) {
            
        } else if (IModuleConstants.JST_CONNECTOR_MODULE.equals(type)) {
        }
        }catch(ExecutionException e){
        	
        }
        
        
        
        
    }
    public IProject getProject(IDataModel model ) {
        String projectName = model.getProperty(IFacetDataModelProperties.FACET_PROJECT_NAME).toString();
        if (projectName != null) {
            return ResourcesPlugin.getWorkspace().getRoot().getProject( projectName);
        }
        return null;
    }
    
}
