// <editor-fold defaultstate="collapsed" desc="CDDL Licence">
/*
 * The contents of this file are subject to the terms 
 * of the Common Development and Distribution License 
 * (the "License").  You may not use this file except 
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at 
 * glassfish/bootstrap/legal/CDDLv1.0.txt or 
 * https://glassfish.dev.java.net/public/CDDLv1.0.html. 
 * See the License for the specific language governing 
 * permissions and limitations under the License.
 * 
 * When distributing Covered Code, include this CDDL 
 * HEADER in each file and include the License file at 
 * glassfish/bootstrap/legal/CDDLv1.0.txt.  If applicable, 
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
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jst.j2ee.internal.archive.operations.JavaComponentCreationDataModelProvider;
import org.eclipse.wst.common.componentcore.ComponentCore;
import org.eclipse.wst.common.componentcore.datamodel.properties.IComponentCreationDataModelProperties;
import org.eclipse.wst.common.componentcore.resources.IVirtualComponent;
import org.eclipse.wst.common.frameworks.datamodel.DataModelFactory;
import org.eclipse.wst.common.frameworks.datamodel.IDataModel;
import org.eclipse.wst.common.project.facet.core.IDelegate;
import org.eclipse.wst.common.project.facet.core.IProjectFacetVersion;

public class MyFacet implements IDelegate {
    
    
    
    public void execute(IProject prj, IProjectFacetVersion fv,Object config, IProgressMonitor monitor) throws CoreException {
        
        IVirtualComponent virtualC = ComponentCore.createComponent(prj);
        IDataModel model = DataModelFactory.createDataModel(new JavaComponentCreationDataModelProvider());
        model.setStringProperty(IComponentCreationDataModelProperties.COMPONENT_NAME, virtualC.getName());
        model.setStringProperty(IComponentCreationDataModelProperties.PROJECT_NAME, prj.getName());
        SunWebXmlCreate op = new SunWebXmlCreate(model);
        op.execute();
    }
    
}
