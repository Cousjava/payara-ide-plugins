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
