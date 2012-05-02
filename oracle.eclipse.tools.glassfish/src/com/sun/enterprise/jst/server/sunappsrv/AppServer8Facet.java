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

/**
 *
 * @author ludo
 */
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jst.common.project.facet.JavaProjectFacetCreationDataModelProvider;
import org.eclipse.jst.j2ee.project.JavaEEProjectUtilities;
import org.eclipse.wst.common.componentcore.datamodel.properties.IFacetDataModelProperties;
import org.eclipse.wst.common.frameworks.datamodel.DataModelFactory;
import org.eclipse.wst.common.frameworks.datamodel.IDataModel;
import org.eclipse.wst.common.project.facet.core.IDelegate;
import org.eclipse.wst.common.project.facet.core.IProjectFacetVersion;

import com.sun.enterprise.jst.server.sunappsrv.ejbjar.SunEjbJarXmlCreate;
import com.sun.enterprise.jst.server.sunappsrv.web.IndexJSPCreate;
import com.sun.enterprise.jst.server.sunappsrv.web.SunWebXmlCreate;

public class AppServer8Facet implements IDelegate {
    
    
    
    public void execute(IProject prj, IProjectFacetVersion fv,Object config, IProgressMonitor monitor) throws CoreException {
        
        //IVirtualComponent virtualC = ComponentCore.createComponent(prj);
		IDataModel model = DataModelFactory.createDataModel(new JavaProjectFacetCreationDataModelProvider());
		model.setStringProperty(IFacetDataModelProperties.FACET_PROJECT_NAME, prj.getName());

        
 ///       IDataModel model = DataModelFactory.createDataModel(new JavaComponentCreationDataModelProvider());
///        model.setStringProperty(IComponentCreationDataModelProperties.COMPONENT_NAME, virtualC.getName());
///        model.setStringProperty(IComponentCreationDataModelProperties.PROJECT_NAME, prj.getName());
        IProject project = getProject(model);
        try{
        	if (JavaEEProjectUtilities.isDynamicWebProject(project)) {
        		SunWebXmlCreate swa= new SunWebXmlCreate(model,"8.x");
        		swa.execute(monitor,null);
        		IndexJSPCreate i= new IndexJSPCreate(model);
        		i.execute(monitor,null);
        	} else if (JavaEEProjectUtilities.isEJBProject(project)) {
        		SunEjbJarXmlCreate sej= new SunEjbJarXmlCreate(model,"8.x");
        		sej.execute(monitor,null);
        	} else if (JavaEEProjectUtilities.isEARProject(project)) {
            
        } else if (JavaEEProjectUtilities.isJCAProject(project)) {
        }
        }catch (ExecutionException e){
        	
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
