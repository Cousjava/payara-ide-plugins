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
package com.sun.enterprise.jst.server.sunappsrv.web;

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

import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;

public class SunWebXmlCreate extends AbstractDataModelOperation  {
    private String version;

    
    public SunWebXmlCreate(IDataModel model,String version /*8.x or 9.x */) {
        super(model);
        this.version=version;
    }
    
    public IStatus execute(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
        
        IVirtualComponent comp = ComponentCore.createComponent(getProject());       
        createDeploymentPlan(getWebDeploymentPlanFile(comp));
        
        return Status.OK_STATUS;
    }
    

    
    public static IFile getWebDeploymentPlanFile(IVirtualComponent comp) {
        IPath deployPlanPath = comp.getRootFolder().getUnderlyingFolder()
        .getProjectRelativePath().append("WEB-INF").append("sun-web.xml");
        return comp.getProject().getFile(deployPlanPath);
    }
    
    
    private  void createDeploymentPlan(IFile deployPlanFile) {

    	ByteArrayInputStream is=null;
        try {
            
            String moduleName =         model.getStringProperty(IFacetDataModelProperties.FACET_PROJECT_NAME);
             is = new ByteArrayInputStream(getDefautSunWeb(moduleName).getBytes());

            deployPlanFile.create(is, false, null);
      //      SunAppSrvPlugin.logMessage(getDefautSunWeb(moduleName));

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
    
    private String getDefautSunWeb(String contextRoot){
    	String h=null;
    	if (version.equals("8.x")){
    		h="<!DOCTYPE sun-web-app PUBLIC \"-//Sun Microsystems, Inc.//DTD Application Server 8.1 Servlet 2.4//EN\" \"http://www.sun.com/software/appserver/dtds/sun-web-app_2_4-0.dtd\">\n";
	
    	}
    	else{
    		h="<!DOCTYPE sun-web-app PUBLIC \"-//Sun Microsystems, Inc.//DTD Application Server 9.0 Servlet 2.5//EN\" \"http://www.sun.com/software/appserver/dtds/sun-web-app_2_5-0.dtd\">\n";

    	}
    	return "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"+
    	h+
"<sun-web-app error-url=\"\">\n"+
"  <context-root>/"+contextRoot+"</context-root>\n"+
"  <class-loader delegate=\"true\"/>\n"+
"  <jsp-config>\n"+
"    <property name=\"keepgenerated\" value=\"true\">\n"+
"      <description>Keep a copy of the generated servlet class java code.</description>\n"+
"    </property>\n"+
"  </jsp-config>\n"+
"</sun-web-app>\n";
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
