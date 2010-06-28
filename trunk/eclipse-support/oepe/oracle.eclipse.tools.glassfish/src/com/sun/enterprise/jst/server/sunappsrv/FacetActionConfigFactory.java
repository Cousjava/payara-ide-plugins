package com.sun.enterprise.jst.server.sunappsrv;


import org.eclipse.core.runtime.CoreException;

import org.eclipse.wst.common.project.facet.core.ActionConfig;
import org.eclipse.wst.common.project.facet.core.IActionConfigFactory;


public class FacetActionConfigFactory implements IActionConfigFactory{

	public Object create() throws CoreException {
		return new RealConfig();
	}	
	public static class RealConfig extends ActionConfig{		
	}	
}
