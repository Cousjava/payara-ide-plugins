/*
 * Copyright (c) 2010 Oracle and/or its affiliates. All rights reserved.
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
