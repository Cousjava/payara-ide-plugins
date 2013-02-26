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

package com.sun.enterprise.jst.server.sunappsrv.v3;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jst.j2ee.model.IModelProvider;
import org.eclipse.jst.javaee.web.WebApp;
import org.eclipse.jst.jee.model.internal.Web25MergedModelProvider;
import org.eclipse.wst.common.project.facet.core.ProjectFacetsManager;

public class Web30ModelProvider extends Web25MergedModelProvider {

	private IProject project;
	
	public Web30ModelProvider(IProject project) {
		super(project);
		this.project = project;
	}

	protected IModelProvider loadAnnotationModel(WebApp ddModel) throws CoreException {
		return new Web30AnnotationReader(ProjectFacetsManager.create(project), ddModel);
	}
	
}
