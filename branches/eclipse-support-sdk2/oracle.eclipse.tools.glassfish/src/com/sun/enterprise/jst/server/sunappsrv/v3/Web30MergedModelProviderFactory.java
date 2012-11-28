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

import java.util.HashMap;

import org.eclipse.core.resources.IProject;
import org.eclipse.jst.j2ee.model.IModelProvider;
import org.eclipse.jst.jee.model.internal.Web25MergedModelProviderFactory;
import org.eclipse.wst.common.componentcore.resources.IVirtualComponent;

import com.sun.enterprise.jst.server.sunappsrv.v3.wizards.WizardUtil;

public class Web30MergedModelProviderFactory extends Web25MergedModelProviderFactory {

	private HashMap<IProject, IModelProvider> xmlResources = new HashMap<IProject, IModelProvider>();

	public IModelProvider create(IProject project) {
		if (WizardUtil.isWebProjectWithGF3Runtime(project)) {
			IModelProvider result = getResource(project);
			if (result == null || ((Web30ModelProvider) result).isDisposed()) {
				result = new Web30ModelProvider(project);
				addResource(project, result);
			}
			return result;
		} else {
			return super.create(project);
		}
	}

	public IModelProvider create(IVirtualComponent component) {
		return create(component.getProject());
	}

	private void addResource(IProject project, IModelProvider modelProvider){
		xmlResources.put(project, modelProvider);
	}

	private IModelProvider getResource(IProject project){
		return xmlResources.get(project);
	}
}
