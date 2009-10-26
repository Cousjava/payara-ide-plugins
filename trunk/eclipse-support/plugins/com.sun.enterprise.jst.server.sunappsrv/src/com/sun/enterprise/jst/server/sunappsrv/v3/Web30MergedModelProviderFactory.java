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
