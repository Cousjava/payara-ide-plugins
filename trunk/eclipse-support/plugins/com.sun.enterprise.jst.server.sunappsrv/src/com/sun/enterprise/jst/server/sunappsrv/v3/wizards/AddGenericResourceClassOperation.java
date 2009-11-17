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

package com.sun.enterprise.jst.server.sunappsrv.v3.wizards;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.emf.codegen.jet.JETException;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jst.j2ee.internal.web.operations.NewWebClassOperation;
import org.eclipse.wst.common.frameworks.datamodel.IDataModel;
import org.eclipse.wst.common.frameworks.internal.enablement.nonui.WFTWrappedException;

@SuppressWarnings("restriction")
public class AddGenericResourceClassOperation extends NewWebClassOperation {

	/**
	 * folder location of the Generic Resource creation templates directory
	 */
	protected static final String TEMPLATE_FILE = "/templates/genericresource.javajet"; //$NON-NLS-1$

	public AddGenericResourceClassOperation(IDataModel dataModel) {
		super(dataModel);
	}

	protected void generateUsingTemplates(IProgressMonitor monitor,
			IPackageFragment fragment) throws WFTWrappedException,
			CoreException {
		// Create the template model
		AddGenericResourceTemplateModel tempModel = new AddGenericResourceTemplateModel(model);
		// Using the WTPJetEmitter, generate the java source
		// template model
		try {
			if (fragment != null) {
				// Create the Generic Resource java file
				doGeneration(monitor, fragment, GenericResourceTemplate.create(null), tempModel);
				// also generate the second class if necessary
				if (!tempModel.isSimplePattern()) {
					tempModel.setIsContainerClass();
					doGeneration(monitor, fragment, ContainerResourceTemplate.create(null), tempModel);
				}
			}
		} catch (Exception e) {
			throw new WFTWrappedException(e);
		}
	}

	private void doGeneration(IProgressMonitor monitor, IPackageFragment fragment, 
			Object tempImpl, AddGenericResourceTemplateModel tempModel) throws JavaModelException, JETException {
		try {
			Method method = tempImpl.getClass().getMethod("generate", //$NON-NLS-1$
					new Class[] { Object.class });
			String source = (String) method.invoke(tempImpl, tempModel);
			String javaFileName = tempModel.getClassName() + ".java"; //$NON-NLS-1$
			createJavaFile(monitor, fragment, source, javaFileName);
		} catch (SecurityException e) {
			throw new JETException(e);
		} catch (NoSuchMethodException e) {
			throw new JETException(e);
		} catch (IllegalArgumentException e) {
			throw new JETException(e);
		} catch (IllegalAccessException e) {
			throw new JETException(e);
		} catch (InvocationTargetException e) {
			throw new JETException(e);
		}
	}

	protected IFile createJavaFile(IProgressMonitor monitor, IPackageFragment fragment, String source, String className) throws JavaModelException {
		if (fragment != null) {
			ICompilationUnit cu = fragment.getCompilationUnit(className);
			// Add the compilation unit to the java file
			if (cu == null || !cu.exists())
				cu = fragment.createCompilationUnit(className, source,
						true, monitor);
			return (IFile) cu.getResource();
		}
		return null;
	}

	@Override
	protected AddGenericResourceTemplateModel createTemplateModel() {
		return new AddGenericResourceTemplateModel(model);
	}

	@Override
	protected String getTemplateFile() {
		return TEMPLATE_FILE;
	}

	@Override
	protected Object getTemplateImplementation() {
		return GenericResourceTemplate.create(null);
	}
}
