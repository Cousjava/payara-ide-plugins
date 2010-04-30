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

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.emf.codegen.jet.JETException;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jst.j2ee.ejb.internal.operations.NewEnterpriseBeanClassOperation;
import org.eclipse.wst.common.frameworks.datamodel.IDataModel;
import org.eclipse.wst.common.frameworks.internal.enablement.nonui.WFTWrappedException;

@SuppressWarnings("restriction")
public class AddEjbTimerClassOperation extends NewEnterpriseBeanClassOperation {

	/**
	 * folder location of the EJB Timer creation templates directory
	 */
	protected static final String TEMPLATE_FILE = "/templates/ejbtimer.javajet"; //$NON-NLS-1$

	public AddEjbTimerClassOperation(IDataModel dataModel) {
		super(dataModel);
	}

	protected void generateUsingTemplates(IProgressMonitor monitor,
			IPackageFragment fragment) throws WFTWrappedException,
			CoreException {
		// Create the enterprise bean template model
		AddEjbTimerTemplateModel tempModel = new AddEjbTimerTemplateModel(model);
		// Using the WTPJetEmitter, generate the java source based on the bean
		// template model
		try {
			if (fragment != null) {
				// Create the EJB Timer java file
				EjbTimerTemplate tempImpl = EjbTimerTemplate.create(null);

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
		} catch (Exception e) {
			throw new WFTWrappedException(e);
		}
	}
}
