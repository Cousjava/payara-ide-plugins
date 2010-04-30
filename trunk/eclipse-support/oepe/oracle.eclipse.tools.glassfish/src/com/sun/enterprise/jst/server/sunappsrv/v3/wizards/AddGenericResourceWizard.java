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

import static org.eclipse.jst.j2ee.internal.common.operations.INewJavaClassDataModelProperties.JAVA_PACKAGE;
import static org.eclipse.jst.j2ee.internal.common.operations.INewJavaClassDataModelProperties.PROJECT;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jst.j2ee.internal.plugin.J2EEEditorUtility;
import org.eclipse.jst.j2ee.internal.project.J2EEProjectUtilities;
import org.eclipse.jst.servlet.ui.internal.plugin.ServletUIPlugin;
import org.eclipse.jst.servlet.ui.internal.wizard.NewWebArtifactWizard;
import org.eclipse.wst.common.frameworks.datamodel.IDataModel;
import org.eclipse.wst.common.frameworks.datamodel.IDataModelProvider;

@SuppressWarnings("restriction")
public class AddGenericResourceWizard extends NewWebArtifactWizard {

	public AddGenericResourceWizard() {
		this(null);
	}

	public AddGenericResourceWizard(IDataModel model) {
		super(model);
		setWindowTitle(Messages.genericResourceWizardTitle);
	}

	@Override
	protected void doAddPages() {
		AddGenericResourceWizardPage page1 = new AddGenericResourceWizardPage(getDataModel(),
				"page1", Messages.genericResourceWizardDescription, //$NON-NLS-1$
				Messages.genericResourceWizardTitle, J2EEProjectUtilities.DYNAMIC_WEB);
		addPage(page1);
	}

	@Override
	protected IDataModelProvider getDefaultProvider() {
		return (IDataModelProvider) new AddGenericResourceDataModelProvider();
	}

	@Override
	protected ImageDescriptor getImage() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected String getTitle() {
		return Messages.genericResourceWizardTitle;
	}

	/* (non-Javadoc)
	 * @see org.eclipse.wst.common.frameworks.internal.datamodel.ui.DataModelWizard#postPerformFinish()
	 */
	@Override
	protected void postPerformFinish() throws InvocationTargetException {
		openJavaClass();
		super.postPerformFinish();
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jst.servlet.ui.internal.wizard.NewWebArtifactWizard#openJavaClass()
	 */
	@Override
	protected void openJavaClass() {
		IDataModel model = getDataModel();
		if (model.getBooleanProperty(AddGenericResourceDataModelProvider.IN_CONTAINER_CLASS)) {
			try {
				String className = model.getStringProperty(AddGenericResourceDataModelProvider.ORIGINAL_CLASS_NAME);
				String packageName = model.getStringProperty(JAVA_PACKAGE);

				if (packageName != null && packageName.trim().length() > 0)
					className = packageName + "." + className; //$NON-NLS-1$

				IProject p = (IProject) model.getProperty(PROJECT);
				IJavaProject javaProject = J2EEEditorUtility.getJavaProject(p);
				IFile file = (IFile) javaProject.findType(className).getResource();
				openEditor(file);
			} catch (Exception cantOpen) {
				ServletUIPlugin.log(cantOpen);
			}	
		}
		super.openJavaClass();
	}
}
