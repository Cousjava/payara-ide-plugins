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

import java.util.HashSet;
import java.util.Set;
import java.util.StringTokenizer;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaConventions;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.internal.compiler.impl.CompilerOptions;
import org.eclipse.jst.j2ee.internal.common.operations.INewJavaClassDataModelProperties;
import org.eclipse.jst.j2ee.internal.common.operations.NewJavaClassDataModelProvider;
import org.eclipse.jst.j2ee.internal.common.operations.NewJavaEEArtifactClassOperation;
import org.eclipse.jst.j2ee.internal.web.operations.AddWebClassOperation;
import org.eclipse.jst.j2ee.internal.web.operations.NewWebClassDataModelProvider;
import org.eclipse.wst.common.frameworks.datamodel.IDataModel;
import org.eclipse.wst.common.frameworks.datamodel.IDataModelOperation;
import org.eclipse.wst.common.frameworks.datamodel.IDataModelProvider;
import org.eclipse.wst.common.frameworks.internal.plugin.WTPCommonPlugin;

import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;

@SuppressWarnings("restriction")
public class AddGenericResourceDataModelProvider extends
	NewWebClassDataModelProvider {

	public static final String PATTERN = "AddGenericResource.PATTERN"; //$NON-NLS-1$
	public static final String PATH = "AddGenericResource.PATH"; //$NON-NLS-1$
	public static final String MIME_TYPE = "AddGenericResource.MIME_TYPE"; //$NON-NLS-1$
	public static final String REPRESENTATION_CLASS = "AddGenericResource.REPRESENTATION_CLASS"; //$NON-NLS-1$
	public static final String CONTAINER_REPRESENTATION_CLASS = "AddGenericResource.CONTAINER_REPRESENTATION_CLASS"; //$NON-NLS-1$
	public static final String CONTAINER_PATH = "AddGenericResource.CONTAINER_PATH"; //$NON-NLS-1$
	public static final String IN_CONTAINER_CLASS = "AddGenericResource.IN_CONTAINER_CLASS"; //$NON-NLS-1$
	public static final String ORIGINAL_CLASS_NAME = "AddGenericResource.ORIGINAL_CLASS_NAME"; //$NON-NLS-1$
	
	public IDataModelOperation getDefaultOperation() {
		return new AddWebClassOperation(getDataModel()) {

			@Override
			protected NewJavaEEArtifactClassOperation getNewClassOperation() {
				return new AddGenericResourceClassOperation(getDataModel());
			}
			protected void generateMetaData(IDataModel aModel, String qualifiedClassName) {
				// for now, do nothing here - data model should be ok as is
			}
		};
	}

	/**
	 * Subclasses may extend this method to add their own data model's
	 * properties as valid base properties.
	 * 
	 * @see org.eclipse.wst.common.frameworks.datamodel.IDataModelProvider#getPropertyNames()
	 */
	@SuppressWarnings("unchecked")
	@Override
	public Set<String> getPropertyNames() {
		// Add Resource specific properties defined in this data model
		Set<String> propertyNames = super.getPropertyNames();

		propertyNames.add(REPRESENTATION_CLASS);
		propertyNames.add(MIME_TYPE);
		propertyNames.add(CONTAINER_REPRESENTATION_CLASS);
		propertyNames.add(CONTAINER_PATH);
		propertyNames.add(PATH);
		propertyNames.add(PATTERN);
		propertyNames.add(IN_CONTAINER_CLASS);
		propertyNames.add(ORIGINAL_CLASS_NAME);

		return propertyNames;
	}

	/**
	 * Subclasses may extend this method to provide their own default values for
	 * any of the properties in the data model hierarchy. This method does not
	 * accept a null parameter. It may return null.
	 * 
	 * @see NewJavaClassDataModelProvider#getDefaultProperty(String)
	 * @see IDataModelProvider#getDefaultProperty(String)
	 * 
	 * @param propertyName
	 * @return Object default value of property
	 */
	@Override
	public Object getDefaultProperty(String propertyName) {
		if (REPRESENTATION_CLASS.equals(propertyName) || CONTAINER_REPRESENTATION_CLASS.equals(propertyName)) {
			return "java.lang.String"; //$NON-NLS-1$
		}

		if (PATTERN.equals(propertyName)) {
			return AddGenericResourceTemplateModel.SIMPLE_PATTERN;
		}

		if (PATH.equals(propertyName)) {
			return (isSimplePattern() ? "generic" : (isClientControlledPattern() ? "{name}" : "{id}")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		}

		if (CONTAINER_PATH.equals(propertyName)) {
			String className = getStringProperty(INewJavaClassDataModelProperties.CLASS_NAME);
			if ((className != null) && (className.length() > 0)) {
				return "/" + className.substring(0, 1).toLowerCase() + className.substring(1) + "s"; //$NON-NLS-1$ //$NON-NLS-2$
			}
		}

		if (IN_CONTAINER_CLASS.equals(propertyName)) {
			return Boolean.FALSE;
		}

		// Otherwise check super for default value for property
		return super.getDefaultProperty(propertyName);
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jst.j2ee.internal.web.operations.NewWebClassDataModelProvider#isPropertyEnabled(java.lang.String)
	 */
	@Override
	public boolean isPropertyEnabled(String propertyName) {
		if (CONTAINER_REPRESENTATION_CLASS.equals(propertyName) || CONTAINER_PATH.equals(propertyName)) {
			return !isSimplePattern();
		}
		return super.isPropertyEnabled(propertyName);
	}

	public IStatus validate(String propertyName) {
		if (MIME_TYPE.equals(propertyName)) {
			String value = (String) getProperty(MIME_TYPE);
			if (value == null || value.trim().length() == 0) {
				return SunAppSrvPlugin.createErrorStatus(
						Messages.errorMimeTypeMissing, null);
			}
		}
		if (REPRESENTATION_CLASS.equals(propertyName)) {
			return validateRepClass(REPRESENTATION_CLASS, Messages.errorRepresentationClassMissing,
					Messages.errorRepresentationClassInvalid);
		}

		if (PATH.equals(propertyName)) {
			return validatePath((String) getProperty(PATH));
		}

		// these only need validation in the case of pattern != simple
		if (isPropertyEnabled(propertyName)) {
			if (CONTAINER_PATH.equals(propertyName)) {
				String value = (String) getProperty(CONTAINER_PATH);
				if (value == null || value.trim().length() == 0) {
					return SunAppSrvPlugin.createErrorStatus(
							Messages.errorContainerPathMissing, null);
				}
			}
			if (CONTAINER_REPRESENTATION_CLASS.equals(propertyName)) {
				return validateRepClass(CONTAINER_REPRESENTATION_CLASS, Messages.errorContainerRepresentationClassMissing,
						Messages.errorContainerRepresentationClassInvalid);
			}
		}
		IStatus status = super.validate(propertyName);
		return status;
	}

	private boolean isSimplePattern() {
		return AddGenericResourceTemplateModel.SIMPLE_PATTERN.equals(getStringProperty(PATTERN));
	}

	private boolean isClientControlledPattern() {
		return AddGenericResourceTemplateModel.CLIENT_CONTAINER_PATTERN.equals(getStringProperty(PATTERN));
	}

	protected IStatus validateRepClass(String propertyName, String errorMessageKeyMissing, String errorMessageKeyInvalid) {
		String value = (String) getProperty(propertyName);
		if (value == null || value.trim().length() == 0) {
			return SunAppSrvPlugin.createErrorStatus(errorMessageKeyMissing, null);
		}
		// Check that unqualified class name is valid by standard java conventions
		String className = value;
		int index = value.lastIndexOf("."); //$NON-NLS-1$
		if (index != -1) {
			className = value.substring(index + 1);
		}
		IStatus javaStatus = validateJavaClassName(className);
		if (javaStatus.getSeverity() != IStatus.ERROR) {
			// If the class does not exist, throw an error
			IJavaProject javaProject = JavaCore.create(getTargetProject());
			IType type = null;
			try {
				type = javaProject.findType(value);
			} catch (Exception e) {
				// Just throw error below
			}
			if (type == null) {
				return WTPCommonPlugin.createErrorStatus(errorMessageKeyInvalid);
			}
			return WTPCommonPlugin.OK_STATUS;
		}
		return javaStatus;
	}

	private IStatus validatePath(String path) {
		if (path == null || path.trim().length() == 0) {
			return SunAppSrvPlugin.createErrorStatus(
					Messages.errorPathMissing, null);
		}
		if (!isSimplePattern()) {
			StringTokenizer segments = new StringTokenizer(path, "/ "); //$NON-NLS-1$
			Set<String> pathParts = new HashSet<String>();
			while (segments.hasMoreTokens()) {
				String segment = segments.nextToken();
				if (segment.startsWith("{")) { //$NON-NLS-1$
					if (segment.length() > 2 && segment.endsWith("}")) { //$NON-NLS-1$
						String pathPart = segment.substring(1, segment.length() - 1);
						IStatus javaStatus = JavaConventions.validateIdentifier(pathPart, 
								CompilerOptions.VERSION_1_3,CompilerOptions.VERSION_1_3);
						if (javaStatus.getSeverity() == IStatus.ERROR) {
							String msg = javaStatus.getMessage();
							return WTPCommonPlugin.createErrorStatus(msg);
						} else if (javaStatus.getSeverity() == IStatus.WARNING) {
							String msg = javaStatus.getMessage();
							return WTPCommonPlugin.createWarningStatus(msg);
						}
						if (pathParts.contains(pathPart)) {
							return WTPCommonPlugin.createErrorStatus(Messages.errorPathInvalid);
						} else {
							pathParts.add(pathPart);
						}
					} else {
						return WTPCommonPlugin.createErrorStatus(Messages.errorPathInvalid);
					}
				} else {
					if (segment.contains("{") || segment.contains("}")) { //$NON-NLS-1$ //$NON-NLS-2$
						return WTPCommonPlugin.createErrorStatus(Messages.errorPathInvalid);
					}
				}
			}
		}
		return WTPCommonPlugin.OK_STATUS;
	}
}
