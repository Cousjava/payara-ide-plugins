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

import static org.eclipse.jst.j2ee.ejb.internal.operations.INewEnterpriseBeanClassDataModelProperties.EJB_NAME;
import static org.eclipse.jst.j2ee.ejb.internal.operations.INewSessionBeanClassDataModelProperties.LOCAL;
import static org.eclipse.jst.j2ee.ejb.internal.operations.INewSessionBeanClassDataModelProperties.STATE_TYPE;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.emf.codegen.jet.JETException;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jst.ejb.ui.internal.wizard.AddSessionBeanWizard;
import org.eclipse.jst.ejb.ui.internal.wizard.AddSessionBeanWizardPage;
import org.eclipse.jst.ejb.ui.internal.wizard.NewSessionBeanClassWizardPage;
import org.eclipse.jst.j2ee.ejb.internal.operations.AddSessionBeanOperation;
import org.eclipse.jst.j2ee.ejb.internal.operations.CreateSessionBeanTemplateModel;
import org.eclipse.jst.j2ee.ejb.internal.operations.NewSessionBeanClassDataModelProvider;
import org.eclipse.jst.j2ee.ejb.internal.operations.NewSessionBeanClassOperation;
import org.eclipse.jst.j2ee.internal.common.operations.CreateJavaEEArtifactTemplateModel;
import org.eclipse.jst.j2ee.internal.common.operations.NewJavaEEArtifactClassOperation;
import org.eclipse.jst.j2ee.internal.project.J2EEProjectUtilities;
import org.eclipse.jst.j2ee.project.JavaEEProjectUtilities;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.wst.common.componentcore.internal.util.IModuleConstants;
import org.eclipse.wst.common.frameworks.datamodel.DataModelPropertyDescriptor;
import org.eclipse.wst.common.frameworks.datamodel.IDataModel;
import org.eclipse.wst.common.frameworks.datamodel.IDataModelOperation;
import org.eclipse.wst.common.frameworks.datamodel.IDataModelProvider;
import org.eclipse.wst.common.frameworks.internal.WTPPlugin;
import org.eclipse.wst.server.core.IRuntime;

import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;

@SuppressWarnings("restriction")
public class JavaEE6SessionBeanWizard extends AddSessionBeanWizard {

	private static final String V3_RUNTIME = "com.sun.enterprise.jst.server.runtime.sunappsrv92"; //$NON-NLS-1$

	// mimic state type enumeration and value with our own constants
	private static final String StateType_SINGLETON = "SINGLETON"; //$NON-NLS-1$
	private static final String EJBCreationResourceHandler_STATE_TYPE_SINGLETON = "Singleton"; //$NON-NLS-1$

	// share help context with superclass, but copy the string here so no dependency issues
	private static final String PLUGIN_EJB_UI = "org.eclipse.jst.ejb.ui."; //$NON-NLS-1$
	private static final String EJB_SESSION_BEAN_WIZARD_ADD_SESSION_BEAN_PAGE_1 = PLUGIN_EJB_UI + "sessbw1100"; //$NON-NLS-1$

	public JavaEE6SessionBeanWizard(IDataModel model) {
		super(model);
		setWindowTitle(Messages.wizardTitle);
	}

	public JavaEE6SessionBeanWizard() {
		this(null);
	}

	/* (non-Javadoc)
	 * @see org.eclipse.wst.common.frameworks.internal.datamodel.ui.DataModelWizard#addPage(org.eclipse.jface.wizard.IWizardPage)
	 */
	@Override
	public void addPage(IWizardPage page) {
		// we don't need the second page
		if (page instanceof AddSessionBeanWizardPage) {
			return;
		}
		// replace first page with a subclass that lets us do different project validation
		if (page instanceof NewSessionBeanClassWizardPage) {
			NewSessionBeanClassWizardPage page1 = new NewSessionBeanClassWizardPage(
					getDataModel(), page.getName(), page.getDescription(),
					Messages.wizardTitle, J2EEProjectUtilities.DYNAMIC_WEB + J2EEProjectUtilities.EJB) {

						/* (non-Javadoc)
						 * @see org.eclipse.jst.j2ee.internal.wizard.NewJavaClassWizardPage#isProjectValid(org.eclipse.core.resources.IProject)
						 */
						@Override
						protected boolean isProjectValid(IProject project) {
							// super's test for isProjectValid requires an ejb project and we don't 
							// want to do that, so result is basically a test of the grandsuper's isProjectValid
							// with the addition of allowing both ejb and web projects
							try {
								boolean result = project.isAccessible() && 
									project.hasNature(IModuleConstants.MODULE_NATURE_ID) && 
									(JavaEEProjectUtilities.isDynamicWebProject(project) ||
									JavaEEProjectUtilities.isEJBProject(project));

								if (result) {
									IRuntime runtime = J2EEProjectUtilities.getServerRuntime(project);
									if ((runtime != null) && runtime.getRuntimeType().getId().equals(V3_RUNTIME)){
										return true;
									}
								}
							} catch (CoreException e) {
								e.printStackTrace();
							}

							return false;
						}

						@Override
						protected Composite createTopLevelComposite(Composite parent) {
							Composite composite = super.createTopLevelComposite(parent);

							projectNameLabel.setText(Messages.ProjectName);

							return composite;
						}
			};
			page1.setInfopopID(EJB_SESSION_BEAN_WIZARD_ADD_SESSION_BEAN_PAGE_1);
			page = page1;
		}
		super.addPage(page);
	}
	@Override
	protected IDataModelProvider getDefaultProvider() {
		return (IDataModelProvider) new NewSessionBeanClassDataModelProvider() {
			/* (non-Javadoc)
			 * @see org.eclipse.jst.j2ee.ejb.internal.operations.NewSessionBeanClassDataModelProvider#getDefaultOperation()
			 */
			@Override
			public IDataModelOperation getDefaultOperation() {
				return new AddSessionBeanOperation(model) {

					/* (non-Javadoc)
					 * @see org.eclipse.jst.j2ee.ejb.internal.operations.AddSessionBeanOperation#getNewClassOperation()
					 */
					@Override
					protected NewJavaEEArtifactClassOperation getNewClassOperation() {
						return new NewSessionBeanClassOperation(model) {

							/* (non-Javadoc)
							 * @see org.eclipse.jst.j2ee.ejb.internal.operations.NewSessionBeanClassOperation#generateInterfacesUsingTemplates(org.eclipse.core.runtime.IProgressMonitor, org.eclipse.jdt.core.IPackageFragment, org.eclipse.jst.j2ee.ejb.internal.operations.CreateSessionBeanTemplateModel)
							 */
							@Override
							protected void generateInterfacesUsingTemplates(
									IProgressMonitor monitor,
									IPackageFragment fragment,
									CreateSessionBeanTemplateModel tempModel)
									throws JETException, JavaModelException {
								if (!(tempModel instanceof CreateSessionBeanWithSingletonTemplateModel) && 
										(tempModel instanceof CreateSessionBeanTemplateModel)) {
										tempModel = new CreateSessionBeanWithSingletonTemplateModel(model);
								}
								super.generateInterfacesUsingTemplates(monitor, fragment, tempModel);
							}

							// this method is the eclipse 3.5 way of doing things
							// we can't use super or officially "override" with the annotation because
							// the method doesn't exist in the 3.4.x code
							//@Override
							@SuppressWarnings("unused")
							protected String generateTemplateSource(CreateJavaEEArtifactTemplateModel templateModel, Object templateImpl) 
								throws JETException {
					            try {
					    			Method generateTemplateSource = templateImpl.getClass().getMethod("generate", new Class[] { Object.class }); //$NON-NLS-1$
					                if (generateTemplateSource != null) {
										if (!(templateModel instanceof CreateSessionBeanWithSingletonTemplateModel) && 
												(templateModel instanceof CreateSessionBeanTemplateModel)) {
											templateModel = new CreateSessionBeanWithSingletonTemplateModel(model);
										}
					                	return (String)generateTemplateSource.invoke(templateImpl, templateModel);
					                }
					            } catch (SecurityException e) {
					                SunAppSrvPlugin.logMessage("in JavaEE6SessionBeanWizard generateTemplateSource : security exception"); //$NON-NLS-1$
					            } catch (NoSuchMethodException e) {
					                SunAppSrvPlugin.logMessage("in JavaEE6SessionBeanWizard generateTemplateSource : no such method exception"); //$NON-NLS-1$
					            } catch (IllegalArgumentException e) {
					                SunAppSrvPlugin.logMessage("in JavaEE6SessionBeanWizard generateTemplateSource : illegal argument exception"); //$NON-NLS-1$
					            } catch (IllegalAccessException e) {
					                SunAppSrvPlugin.logMessage("in JavaEE6SessionBeanWizard generateTemplateSource : illegal access exception"); //$NON-NLS-1$
					            } catch (InvocationTargetException e) {
					                SunAppSrvPlugin.logMessage("in JavaEE6SessionBeanWizard generateTemplateSource : invocation target exception"); //$NON-NLS-1$
					            }
					            return null;
							}
							// this method is the eclipse 3.4.x way of doing things
							/* (non-Javadoc)
							 * @see org.eclipse.jst.j2ee.internal.common.operations.NewJavaEEArtifactClassOperation#generateTemplateSource(org.eclipse.wst.common.frameworks.internal.WTPPlugin, org.eclipse.jst.j2ee.internal.common.operations.CreateJavaEEArtifactTemplateModel, java.lang.String, org.eclipse.core.runtime.IProgressMonitor)
							 */
							@Override
							protected String generateTemplateSource(
									WTPPlugin plugin,
									CreateJavaEEArtifactTemplateModel tempModel,
									String template_file,
									IProgressMonitor monitor)
									throws JETException {
								if (!(tempModel instanceof CreateSessionBeanWithSingletonTemplateModel) && 
									(tempModel instanceof CreateSessionBeanTemplateModel)) {
									tempModel = new CreateSessionBeanWithSingletonTemplateModel(model);
								}
								return super.generateTemplateSource(plugin, tempModel, template_file, monitor);
							}
						};
					}
				};
			}
			@Override
			public IStatus validate(String propertyName) {
				// existing class name is already checked
				// we need to override to remove the error condition for EJB_NAME
				if (EJB_NAME.equals(propertyName)){
					return null;
				}
				return super.validate(propertyName);
			}
			@Override
			public DataModelPropertyDescriptor[] getValidPropertyDescriptors(String propertyName) {
				DataModelPropertyDescriptor[] superDescriptors = super.getValidPropertyDescriptors(propertyName);

				// add SINGLETON choices to STATE_TYPE
				if (propertyName.equals(STATE_TYPE)) {
					int count = superDescriptors.length;
					List<Object> values = new ArrayList<Object>(count + 1);
					List<String> descriptions = new ArrayList<String>(count + 1);

					// copy values and descriptions from superclass
					for (int i = 0; i < count; i++) {
						DataModelPropertyDescriptor descriptor = superDescriptors[i];
						values.add(descriptor.getPropertyValue());
						descriptions.add(descriptor.getPropertyDescription());
					}
					// now add an entry to both arrays for the singleton case
					values.add(StateType_SINGLETON);
					descriptions.add(EJBCreationResourceHandler_STATE_TYPE_SINGLETON);

					return DataModelPropertyDescriptor.createDescriptors(values.toArray(),
							(String[])descriptions.toArray(new String[descriptions.size()]));
				} 
				
				return superDescriptors;
			}
			/* (non-Javadoc)
			 * @see org.eclipse.jst.j2ee.ejb.internal.operations.NewSessionBeanClassDataModelProvider#getDefaultProperty(java.lang.String)
			 */
			@Override
			public Object getDefaultProperty(String propertyName) {
				// local checkbox should not be on by default in the Java EE 6 case
				if (propertyName.equals(LOCAL)) {
					return Boolean.FALSE;
				}
				return super.getDefaultProperty(propertyName);
			}
		};
	}

	static class CreateSessionBeanWithSingletonTemplateModel extends CreateSessionBeanTemplateModel {
		private static final String SINGLETON_ANNOTATION = "@Singleton"; //$NON-NLS-1$
		private static final String QUALIFIED_SINGLETON = "javax.ejb.Singleton"; //$NON-NLS-1$

		public CreateSessionBeanWithSingletonTemplateModel(IDataModel dataModel) {
			super(dataModel);
		}

		/* (non-Javadoc)
		 * @see org.eclipse.jst.j2ee.ejb.internal.operations.CreateSessionBeanTemplateModel#getImports()
		 */
		@Override
		public Collection<String> getImports() {
			// if singleton, can't call super to get imports because state_type comes across as invalid
			// and throws an exception in super's method
			String stateType = dataModel.getStringProperty(STATE_TYPE);
			if (stateType.equals(StateType_SINGLETON)) {
				return Collections.singletonList(QUALIFIED_SINGLETON);
			}
				
			return super.getImports();
		}

		/* (non-Javadoc)
		 * @see org.eclipse.jst.j2ee.ejb.internal.operations.CreateSessionBeanTemplateModel#getClassAnnotation()
		 */
		@Override
		public String getClassAnnotation() {
			String stateType = dataModel.getStringProperty(STATE_TYPE);
			if (stateType.equals(StateType_SINGLETON)) {
				return SINGLETON_ANNOTATION;
			}

			return super.getClassAnnotation();
		}
	}
}
