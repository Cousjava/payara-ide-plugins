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
import static org.eclipse.jst.j2ee.ejb.internal.operations.INewEnterpriseBeanClassDataModelProperties.MAPPED_NAME;
import static org.eclipse.jst.j2ee.ejb.internal.operations.INewSessionBeanClassDataModelProperties.LOCAL;
import static org.eclipse.jst.j2ee.ejb.internal.operations.INewSessionBeanClassDataModelProperties.REMOTE;
import static org.eclipse.jst.j2ee.ejb.internal.operations.INewSessionBeanClassDataModelProperties.STATE_TYPE;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.emf.codegen.jet.JETException;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jst.ejb.ui.internal.util.EJBUIMessages;
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
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.forms.widgets.ExpandableComposite;
import org.eclipse.wst.common.frameworks.datamodel.DataModelPropertyDescriptor;
import org.eclipse.wst.common.frameworks.datamodel.IDataModel;
import org.eclipse.wst.common.frameworks.datamodel.IDataModelOperation;
import org.eclipse.wst.common.frameworks.datamodel.IDataModelProvider;
import org.eclipse.wst.common.frameworks.internal.WTPPlugin;
import org.eclipse.wst.common.frameworks.internal.datamodel.ui.DataModelSynchHelper;
import org.eclipse.wst.common.frameworks.internal.plugin.WTPCommonPlugin;

import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;

@SuppressWarnings("restriction")
public class JavaEE6SessionBeanWizard extends AddSessionBeanWizard {

	// mimic state type enumeration and value with our own constants
	private static final String StateType_SINGLETON = "SINGLETON"; //$NON-NLS-1$
	private static final String EJBCreationResourceHandler_STATE_TYPE_SINGLETON = "Singleton"; //$NON-NLS-1$

	// share help context with superclass, but copy the string here so no dependency issues
	private static final String PLUGIN_EJB_UI = "org.eclipse.jst.ejb.ui."; //$NON-NLS-1$
	private static final String EJB_SESSION_BEAN_WIZARD_ADD_SESSION_BEAN_PAGE_1 = PLUGIN_EJB_UI + "sessbw1100"; //$NON-NLS-1$
	private static final String EJB_SESSION_BEAN_WIZARD_ADD_SESSION_BEAN_PAGE_2 = PLUGIN_EJB_UI + "sessbw1200"; //$NON-NLS-1$

	private static final String NO_INTERFACE = "INewSessionBeanClassDataModelProperties.NO_INTERFACE"; //$NON-NLS-1$
	private static final String IEjbWizardConstants_NO_INTERFACE = "No-interface"; //$NON-NLS-1$

	public JavaEE6SessionBeanWizard(IDataModel model) {
		super(model);
		setWindowTitle(Messages.sessionWizardTitle);
	}

	public JavaEE6SessionBeanWizard() {
		this(null);
	}

	/* (non-Javadoc)
	 * @see org.eclipse.wst.common.frameworks.internal.datamodel.ui.DataModelWizard#addPage(org.eclipse.jface.wizard.IWizardPage)
	 */
	@Override
	public void addPage(IWizardPage page) {
		// replace first page with a subclass that lets us do different project validation
		if (page instanceof NewSessionBeanClassWizardPage) {
			NewSessionBeanClassWizardPage page1 = new NewSessionBeanClassWizardPage(
					getDataModel(), page.getName(), page.getDescription(),
					Messages.sessionWizardTitle, J2EEProjectUtilities.DYNAMIC_WEB + J2EEProjectUtilities.EJB) {

						/* (non-Javadoc)
						 * @see org.eclipse.jst.j2ee.internal.wizard.NewJavaClassWizardPage#isProjectValid(org.eclipse.core.resources.IProject)
						 */
						@Override
						protected boolean isProjectValid(IProject project) {
							// super's test for isProjectValid requires an ejb project and we don't 
							// want to do that, so result is basically a test of the grandsuper's isProjectValid
							// with the addition of allowing both ejb and web projects
							return WizardUtil.isWebOrEJBProjectWithGF3Runtime(project);
						}

						@Override
						protected Composite createTopLevelComposite(Composite parent) {
							Composite composite = super.createTopLevelComposite(parent);

							projectNameLabel.setText(Messages.ProjectName);
							Group group = lookupLocalGroup();
							if (group != null) {
								Button nointerfaceCheckbox = new Button(group, SWT.CHECK);
								nointerfaceCheckbox.setText(IEjbWizardConstants_NO_INTERFACE);
								synchHelper.synchCheckbox(nointerfaceCheckbox, NO_INTERFACE, null);
							}
							return composite;
						}
						/* (non-Javadoc)
						 * @see org.eclipse.wst.common.frameworks.internal.datamodel.ui.DataModelWizardPage#initializeSynchHelper(org.eclipse.wst.common.frameworks.datamodel.IDataModel)
						 */
						@Override
						public DataModelSynchHelper initializeSynchHelper(
								IDataModel dm) {
							return new ExposedControlSynchHelper(dm);
						}
						private Group lookupLocalGroup() {
							if (synchHelper instanceof ExposedControlSynchHelper) {
								Control localControl = ((ExposedControlSynchHelper)synchHelper).getControl(LOCAL);
								if (localControl instanceof Button) {
									Control parentControl = ((Button)localControl).getParent();
									if (parentControl instanceof Group) {
										return (Group)parentControl;
									}
								}
							}
							return null;
						}

						/* (non-Javadoc)
						 * @see org.eclipse.jst.ejb.ui.internal.wizard.NewSessionBeanClassWizardPage#getValidationPropertyNames()
						 */
						@Override
						protected String[] getValidationPropertyNames() {
							String[] retVal = null;
							String[] baseVals = super.getValidationPropertyNames();
							retVal = new String[baseVals.length+3];
							for (int cnt=0; cnt < baseVals.length; cnt++)
							{
								retVal[cnt] = baseVals[cnt];
							}
							retVal[baseVals.length] = LOCAL;
							retVal[baseVals.length+1] = REMOTE;
							retVal[baseVals.length+2] = NO_INTERFACE;
							return retVal;
						}
			};
			page1.setInfopopID(EJB_SESSION_BEAN_WIZARD_ADD_SESSION_BEAN_PAGE_1);
			page = page1;
		}
		// replace second page with a subclass that lets us have a different title and remove some unneeded components
		if (page instanceof AddSessionBeanWizardPage) {
			AddSessionBeanWizardPage page2 = new AddSessionBeanWizardPage(
					getDataModel(), page.getName()) {
				@Override
				protected Composite createTopLevelComposite(Composite parent) {
					Composite composite = super.createTopLevelComposite(parent);

					// find and remove mapped name label, text and entire expandable ejb 2.1 section
					hideControl(lookupControlWithPropertyName(MAPPED_NAME));
					hideControl(findLabelWithName(composite, EJBUIMessages.MAPPED_NAME));
					hideControl(findFirstControlOfType(composite, ExpandableComposite.class));

					return composite;
				}
				/* (non-Javadoc)
				 * @see org.eclipse.jst.ejb.ui.internal.wizard.AddEnterpriseBeanWizardPage#createStubsComposite(org.eclipse.swt.widgets.Composite)
				 */
				@Override
				protected void createStubsComposite(Composite parent) {
					// skip this
				}
				/* (non-Javadoc)
				 * @see org.eclipse.wst.common.frameworks.internal.datamodel.ui.DataModelWizardPage#initializeSynchHelper(org.eclipse.wst.common.frameworks.datamodel.IDataModel)
				 */
				@Override
				public DataModelSynchHelper initializeSynchHelper(
						IDataModel dm) {
					return new ExposedControlSynchHelper(dm);
				}
				private void hideControl(Control control) {
					if (control != null) {
						control.setVisible(false);
						GridData data = new GridData();
					    data.exclude = true;
					    data.horizontalSpan = 2;
					    data.horizontalAlignment = SWT.FILL;
					    control.setLayoutData(data);
					}
				}
				private Label findLabelWithName(Composite composite, String nameToFind) {
					Control[] children = composite.getChildren();
					for (int i = 0; i < children.length; i++) {
						Control control = children[i];
						if ((control instanceof Label) && ((Label)control).getText().equals(nameToFind)) {
							return (Label)control;
						}
						if (control instanceof Composite) {
							Control innerControl = findLabelWithName((Composite)control, nameToFind);
							if (innerControl != null) {
								return (Label)innerControl;
							}
						}
					}
					return null;
				}
				@SuppressWarnings("unchecked")
				private Control findFirstControlOfType(Composite composite, Class typeToFind) {
					Control[] children = composite.getChildren();
					for (int i = 0; i < children.length; i++) {
						Control control = children[i];
						if (typeToFind.isAssignableFrom(control.getClass())) {
							return control;
						}
						if (control instanceof Composite) {
							Control innerControl = findFirstControlOfType((Composite)control, typeToFind);
							if (innerControl != null) {
								return innerControl;
							}
						}
					}
					return null;
				}
				private Control lookupControlWithPropertyName(String propertyName) {
					if (synchHelper instanceof ExposedControlSynchHelper) {
						return ((ExposedControlSynchHelper)synchHelper).getControl(propertyName);
						
					}
					return null;
				}
			};
			page2.setTitle(Messages.sessionWizardTitle);
			page2.setInfopopID(EJB_SESSION_BEAN_WIZARD_ADD_SESSION_BEAN_PAGE_2);
			page = page2;
		}
		super.addPage(page);
	}
	@Override
	protected IDataModelProvider getDefaultProvider() {
		return (IDataModelProvider) new NewSessionBeanClassDataModelProvider() {
			/* (non-Javadoc)
			 * @see org.eclipse.jst.j2ee.ejb.internal.operations.NewSessionBeanClassDataModelProvider#getPropertyNames()
			 */
			@Override
			public Set<String> getPropertyNames() {
				Set<String> propertyNames = (Set<String>) super.getPropertyNames();

				propertyNames.add(NO_INTERFACE);
				return propertyNames;
			}
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
				// need to make sure at least one of these are set to true
				if (propertyName.equals(LOCAL) || 
						propertyName.equals(REMOTE) ||
						propertyName.equals(NO_INTERFACE)) {
					boolean oneCheckbox = validateOneCheckboxTrue();
					if (!oneCheckbox) {
						return WTPCommonPlugin.createErrorStatus(
								Messages.errorBusinessInterfaceMissing);
					}
				}

				return super.validate(propertyName);
			}
			private boolean validateOneCheckboxTrue() {
				boolean hasLocal = getBooleanProperty(LOCAL);
				boolean hasRemote = getBooleanProperty(REMOTE);
				boolean hasNoInterface = getBooleanProperty(NO_INTERFACE);
				return (hasLocal || hasRemote || hasNoInterface);
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
				// no-interface checkbox should be default instead of local checkbox in the Java EE 6 case
				if (propertyName.equals(LOCAL)) {
					return Boolean.FALSE;
				}
				if (propertyName.equals(NO_INTERFACE)) {
					return Boolean.TRUE;
				}
				return super.getDefaultProperty(propertyName);
			}
			
			/**
			 * Super method breaks with CCE when we have non-EJB project, so leave it for the future to fix itself and no check atm.
			 */
			@Override
			protected IStatus validateEjbName() {
				try {
					return super.validateEjbName();
				} catch (ClassCastException e) {
					// ignore atm
					// SunAppSrvPlugin.logMessage("failed to validate EjbName", e);
					return Status.OK_STATUS;
				}
			}
		};
	}

	static class ExposedControlSynchHelper extends DataModelSynchHelper {

		public ExposedControlSynchHelper(IDataModel model) {
			super(model);
		}
		protected Control getControl(String propertyName) {
			Control control = null;
			if (propertyToWidgetHash != null) {
				control = (Control) propertyToWidgetHash.get(propertyName);
			}
			return control;
		}
	}

	static class CreateSessionBeanWithSingletonTemplateModel extends CreateSessionBeanTemplateModel {
		private static final String SINGLETON_ANNOTATION = "@Singleton"; //$NON-NLS-1$
		private static final String QUALIFIED_SINGLETON = "javax.ejb.Singleton"; //$NON-NLS-1$
		private static final String LOCALBEAN_ANNOTATION = "@LocalBean"; //$NON-NLS-1$
		private static final String QUALIFIED_LOCALBEAN = "javax.ejb.LocalBean"; //$NON-NLS-1$

		public CreateSessionBeanWithSingletonTemplateModel(IDataModel dataModel) {
			super(dataModel);
		}

		/* (non-Javadoc)
		 * @see org.eclipse.jst.j2ee.ejb.internal.operations.CreateSessionBeanTemplateModel#getImports()
		 */
		@Override
		public Collection<String> getImports() {
			Collection<String> returnImports = null;

			// if singleton, can't call super to get imports because state_type comes across as invalid
			// and throws an exception in super's method
			String stateType = dataModel.getStringProperty(STATE_TYPE);
			if (stateType.equals(StateType_SINGLETON)) {
				returnImports = new ArrayList<String>();
				returnImports.add(QUALIFIED_SINGLETON);
			} else {
				returnImports = super.getImports();
			}

			if (isNoInterface()) {
				returnImports.add(QUALIFIED_LOCALBEAN);
			}

			return returnImports;
		}

		/* (non-Javadoc)
		 * @see org.eclipse.jst.j2ee.ejb.internal.operations.CreateSessionBeanTemplateModel#getClassAnnotation()
		 */
		@Override
		public String getClassAnnotation() {
			String noInterfacePrefix = (isNoInterface() ? LOCALBEAN_ANNOTATION + "\n" : ""); //$NON-NLS-1$ //$NON-NLS-2$
			String stateType = dataModel.getStringProperty(STATE_TYPE);
			String returnAnnotation = (stateType.equals(StateType_SINGLETON) ? SINGLETON_ANNOTATION : super.getClassAnnotation());

			return noInterfacePrefix + returnAnnotation;
		}
		private boolean isNoInterface() {
			return Boolean.TRUE.equals(dataModel.getBooleanProperty(NO_INTERFACE));
		}
	}
}
