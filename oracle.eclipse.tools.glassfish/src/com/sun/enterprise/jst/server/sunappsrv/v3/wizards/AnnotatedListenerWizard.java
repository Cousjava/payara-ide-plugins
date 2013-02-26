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

// </editor-fold>

package com.sun.enterprise.jst.server.sunappsrv.v3.wizards;

import static org.eclipse.jst.j2ee.application.internal.operations.IAnnotationsDataModel.USE_ANNOTATIONS;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.emf.codegen.jet.JETException;
import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTParser;
import org.eclipse.jdt.core.dom.Annotation;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.TypeDeclaration;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jst.j2ee.internal.common.operations.CreateJavaEEArtifactTemplateModel;
import org.eclipse.jst.j2ee.internal.common.operations.NewJavaEEArtifactClassOperation;
import org.eclipse.jst.j2ee.internal.project.J2EEProjectUtilities;
import org.eclipse.jst.j2ee.internal.web.operations.AddListenerOperation;
import org.eclipse.jst.j2ee.internal.web.operations.NewListenerClassDataModelProvider;
import org.eclipse.jst.j2ee.internal.web.operations.NewListenerClassOperation;
import org.eclipse.jst.servlet.ui.IWebUIContextIds;
import org.eclipse.jst.servlet.ui.internal.wizard.AddListenerWizard;
import org.eclipse.jst.servlet.ui.internal.wizard.NewListenerClassWizardPage;
import org.eclipse.wst.common.frameworks.datamodel.IDataModelOperation;
import org.eclipse.wst.common.frameworks.datamodel.IDataModelProvider;
import org.eclipse.wst.common.frameworks.internal.WTPPlugin;

/**
 * This is a wizard which creates a new annotated listener, for use 
 * with Java EE 6.  It is provided as a temporary measure to allow for 
 * some Java EE 6 capabilities with v3 in advance of Eclipse WTP providing
 * standard Java EE 6 support.
 */

@SuppressWarnings("restriction")
public class AnnotatedListenerWizard extends AddListenerWizard {

	@Override
	protected IDataModelProvider getDefaultProvider() {
		return new NewListenerClassDataModelProvider() {

			/* (non-Javadoc)
			 * @see org.eclipse.jst.j2ee.internal.web.operations.NewListenerClassDataModelProvider#getDefaultOperation()
			 */
			@Override
			public IDataModelOperation getDefaultOperation() {
				return new AddListenerOperation(model) {

					/* (non-Javadoc)
					 * @see org.eclipse.jst.j2ee.internal.web.operations.AddListenerOperation#getNewClassOperation()
					 */
					@Override
					protected NewJavaEEArtifactClassOperation getNewClassOperation() {
						return new NewListenerClassOperation(model) {
							// this method is the eclipse 3.5 way of doing things
							// we can't use super because the method doesn't exist
							// in the older 3.4 code
							@Override
							protected String generateTemplateSource(CreateJavaEEArtifactTemplateModel templateModel, Object templateImpl) 
								throws JETException {
								String superSource = CodeGenerationUtils.get35SuperTemplateSource(templateModel, templateImpl);
								if (superSource != null) {
									return operateOnSourceGeneratedBySuper(superSource, templateModel);
								}
					            return null;
							}
							// this method is the eclipse 3.4.x way of doing things
							@Override
							protected String generateTemplateSource(WTPPlugin plugin, CreateJavaEEArtifactTemplateModel tempModel, String template_file, IProgressMonitor monitor) throws JETException {
								return operateOnSourceGeneratedBySuper(
										super.generateTemplateSource(plugin, tempModel, template_file, monitor), tempModel);
							}

							@SuppressWarnings("unchecked")
							private String operateOnSourceGeneratedBySuper(String source, CreateJavaEEArtifactTemplateModel tempModel) {
								ASTParser parser = ASTParser.newParser(AST.JLS3);  // handles JDK 1.0, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6
								parser.setSource(source.toCharArray());
								CompilationUnit result = (CompilationUnit) parser.createAST(null);
								result.recordModifications();
								AST ast = result.getAST();
								TypeDeclaration firstType = (TypeDeclaration)result.types().get(0);
								Annotation ann = ast.newMarkerAnnotation();

								result.imports().add(CodeGenerationUtils.addWebAnnotationImport(ast, "WebListener"));	//$NON-NLS-1$
								ann.setTypeName(ast.newSimpleName("WebListener"));	//$NON-NLS-1$
								firstType.modifiers().add(0, ann);
								CodeGenerationUtils.cleanupJavadoc(firstType.getJavadoc(), "@web.listener"); //$NON-NLS-1$

								return CodeGenerationUtils.getRewrittenSource(source, result);
							}
						};
					}
				};
			}
		};
	}

	/* (non-Javadoc)
	 * @see org.eclipse.wst.common.frameworks.internal.datamodel.ui.DataModelWizard#addPage(org.eclipse.jface.wizard.IWizardPage)
	 */
	@Override
	public void addPage(IWizardPage page) {
		// replace first page with a subclass that lets us do project validation
		if (page instanceof NewListenerClassWizardPage) {
			NewListenerClassWizardPage page1 = new NewListenerClassWizardPage(
					getDataModel(), page.getName(), page.getDescription(),
					page.getTitle(), J2EEProjectUtilities.DYNAMIC_WEB) {

						/* (non-Javadoc)
						 * @see org.eclipse.jst.j2ee.internal.wizard.NewJavaClassWizardPage#isProjectValid(org.eclipse.core.resources.IProject)
						 */
						@Override
						protected boolean isProjectValid(IProject project) {
							if (super.isProjectValid(project)) {
								return WizardUtil.hasGF3Runtime(project);
							}
							return false;
						}			
			};
			page1.setInfopopID(IWebUIContextIds.WEBEDITOR_LISTENER_PAGE_ADD_LISTENER_WIZARD_1);
			page = page1;
		}
		super.addPage(page);
	}

	/* (non-Javadoc)
	 * @see org.eclipse.wst.common.frameworks.internal.datamodel.ui.DataModelWizard#prePerformFinish()
	 */
	@Override
	protected boolean prePerformFinish() {
		getDataModel().setProperty(USE_ANNOTATIONS, true);
		return super.prePerformFinish();
	}
}
