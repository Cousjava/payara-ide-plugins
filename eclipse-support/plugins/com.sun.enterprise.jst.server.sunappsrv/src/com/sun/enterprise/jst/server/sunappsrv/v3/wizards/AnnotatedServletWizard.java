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

import static org.eclipse.jst.j2ee.application.internal.operations.IAnnotationsDataModel.USE_ANNOTATIONS;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.emf.codegen.jet.JETException;
import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTParser;
import org.eclipse.jdt.core.dom.Annotation;
import org.eclipse.jdt.core.dom.ArrayInitializer;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.ImportDeclaration;
import org.eclipse.jdt.core.dom.Javadoc;
import org.eclipse.jdt.core.dom.MemberValuePair;
import org.eclipse.jdt.core.dom.NormalAnnotation;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.StringLiteral;
import org.eclipse.jdt.core.dom.TagElement;
import org.eclipse.jdt.core.dom.TypeDeclaration;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jst.j2ee.internal.common.operations.CreateJavaEEArtifactTemplateModel;
import org.eclipse.jst.j2ee.internal.common.operations.NewJavaEEArtifactClassOperation;
import org.eclipse.jst.j2ee.internal.project.J2EEProjectUtilities;
import org.eclipse.jst.j2ee.internal.web.operations.AddServletOperation;
import org.eclipse.jst.j2ee.internal.web.operations.CreateServletTemplateModel;
import org.eclipse.jst.j2ee.internal.web.operations.NewServletClassDataModelProvider;
import org.eclipse.jst.j2ee.internal.web.operations.NewServletClassOperation;
import org.eclipse.jst.servlet.ui.IWebUIContextIds;
import org.eclipse.jst.servlet.ui.internal.wizard.AddServletWizard;
import org.eclipse.jst.servlet.ui.internal.wizard.NewServletClassWizardPage;
import org.eclipse.text.edits.MalformedTreeException;
import org.eclipse.text.edits.TextEdit;
import org.eclipse.wst.common.frameworks.datamodel.IDataModelOperation;
import org.eclipse.wst.common.frameworks.datamodel.IDataModelProvider;
import org.eclipse.wst.common.frameworks.internal.WTPPlugin;
import org.eclipse.wst.server.core.IRuntime;

import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;

/**
 * This is a wizard which creates a new annotated servlet, for use 
 * with Java EE 6.  It is provided as a temporary measure to allow for 
 * some Java EE 6 capabilities with v3 in advance of Eclipse WTP providing
 * standard Java EE 6 support.
 */

@SuppressWarnings("restriction")
public class AnnotatedServletWizard extends AddServletWizard {

	private static final String V3_RUNTIME = "com.sun.enterprise.jst.server.runtime.sunappsrv92"; //$NON-NLS-1$

	@Override
	protected IDataModelProvider getDefaultProvider() {
		return new NewServletClassDataModelProvider() {

			/* (non-Javadoc)
			 * @see org.eclipse.jst.j2ee.internal.web.operations.NewServletClassDataModelProvider#getDefaultOperation()
			 */
			@Override
			public IDataModelOperation getDefaultOperation() {
				return new AddServletOperation(model) {

					/* (non-Javadoc)
					 * @see org.eclipse.jst.j2ee.internal.web.operations.AddServletOperation#getNewClassOperation()
					 */
					@Override
					protected NewJavaEEArtifactClassOperation getNewClassOperation() {
						return new NewServletClassOperation(getDataModel()) {
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
					                	Object superSource = generateTemplateSource.invoke(templateImpl, templateModel);
										return operateOnSourceGeneratedBySuper((String)superSource, templateModel);
					                }
					            } catch (SecurityException e) {
					                SunAppSrvPlugin.logMessage("in AnnotatedServletWizard generateTemplateSource : security exception"); //$NON-NLS-1$
					            } catch (NoSuchMethodException e) {
					                SunAppSrvPlugin.logMessage("in AnnotatedServletWizard generateTemplateSource : no such method exception"); //$NON-NLS-1$
					            } catch (IllegalArgumentException e) {
					                SunAppSrvPlugin.logMessage("in AnnotatedServletWizard generateTemplateSource : illegal argument exception"); //$NON-NLS-1$
					            } catch (IllegalAccessException e) {
					                SunAppSrvPlugin.logMessage("in AnnotatedServletWizard generateTemplateSource : illegal access exception"); //$NON-NLS-1$
					            } catch (InvocationTargetException e) {
					                SunAppSrvPlugin.logMessage("in AnnotatedServletWizard generateTemplateSource : invocation target exception"); //$NON-NLS-1$
					            }
					            return null;
							}
							// this method is the eclipse 3.4.x way of doing things
							@Override
							protected String generateTemplateSource(WTPPlugin plugin, CreateJavaEEArtifactTemplateModel tempModel, String template_file, IProgressMonitor monitor) throws JETException {
								return operateOnSourceGeneratedBySuper(
										super.generateTemplateSource(plugin, tempModel, template_file, monitor), tempModel);
							}

							private String operateOnSourceGeneratedBySuper(String source, CreateJavaEEArtifactTemplateModel tempModel) {
								ASTParser parser = ASTParser.newParser(AST.JLS3);  // handles JDK 1.0, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6
								parser.setSource(source.toCharArray());
								CompilationUnit result = (CompilationUnit) parser.createAST(null);
								result.recordModifications();
								AST ast = result.getAST();
								TypeDeclaration firstType = (TypeDeclaration)result.types().get(0);

								result.imports().add(addWebAnnotationImport(ast, "WebServlet"));	//$NON-NLS-1$
								firstType.modifiers().add(0, addWebServletAnnotation(ast, tempModel, result));
								cleanupJavadoc(firstType.getJavadoc());

								return getRewrittenSource(source, result);
							}

							private String getRewrittenSource(String source, CompilationUnit result) {
								Document doc = new Document(source);
								TextEdit edits = result.rewrite(doc,null);
								try {
									edits.apply(doc);
									return doc.get();
								} catch (MalformedTreeException e) {
									e.printStackTrace();
								} catch (BadLocationException e) {
									e.printStackTrace();
								}
								return source;
							}

							private void cleanupJavadoc(Javadoc javadoc) {
								Iterator it = javadoc.tags().iterator();
								while (it.hasNext()) {
									TagElement tagElement = (TagElement) it.next();
									String tagName = tagElement.getTagName();
									if ((tagName != null) && tagName.startsWith("@web.servlet")) {	//$NON-NLS-1$
										it.remove();
									}
								}
							}

							private Annotation addWebServletAnnotation(AST ast, CreateJavaEEArtifactTemplateModel tempModel,
									CompilationUnit result) {
								Annotation ann = ast.newNormalAnnotation();
								ann.setTypeName(ast.newSimpleName("WebServlet"));	//$NON-NLS-1$

								if (tempModel instanceof CreateServletTemplateModel) {
									CreateServletTemplateModel servletModel = (CreateServletTemplateModel)tempModel;
									List annValues = ((NormalAnnotation) ann).values();
									List<String[]> initParams = servletModel.getInitParams();

									annValues.add(addName(ast, servletModel.getServletName()));
									annValues.add(addURLPatterns(ast, servletModel.getServletMappings()));
									if (initParams != null) {
										annValues.add(addInitParams(ast, initParams, result));
									}
								}

								return ann;
							}

							private MemberValuePair addName(AST ast, String servletName) {
								MemberValuePair annotationProperty = ast.newMemberValuePair();
								StringLiteral literal = ast.newStringLiteral();

								literal.setLiteralValue(servletName);
								annotationProperty.setName(ast.newSimpleName("name"));	//$NON-NLS-1$
								annotationProperty.setValue(literal);

								return annotationProperty;
							}

							private MemberValuePair addURLPatterns(AST ast, List<String[]> servletMappings) {
								MemberValuePair annotationProperty = ast.newMemberValuePair();
								ArrayInitializer arrayInit = ast.newArrayInitializer();

								annotationProperty.setName(ast.newSimpleName("urlPatterns"));	//$NON-NLS-1$
								for (Iterator iterator = servletMappings.iterator(); iterator.hasNext();) {
									String[] strings = (String[]) iterator.next();
									StringLiteral literal = ast.newStringLiteral();

									literal.setLiteralValue(strings[0]);
									arrayInit.expressions().add(literal);
								}
								annotationProperty.setValue(arrayInit);
								return annotationProperty;
							}

							private MemberValuePair addInitParams(AST ast, List<String[]> initParams, CompilationUnit result) {
								MemberValuePair annotationProperty = ast.newMemberValuePair();
								ArrayInitializer arrayInit = ast.newArrayInitializer();

								result.imports().add(addWebAnnotationImport(ast, "WebInitParam"));	//$NON-NLS-1$

								annotationProperty.setName(ast.newSimpleName("initParams"));	//$NON-NLS-1$
								for (Iterator iterator = initParams.iterator(); iterator.hasNext();) {
									String[] strings = (String[]) iterator.next();
									StringLiteral literal = ast.newStringLiteral();
									Annotation ann = ast.newNormalAnnotation();
									MemberValuePair annotationProperty1 = ast.newMemberValuePair();
									List annValues = ((NormalAnnotation) ann).values();

									ann.setTypeName(ast.newSimpleName("WebInitParam"));	//$NON-NLS-1$
									literal.setLiteralValue(strings[0]);
									annotationProperty1.setName(ast.newSimpleName("name"));	//$NON-NLS-1$
									annotationProperty1.setValue(literal);
									annValues.add(annotationProperty1);

									annotationProperty1 = ast.newMemberValuePair();
									literal = ast.newStringLiteral();
									literal.setLiteralValue(strings[1]);
									annotationProperty1.setName(ast.newSimpleName("value"));	//$NON-NLS-1$
									annotationProperty1.setValue(literal);
									annValues.add(annotationProperty1);

									arrayInit.expressions().add(ann);
								}
								annotationProperty.setValue(arrayInit);

								return annotationProperty;
							}

							private ImportDeclaration addWebAnnotationImport(AST ast, String className) {
								ImportDeclaration importDecl = ast.newImportDeclaration();
								QualifiedName name = ast.newQualifiedName(
									ast.newSimpleName("javax"),			//$NON-NLS-1$
									ast.newSimpleName("servlet"));		//$NON-NLS-1$
								name = ast.newQualifiedName(name,
									ast.newSimpleName("annotation"));	//$NON-NLS-1$
								name = ast.newQualifiedName(name, 
									ast.newSimpleName(className));
								importDecl.setName(name);
								return importDecl;
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
		if (page instanceof NewServletClassWizardPage) {
			NewServletClassWizardPage page1 = new NewServletClassWizardPage(
					getDataModel(), page.getName(), page.getDescription(),
					page.getTitle(), J2EEProjectUtilities.DYNAMIC_WEB) {

						/* (non-Javadoc)
						 * @see org.eclipse.jst.j2ee.internal.wizard.NewJavaClassWizardPage#isProjectValid(org.eclipse.core.resources.IProject)
						 */
						@Override
						protected boolean isProjectValid(IProject project) {
							if (super.isProjectValid(project)) {
								try {
									IRuntime runtime = J2EEProjectUtilities.getServerRuntime(project);
									if ((runtime != null) && runtime.getRuntimeType().getId().equals(V3_RUNTIME)){
										return true;
									}
								} catch (CoreException e) {
									// TODO Auto-generated catch block
									e.printStackTrace();
								}
							}
							return false;
						}			
			};
			page1.setInfopopID(IWebUIContextIds.WEBEDITOR_SERVLET_PAGE_ADD_SERVLET_WIZARD_1);
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
