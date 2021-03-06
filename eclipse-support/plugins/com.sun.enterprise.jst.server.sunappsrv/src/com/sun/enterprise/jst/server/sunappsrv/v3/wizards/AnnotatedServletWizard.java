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
import static org.eclipse.jst.j2ee.internal.web.operations.INewServletClassDataModelProperties.GET_SERVLET_INFO;

import java.io.IOException;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.emf.codegen.jet.JETException;
import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTParser;
import org.eclipse.jdt.core.dom.Annotation;
import org.eclipse.jdt.core.dom.ArrayInitializer;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.Javadoc;
import org.eclipse.jdt.core.dom.MemberValuePair;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Modifier;
import org.eclipse.jdt.core.dom.NormalAnnotation;
import org.eclipse.jdt.core.dom.PrimitiveType;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.StringLiteral;
import org.eclipse.jdt.core.dom.TagElement;
import org.eclipse.jdt.core.dom.TryStatement;
import org.eclipse.jdt.core.dom.TypeDeclaration;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;
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
import org.eclipse.wst.common.frameworks.datamodel.IDataModelOperation;
import org.eclipse.wst.common.frameworks.datamodel.IDataModelProvider;
import org.eclipse.wst.common.frameworks.internal.WTPPlugin;

/**
 * This is a wizard which creates a new annotated servlet, for use 
 * with Java EE 6.  It is provided as a temporary measure to allow for 
 * some Java EE 6 capabilities with v3 in advance of Eclipse WTP providing
 * standard Java EE 6 support.
 */

@SuppressWarnings("restriction")
public class AnnotatedServletWizard extends AddServletWizard {

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

								result.imports().add(CodeGenerationUtils.addWebAnnotationImport(ast, "WebServlet"));	//$NON-NLS-1$
								firstType.modifiers().add(0, addWebServletAnnotation(ast, tempModel, result));
								CodeGenerationUtils.cleanupJavadoc(firstType.getJavadoc(), "@web.servlet"); //$NON-NLS-1$
								addHelpfulMethodBodies(ast, firstType);
								addProcessRequestMethod(ast, firstType, result, 
										((CreateServletTemplateModel)tempModel).getServletName());
								if (model.getBooleanProperty(GET_SERVLET_INFO)) {
									removeGetServletInfoMethodStub(ast, firstType);
									addGetServletInfoMethod(ast, firstType, result);
								}
								return CodeGenerationUtils.getRewrittenSource(source, result);
							}

							@SuppressWarnings("unchecked")
							private void addHelpfulMethodBodies(AST ast, TypeDeclaration firstType) {
								MethodDeclaration[] methods = firstType.getMethods();

								for (int i = 0; i < methods.length; i++) {
									MethodDeclaration methodDeclaration = methods[i];
									if (!methodDeclaration.isConstructor()) {
										String methodName = methodDeclaration.getName().getIdentifier();
										
										// only add to doGet and doPost
										// generate this:
										// processRequest(request, response);
										if (methodName.equals("doPost") || methodName.equals("doGet")) { //$NON-NLS-1$ //$NON-NLS-2$
											List<Statement> statements = methodDeclaration.getBody().statements();
											MethodInvocation methodInvocation = ast.newMethodInvocation();
											ExpressionStatement expressionStatement = ast.newExpressionStatement(methodInvocation);

											methodInvocation.setName(ast.newSimpleName("processRequest")); //$NON-NLS-1$
											methodInvocation.arguments().add(ast.newSimpleName("request")); //$NON-NLS-1$
											methodInvocation.arguments().add(ast.newSimpleName("response")); //$NON-NLS-1$
											statements.add(expressionStatement);
										}
									}
								}
							}
							@SuppressWarnings("unchecked")
							private void addProcessRequestMethod(AST ast, TypeDeclaration firstType, CompilationUnit result, 
									String servletName) {
								MethodDeclaration methodDeclaration = generateProcessRequestMethodDeclaration(ast);
								List<Statement> statements = methodDeclaration.getBody().statements();

								methodDeclaration.setJavadoc(getProcessRequestJavadocComment(ast));
								firstType.bodyDeclarations().add(methodDeclaration);

								// generate this:
								// response.setContentType("text/html;charset=UTF-8");
								statements.add(CodeGenerationUtils.getExpressionStatement(ast, "response", //$NON-NLS-1$
										"setContentType", "text/html;charset=UTF-8")); //$NON-NLS-1$ //$NON-NLS-2$

								// generate this:
								// PrintWriter out = response.getWriter();
								VariableDeclarationFragment vdf = ast.newVariableDeclarationFragment();
								vdf.setName(ast.newSimpleName("out")); //$NON-NLS-1$
								VariableDeclarationStatement vds = ast.newVariableDeclarationStatement(vdf);
								vds.setType(ast.newSimpleType(ast.newSimpleName("PrintWriter"))); //$NON-NLS-1$
								MethodInvocation methodInvocation = ast.newMethodInvocation();
								methodInvocation.setExpression(ast.newSimpleName("response")); //$NON-NLS-1$
								methodInvocation.setName(ast.newSimpleName("getWriter")); //$NON-NLS-1$
								vdf.setInitializer(methodInvocation);
								statements.add(vds);

								// add corresponding import for PrintWriter:
								result.imports().add(CodeGenerationUtils.addImport(ast, "java", "io", null, "PrintWriter")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

								statements.add(getProcessRequestTryStatement(ast, servletName));
							}
							
							private void removeGetServletInfoMethodStub(AST ast, TypeDeclaration firstType) {
								MethodDeclaration[] methods = firstType.getMethods();
								MethodDeclaration foundMethod = null;

								for (int i = 0; i < methods.length; i++) {
									MethodDeclaration methodDeclaration = methods[i];
									if (!methodDeclaration.isConstructor()) {
										if ("getServletInfo".equals(methodDeclaration.getName().getIdentifier())) { //$NON-NLS-1$
											foundMethod = methodDeclaration;
											break;
										}
									}
								}
								if (foundMethod != null) {
									firstType.bodyDeclarations().remove(foundMethod);
								}
							}

							@SuppressWarnings("unchecked")
							private void addGetServletInfoMethod(AST ast, TypeDeclaration firstType, CompilationUnit result) {
								MethodDeclaration methodDeclaration = generateGetServletInfoMethodDeclaration(ast);
								List<Statement> statements = methodDeclaration.getBody().statements();

								methodDeclaration.setJavadoc(getGetServletInfoJavadocComment(ast));
								firstType.bodyDeclarations().add(methodDeclaration);

								// generate this: 
								// return "Short description";
								ReturnStatement returnStatement = ast.newReturnStatement();
								StringLiteral literal = ast.newStringLiteral();
								literal.setLiteralValue("Short description"); //$NON-NLS-1$
								returnStatement.setExpression(literal);
								statements.add(returnStatement);
							}

							// generate this: 
							// protected void processRequest(HttpServletRequest request, HttpServletResponse response)
							// 	throws ServletException, IOException {
							@SuppressWarnings("unchecked")
							private MethodDeclaration generateProcessRequestMethodDeclaration(AST ast) {
								MethodDeclaration methodDeclaration = ast.newMethodDeclaration();

								methodDeclaration.setBody(ast.newBlock());
								methodDeclaration.setConstructor(false);
								methodDeclaration.modifiers().add(ast.newModifier(Modifier.ModifierKeyword.PROTECTED_KEYWORD));
								methodDeclaration.setName(ast.newSimpleName("processRequest")); //$NON-NLS-1$
								methodDeclaration.setReturnType2(ast.newPrimitiveType(PrimitiveType.VOID));
								SingleVariableDeclaration variableDeclaration = ast.newSingleVariableDeclaration();
								variableDeclaration.setType(ast.newSimpleType(ast.newSimpleName("HttpServletRequest"))); //$NON-NLS-1$
								variableDeclaration.setName(ast.newSimpleName("request")); //$NON-NLS-1$
								methodDeclaration.parameters().add(variableDeclaration);
								variableDeclaration = ast.newSingleVariableDeclaration();
								variableDeclaration.setType(ast.newSimpleType(ast.newSimpleName("HttpServletResponse"))); //$NON-NLS-1$
								variableDeclaration.setName(ast.newSimpleName("response")); //$NON-NLS-1$
								methodDeclaration.parameters().add(variableDeclaration);
								methodDeclaration.thrownExceptions().add(ast.newSimpleName("ServletException")); //$NON-NLS-1$
								methodDeclaration.thrownExceptions().add(ast.newSimpleName("IOException")); //$NON-NLS-1$

								return methodDeclaration;
							}

							// generate this: 
						    /** 
						     * Processes requests for both HTTP <code>GET</code> and <code>POST</code> methods.
						     * @param request servlet request
						     * @param response servlet response
						     * @throws ServletException if a servlet-specific error occurs
						     * @throws IOException if an I/O error occurs
						     */
							@SuppressWarnings("unchecked")
							private Javadoc getProcessRequestJavadocComment(AST ast) {
								Javadoc docComment = ast.newJavadoc();
								List tags = docComment.tags();

								tags.add(CodeGenerationUtils.getTagElement(ast, null, null, 
									"Processes requests for both HTTP <code>GET</code> and <code>POST</code> methods.")); //$NON-NLS-1$
								tags.add(CodeGenerationUtils.getTagElement(ast, TagElement.TAG_PARAM, "request", "servlet request")); //$NON-NLS-1$ //$NON-NLS-2$
								tags.add(CodeGenerationUtils.getTagElement(ast, TagElement.TAG_PARAM, "response", "servlet response")); //$NON-NLS-1$ //$NON-NLS-2$
								tags.add(CodeGenerationUtils.getTagElement(ast, TagElement.TAG_THROWS, "ServletException", //$NON-NLS-1$
									"if a servlet-specific error occurs")); //$NON-NLS-1$
								tags.add(CodeGenerationUtils.getTagElement(ast, TagElement.TAG_THROWS, "IOException", //$NON-NLS-1$ 
									"if an I/O error occurs")); //$NON-NLS-1$

								return docComment;
							}

							// generate this: 
					        // try { 
				            //	out.println("<html>");
				            //	out.println("<head>");
				            //	out.println("<title>Servlet <servletname></title>");  
				            //	out.println("</head>");
				            //	out.println("<body>");
				            //	out.println("<h1>Servlet <servletname> at " + request.getContextPath () + "</h1>");
				            //	out.println("</body>");
				            //	out.println("</html>");
					        // }
							@SuppressWarnings("unchecked")
							private TryStatement getProcessRequestTryStatement(AST ast, String servletName) {
								TryStatement tryStatement = ast.newTryStatement();
								Block tryBlock = ast.newBlock();
								List<Statement> tryStatements = tryBlock.statements();

								tryStatements.add(CodeGenerationUtils.getExpressionStatement(ast, "out", "println", "<html>")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
								tryStatements.add(CodeGenerationUtils.getExpressionStatement(ast, "out", "println", "<head>")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
								tryStatements.add(CodeGenerationUtils.getExpressionStatement(ast, "out", "println", //$NON-NLS-1$ //$NON-NLS-2$
										"<title>Servlet " + servletName + "</title>")); //$NON-NLS-1$ //$NON-NLS-2$
								tryStatements.add(CodeGenerationUtils.getExpressionStatement(ast, "out", "println", "</head>")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
								tryStatements.add(CodeGenerationUtils.getExpressionStatement(ast, "out", "println", "<body>")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
								tryStatements.add(CodeGenerationUtils.getExpressionStatement(ast, "out", "println",  //$NON-NLS-1$ //$NON-NLS-2$
										getProcessRequestInfixExpression(ast, servletName)));
								tryStatements.add(CodeGenerationUtils.getExpressionStatement(ast, "out", "println", "</body>")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
								tryStatements.add(CodeGenerationUtils.getExpressionStatement(ast, "out", "println", "</html>")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
								tryStatement.setBody(tryBlock);
								tryStatement.setFinally(getProcessRequestFinallyBlock(ast));

								return tryStatement;
							}
							
							// generate this: 
							// <h1>Servlet <servletname> at " + request.getContextPath () + "</h1>"" +
							private InfixExpression getProcessRequestInfixExpression(AST ast, String servletName) {
								MethodInvocation methodInvocation = 
									CodeGenerationUtils.getMethodInvocation(ast, "request", "getContextPath"); //$NON-NLS-1$ //$NON-NLS-2$
								InfixExpression infixExpression = ast.newInfixExpression();
								InfixExpression infixExpression2 = ast.newInfixExpression();
								StringLiteral literal = ast.newStringLiteral();

								infixExpression.setOperator(InfixExpression.Operator.PLUS);
								literal.setLiteralValue("<h1>Servlet " + servletName + " at "); //$NON-NLS-1$ //$NON-NLS-2$
								infixExpression.setLeftOperand(literal);
								infixExpression.setRightOperand(methodInvocation);
								infixExpression2.setOperator(InfixExpression.Operator.PLUS);
								infixExpression2.setLeftOperand(infixExpression);
								literal = ast.newStringLiteral();
								literal.setLiteralValue("</h1>"); //$NON-NLS-1$
								infixExpression2.setRightOperand(literal);

								return infixExpression2;
							}

							// generate this: 
					        // } finally { 
					        //    out.close();
					        // }
							@SuppressWarnings("unchecked")
							private Block getProcessRequestFinallyBlock(AST ast) {
								Block finallyBlock = ast.newBlock();
								finallyBlock.statements().add(
										CodeGenerationUtils.getExpressionStatement(ast, "out", "close")); //$NON-NLS-1$ //$NON-NLS-2$
								return finallyBlock;
							}

							// generate this: 
							// @Override
							// public String getServletInfo() {
							@SuppressWarnings("unchecked")
							private MethodDeclaration generateGetServletInfoMethodDeclaration(AST ast) {
								MethodDeclaration methodDeclaration = ast.newMethodDeclaration();
								

								methodDeclaration.setBody(ast.newBlock());
								methodDeclaration.setConstructor(false);
								methodDeclaration.modifiers().add(ast.newModifier(Modifier.ModifierKeyword.PUBLIC_KEYWORD));
								methodDeclaration.setName(ast.newSimpleName("getServletInfo")); //$NON-NLS-1$
								methodDeclaration.setReturnType2(ast.newSimpleType(ast.newSimpleName("String"))); //$NON-NLS-1$

								Annotation ann = ast.newMarkerAnnotation();
								ann.setTypeName(ast.newSimpleName("Override"));	//$NON-NLS-1$
								methodDeclaration.modifiers().add(0, ann);

								return methodDeclaration;
							}

							// generate this: 
							/** 
							 * Returns a short description of the servlet.
							 * @return a String containing servlet description
							 */
							@SuppressWarnings("unchecked")
							private Javadoc getGetServletInfoJavadocComment(AST ast) {
								Javadoc docComment = ast.newJavadoc();
								List tags = docComment.tags();

								tags.add(CodeGenerationUtils.getTagElement(ast, null, null, 
									"Returns a short description of the servlet.")); //$NON-NLS-1$
								tags.add(CodeGenerationUtils.getTagElement(ast, TagElement.TAG_RETURN, null, 
									"a String containing servlet description")); //$NON-NLS-1$

								return docComment;
							}

							@SuppressWarnings("unchecked")
							private Annotation addWebServletAnnotation(AST ast, CreateJavaEEArtifactTemplateModel tempModel,
									CompilationUnit result) {
								Annotation ann = ast.newNormalAnnotation();
								ann.setTypeName(ast.newSimpleName("WebServlet"));	//$NON-NLS-1$

								if (tempModel instanceof CreateServletTemplateModel) {
									CreateServletTemplateModel servletModel = (CreateServletTemplateModel)tempModel;
									List<MemberValuePair> annValues = ((NormalAnnotation) ann).values();
									List<String[]> initParams = servletModel.getInitParams();

									annValues.add(CodeGenerationUtils.addName(ast, servletModel.getServletName(), "name")); //$NON-NLS-1$
									annValues.add(addURLPatterns(ast, servletModel.getServletMappings()));
									if (initParams != null) {
										annValues.add(CodeGenerationUtils.addInitParams(ast, initParams, result));
									}
								}

								return ann;
							}

							@SuppressWarnings("unchecked")
							private MemberValuePair addURLPatterns(AST ast, List<String[]> servletMappings) {
								MemberValuePair annotationProperty = ast.newMemberValuePair();
								ArrayInitializer arrayInit = ast.newArrayInitializer();

								annotationProperty.setName(ast.newSimpleName("urlPatterns"));	//$NON-NLS-1$
								for (Iterator<String[]> iterator = servletMappings.iterator(); iterator.hasNext();) {
									String[] strings = iterator.next();
									StringLiteral literal = ast.newStringLiteral();

									literal.setLiteralValue(strings[0]);
									arrayInit.expressions().add(literal);
								}
								annotationProperty.setValue(arrayInit);
								return annotationProperty;
							}
						};
					}

				};
			}

			/* (non-Javadoc)
			 * @see org.eclipse.jst.j2ee.internal.web.operations.NewServletClassDataModelProvider#getDefaultProperty(java.lang.String)
			 */
			@Override
			public Object getDefaultProperty(String propertyName) {
				// Generate a getServletInfo method by default
				if (propertyName.equals(GET_SERVLET_INFO)) {
					return Boolean.TRUE;
				}
				
				return super.getDefaultProperty(propertyName);
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
								return WizardUtil.hasGF3Runtime(project);
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
