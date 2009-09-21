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
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.ImportDeclaration;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.Javadoc;
import org.eclipse.jdt.core.dom.MemberValuePair;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Modifier;
import org.eclipse.jdt.core.dom.NormalAnnotation;
import org.eclipse.jdt.core.dom.PrimitiveType;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.StringLiteral;
import org.eclipse.jdt.core.dom.TagElement;
import org.eclipse.jdt.core.dom.TextElement;
import org.eclipse.jdt.core.dom.TryStatement;
import org.eclipse.jdt.core.dom.TypeDeclaration;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;
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

							@SuppressWarnings("unchecked")
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
								addHelpfulMethodBodies(ast, firstType);
								addProcessRequestMethod(ast, firstType, result, 
										((CreateServletTemplateModel)tempModel).getServletName());
								addGetServletInfoMethod(ast, firstType, result);
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

							@SuppressWarnings("unchecked")
							private void addHelpfulMethodBodies(AST ast, TypeDeclaration firstType) {
								MethodDeclaration[] methods = firstType.getMethods();

								for (int i = 0; i < methods.length; i++) {
									MethodDeclaration methodDeclaration = methods[i];
									if (!methodDeclaration.isConstructor()) {
										SimpleName methodName = methodDeclaration.getName();
										
										// only add to doGet and doPost
										// generate this:
										// processRequest(request, response);
										if (methodName.getIdentifier().startsWith("do")) { //$NON-NLS-1$
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
								statements.add(getExpressionStatement(ast, "response", //$NON-NLS-1$
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
								ImportDeclaration importDecl = ast.newImportDeclaration();
								QualifiedName name = ast.newQualifiedName(
										ast.newSimpleName("java"),			//$NON-NLS-1$
										ast.newSimpleName("io"));		//$NON-NLS-1$
								name = ast.newQualifiedName(name, 
										ast.newSimpleName("PrintWriter")); //$NON-NLS-1$
								importDecl.setName(name);
								result.imports().add(importDecl);

								statements.add(getProcessRequestTryStatement(ast, servletName));
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

								tags.add(getTagElement(ast, null, null, 
									"Processes requests for both HTTP <code>GET</code> and <code>POST</code> methods.")); //$NON-NLS-1$
								tags.add(getTagElement(ast, TagElement.TAG_PARAM, "request", "servlet request")); //$NON-NLS-1$ //$NON-NLS-2$
								tags.add(getTagElement(ast, TagElement.TAG_PARAM, "response", "servlet response")); //$NON-NLS-1$ //$NON-NLS-2$
								tags.add(getTagElement(ast, TagElement.TAG_THROWS, "ServletException", //$NON-NLS-1$
									"if a servlet-specific error occurs")); //$NON-NLS-1$
								tags.add(getTagElement(ast, TagElement.TAG_THROWS, "IOException", //$NON-NLS-1$ 
									"if an I/O error occurs")); //$NON-NLS-1$

								return docComment;
							}

							@SuppressWarnings("unchecked")
							private TagElement getTagElement(AST ast, String tagName, String fragmentName, String textValue) {
								TagElement tagElement = ast.newTagElement();
								tagElement.setTagName(tagName);
								if (fragmentName != null) {
									tagElement.fragments().add(ast.newSimpleName(fragmentName));
								}
								TextElement te = ast.newTextElement();
								tagElement.fragments().add(te);
								te.setText(textValue);
								return tagElement;

							}

							@SuppressWarnings("unchecked")
							private MethodInvocation getMethodInvocation(AST ast, String methodExpr, 
									String methodName, Expression methodExpression) {
								MethodInvocation methodInvocation = ast.newMethodInvocation();
								methodInvocation.setExpression(ast.newSimpleName(methodExpr));
								methodInvocation.setName(ast.newSimpleName(methodName));
								if (methodExpression != null) {
									methodInvocation.arguments().add(methodExpression);
								}
								return methodInvocation;
							}

							private MethodInvocation getMethodInvocation(AST ast, String methodExpr, 
									String methodName, String methodLiteral) {
								StringLiteral literal = null;
								if (methodLiteral != null) {
									literal = ast.newStringLiteral();
									literal.setLiteralValue(methodLiteral);
								}
								return getMethodInvocation(ast, methodExpr, methodName, literal);
							}

							private ExpressionStatement getExpressionStatement(AST ast, String methodExpr, 
									String methodName, String methodLiteral) {
								return ast.newExpressionStatement(
										getMethodInvocation(ast, methodExpr, methodName, methodLiteral));
							}

							private ExpressionStatement getExpressionStatement(AST ast, String methodExpr, 
									String methodName, Expression methodExpression) {
								return ast.newExpressionStatement(
										getMethodInvocation(ast, methodExpr, methodName, methodExpression));
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

								tryStatements.add(getExpressionStatement(ast, "out", "println", "<html>")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
								tryStatements.add(getExpressionStatement(ast, "out", "println", "<head>")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
								tryStatements.add(getExpressionStatement(ast, "out", "println", //$NON-NLS-1$ //$NON-NLS-2$
										"<title>Servlet " + servletName + "</title>")); //$NON-NLS-1$ //$NON-NLS-2$
								tryStatements.add(getExpressionStatement(ast, "out", "println", "</head>")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
								tryStatements.add(getExpressionStatement(ast, "out", "println", "<body>")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
								tryStatements.add(getExpressionStatement(ast, "out", "println",  //$NON-NLS-1$ //$NON-NLS-2$
										getProcessRequestInfixExpression(ast, servletName)));
								tryStatements.add(getExpressionStatement(ast, "out", "println", "</body>")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
								tryStatements.add(getExpressionStatement(ast, "out", "println", "</html>")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
								tryStatement.setBody(tryBlock);
								tryStatement.setFinally(getProcessRequestFinallyBlock(ast));

								return tryStatement;
							}
							
							// generate this: 
							// <h1>Servlet <servletname> at " + request.getContextPath () + "</h1>"" +
							private InfixExpression getProcessRequestInfixExpression(AST ast, String servletName) {
								MethodInvocation methodInvocation = 
									getMethodInvocation(ast, "request", "getContextPath", (Expression)null); //$NON-NLS-1$ //$NON-NLS-2$
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
										getExpressionStatement(ast, "out", "close", (Expression)null)); //$NON-NLS-1$ //$NON-NLS-2$
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

								tags.add(getTagElement(ast, null, null, 
									"Returns a short description of the servlet.")); //$NON-NLS-1$
								tags.add(getTagElement(ast, TagElement.TAG_RETURN, null, 
									"a String containing servlet description")); //$NON-NLS-1$

								return docComment;
							}

							@SuppressWarnings("unchecked")
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

							@SuppressWarnings("unchecked")
							private Annotation addWebServletAnnotation(AST ast, CreateJavaEEArtifactTemplateModel tempModel,
									CompilationUnit result) {
								Annotation ann = ast.newNormalAnnotation();
								ann.setTypeName(ast.newSimpleName("WebServlet"));	//$NON-NLS-1$

								if (tempModel instanceof CreateServletTemplateModel) {
									CreateServletTemplateModel servletModel = (CreateServletTemplateModel)tempModel;
									List<MemberValuePair> annValues = ((NormalAnnotation) ann).values();
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

							@SuppressWarnings("unchecked")
							private MemberValuePair addInitParams(AST ast, List<String[]> initParams, CompilationUnit result) {
								MemberValuePair annotationProperty = ast.newMemberValuePair();
								ArrayInitializer arrayInit = ast.newArrayInitializer();

								result.imports().add(addWebAnnotationImport(ast, "WebInitParam"));	//$NON-NLS-1$

								annotationProperty.setName(ast.newSimpleName("initParams"));	//$NON-NLS-1$
								for (Iterator<String[]> iterator = initParams.iterator(); iterator.hasNext();) {
									String[] strings = iterator.next();
									StringLiteral literal = ast.newStringLiteral();
									Annotation ann = ast.newNormalAnnotation();
									MemberValuePair annotationProperty1 = ast.newMemberValuePair();
									List<MemberValuePair> annValues = ((NormalAnnotation) ann).values();

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
