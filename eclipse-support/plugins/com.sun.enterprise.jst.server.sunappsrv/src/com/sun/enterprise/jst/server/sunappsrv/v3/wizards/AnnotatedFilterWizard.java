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

import java.util.HashSet;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.emf.codegen.jet.JETException;
import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTParser;
import org.eclipse.jdt.core.dom.Annotation;
import org.eclipse.jdt.core.dom.ArrayAccess;
import org.eclipse.jdt.core.dom.ArrayInitializer;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.CastExpression;
import org.eclipse.jdt.core.dom.CatchClause;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.FieldAccess;
import org.eclipse.jdt.core.dom.FieldDeclaration;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.InstanceofExpression;
import org.eclipse.jdt.core.dom.Javadoc;
import org.eclipse.jdt.core.dom.MemberValuePair;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Modifier;
import org.eclipse.jdt.core.dom.NormalAnnotation;
import org.eclipse.jdt.core.dom.ParameterizedType;
import org.eclipse.jdt.core.dom.PostfixExpression;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.PrimitiveType;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.StringLiteral;
import org.eclipse.jdt.core.dom.TagElement;
import org.eclipse.jdt.core.dom.ThrowStatement;
import org.eclipse.jdt.core.dom.TryStatement;
import org.eclipse.jdt.core.dom.TypeDeclaration;
import org.eclipse.jdt.core.dom.VariableDeclarationExpression;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;
import org.eclipse.jdt.core.dom.Assignment.Operator;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jst.j2ee.internal.common.operations.CreateJavaEEArtifactTemplateModel;
import org.eclipse.jst.j2ee.internal.common.operations.NewJavaEEArtifactClassOperation;
import org.eclipse.jst.j2ee.internal.project.J2EEProjectUtilities;
import org.eclipse.jst.j2ee.internal.web.operations.AddFilterOperation;
import org.eclipse.jst.j2ee.internal.web.operations.CreateFilterTemplateModel;
import org.eclipse.jst.j2ee.internal.web.operations.IFilterMappingItem;
import org.eclipse.jst.j2ee.internal.web.operations.NewFilterClassDataModelProvider;
import org.eclipse.jst.j2ee.internal.web.operations.NewFilterClassOperation;
import org.eclipse.jst.servlet.ui.IWebUIContextIds;
import org.eclipse.jst.servlet.ui.internal.wizard.AddFilterWizard;
import org.eclipse.jst.servlet.ui.internal.wizard.NewFilterClassWizardPage;
import org.eclipse.wst.common.frameworks.datamodel.IDataModelOperation;
import org.eclipse.wst.common.frameworks.datamodel.IDataModelProvider;
import org.eclipse.wst.common.frameworks.internal.WTPPlugin;

/**
 * This is a wizard which creates a new annotated filter, for use 
 * with Java EE 6.  It is provided as a temporary measure to allow for 
 * some Java EE 6 capabilities with v3 in advance of Eclipse WTP providing
 * standard Java EE 6 support.
 */

@SuppressWarnings("restriction")
public class AnnotatedFilterWizard extends AddFilterWizard {

	@Override
	protected IDataModelProvider getDefaultProvider() {
		return new NewFilterClassDataModelProvider() {

			/* (non-Javadoc)
			 * @see org.eclipse.jst.j2ee.internal.web.operations.NewFilterClassDataModelProvider#getDefaultOperation()
			 */
			@Override
			public IDataModelOperation getDefaultOperation() {
				return new AddFilterOperation(model) {

					/* (non-Javadoc)
					 * @see org.eclipse.jst.j2ee.internal.web.operations.AddFilterOperation#getNewClassOperation()
					 */
					@Override
					protected NewJavaEEArtifactClassOperation getNewClassOperation() {
						return new NewFilterClassOperation(getDataModel()) {
							// this method is the eclipse 3.5 way of doing things
							// we can't use super or officially "override" with the annotation because
							// the method doesn't exist in the 3.4.x code
							//@Override
							@SuppressWarnings("unused")
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
								String filterName = ((CreateFilterTemplateModel)tempModel).getFilterName();

								result.imports().add(CodeGenerationUtils.addWebAnnotationImport(ast, "WebFilter"));	//$NON-NLS-1$
								firstType.modifiers().add(0, addWebFilterAnnotation(ast, tempModel, result));
								CodeGenerationUtils.cleanupJavadoc(firstType.getJavadoc(), "@web.filter"); //$NON-NLS-1$
								addImports(ast, result);
								addFields(ast, firstType);
								addHelpfulMethodBodies(ast, firstType, filterName);
								addMethod(firstType, generateDoBeforeProcessingMethodDeclaration(ast, filterName));
								addMethod(firstType, generateDoAfterProcessingMethodDeclaration(ast, filterName));
								addMethod(firstType, generateToStringMethodDeclaration(ast, filterName));
								addMethod(firstType, generateGetFilterConfigMethodDeclaration(ast));
								addMethod(firstType, generateSetFilterConfigMethodDeclaration(ast));
								addMethod(firstType, generateSendProcessingErrorMethodDeclaration(ast));
								addMethod(firstType, generateLogMethodDeclaration(ast));
								addMethod(firstType, generateGetStackTraceMethodDeclaration(ast));

								return CodeGenerationUtils.getRewrittenSource(source, result);
							}

							@SuppressWarnings("unchecked")
							private void addImports(AST ast, CompilationUnit result) {
								List imports = result.imports();

								imports.add(CodeGenerationUtils.addImport(ast, "java", "io", null, "PrintStream")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
								imports.add(CodeGenerationUtils.addImport(ast, "java", "io", null, "PrintWriter")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
								imports.add(CodeGenerationUtils.addImport(ast, "java", "io", null, "StringWriter")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
								imports.add(CodeGenerationUtils.addImport(ast, "java", "util", null, "Enumeration")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
							}

							@SuppressWarnings("unchecked")
							private void addFields(AST ast, TypeDeclaration firstType) {
								// generate this:								
							    // private static final boolean debug = false;
								VariableDeclarationFragment vdf = ast.newVariableDeclarationFragment();
								FieldDeclaration vds = ast.newFieldDeclaration(vdf);
								vdf.setName(ast.newSimpleName("debug")); //$NON-NLS-1$
								List modifiers = vds.modifiers();
								vds.setType(ast.newPrimitiveType(PrimitiveType.BOOLEAN));
								modifiers.add(ast.newModifier(Modifier.ModifierKeyword.PRIVATE_KEYWORD));
								modifiers.add(ast.newModifier(Modifier.ModifierKeyword.STATIC_KEYWORD));
								modifiers.add(ast.newModifier(Modifier.ModifierKeyword.FINAL_KEYWORD));
								vdf.setInitializer(ast.newBooleanLiteral(true));
								firstType.bodyDeclarations().add(0, vds);

								// generate this:								
							    // The filter configuration object we are associated with.  If this
							    // value is null, this filter instance is not currently configured. 
							    // private FilterConfig filterConfig = null;
								vdf = ast.newVariableDeclarationFragment();
								vds = ast.newFieldDeclaration(vdf);
								vdf.setName(ast.newSimpleName("filterConfig")); //$NON-NLS-1$
								vds.setType(ast.newSimpleType(ast.newSimpleName("FilterConfig"))); //$NON-NLS-1$
								vds.modifiers().add(ast.newModifier(Modifier.ModifierKeyword.PRIVATE_KEYWORD));
								vdf.setInitializer(ast.newNullLiteral());
								Javadoc docComment = ast.newJavadoc();
								List tags = docComment.tags();
								tags.add(CodeGenerationUtils.getTagElement(ast, null, null, 
									"The filter configuration object we are associated with.  If this")); //$NON-NLS-1$
								tags.add(CodeGenerationUtils.getTagElement(ast, null, null, 
									"value is null, this filter instance is not currently configured.")); //$NON-NLS-1$
								vds.setJavadoc(docComment);

								firstType.bodyDeclarations().add(1, vds);
							}

							@SuppressWarnings("unchecked")
							private void addHelpfulMethodBodies(AST ast, TypeDeclaration firstType, String filterName) {
								MethodDeclaration[] methods = firstType.getMethods();

								for (int i = 0; i < methods.length; i++) {
									MethodDeclaration methodDeclaration = methods[i];
									if (!methodDeclaration.isConstructor()) {
										String methodName = methodDeclaration.getName().getIdentifier();
										
										if (methodName.equals("init")) { //$NON-NLS-1$
											List<Statement> statements = methodDeclaration.getBody().statements();
											addHelpfulInitMethodBody(ast, statements, filterName);
										} else if (methodName.equals("doFilter")) { //$NON-NLS-1$
											List<Statement> statements = methodDeclaration.getBody().statements();
											addHelpfulDoFilterMethodBody(ast, statements, filterName);
										}
									}
								}
							}
							@SuppressWarnings("unchecked")
							private void addHelpfulInitMethodBody(AST ast, List<Statement> statements, String filterName) {
								// generate this:
								// this.filterConfig = fConfig;
								Assignment assignment = ast.newAssignment();
								FieldAccess fieldAccess = ast.newFieldAccess();
								
								fieldAccess.setExpression(ast.newThisExpression());
								fieldAccess.setName(ast.newSimpleName("filterConfig")); //$NON-NLS-1$
								assignment.setLeftHandSide(fieldAccess);
								assignment.setOperator(Operator.ASSIGN);
								assignment.setRightHandSide(ast.newSimpleName("fConfig")); //$NON-NLS-1$
								statements.add(ast.newExpressionStatement(assignment));
	
								// generate this:
								// if (fConfig != null) {
								IfStatement ifStatement = ast.newIfStatement();
								InfixExpression infixExpr = ast.newInfixExpression();
								infixExpr.setLeftOperand(ast.newSimpleName("fConfig")); //$NON-NLS-1$
								infixExpr.setOperator(InfixExpression.Operator.NOT_EQUALS);
								infixExpr.setRightOperand(ast.newNullLiteral());
								ifStatement.setExpression(infixExpr);
								
								// generate this:
								//    if (debug) { 
								//		log("NewSimpleFilter1: Initializing filter");
								//    }
								// }
								IfStatement innerIfStatement = ast.newIfStatement();
								MethodInvocation methodInvocation = ast.newMethodInvocation();
								Block thenBlock = ast.newBlock();
								ExpressionStatement expressionStatement = ast.newExpressionStatement(methodInvocation);
								StringLiteral stringLiteral = ast.newStringLiteral();

								innerIfStatement.setExpression(ast.newSimpleName("debug")); //$NON-NLS-1$);
								methodInvocation.setName(ast.newSimpleName("log")); //$NON-NLS-1$
								stringLiteral.setLiteralValue(filterName + ": Initializing filter"); //$NON-NLS-1$
								methodInvocation.arguments().add(stringLiteral);
								thenBlock.statements().add(expressionStatement);
								innerIfStatement.setThenStatement(thenBlock);
								thenBlock = ast.newBlock();
								thenBlock.statements().add(innerIfStatement);
								ifStatement.setThenStatement(thenBlock);
								statements.add(ifStatement);
							}

							@SuppressWarnings("unchecked")
							private void addHelpfulDoFilterMethodBody(AST ast, List<Statement> statements, String filterName) {
								Statement existingStatement = statements.remove(0);

								// generate this:
								//    if (debug) { 
								//		log("<filtername>: doFilter()");
								//    }
								// }
								IfStatement ifStatement = ast.newIfStatement();
								MethodInvocation methodInvocation = ast.newMethodInvocation();
								Block thenBlock = ast.newBlock();
								ExpressionStatement expressionStatement = ast.newExpressionStatement(methodInvocation);
								StringLiteral stringLiteral = ast.newStringLiteral();
	
								ifStatement.setExpression(ast.newSimpleName("debug")); //$NON-NLS-1$);
								methodInvocation.setName(ast.newSimpleName("log")); //$NON-NLS-1$
								stringLiteral.setLiteralValue(filterName + ": doFilter()"); //$NON-NLS-1$
								methodInvocation.arguments().add(stringLiteral);
								thenBlock.statements().add(expressionStatement);
								ifStatement.setThenStatement(thenBlock);
								statements.add(ifStatement);

								// generate this:
								// doBeforeProcessing(request, response);
								methodInvocation = ast.newMethodInvocation();
								methodInvocation.setName(ast.newSimpleName("doBeforeProcessing")); //$NON-NLS-1$
								methodInvocation.arguments().add(ast.newSimpleName("request")); //$NON-NLS-1$
								methodInvocation.arguments().add(ast.newSimpleName("response")); //$NON-NLS-1$
								statements.add(ast.newExpressionStatement(methodInvocation));
							
								// generate this:
							    // Throwable problem = null;
								VariableDeclarationFragment vdf = ast.newVariableDeclarationFragment();
								VariableDeclarationStatement vds = ast.newVariableDeclarationStatement(vdf);
								vdf.setName(ast.newSimpleName("problem")); //$NON-NLS-1$
								vds.setType(ast.newSimpleType(ast.newSimpleName("Throwable"))); //$NON-NLS-1$
								vdf.setInitializer(ast.newNullLiteral());
								statements.add(vds);

								// generate this: 
								// try {
							    //	chain.doFilter(request, response);
								// }
								TryStatement tryStatement = ast.newTryStatement();
								Block tryBlock = ast.newBlock();
								tryBlock.statements().add(existingStatement);		
								tryStatement.setBody(tryBlock);
								statements.add(tryStatement);

								// generate this: 
								// catch(Throwable t) {
								//    problem = t;
								//    t.printStackTrace();
								// }
								CatchClause catchClause = ast.newCatchClause();
								Block catchBlock = ast.newBlock();
								SingleVariableDeclaration svd = ast.newSingleVariableDeclaration();
								List<Statement> catchStmts = catchBlock.statements();		
								catchClause.setBody(ast.newBlock());
								svd.setType(ast.newSimpleType(ast.newSimpleName("Throwable")));  //$NON-NLS-1$
								svd.setName(ast.newSimpleName("t"));	//$NON-NLS-1$
								catchClause.setException(svd);
								catchClause.setBody(catchBlock);
								Assignment assignment = ast.newAssignment();
								assignment.setLeftHandSide(ast.newSimpleName("problem"));//$NON-NLS-1$
								assignment.setOperator(Operator.ASSIGN);
								assignment.setRightHandSide(ast.newSimpleName("t"));//$NON-NLS-1$
								catchStmts.add(ast.newExpressionStatement(assignment));
								catchStmts.add(CodeGenerationUtils.getExpressionStatement(ast, "t", "printStackTrace")); //$NON-NLS-1$ //$NON-NLS-2$
								tryStatement.catchClauses().add(catchClause);

								// generate this:
								// doAfterProcessing(request, response);
								methodInvocation = ast.newMethodInvocation();
								methodInvocation.setName(ast.newSimpleName("doAfterProcessing")); //$NON-NLS-1$
								methodInvocation.arguments().add(ast.newSimpleName("request")); //$NON-NLS-1$
								methodInvocation.arguments().add(ast.newSimpleName("response")); //$NON-NLS-1$
								statements.add(ast.newExpressionStatement(methodInvocation));

								// generate this:
								// if (problem != null) {
								ifStatement = ast.newIfStatement();
								InfixExpression infixExpr = ast.newInfixExpression();
								infixExpr.setLeftOperand(ast.newSimpleName("problem")); //$NON-NLS-1$
								infixExpr.setOperator(InfixExpression.Operator.NOT_EQUALS);
								infixExpr.setRightOperand(ast.newNullLiteral());
								ifStatement.setExpression(infixExpr);
								
								// generate this:
								// if (problem instanceof ServletException) throw (ServletException)problem;
							    // if (problem instanceof IOException) throw (IOException)problem;
							    // sendProcessingError(problem, response);
								thenBlock = ast.newBlock();
								ifStatement.setThenStatement(thenBlock);
								List<Statement> ifStatements = thenBlock.statements();
								ifStatements.add(getIfRethrowStatement(ast, "ServletException")); //$NON-NLS-1$
								ifStatements.add(getIfRethrowStatement(ast, "IOException")); //$NON-NLS-1$
								methodInvocation = ast.newMethodInvocation();
								methodInvocation.setName(ast.newSimpleName("sendProcessingError")); //$NON-NLS-1$
								methodInvocation.arguments().add(ast.newSimpleName("problem")); //$NON-NLS-1$
								methodInvocation.arguments().add(ast.newSimpleName("response")); //$NON-NLS-1$
								ifStatements.add(ast.newExpressionStatement(methodInvocation));
								statements.add(ifStatement);
								
							}

							private IfStatement getIfRethrowStatement(AST ast, String exceptionType) {
								// generate this:
								// if (problem instanceof <exceptionType>) throw (<exceptionType>)problem;
								IfStatement ifStatement = ast.newIfStatement();
								InstanceofExpression instExpr = ast.newInstanceofExpression();
								ThrowStatement throwStmt = ast.newThrowStatement();
								CastExpression castExpression = ast.newCastExpression();

								instExpr.setLeftOperand(ast.newSimpleName("problem")); //$NON-NLS-1$
								instExpr.setRightOperand(ast.newSimpleType(ast.newSimpleName(exceptionType)));
								ifStatement.setExpression(instExpr);
								castExpression.setExpression(ast.newSimpleName("problem"));  //$NON-NLS-1$
								castExpression.setType(ast.newSimpleType(ast.newSimpleName(exceptionType)));
								throwStmt.setExpression(castExpression);
								ifStatement.setThenStatement(throwStmt);

								return ifStatement;
							}

							@SuppressWarnings("unchecked")
							private void addMethod(TypeDeclaration firstType, MethodDeclaration methodDeclaration) {
								firstType.bodyDeclarations().add(methodDeclaration);
							}

							@SuppressWarnings("unchecked")
							private MethodDeclaration generateDoBeforeProcessingMethodDeclaration(AST ast, String filterName) {
								MethodDeclaration methodDeclaration = ast.newMethodDeclaration();

								// generate this: 
							    // private void doBeforeProcessing(ServletRequest request, ServletResponse response)
								// 		throws IOException, ServletException {
								methodDeclaration.setBody(ast.newBlock());
								methodDeclaration.setConstructor(false);
								methodDeclaration.modifiers().add(ast.newModifier(Modifier.ModifierKeyword.PRIVATE_KEYWORD));
								methodDeclaration.setName(ast.newSimpleName("doBeforeProcessing")); //$NON-NLS-1$
								methodDeclaration.setReturnType2(ast.newPrimitiveType(PrimitiveType.VOID));
								SingleVariableDeclaration variableDeclaration = ast.newSingleVariableDeclaration();
								variableDeclaration.setType(ast.newSimpleType(ast.newSimpleName("ServletRequest"))); //$NON-NLS-1$
								variableDeclaration.setName(ast.newSimpleName("request")); //$NON-NLS-1$
								methodDeclaration.parameters().add(variableDeclaration);
								variableDeclaration = ast.newSingleVariableDeclaration();
								variableDeclaration.setType(ast.newSimpleType(ast.newSimpleName("ServletResponse"))); //$NON-NLS-1$
								variableDeclaration.setName(ast.newSimpleName("response")); //$NON-NLS-1$
								methodDeclaration.parameters().add(variableDeclaration);
								methodDeclaration.thrownExceptions().add(ast.newSimpleName("ServletException")); //$NON-NLS-1$
								methodDeclaration.thrownExceptions().add(ast.newSimpleName("IOException")); //$NON-NLS-1$

								// generate this:
								//    if (debug) { 
								//		log("<filtername>: DoBeforeProcessing");
								//    }
								// }
								List<Statement> statements = methodDeclaration.getBody().statements();
								IfStatement ifStatement = ast.newIfStatement();
								MethodInvocation methodInvocation = ast.newMethodInvocation();
								Block thenBlock = ast.newBlock();
								ExpressionStatement expressionStatement = ast.newExpressionStatement(methodInvocation);
								StringLiteral stringLiteral = ast.newStringLiteral();

								ifStatement.setExpression(ast.newSimpleName("debug")); //$NON-NLS-1$);
								methodInvocation.setName(ast.newSimpleName("log")); //$NON-NLS-1$
								stringLiteral.setLiteralValue(filterName + ": DoBeforeProcessing"); //$NON-NLS-1$
								methodInvocation.arguments().add(stringLiteral);
								thenBlock.statements().add(expressionStatement);
								ifStatement.setThenStatement(thenBlock);
								statements.add(ifStatement);

								// generate this: 
								// for (Enumeration<String> en = request.getParameterNames(); en.hasMoreElements(); ) {
								ForStatement forStatement = ast.newForStatement();
								VariableDeclarationFragment vdf = ast.newVariableDeclarationFragment();
								VariableDeclarationExpression vde = ast.newVariableDeclarationExpression(vdf);
								ParameterizedType paramType = ast.newParameterizedType(
										ast.newSimpleType(ast.newSimpleName("Enumeration"))); //$NON-NLS-1$
								vdf.setName(ast.newSimpleName("en")); //$NON-NLS-1$
								paramType.typeArguments().add(ast.newSimpleType(ast.newSimpleName("String"))); //$NON-NLS-1$
								vde.setType(paramType);
								vdf.setInitializer(CodeGenerationUtils.getMethodInvocation(ast, "request", "getParameterNames")); //$NON-NLS-1$ //$NON-NLS-2$
								forStatement.initializers().add(vde);
								forStatement.setExpression(CodeGenerationUtils.getMethodInvocation(ast, "en", "hasMoreElements")); //$NON-NLS-1$ //$NON-NLS-2$

								// generate this: 
						    	// String name = (String)en.nextElement();
								Block forBlock = ast.newBlock();
								forStatement.setBody(forBlock);
								List<Statement> forStatements = forBlock.statements();
								vdf = ast.newVariableDeclarationFragment();
								VariableDeclarationStatement vds = ast.newVariableDeclarationStatement(vdf);
								methodInvocation = CodeGenerationUtils.getMethodInvocation(ast, "en", "nextElement"); //$NON-NLS-1$ //$NON-NLS-2$
								vdf.setName(ast.newSimpleName("name")); //$NON-NLS-1$
								vds.setType(ast.newSimpleType(ast.newSimpleName("String"))); //$NON-NLS-1$
								CastExpression castExpr = ast.newCastExpression();
								castExpr.setExpression(methodInvocation);
								castExpr.setType(ast.newSimpleType(ast.newSimpleName("String"))); //$NON-NLS-1$
								vdf.setInitializer(castExpr);
								forStatements.add(vds);

								// generate this: 
						    	// String values[] = request.getParameterValues(name);
								vdf = ast.newVariableDeclarationFragment();
								vds = ast.newVariableDeclarationStatement(vdf);
								vdf.setName(ast.newSimpleName("values")); //$NON-NLS-1$
								vds.setType(ast.newArrayType(ast.newSimpleType(ast.newSimpleName("String")))); //$NON-NLS-1$
								vdf.setInitializer(CodeGenerationUtils.getMethodInvocation(ast, "request", "getParameterValues", ast.newSimpleName("name"))); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
								forStatements.add(vds);

								// generate this: 
						    	// int n = values.length;
								vdf = ast.newVariableDeclarationFragment();
								vds = ast.newVariableDeclarationStatement(vdf);
								vdf.setName(ast.newSimpleName("n")); //$NON-NLS-1$
								vds.setType(ast.newPrimitiveType(PrimitiveType.INT));
								FieldAccess fieldAccess = ast.newFieldAccess();
								fieldAccess.setExpression(ast.newSimpleName("values")); //$NON-NLS-1$
								fieldAccess.setName(ast.newSimpleName("length")); //$NON-NLS-1$
								vdf.setInitializer(fieldAccess);
								forStatements.add(vds);

								// generate this: 
								// StringBuffer buf = new StringBuffer();
								// buf.append(name);
								// buf.append("=");
								forStatements.add(getConstructorVariableDeclarationStatement(ast, "StringBuffer", "buf", (Expression)null)); //$NON-NLS-1$ //$NON-NLS-2$
								forStatements.add(CodeGenerationUtils.getExpressionStatement(ast, "buf", "append", ast.newSimpleName("name"))); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
								forStatements.add(CodeGenerationUtils.getExpressionStatement(ast, "buf", "append", "=")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

								// generate this: 
								// for(int i=0; i < n; i++) {
								ForStatement innerForStatement = ast.newForStatement();
								InfixExpression infixExpr = ast.newInfixExpression();
								vdf = ast.newVariableDeclarationFragment();
								vde = ast.newVariableDeclarationExpression(vdf);
								vdf.setName(ast.newSimpleName("i")); //$NON-NLS-1$
								vde.setType(ast.newPrimitiveType(PrimitiveType.INT));
								vdf.setInitializer(ast.newNumberLiteral("0")); //$NON-NLS-1$
								innerForStatement.initializers().add(vde);
								infixExpr.setLeftOperand(ast.newSimpleName("i")); //$NON-NLS-1$
								infixExpr.setOperator(InfixExpression.Operator.LESS);
								infixExpr.setRightOperand(ast.newSimpleName("n")); //$NON-NLS-1$
								innerForStatement.setExpression(infixExpr);
								PostfixExpression postfixExpr = ast.newPostfixExpression();
								postfixExpr.setOperand(ast.newSimpleName("i")); //$NON-NLS-1$
								postfixExpr.setOperator(PostfixExpression.Operator.INCREMENT);
								innerForStatement.updaters().add(postfixExpr);
								forStatements.add(innerForStatement);

								forBlock = ast.newBlock();
								innerForStatement.setBody(forBlock);
								List<Statement> innerForStatements = forBlock.statements();

								// generate this: 
						    	// buf.append(values[i]);
								ArrayAccess arrayAcess = ast.newArrayAccess();
								arrayAcess.setArray(ast.newSimpleName("values")); //$NON-NLS-1$
								arrayAcess.setIndex(ast.newSimpleName("i")); //$NON-NLS-1$
								innerForStatements.add(CodeGenerationUtils.getExpressionStatement(ast, "buf", "append", arrayAcess)); //$NON-NLS-1$ //$NON-NLS-2$

								// generate this: 
						    	// if (i < n-1)
					            //		buf.append(",");
								ifStatement = ast.newIfStatement();
								InfixExpression innerInfixExpr = ast.newInfixExpression();
								innerInfixExpr.setLeftOperand(ast.newSimpleName("n")); //$NON-NLS-1$
								innerInfixExpr.setOperator(InfixExpression.Operator.MINUS);
								innerInfixExpr.setRightOperand(ast.newNumberLiteral("1")); //$NON-NLS-1$
								infixExpr = ast.newInfixExpression();
								infixExpr.setLeftOperand(ast.newSimpleName("i")); //$NON-NLS-1$
								infixExpr.setOperator(InfixExpression.Operator.LESS);
								infixExpr.setRightOperand(innerInfixExpr);
								ifStatement.setExpression(infixExpr);
								ifStatement.setThenStatement(CodeGenerationUtils.getExpressionStatement(ast, "buf", "append", ",")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
								innerForStatements.add(ifStatement);

								// generate this: 
						    	// log(buf.toString());
								methodInvocation = ast.newMethodInvocation();
								methodInvocation.setName(ast.newSimpleName("log")); //$NON-NLS-1$
								methodInvocation.arguments().add(CodeGenerationUtils.getMethodInvocation(ast, "buf", "toString")); //$NON-NLS-1$ //$NON-NLS-2$ 
								forStatements.add(ast.newExpressionStatement(methodInvocation));
								statements.add(forStatement);

								return methodDeclaration;
						    } 

							@SuppressWarnings("unchecked")
							private MethodDeclaration generateDoAfterProcessingMethodDeclaration(AST ast, String filterName) {
								MethodDeclaration methodDeclaration = ast.newMethodDeclaration();

								// generate this: 
							    // private void doAfterProcessing(ServletRequest request, ServletResponse response)
								// 		throws IOException, ServletException {
								methodDeclaration.setBody(ast.newBlock());
								methodDeclaration.setConstructor(false);
								methodDeclaration.modifiers().add(ast.newModifier(Modifier.ModifierKeyword.PRIVATE_KEYWORD));
								methodDeclaration.setName(ast.newSimpleName("doAfterProcessing")); //$NON-NLS-1$
								methodDeclaration.setReturnType2(ast.newPrimitiveType(PrimitiveType.VOID));
								SingleVariableDeclaration variableDeclaration = ast.newSingleVariableDeclaration();
								variableDeclaration.setType(ast.newSimpleType(ast.newSimpleName("ServletRequest"))); //$NON-NLS-1$
								variableDeclaration.setName(ast.newSimpleName("request")); //$NON-NLS-1$
								methodDeclaration.parameters().add(variableDeclaration);
								variableDeclaration = ast.newSingleVariableDeclaration();
								variableDeclaration.setType(ast.newSimpleType(ast.newSimpleName("ServletResponse"))); //$NON-NLS-1$
								variableDeclaration.setName(ast.newSimpleName("response")); //$NON-NLS-1$
								methodDeclaration.parameters().add(variableDeclaration);
								methodDeclaration.thrownExceptions().add(ast.newSimpleName("ServletException")); //$NON-NLS-1$
								methodDeclaration.thrownExceptions().add(ast.newSimpleName("IOException")); //$NON-NLS-1$

								// generate this:
								//    if (debug) { 
								//		log("<filtername>: DoAfterProcessing");
								//    }
								// }
								List<Statement> statements = methodDeclaration.getBody().statements();
								IfStatement ifStatement = ast.newIfStatement();
								MethodInvocation methodInvocation = ast.newMethodInvocation();
								Block thenBlock = ast.newBlock();
								ExpressionStatement expressionStatement = ast.newExpressionStatement(methodInvocation);
								StringLiteral stringLiteral = ast.newStringLiteral();

								ifStatement.setExpression(ast.newSimpleName("debug")); //$NON-NLS-1$);
								methodInvocation.setName(ast.newSimpleName("log")); //$NON-NLS-1$
								stringLiteral.setLiteralValue(filterName + ": DoAfterProcessing"); //$NON-NLS-1$
								methodInvocation.arguments().add(stringLiteral);
								thenBlock.statements().add(expressionStatement);
								ifStatement.setThenStatement(thenBlock);
								statements.add(ifStatement);

								// generate this: 
								// for (Enumeration<String> en = request.getAttributeNames(); en.hasMoreElements(); ) {
								ForStatement forStatement = ast.newForStatement();
								VariableDeclarationFragment vdf = ast.newVariableDeclarationFragment();
								VariableDeclarationExpression vde = ast.newVariableDeclarationExpression(vdf);
								ParameterizedType paramType = ast.newParameterizedType(
										ast.newSimpleType(ast.newSimpleName("Enumeration"))); //$NON-NLS-1$
								vdf.setName(ast.newSimpleName("en")); //$NON-NLS-1$
								paramType.typeArguments().add(ast.newSimpleType(ast.newSimpleName("String"))); //$NON-NLS-1$
								vde.setType(paramType);
								vdf.setInitializer(CodeGenerationUtils.getMethodInvocation(ast, "request", "getAttributeNames")); //$NON-NLS-1$ //$NON-NLS-2$
								forStatement.initializers().add(vde);
								forStatement.setExpression(CodeGenerationUtils.getMethodInvocation(ast, "en", "hasMoreElements")); //$NON-NLS-1$ //$NON-NLS-2$

								// generate this: 
						    	// String name = (String)en.nextElement();
								Block forBlock = ast.newBlock();
								forStatement.setBody(forBlock);
								List<Statement> forStatements = forBlock.statements();
								vdf = ast.newVariableDeclarationFragment();
								VariableDeclarationStatement vds = ast.newVariableDeclarationStatement(vdf);
								methodInvocation = CodeGenerationUtils.getMethodInvocation(ast, "en", "nextElement"); //$NON-NLS-1$ //$NON-NLS-2$
								vdf.setName(ast.newSimpleName("name")); //$NON-NLS-1$
								vds.setType(ast.newSimpleType(ast.newSimpleName("String"))); //$NON-NLS-1$
								CastExpression castExpr = ast.newCastExpression();
								castExpr.setExpression(methodInvocation);
								castExpr.setType(ast.newSimpleType(ast.newSimpleName("String"))); //$NON-NLS-1$
								vdf.setInitializer(castExpr);
								forStatements.add(vds);

								// generate this: 
						    	// Object value = request.getAttribute(name);
								vdf = ast.newVariableDeclarationFragment();
								vds = ast.newVariableDeclarationStatement(vdf);
								vdf.setName(ast.newSimpleName("value")); //$NON-NLS-1$
								vds.setType(ast.newSimpleType(ast.newSimpleName("Object"))); //$NON-NLS-1$
								vdf.setInitializer(CodeGenerationUtils.getMethodInvocation(ast, "request", "getAttribute", ast.newSimpleName("name"))); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
								forStatements.add(vds);

								// generate this: 
						    	// log("attribute: " + name + "=" + value.toString());
								methodInvocation = 
									CodeGenerationUtils.getMethodInvocation(ast, "value", "toString"); //$NON-NLS-1$ //$NON-NLS-2$
								InfixExpression infixExpression = ast.newInfixExpression();
								InfixExpression infixExpression2 = ast.newInfixExpression();
								InfixExpression infixExpression3 = ast.newInfixExpression();
								StringLiteral literal = ast.newStringLiteral();

								infixExpression.setOperator(InfixExpression.Operator.PLUS);
								literal.setLiteralValue("attribute: " ); //$NON-NLS-1$
								infixExpression.setLeftOperand(literal);
								infixExpression.setOperator(InfixExpression.Operator.PLUS);
								infixExpression.setRightOperand(ast.newSimpleName("name")); //$NON-NLS-1$
								infixExpression2.setLeftOperand(infixExpression);
								infixExpression2.setOperator(InfixExpression.Operator.PLUS);
								literal = ast.newStringLiteral();
								literal.setLiteralValue("="); //$NON-NLS-1$
								infixExpression2.setRightOperand(literal);
								infixExpression3.setLeftOperand(infixExpression2);
								infixExpression3.setOperator(InfixExpression.Operator.PLUS);
								infixExpression3.setRightOperand(methodInvocation);
								methodInvocation = ast.newMethodInvocation();
								methodInvocation.setName(ast.newSimpleName("log")); //$NON-NLS-1$
								methodInvocation.arguments().add(infixExpression3);
								forStatements.add(ast.newExpressionStatement(methodInvocation));
								statements.add(forStatement);

								// generate this: 
								// PrintWriter respOut = new PrintWriter(response.getWriter());
								// respOut.println("<P><B>This has been appended by an intrusive filter.</B>");
								statements.add(getConstructorVariableDeclarationStatement(ast, "PrintWriter", "respOut",  //$NON-NLS-1$ //$NON-NLS-2$
										CodeGenerationUtils.getMethodInvocation(ast, "response", "getWriter")));	//$NON-NLS-1$ //$NON-NLS-2$
								statements.add(CodeGenerationUtils.getExpressionStatement(ast, "respOut", "println", "<P><B>This has been appended by an intrusive filter.</B>")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
								
								return methodDeclaration;
						    } 

							@SuppressWarnings("unchecked")
							private MethodDeclaration generateLogMethodDeclaration(AST ast) {
								MethodDeclaration methodDeclaration = ast.newMethodDeclaration();

								// generate this: 
							    // public void log(String msg) {
								methodDeclaration.setBody(ast.newBlock());
								methodDeclaration.setConstructor(false);
								methodDeclaration.modifiers().add(ast.newModifier(Modifier.ModifierKeyword.PUBLIC_KEYWORD));
								methodDeclaration.setName(ast.newSimpleName("log")); //$NON-NLS-1$
								methodDeclaration.setReturnType2(ast.newPrimitiveType(PrimitiveType.VOID));
								SingleVariableDeclaration variableDeclaration = ast.newSingleVariableDeclaration();
								variableDeclaration.setType(ast.newSimpleType(ast.newSimpleName("String"))); //$NON-NLS-1$
								variableDeclaration.setName(ast.newSimpleName("msg")); //$NON-NLS-1$
								methodDeclaration.parameters().add(variableDeclaration);

								// generate this: 
							    //	filterConfig.getServletContext().log(msg); 
								List<Statement> statements = methodDeclaration.getBody().statements();
								MethodInvocation firstCall = CodeGenerationUtils.getMethodInvocation(ast, "filterConfig", //$NON-NLS-1$
										"getServletContext"); //$NON-NLS-1$
								MethodInvocation methodInvocation = ast.newMethodInvocation();
								ExpressionStatement expressionStatement = ast.newExpressionStatement(methodInvocation);

								methodInvocation.setExpression(firstCall);
								methodInvocation.setName(ast.newSimpleName("log")); //$NON-NLS-1$
								methodInvocation.arguments().add(ast.newSimpleName("msg")); //$NON-NLS-1$
								statements.add(expressionStatement);

								return methodDeclaration;
							}

							@SuppressWarnings("unchecked")
							private MethodDeclaration generateGetStackTraceMethodDeclaration(AST ast) {
								MethodDeclaration methodDeclaration = ast.newMethodDeclaration();

								// generate this: 
							    // public static String getStackTrace(Throwable t) {
								methodDeclaration.setBody(ast.newBlock());
								methodDeclaration.setConstructor(false);
								methodDeclaration.modifiers().add(ast.newModifier(Modifier.ModifierKeyword.PUBLIC_KEYWORD));
								methodDeclaration.modifiers().add(ast.newModifier(Modifier.ModifierKeyword.STATIC_KEYWORD));
								methodDeclaration.setName(ast.newSimpleName("getStackTrace")); //$NON-NLS-1$
								methodDeclaration.setReturnType2(ast.newSimpleType(ast.newSimpleName("String"))); //$NON-NLS-1$
								SingleVariableDeclaration variableDeclaration = ast.newSingleVariableDeclaration();
								variableDeclaration.setType(ast.newSimpleType(ast.newSimpleName("Throwable"))); //$NON-NLS-1$
								variableDeclaration.setName(ast.newSimpleName("t")); //$NON-NLS-1$
								methodDeclaration.parameters().add(variableDeclaration);

								// generate this: 
							    //	  String stackTrace = null;
								List<Statement> statements = methodDeclaration.getBody().statements();
								VariableDeclarationFragment vdf = ast.newVariableDeclarationFragment();
								VariableDeclarationStatement vds = ast.newVariableDeclarationStatement(vdf);
								vdf.setName(ast.newSimpleName("stackTrace")); //$NON-NLS-1$
								vds.setType(ast.newSimpleType(ast.newSimpleName("String"))); //$NON-NLS-1$
								vdf.setInitializer(ast.newNullLiteral());
								statements.add(vds);

								TryStatement tryStatement = ast.newTryStatement();
								Block tryBlock = ast.newBlock();
								List<Statement> tryStatements = tryBlock.statements();

								// generate this: 
							    //	  try {
							    //	    StringWriter sw = new StringWriter();
							    //	    PrintWriter pw = new PrintWriter(sw);
							    //	    t.printStackTrace(pw);
							    //	    pw.close();
							    //	    sw.close();
								tryStatements.add(getConstructorVariableDeclarationStatement(ast, "StringWriter", "sw", (Expression)null)); //$NON-NLS-1$ //$NON-NLS-2$
								tryStatements.add(getConstructorVariableDeclarationStatement(ast, "PrintWriter", "pw", "sw")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
								tryStatements.add(CodeGenerationUtils.getExpressionStatement(ast, "t", "printStackTrace", ast.newSimpleName("pw"))); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
								tryStatements.add(CodeGenerationUtils.getExpressionStatement(ast, "pw", "close")); //$NON-NLS-1$ //$NON-NLS-2$
								tryStatements.add(CodeGenerationUtils.getExpressionStatement(ast, "sw", "close")); //$NON-NLS-1$ //$NON-NLS-2$

								// generate this: 
							    //	    stackTrace = sw.getBuffer().toString();
								MethodInvocation firstCall = CodeGenerationUtils.getMethodInvocation(ast, "sw", //$NON-NLS-1$
										"getBuffer"); //$NON-NLS-1$
								MethodInvocation methodInvocation = ast.newMethodInvocation();
								methodInvocation.setExpression(firstCall);
								methodInvocation.setName(ast.newSimpleName("toString")); //$NON-NLS-1$
								Assignment assignment = ast.newAssignment();
								assignment.setLeftHandSide(ast.newSimpleName("stackTrace"));//$NON-NLS-1$
								assignment.setOperator(Operator.ASSIGN);
								assignment.setRightHandSide(methodInvocation);
								tryStatements.add(ast.newExpressionStatement(assignment));

								// generate this: 
							    //	  catch(Exception ex) {}
								tryStatement.catchClauses().add(getEmptyCatchClause(ast));
								tryStatement.setBody(tryBlock);
								statements.add(tryStatement);

								// generate this: 
							    //	  return stackTrace;
								ReturnStatement returnStatement = ast.newReturnStatement();
								returnStatement.setExpression(ast.newSimpleName("stackTrace")); //$NON-NLS-1$;
								statements.add(returnStatement);

								return methodDeclaration;
							}

							@SuppressWarnings("unchecked")
							private MethodDeclaration generateSendProcessingErrorMethodDeclaration(AST ast) {
								MethodDeclaration methodDeclaration = ast.newMethodDeclaration();

								// generate this: 
							    // private void sendProcessingError(Throwable t, ServletResponse response) {
								methodDeclaration.setBody(ast.newBlock());
								methodDeclaration.setConstructor(false);
								methodDeclaration.modifiers().add(ast.newModifier(Modifier.ModifierKeyword.PRIVATE_KEYWORD));
								methodDeclaration.setName(ast.newSimpleName("sendProcessingError")); //$NON-NLS-1$
								methodDeclaration.setReturnType2(ast.newPrimitiveType(PrimitiveType.VOID));
								SingleVariableDeclaration variableDeclaration = ast.newSingleVariableDeclaration();
								variableDeclaration.setType(ast.newSimpleType(ast.newSimpleName("Throwable"))); //$NON-NLS-1$
								variableDeclaration.setName(ast.newSimpleName("t")); //$NON-NLS-1$
								methodDeclaration.parameters().add(variableDeclaration);
								variableDeclaration = ast.newSingleVariableDeclaration();
								variableDeclaration.setType(ast.newSimpleType(ast.newSimpleName("ServletResponse"))); //$NON-NLS-1$
								variableDeclaration.setName(ast.newSimpleName("response")); //$NON-NLS-1$
								methodDeclaration.parameters().add(variableDeclaration);

								// generate this: 
						    	// String stackTrace = getStackTrace(t); 
								List<Statement> statements = methodDeclaration.getBody().statements();
								VariableDeclarationFragment vdf = ast.newVariableDeclarationFragment();
								VariableDeclarationStatement vds = ast.newVariableDeclarationStatement(vdf);
								MethodInvocation methodInvocation = ast.newMethodInvocation();
								vdf.setName(ast.newSimpleName("stackTrace")); //$NON-NLS-1$
								vds.setType(ast.newSimpleType(ast.newSimpleName("String"))); //$NON-NLS-1$
								methodInvocation.setName(ast.newSimpleName("getStackTrace")); //$NON-NLS-1$
								methodInvocation.arguments().add(ast.newSimpleName("t")); //$NON-NLS-1$
								vdf.setInitializer(methodInvocation);
								statements.add(vds);

								// generate this:
								// if (stackTrace != null && !stackTrace.equals("")) {
								IfStatement ifStatement = ast.newIfStatement();
								InfixExpression infixExpr = ast.newInfixExpression();
								InfixExpression infixExprL = ast.newInfixExpression();
								PrefixExpression prefixExprR = ast.newPrefixExpression();
								infixExprL.setLeftOperand(ast.newSimpleName("stackTrace")); //$NON-NLS-1$
								infixExprL.setOperator(InfixExpression.Operator.NOT_EQUALS);
								infixExprL.setRightOperand(ast.newNullLiteral());
								
								prefixExprR.setOperand(CodeGenerationUtils.getMethodInvocation(ast, "stackTrace", "equals", "")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
								prefixExprR.setOperator(PrefixExpression.Operator.NOT);
								infixExpr.setLeftOperand(infixExprL);
								infixExpr.setRightOperand(prefixExprR);
								infixExpr.setOperator(InfixExpression.Operator.CONDITIONAL_AND);
								ifStatement.setExpression(infixExpr);
								
								TryStatement tryStatement = ast.newTryStatement();
								Block tryBlock = ast.newBlock();
								List<Statement> tryStatements = tryBlock.statements();
								
								Block thenBlock = ast.newBlock();
								thenBlock.statements().add(tryStatement);
								ifStatement.setThenStatement(thenBlock);
								statements.add(ifStatement);

								// generate this: 
							    //	  try {
					    		//		response.setContentType("text/html");
							    //	    PrintStream ps = new PrintStream(response.getOutputStream());
								//		PrintWriter pw = new PrintWriter(ps); 
					    		// 		pw.print("<html>\n<head>\n<title>Error</title>\n</head>\n<body>\n");
					    		//		pw.print("<h1>The resource did not process correctly</h1>\n<pre>\n"); 
					    		//		pw.print(stackTrace); 
					    		//		pw.print("</pre></body>\n</html>"); //NOI18N
							    //	    pw.close();
							    //	    ps.close();
					    		//		response.getOutputStream().close();
								tryStatements.add(CodeGenerationUtils.getExpressionStatement(ast, "response", //$NON-NLS-1$
										"setContentType", "text/html")); //$NON-NLS-1$ //$NON-NLS-2$
		// TODO - because lines are long, eclipse automatically wraps and the formatting of some of these gened lines is a bit strange
		// not sure if there's anything we can do
								tryStatements.add(getConstructorVariableDeclarationStatement(ast, "PrintStream", "ps",  //$NON-NLS-1$ //$NON-NLS-2$
										CodeGenerationUtils.getMethodInvocation(ast, "response", "getOutputStream")));	//$NON-NLS-1$ //$NON-NLS-2$
								tryStatements.add(getConstructorVariableDeclarationStatement(ast, "PrintWriter", "pw", "ps")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
								tryStatements.add(CodeGenerationUtils.getExpressionStatement(ast, "pw", "print", "<html>\n<head>\n<title>Error</title>\n</head>\n<body>\n")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
								tryStatements.add(CodeGenerationUtils.getExpressionStatement(ast, "pw", "print", "<h1>The resource did not process correctly</h1>\n<pre>\n")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
								tryStatements.add(CodeGenerationUtils.getExpressionStatement(ast, "pw", "print", ast.newSimpleName("stackTrace"))); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
								tryStatements.add(CodeGenerationUtils.getExpressionStatement(ast, "pw", "print", "</pre></body>\n</html>")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
								tryStatements.add(CodeGenerationUtils.getExpressionStatement(ast, "pw", "close")); //$NON-NLS-1$ //$NON-NLS-2$
								tryStatements.add(CodeGenerationUtils.getExpressionStatement(ast, "ps", "close")); //$NON-NLS-1$ //$NON-NLS-2$
								MethodInvocation firstCall = CodeGenerationUtils.getMethodInvocation(ast, "response", //$NON-NLS-1$
										"getOutputStream"); //$NON-NLS-1$
								methodInvocation = ast.newMethodInvocation();
								ExpressionStatement expressionStatement = ast.newExpressionStatement(methodInvocation);

								methodInvocation.setExpression(firstCall);
								methodInvocation.setName(ast.newSimpleName("close")); //$NON-NLS-1$
								tryStatements.add(expressionStatement);

								// generate this: 
							    //	  catch(Exception ex) {}
								tryStatement.catchClauses().add(getEmptyCatchClause(ast));
								tryStatement.setBody(tryBlock);

								tryStatement = ast.newTryStatement();
								tryBlock = ast.newBlock();
								tryStatements = tryBlock.statements();
								
								// generate this: 
								//    	else {
								thenBlock = ast.newBlock();
								thenBlock.statements().add(tryStatement);
								ifStatement.setElseStatement(thenBlock);
	
								// generate this: 
							    //	  try {
							    //	    PrintStream ps = new PrintStream(response.getOutputStream());
					    		//		t.printStackTrace(ps); 
							    //	    ps.close();
					    		//		response.getOutputStream().close();
								tryStatements.add(getConstructorVariableDeclarationStatement(ast, "PrintStream", "ps",  //$NON-NLS-1$ //$NON-NLS-2$
										CodeGenerationUtils.getMethodInvocation(ast, "response", "getOutputStream")));	//$NON-NLS-1$ //$NON-NLS-2$
								tryStatements.add(CodeGenerationUtils.getExpressionStatement(ast, "t", "printStackTrace", ast.newSimpleName("ps"))); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
								tryStatements.add(CodeGenerationUtils.getExpressionStatement(ast, "ps", "close")); //$NON-NLS-1$ //$NON-NLS-2$
								firstCall = CodeGenerationUtils.getMethodInvocation(ast, "response", //$NON-NLS-1$
										"getOutputStream"); //$NON-NLS-1$
								methodInvocation = ast.newMethodInvocation();
								expressionStatement = ast.newExpressionStatement(methodInvocation);

								methodInvocation.setExpression(firstCall);
								methodInvocation.setName(ast.newSimpleName("close")); //$NON-NLS-1$
								tryStatements.add(expressionStatement);

								// generate this: 
							    //	  catch(Exception ex) {}
								tryStatement.catchClauses().add(getEmptyCatchClause(ast));
								tryStatement.setBody(tryBlock);

								return methodDeclaration;
							}

							@SuppressWarnings("unchecked")
							private MethodDeclaration generateToStringMethodDeclaration(AST ast, String filterName) {
								MethodDeclaration methodDeclaration = ast.newMethodDeclaration();
								Javadoc docComment = ast.newJavadoc();

								// generate this: 
							    // /**
							    //   * Return a String representation of this object.
							    //   */
								docComment.tags().add(CodeGenerationUtils.getTagElement(ast, null, null, 
									"Return a String representation of this object.")); //$NON-NLS-1$
								methodDeclaration.setJavadoc(docComment);

								// generate this: 
								// public String toString() {
								methodDeclaration.setBody(ast.newBlock());
								methodDeclaration.setConstructor(false);
								methodDeclaration.modifiers().add(ast.newModifier(Modifier.ModifierKeyword.PUBLIC_KEYWORD));
								methodDeclaration.setName(ast.newSimpleName("toString")); //$NON-NLS-1$
								methodDeclaration.setReturnType2(ast.newSimpleType(ast.newSimpleName("String"))); //$NON-NLS-1$

								// generate this: 
								// @Override
								Annotation ann = ast.newMarkerAnnotation();
								ann.setTypeName(ast.newSimpleName("Override"));	//$NON-NLS-1$
								methodDeclaration.modifiers().add(0, ann);

								// if (filterConfig == null)
								List<Statement> statements = methodDeclaration.getBody().statements();
								IfStatement ifStatement = ast.newIfStatement();
								InfixExpression infixExpr = ast.newInfixExpression();
								infixExpr.setLeftOperand(ast.newSimpleName("filterConfig")); //$NON-NLS-1$
								infixExpr.setOperator(InfixExpression.Operator.EQUALS);
								infixExpr.setRightOperand(ast.newNullLiteral());
								ifStatement.setExpression(infixExpr);

								// generate this: 
							    // return ("<filtername>()");
								ReturnStatement returnStatement = ast.newReturnStatement();
								StringLiteral stringLiteral = ast.newStringLiteral();
								stringLiteral.setLiteralValue(filterName + "()");	//$NON-NLS-1$;
								returnStatement.setExpression(stringLiteral);
								ifStatement.setThenStatement(returnStatement);
								statements.add(ifStatement);

								// generate this: 
								// StringBuffer sb = new StringBuffer("<filtername>(");
								// sb.append(filterConfig);
								// sb.append(")");
								stringLiteral = ast.newStringLiteral();
								stringLiteral.setLiteralValue(filterName + "(");	//$NON-NLS-1$;
								statements.add(getConstructorVariableDeclarationStatement(ast, "StringBuffer", "sb", stringLiteral)); //$NON-NLS-1$ //$NON-NLS-2$
								statements.add(CodeGenerationUtils.getExpressionStatement(ast, "sb", "append", ast.newSimpleName("filterConfig"))); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
								statements.add(CodeGenerationUtils.getExpressionStatement(ast, "sb", "append", ")")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

								// generate this: 
								// return sb.toString();
								returnStatement = ast.newReturnStatement();
								returnStatement.setExpression(CodeGenerationUtils.getMethodInvocation(ast, "sb", "toString")); //$NON-NLS-1$ //$NON-NLS-2$
								statements.add(returnStatement);

								return methodDeclaration;
							}

							@SuppressWarnings("unchecked")
							private MethodDeclaration generateGetFilterConfigMethodDeclaration(AST ast) {
								MethodDeclaration methodDeclaration = ast.newMethodDeclaration();
								Javadoc docComment = ast.newJavadoc();

								// generate this: 
							    // /**
							    //   * Return the filter configuration object for this filter.
							    //   */
								docComment.tags().add(CodeGenerationUtils.getTagElement(ast, null, null, 
									"Return the filter configuration object for this filter.")); //$NON-NLS-1$
								methodDeclaration.setJavadoc(docComment);

								// generate this: 
							    //  public FilterConfig getFilterConfig() {
								methodDeclaration.setBody(ast.newBlock());
								methodDeclaration.setConstructor(false);
								methodDeclaration.modifiers().add(ast.newModifier(Modifier.ModifierKeyword.PUBLIC_KEYWORD));
								methodDeclaration.setName(ast.newSimpleName("getFilterConfig")); //$NON-NLS-1$
								methodDeclaration.setReturnType2(ast.newSimpleType(ast.newSimpleName("FilterConfig"))); //$NON-NLS-1$
	
								// generate this: 
								//		return this.filterConfig;
								List<Statement> statements = methodDeclaration.getBody().statements();
								FieldAccess fieldAccess = ast.newFieldAccess();
								
								fieldAccess.setExpression(ast.newThisExpression());
								fieldAccess.setName(ast.newSimpleName("filterConfig")); //$NON-NLS-1$
								ReturnStatement returnStatement = ast.newReturnStatement();
								returnStatement.setExpression(fieldAccess);
								statements.add(returnStatement);

								return methodDeclaration;
							}

							@SuppressWarnings("unchecked")
							private MethodDeclaration generateSetFilterConfigMethodDeclaration(AST ast) {
								MethodDeclaration methodDeclaration = ast.newMethodDeclaration();
								Javadoc docComment = ast.newJavadoc();
								List tags = docComment.tags();

								// generate this: 
							    // /**
							    //   * Set the filter configuration object for this filter.
							    //   *
							    //   * @param filterConfig The filter configuration object
							    //   */
								tags.add(CodeGenerationUtils.getTagElement(ast, null, null, 
									"Set the filter configuration object for this filter.")); //$NON-NLS-1$
								tags.add(CodeGenerationUtils.getTagElement(ast, TagElement.TAG_PARAM, "filterConfig", "The filter configuration object")); //$NON-NLS-1$ //$NON-NLS-2$
								methodDeclaration.setJavadoc(docComment);

								// generate this: 
							    //  public void setFilterConfig(FilterConfig filterConfig) {
								methodDeclaration.setBody(ast.newBlock());
								methodDeclaration.setConstructor(false);
								methodDeclaration.modifiers().add(ast.newModifier(Modifier.ModifierKeyword.PUBLIC_KEYWORD));
								methodDeclaration.setName(ast.newSimpleName("setFilterConfig")); //$NON-NLS-1$
								methodDeclaration.setReturnType2(ast.newPrimitiveType(PrimitiveType.VOID));
								SingleVariableDeclaration variableDeclaration = ast.newSingleVariableDeclaration();
								variableDeclaration.setType(ast.newSimpleType(ast.newSimpleName("FilterConfig"))); //$NON-NLS-1$
								variableDeclaration.setName(ast.newSimpleName("filterConfig")); //$NON-NLS-1$
								methodDeclaration.parameters().add(variableDeclaration);
	
								// generate this: 
								// 		this.filterConfig = filterConfig;
								List<Statement> statements = methodDeclaration.getBody().statements();
								Assignment assignment = ast.newAssignment();
								FieldAccess fieldAccess = ast.newFieldAccess();
								
								fieldAccess.setExpression(ast.newThisExpression());
								fieldAccess.setName(ast.newSimpleName("filterConfig")); //$NON-NLS-1$
								assignment.setLeftHandSide(fieldAccess);
								assignment.setOperator(Operator.ASSIGN);
								assignment.setRightHandSide(ast.newSimpleName("filterConfig")); //$NON-NLS-1$
								statements.add(ast.newExpressionStatement(assignment));

								return methodDeclaration;
							}

							private VariableDeclarationStatement getConstructorVariableDeclarationStatement(AST ast, 
									String variableType, String variableName, String argument){
								Expression argExp = ((argument != null) ? ast.newSimpleName(argument) : null);
								return getConstructorVariableDeclarationStatement(ast, variableType, variableName, argExp);
							}

							@SuppressWarnings("unchecked")
							private VariableDeclarationStatement getConstructorVariableDeclarationStatement(AST ast, 
									String variableType, String variableName, Expression argument){
								VariableDeclarationFragment vdf = ast.newVariableDeclarationFragment();
								VariableDeclarationStatement vds = ast.newVariableDeclarationStatement(vdf);
								ClassInstanceCreation creationExp = ast.newClassInstanceCreation();

								vdf.setName(ast.newSimpleName(variableName));
								vds.setType(ast.newSimpleType(ast.newSimpleName(variableType)));
								creationExp.setType(ast.newSimpleType(ast.newSimpleName(variableType)));
								if (argument != null) {
									creationExp.arguments().add(argument);
								}
								vdf.setInitializer(creationExp);

								return vds;

							}

							// generate this: 
						    //	  catch(Exception ex) {}
							private CatchClause getEmptyCatchClause(AST ast) {
								CatchClause catchClause = ast.newCatchClause();
								SingleVariableDeclaration svd = ast.newSingleVariableDeclaration();
								catchClause.setBody(ast.newBlock());
								svd.setType(ast.newSimpleType(ast.newSimpleName("Exception")));  //$NON-NLS-1$
								svd.setName(ast.newSimpleName("ex"));	//$NON-NLS-1$
								catchClause.setException(svd);
								return catchClause;
							}

							@SuppressWarnings("unchecked")
							private Annotation addWebFilterAnnotation(AST ast, CreateJavaEEArtifactTemplateModel tempModel,
									CompilationUnit result) {
								Annotation ann = ast.newNormalAnnotation();
								ann.setTypeName(ast.newSimpleName("WebFilter"));	//$NON-NLS-1$

								if (tempModel instanceof CreateFilterTemplateModel) {
									CreateFilterTemplateModel filterModel = (CreateFilterTemplateModel)tempModel;
									List<MemberValuePair> annValues = ((NormalAnnotation) ann).values();
									List<String[]> initParams = filterModel.getInitParams();

									annValues.add(CodeGenerationUtils.addName(ast, filterModel.getFilterName(), "filterName")); //$NON-NLS-1$
									addFilterMappings(annValues, ast, filterModel.getFilterMappings(), result);
									if (initParams != null) {
										annValues.add(CodeGenerationUtils.addInitParams(ast, initParams, result));
									}
								}

								return ann;
							}

							@SuppressWarnings("unchecked")
							private void addFilterMappings(List<MemberValuePair> annValues, AST ast, 
									List<IFilterMappingItem> filterMappings, CompilationUnit result) {
								HashSet<Integer> allDispatchers = new HashSet<Integer>();
								ArrayInitializer arrayInitURL = ast.newArrayInitializer();
								ArrayInitializer arrayInitServlet = ast.newArrayInitializer();

								for (Iterator<IFilterMappingItem> iterator = filterMappings.iterator(); iterator.hasNext();) {
									IFilterMappingItem filterMapping = iterator.next();
									StringLiteral literal = ast.newStringLiteral();
									ArrayInitializer arrayToUse = null;

									if (filterMapping.isUrlPatternType()) {
										arrayToUse = arrayInitURL;
									} else if (filterMapping.isServletNameType()) {
										arrayToUse = arrayInitServlet;
									}

									if (arrayToUse != null) {
										literal.setLiteralValue(filterMapping.getName());
										arrayToUse.expressions().add(literal);
									}

									int dispatchers = filterMapping.getDispatchers();
							        if ((dispatchers & IFilterMappingItem.REQUEST) > 0) {
							            allDispatchers.add(IFilterMappingItem.REQUEST);
							        }
							        if ((dispatchers & IFilterMappingItem.FORWARD) > 0) {
							            allDispatchers.add(IFilterMappingItem.FORWARD);
							        }
							        if ((dispatchers & IFilterMappingItem.INCLUDE) > 0) {
							            allDispatchers.add(IFilterMappingItem.INCLUDE);
							        }
							        if ((dispatchers & IFilterMappingItem.ERROR) > 0) {
							            allDispatchers.add(IFilterMappingItem.ERROR);
							        }
								}
								if (!arrayInitURL.expressions().isEmpty()) {
									MemberValuePair annotationProperty = ast.newMemberValuePair();
									annotationProperty.setName(ast.newSimpleName("urlPatterns"));	//$NON-NLS-1$
									annotationProperty.setValue(arrayInitURL);
									annValues.add(annotationProperty);
								}
								if (!arrayInitServlet.expressions().isEmpty()) {
									MemberValuePair annotationProperty = ast.newMemberValuePair();
									annotationProperty.setName(ast.newSimpleName("servletNames"));	//$NON-NLS-1$
									annotationProperty.setValue(arrayInitServlet);
									annValues.add(annotationProperty);
								}

								if (!allDispatchers.isEmpty()) {
									MemberValuePair annotationProperty = ast.newMemberValuePair();
									ArrayInitializer arrayInitDispatchers = ast.newArrayInitializer();
									annotationProperty.setName(ast.newSimpleName("dispatcherTypes"));	//$NON-NLS-1$
									
									for (Integer nextDispatcher : allDispatchers) {
										String identifier = null;
										
										switch (nextDispatcher.intValue()) {
											case IFilterMappingItem.REQUEST:
												identifier = "REQUEST"; //$NON-NLS-1$
												break;
											case IFilterMappingItem.FORWARD:
												identifier = "FORWARD"; //$NON-NLS-1$
												break;
											case IFilterMappingItem.INCLUDE:
												identifier = "INCLUDE"; //$NON-NLS-1$
												break;
											case IFilterMappingItem.ERROR:
												identifier = "ERROR"; //$NON-NLS-1$
												break;
											default:
												break;
										}

										if (identifier != null) {
											QualifiedName name = ast.newQualifiedName(ast.newSimpleName("DispatcherType"), //$NON-NLS-1$
													ast.newSimpleName(identifier));
											arrayInitDispatchers.expressions().add(name);
										}
									}
									annotationProperty.setValue(arrayInitDispatchers);
									annValues.add(annotationProperty);

									// add corresponding import for DispatcherType:
									result.imports().add(CodeGenerationUtils.addImport(ast, "javax", "servlet", null, "DispatcherType")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
								}
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
		if (page instanceof NewFilterClassWizardPage) {
			NewFilterClassWizardPage page1 = new NewFilterClassWizardPage(
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
			page1.setInfopopID(IWebUIContextIds.WEBEDITOR_FILTER_PAGE_ADD_FILTER_WIZARD_1);
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
