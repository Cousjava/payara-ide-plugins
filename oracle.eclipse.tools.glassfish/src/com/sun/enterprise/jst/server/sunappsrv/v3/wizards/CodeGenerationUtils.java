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


package com.sun.enterprise.jst.server.sunappsrv.v3.wizards;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Iterator;
import java.util.List;

import org.eclipse.emf.codegen.jet.JETException;
import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.Annotation;
import org.eclipse.jdt.core.dom.ArrayInitializer;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.ImportDeclaration;
import org.eclipse.jdt.core.dom.Javadoc;
import org.eclipse.jdt.core.dom.MemberValuePair;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.NormalAnnotation;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.StringLiteral;
import org.eclipse.jdt.core.dom.TagElement;
import org.eclipse.jdt.core.dom.TextElement;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.Document;
import org.eclipse.text.edits.MalformedTreeException;
import org.eclipse.text.edits.TextEdit;

import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;

public class CodeGenerationUtils {
	static String get35SuperTemplateSource(Object templateModel, Object templateImpl) 
		throws JETException {
	    try {
			Method generateTemplateSource = templateImpl.getClass().getMethod("generate", new Class[] { Object.class }); //$NON-NLS-1$
	        if (generateTemplateSource != null) {
	        	Object superSource = generateTemplateSource.invoke(templateImpl, templateModel);
				return (String)superSource;
	        }
	    } catch (SecurityException e) {
	        SunAppSrvPlugin.logMessage("in CodeGenerationUtils get35SuperTemplateSource : security exception"); //$NON-NLS-1$
	    } catch (NoSuchMethodException e) {
	        SunAppSrvPlugin.logMessage("in CodeGenerationUtils get35SuperTemplateSource : no such method exception"); //$NON-NLS-1$
	    } catch (IllegalArgumentException e) {
	        SunAppSrvPlugin.logMessage("in CodeGenerationUtils get35SuperTemplateSource : illegal argument exception"); //$NON-NLS-1$
	    } catch (IllegalAccessException e) {
	        SunAppSrvPlugin.logMessage("in CodeGenerationUtils get35SuperTemplateSource : illegal access exception"); //$NON-NLS-1$
	    } catch (InvocationTargetException e) {
	        SunAppSrvPlugin.logMessage("in CodeGenerationUtils get35SuperTemplateSource : invocation target exception"); //$NON-NLS-1$
	    }
	    return null;
	}

	static String getRewrittenSource(String source, CompilationUnit result) {
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
	static MethodInvocation getMethodInvocation(AST ast, String methodExpr, 
			String methodName, Expression methodExpression) {
		MethodInvocation methodInvocation = ast.newMethodInvocation();
		methodInvocation.setExpression(ast.newSimpleName(methodExpr));
		methodInvocation.setName(ast.newSimpleName(methodName));
		if (methodExpression != null) {
			methodInvocation.arguments().add(methodExpression);
		}
		return methodInvocation;
	}

	static MethodInvocation getMethodInvocation(AST ast, String methodExpr, 
			String methodName) {
		return getMethodInvocation(ast, methodExpr, methodName, (Expression)null);		
	}

	static MethodInvocation getMethodInvocation(AST ast, String methodExpr, 
			String methodName, String methodLiteral) {
		StringLiteral literal = null;
		if (methodLiteral != null) {
			literal = ast.newStringLiteral();
			literal.setLiteralValue(methodLiteral);
		}
		return getMethodInvocation(ast, methodExpr, methodName, literal);
	}

	static ExpressionStatement getExpressionStatement(AST ast, String methodExpr, 
			String methodName, String methodLiteral) {
		return ast.newExpressionStatement(
				getMethodInvocation(ast, methodExpr, methodName, methodLiteral));
	}

	static ExpressionStatement getExpressionStatement(AST ast, String methodExpr, 
			String methodName, Expression methodExpression) {
		return ast.newExpressionStatement(
				getMethodInvocation(ast, methodExpr, methodName, methodExpression));
	}

	static ExpressionStatement getExpressionStatement(AST ast, String methodExpr, 
			String methodName) {
		return getExpressionStatement(ast, methodExpr, methodName, (Expression)null);
	}

	@SuppressWarnings("unchecked")
	static TagElement getTagElement(AST ast, String tagName, String fragmentName, String textValue) {
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

	static void cleanupJavadoc(Javadoc javadoc, String tagNamePrefix) {
		Iterator it = javadoc.tags().iterator();
		while (it.hasNext()) {
			TagElement tagElement = (TagElement) it.next();
			String tagName = tagElement.getTagName();
			if ((tagName != null) && tagName.startsWith(tagNamePrefix)) {
				it.remove();
			}
		}
	}

	static MemberValuePair addName(AST ast, String servletName, String property) {
		MemberValuePair annotationProperty = ast.newMemberValuePair();
		StringLiteral literal = ast.newStringLiteral();

		literal.setLiteralValue(servletName);
		annotationProperty.setName(ast.newSimpleName(property));
		annotationProperty.setValue(literal);

		return annotationProperty;
	}

	@SuppressWarnings("unchecked")
	static MemberValuePair addInitParams(AST ast, List<String[]> initParams, CompilationUnit result) {
		MemberValuePair annotationProperty = ast.newMemberValuePair();
		ArrayInitializer arrayInit = ast.newArrayInitializer();

		result.imports().add(CodeGenerationUtils.addWebAnnotationImport(ast, "WebInitParam"));	//$NON-NLS-1$

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

	static ImportDeclaration addImport(AST ast, String packagePart1, String packagePart2, 
			String packagePart3, String className) {
		ImportDeclaration importDecl = ast.newImportDeclaration();
		QualifiedName name = ast.newQualifiedName(ast
				.newSimpleName(packagePart1), ast
				.newSimpleName(packagePart2));
		if (packagePart3 != null) {
			name = ast.newQualifiedName(name, ast
					.newSimpleName(packagePart3));
		}
		name = ast.newQualifiedName(name, 
				ast.newSimpleName(className));
		importDecl.setName(name);
		return importDecl;
	}

	static ImportDeclaration addWebAnnotationImport(AST ast, String className) {
		return addImport(ast, "javax", "servlet", "annotation", className); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
	}			
}
