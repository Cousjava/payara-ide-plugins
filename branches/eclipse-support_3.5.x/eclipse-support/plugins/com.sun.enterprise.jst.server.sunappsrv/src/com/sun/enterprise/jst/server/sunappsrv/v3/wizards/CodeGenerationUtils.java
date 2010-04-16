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

	@SuppressWarnings("unchecked")
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
