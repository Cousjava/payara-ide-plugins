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
package com.sun.enterprise.jst.server.sunappsrv.v3;

import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.jdt.core.IAnnotation;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IJavaElementDelta;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IMemberValuePair;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jst.j2ee.model.IModelProviderEvent;
import org.eclipse.jst.javaee.core.EjbLocalRef;
import org.eclipse.jst.javaee.core.JavaEEObject;
import org.eclipse.jst.javaee.core.JavaeeFactory;
import org.eclipse.jst.javaee.core.Listener;
import org.eclipse.jst.javaee.core.ResourceRef;
import org.eclipse.jst.javaee.core.RunAs;
import org.eclipse.jst.javaee.core.SecurityRole;
import org.eclipse.jst.javaee.core.SecurityRoleRef;
import org.eclipse.jst.javaee.core.UrlPatternType;
import org.eclipse.jst.javaee.ejb.SecurityIdentityType;
import org.eclipse.jst.javaee.web.Filter;
import org.eclipse.jst.javaee.web.Servlet;
import org.eclipse.jst.javaee.web.WebApp;
import org.eclipse.jst.javaee.web.WebFactory;
import org.eclipse.jst.jee.model.internal.WebAnnotationFactory;
import org.eclipse.jst.jee.model.internal.common.AbstractAnnotationModelProvider;
import org.eclipse.jst.jee.model.internal.common.ManyToOneRelation;
import org.eclipse.jst.jee.model.internal.common.Result;
import org.eclipse.wst.common.project.facet.core.IFacetedProject;

/** Inspired by {@link org.eclipse.jst.jee.model.internal.WebAnnotationReader} */
public class Web30AnnotationReader extends AbstractAnnotationModelProvider<WebApp> {
	private static final String WEB_SERVLET = "WebServlet"; //$NON-NLS-1$

	private ManyToOneRelation<JavaEEObject, ICompilationUnit> modelToUnit;

	private ManyToOneRelation<JavaEEObject, ICompilationUnit> modelToInterfaceUnit;

	private WebAnnotationFactory annotationFactory;

	private WebApp ddApp;

	public Web30AnnotationReader(IFacetedProject facetedProject, WebApp ddApp) {
		super(facetedProject);
		if (ddApp == null) {
			throw new IllegalArgumentException("The deployment descriptor model can not be null!"); //$NON-NLS-1$
		}
		this.ddApp = ddApp;
	}

	@Override
	protected void preLoad() {
		modelObject = WebFactory.eINSTANCE.createWebApp();
		annotationFactory = WebAnnotationFactory.createFactory();
	}

	@Override
	protected void loadModel() throws CoreException {
		IJavaProject javaProject = JavaCore.create(facetedProject.getProject());
		final Collection<ICompilationUnit> javaUnits = new HashSet<ICompilationUnit>();
		for (final IPackageFragmentRoot root : javaProject.getAllPackageFragmentRoots()) {
			visitJavaFiles(javaUnits, root);
		}
		annotationFactory = WebAnnotationFactory.createFactory();

		modelToInterfaceUnit = new ManyToOneRelation<JavaEEObject, ICompilationUnit>();
		modelToUnit = new ManyToOneRelation<JavaEEObject, ICompilationUnit>();
		for (ICompilationUnit unit : javaUnits) {
			Result result = analyzeCompilationUnit(unit);
			if (result == null) {
				continue;
			}
			processResult(unit, result);
		}
	}

	/**
	 * Analyze this unit for a web artifact. If the file is not a valid java
	 * compilation unit or it does not contain a class the method returns
	 * <code>null</code>
	 * 
	 * Only the primary type of the compilation unit is processed.
	 * 
	 * @param unit
	 *            the unit to be processed.
	 * @return result from processing the file
	 * @throws JavaModelException
	 */
	@SuppressWarnings("unchecked")
	private Result analyzeCompilationUnit(ICompilationUnit unit) throws JavaModelException {
		IType rootType = unit.findPrimaryType();
		/*
		 * If the compilation unit is not valid and can not be compiled the
		 * rootType may be null. This can happen if a class is define as follows
		 * <code> @Stateless public SomeClass implements SomeInterface{}</code>.
		 * Here the "class" word is missed and the type is not valid.
		 */
		if (rootType == null || !rootType.isClass()) {
			return null;
		}
		
		{ // check already known items first
			for (Iterator iter = ddApp.getServlets().iterator(); iter.hasNext();) {
				Servlet servlet = (Servlet) iter.next();
				if (rootType.getFullyQualifiedName().equals(
						servlet.getServletClass())) {
					Result serv = annotationFactory.createServlet(rootType,
							servlet.getServletName());
					addUrlPatternsAsRunAs(rootType, serv);
					return serv;
				}
			}
			for (Iterator iter = ddApp.getListeners().iterator(); iter
					.hasNext();) {
				Listener listener = (Listener) iter.next();
				if (rootType.getFullyQualifiedName().equals(
						listener.getListenerClass())) {
					return annotationFactory.createListener(rootType);
				}
			}
			for (Iterator iter = ddApp.getFilters().iterator(); iter.hasNext();) {
				Filter filter = (Filter) iter.next();
				if (rootType.getFullyQualifiedName().equals(
						filter.getFilterClass())) {
					return annotationFactory.createFilter(rootType, filter
							.getFilterName());
				}
			}
		}

		// gets here when mapping is only by annotation and so wasn't processed above
		IAnnotation serv = getAnnotation(WEB_SERVLET, rootType);
		if (serv != null) {
			Result servlet = annotationFactory.createServlet(rootType, rootType.getElementName());
			addUrlPatternsAsRunAs(rootType, servlet);
			return servlet;
		}

		return null;
	}

	private IAnnotation getAnnotation(String annName, IType rootType) throws JavaModelException {
		for (IAnnotation annotation : rootType.getAnnotations()) {
			String annotationName = annotation.getElementName();
			if (annName.equals(annotationName)) {
				return annotation;
			}
		}
		return null;
	}

	private void addUrlPatternsAsRunAs(IType rootType, Result servlet)
			throws JavaModelException {
		IAnnotation serv = getAnnotation(WEB_SERVLET, rootType);
		// original WebAnnotationFactory processes just "value" property
		if (((Servlet) servlet.getMainObject()).getRunAs() == null) {
			Object urls = getAnnotatedValue("urlPatterns", serv
					.getMemberValuePairs());
			if (urls != null) {
				RunAs runAs = JavaeeFactory.eINSTANCE.createRunAs();
				if (urls instanceof String) {
					runAs.setRoleName((String) urls);
				} else if (urls instanceof Object[]
						&& ((Object[]) urls).length > 0
						&& ((Object[]) urls)[0] instanceof String) {
					runAs.setRoleName((String) ((Object[]) urls)[0]);
				}
				((Servlet) servlet.getMainObject()).setRunAs(runAs);
			}
		}
	}
	
	protected Object getAnnotatedValue(String name, IMemberValuePair[] memberValuePairs) throws JavaModelException {
		for (IMemberValuePair pair : memberValuePairs) {
			if (name.equals(pair.getMemberName())) {
				return pair.getValue();
			}
		}
		return null;
	}


	/**
	 * Process the result from parsing the unit. Depending on the result this
	 * might include adding a session bean, message driven bean, securityRole
	 * etc.
	 * 
	 * @param unit
	 * @param result
	 * @throws JavaModelException
	 */
	private void processResult(ICompilationUnit unit, Result result) throws JavaModelException {
		JavaEEObject mainObject = result.getMainObject();
		if (Servlet.class.isInstance(mainObject)) {
			servletFound(unit, (Servlet) result.getMainObject(), result.getDependedTypes());
		}
		for (JavaEEObject additional : result.getAdditional()) {
			if (EjbLocalRef.class.isInstance(additional)) {
				ejbLocalRefFound(unit, (EjbLocalRef) additional, result.getDependedTypes());
			} else if (ResourceRef.class.isInstance(additional)) {
				resourceRefFound(unit, (ResourceRef) additional, result.getDependedTypes());
			} else if (SecurityRole.class.isInstance(additional)) {
				securityRoleFound(result.getMainObject(), (SecurityRole) additional);
			} else if (SecurityIdentityType.class.isInstance(additional)) {
				securityIdentityTypeFound(unit, (SecurityIdentityType) additional);
			}
		}
	}

	@SuppressWarnings("unchecked")
	private void servletFound(ICompilationUnit unit, Servlet servlet, Collection<IType> dependedTypes)
			throws JavaModelException {
		modelObject.getServlets().add(servlet);
		connectObjectWithFile(unit, servlet, dependedTypes);

		// FIXME 1) why the mapping is not added in the original WebAnnotationReader? who should manage the mapping in theory?
		// FIXME 2) atm we just add up mappings and never clean up 
		if (servlet.getRunAs() != null) {
			String pattern = servlet.getRunAs().getRoleName();

			// Create the servlet mapping instance from the web factory
			org.eclipse.jst.javaee.web.ServletMapping mapping = WebFactory.eINSTANCE
					.createServletMapping();

			mapping.setServletName(servlet.getServletName());
			// Set the URL pattern to map the servlet to
			UrlPatternType url = JavaeeFactory.eINSTANCE.createUrlPatternType();
			url.setValue(pattern);
			mapping.getUrlPatterns().add(url);

			// Add the servlet mapping to the web application model list
			modelObject.getServletMappings().add(mapping);
		}
	}

	private void securityIdentityTypeFound(ICompilationUnit file, SecurityIdentityType additional) {
	}

	@SuppressWarnings("unchecked")
	private void resourceRefFound(ICompilationUnit unit, ResourceRef resourceRef, Collection<IType> dependedTypes)
			throws JavaModelException {
		modelObject.getResourceRefs().add(resourceRef);
		connectObjectWithFile(unit, resourceRef, dependedTypes);
	}

	@SuppressWarnings("unchecked")
	private void ejbLocalRefFound(ICompilationUnit unit, EjbLocalRef localRef, Collection<IType> dependedTypes)
			throws JavaModelException {
		modelObject.getEjbLocalRefs().add(localRef);
		connectObjectWithFile(unit, localRef, dependedTypes);
	}

	private void connectObjectWithFile(ICompilationUnit unit, JavaEEObject localRef, Collection<IType> dependedTypes)
			throws JavaModelException {
		modelToUnit.connect(localRef, unit);
		for (IType type : dependedTypes) {
			if (type.isBinary() || type.isInterface() == false)
				continue;
			modelToInterfaceUnit.connect(localRef, type.getCompilationUnit());
		}
	}

	@Override
	protected void processAddedCompilationUnit(IModelProviderEvent modelEvent, ICompilationUnit unit)
			throws CoreException {
		Result result = analyzeCompilationUnit(unit);
		if (result == null || result.isEmpty())
			return;
		processResult(unit, result);
		modelEvent.addResource(unit);
	}

	@Override
	protected void processChangedCompilationUnit(IModelProviderEvent modelEvent, ICompilationUnit unit)
			throws CoreException {
		if (modelToUnit.containsTarget(unit))
			processChangedModelUnit(modelEvent, unit);
		else
			processAddedCompilationUnit(modelEvent, unit);
	}

	private void processChangedModelUnit(IModelProviderEvent modelEvent, ICompilationUnit unit) throws CoreException {
		processRemovedCompilationUnit(modelEvent, unit);
		processAddedCompilationUnit(modelEvent, unit);
	}

	@Override
	protected void processRemovedCompilationUnit(IModelProviderEvent modelEvent, ICompilationUnit unit)
			throws CoreException {
		if (modelToUnit.containsTarget(unit))
			processRemovedModelResource(modelEvent, unit);
		else if (modelToInterfaceUnit.containsTarget(unit))
			processRemoveInterface(modelEvent, unit);
	}

	private void processRemoveInterface(IModelProviderEvent event, ICompilationUnit unit) {
	}

	@Override
	protected void processRemovedPackage(IModelProviderEvent modelEvent, IJavaElementDelta delta) throws CoreException {
		for (ICompilationUnit unit : modelToUnit.getTargets()) {
			if (unit.getParent().getElementName().equals(delta.getElement().getElementName())) {
				processRemovedCompilationUnit(modelEvent, unit);
			}
		}
	}

	private void processRemovedModelResource(IModelProviderEvent event, ICompilationUnit file) {
		Collection<JavaEEObject> modelObjects = modelToUnit.getSources(file);
		for (JavaEEObject o : modelObjects) {
			if (Servlet.class.isInstance(o))
				disconnectFromRoles(o);
			EcoreUtil.remove((EObject) o);
		}
		modelToUnit.disconnect(file);
		event.setEventCode(event.getEventCode() | IModelProviderEvent.REMOVED_RESOURCE);
		event.addResource(file);
	}

	public void modify(Runnable runnable, IPath modelPath) {
	}

	public IStatus validateEdit(IPath modelPath, Object context) {
		return null;
	}

	@SuppressWarnings("unchecked")
	@Override
	protected Collection<SecurityRoleRef> getSecurityRoleRefs(JavaEEObject target) {
		if (Servlet.class.isInstance(target))
			return ((Servlet) target).getSecurityRoleRefs();
		return null;
	}

	@SuppressWarnings("unchecked")
	@Override
	protected Collection<SecurityRole> getSecurityRoles() {
		return modelObject.getSecurityRoles();
	}

	/** 3.4 had it abstract in super, so have to copy */
	public void dispose() {
		IModelProviderEvent modelEvent = createModelProviderEvent();
		modelEvent.addResource(facetedProject.getProject());
		modelEvent.setEventCode(IModelProviderEvent.UNLOADED_RESOURCE);
		JavaCore.removeElementChangedListener(this);
		modelObject = null;
		notifyListeners(modelEvent);
		clearListeners();
	}

	/** 3.4 had it abstract in super, so have to copy */
	protected void processRemovedFile(IModelProviderEvent modelEvent, IFile file) throws CoreException {
		ICompilationUnit icu = JavaCore.createCompilationUnitFrom(file);
		processRemovedCompilationUnit(modelEvent, icu);
	}

	/** 3.4 had it abstract in super, so have to copy */
	protected void processAddedFile(IModelProviderEvent modelEvent, IFile file) throws CoreException {
		ICompilationUnit icu = JavaCore.createCompilationUnitFrom(file);
		processAddedCompilationUnit(modelEvent, icu);
	}

	/** 3.4 had it abstract in super, so have to copy */
	protected void processChangedFile(IModelProviderEvent modelEvent, IFile file) throws CoreException {
		ICompilationUnit icu = JavaCore.createCompilationUnitFrom(file);
		processChangedCompilationUnit(modelEvent, icu);
	}
}
