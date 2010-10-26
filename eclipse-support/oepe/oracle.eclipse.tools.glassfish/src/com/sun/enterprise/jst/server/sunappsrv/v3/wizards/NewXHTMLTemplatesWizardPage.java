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

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.text.templates.Template;
import org.eclipse.jface.viewers.IContentProvider;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.wst.html.ui.internal.wizard.NewHTMLTemplatesWizardPage;

@SuppressWarnings("restriction")
public class NewXHTMLTemplatesWizardPage extends NewHTMLTemplatesWizardPage {

	/**
	 * Content provider for xhtml templates
	 */
	private class XTemplateContentProvider implements IStructuredContentProvider {
		private IContentProvider delegate;

		public XTemplateContentProvider(IContentProvider delegate) {
			this.delegate = delegate;
		}
		
		/*
		 * @see IContentProvider#dispose()
		 */
		public void dispose() {
			delegate.dispose();
			delegate = null;
		}

		/*
		 * @see IStructuredContentProvider#getElements(Object)
		 */
		public Object[] getElements(Object input) {
			Object[] templs = ((IStructuredContentProvider) delegate).getElements(input);
			List<Object> result = new ArrayList<Object>(templs.length);
			for (Object templ : templs) {
				if (templ instanceof Template) {
					String name = ((Template) templ).getName().toLowerCase();
					if (name.indexOf("xhtml") >= 0 || name.indexOf("facelet") >= 0) {
						result.add(templ);
					}
				}
			}
			return result.toArray(new Object[result.size()]);
		}

		/*
		 * @see IContentProvider#inputChanged(Viewer, Object, Object)
		 */
		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
			delegate.inputChanged(viewer, oldInput, newInput);
		}
	}

	public void createControl(Composite ancestor) {
		super.createControl(ancestor);
		
		try {
			Field declaredField = getClass().getSuperclass().getDeclaredField("fTableViewer");
			declaredField.setAccessible(true);
			TableViewer v = ((TableViewer) declaredField.get(this));
			v.setContentProvider(new XTemplateContentProvider(v.getContentProvider()));

//			declaredField = getClass().getSuperclass().getDeclaredField("fUseTemplateButton");
//			declaredField.setAccessible(true);
//			Button b = ((Button) declaredField.get(this));
//			b.setEnabled(true);
//			b.setVisible(false);
//			GridData gd = ((GridData) b.getLayoutData());
//			gd.exclude = true;
//			b.setLayoutData(gd);
			
		} catch (IllegalArgumentException e) {
			e.printStackTrace();
		} catch (SecurityException e) {
			e.printStackTrace();
		} catch (IllegalAccessException e) {
			e.printStackTrace();
		} catch (NoSuchFieldException e) {
			e.printStackTrace();
		}
	}
}
