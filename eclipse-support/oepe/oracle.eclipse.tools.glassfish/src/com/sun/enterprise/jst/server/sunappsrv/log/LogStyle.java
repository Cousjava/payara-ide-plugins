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


package com.sun.enterprise.jst.server.sunappsrv.log;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.LineStyleEvent;
import org.eclipse.swt.custom.LineStyleListener;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.PlatformUI;

import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;
import com.sun.enterprise.jst.server.sunappsrv.preferences.PreferenceConstants;

public class LogStyle implements LineStyleListener, IPropertyChangeListener {
	Display display = Display.getCurrent();

	IPreferenceStore store = SunAppSrvPlugin.getInstance().getPreferenceStore();
	boolean colorInConsole = store.getBoolean(PreferenceConstants.ENABLE_COLORS_CONSOLE);

	public LogStyle() {
		store.addPropertyChangeListener(this);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#finalize()
	 */
	@Override
	protected void finalize() throws Throwable {
		store.removePropertyChangeListener(this);
		super.finalize();
	}

	public void lineGetStyle(LineStyleEvent event) {
		List<StyleRange> styles = new ArrayList<StyleRange>();	// collection for the StyleRanges
		StyleRange styleRange=null;
		String buf = event.lineText;

		if (colorInConsole){
			if (buf.indexOf ("WARNING:")!=-1){
				styleRange = new StyleRange();
				styleRange.start = event.lineOffset;
				styleRange.length = buf.length();
				styleRange.foreground = display.getSystemColor(SWT.COLOR_DARK_YELLOW);
			}else if (buf.indexOf ("SEVERE:")!=-1){
				String errorColorName = org.eclipse.jface.preference.JFacePreferences.ERROR_COLOR;
				Color errorColor = PlatformUI.getWorkbench().getThemeManager().getCurrentTheme().getColorRegistry().get(errorColorName);
				styleRange = new StyleRange();
				styleRange.start = event.lineOffset;
				styleRange.length = buf.length();
				styleRange.fontStyle = SWT.BOLD;
				styleRange.foreground = errorColor;
			}else if (buf.indexOf ("INFO:")!=-1){
			}else {
				styleRange = new StyleRange();
				styleRange.start = event.lineOffset;
				styleRange.length = buf.length();
				styleRange.fontStyle = SWT.ITALIC;
			}

			if (styleRange!=null){
				styles.add(styleRange);

				// Set the styles for the line
				event.styles = (StyleRange[]) styles.toArray(new StyleRange[0]);
			}
		}			
	}

	public void propertyChange(PropertyChangeEvent event) {
		if (event.getProperty().equals(PreferenceConstants.ENABLE_COLORS_CONSOLE)) {
			colorInConsole= store.getBoolean(PreferenceConstants.ENABLE_COLORS_CONSOLE);
		}
	}
}
