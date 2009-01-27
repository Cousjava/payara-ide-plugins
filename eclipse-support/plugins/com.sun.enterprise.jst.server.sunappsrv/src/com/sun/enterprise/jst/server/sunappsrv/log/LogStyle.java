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
