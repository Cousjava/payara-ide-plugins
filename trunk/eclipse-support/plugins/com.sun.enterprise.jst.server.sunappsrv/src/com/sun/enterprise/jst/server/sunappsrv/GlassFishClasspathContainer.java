// <editor-fold defaultstate="collapsed" desc="CDDL Licence">
/*
 * The contents of this file are subject to the terms 
 * of the Common Development and Distribution License 
 * (the "License").  You may not use this file except 
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at 
 * glassfishplugins/www/license/CDDLv1.0.txt or 
 * https://glassfish.dev.java.net/public/CDDLv1.0.html. 
 * See the License for the specific language governing 
 * permissions and limitations under the License.
 * 
 * When distributing Covered Code, include this CDDL 
 * HEADER in each file and include the License file at 
 * glassfishplugins/www/license/CDDLv1.0.txt.  If applicable, 
 * add the following below this CDDL HEADER, with the 
 * fields enclosed by brackets "[]" replaced with your 
 * own identifying information: Portions Copyright [yyyy] 
 * [name of copyright owner]
 */
// </editor-fold>

package com.sun.enterprise.jst.server.sunappsrv;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.IClasspathContainer;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.JavaCore;

public class GlassFishClasspathContainer implements IClasspathContainer {
	private IClasspathEntry[] _entries;
	private IPath containerPath;
	public GlassFishClasspathContainer(IPath containerPath) {
		this.containerPath = containerPath;
		List<IClasspathEntry> entries = new ArrayList<IClasspathEntry>();
	    SunAppSrvPlugin.logMessage("in GlassFishClasspathContainer CTOR ");
		IClasspathEntry cpe = JavaCore.newLibraryEntry(
				new Path("/Users/ludo/glassfish-snapshot-v3-prelude-09_29_2008/glassfishv3-prelude/javadb/lib/derbyclient.jar"), 
				null, null);
		entries.add(cpe);

		IClasspathEntry[] cpes = new IClasspathEntry[entries.size()];
		_entries = (IClasspathEntry[])entries.toArray(cpes);
	}

	public IClasspathEntry[] getClasspathEntries() {      
		return _entries;       
	}

	public String getDescription() {
		return "GlassFish v3  Libraries";
	}

	public int getKind() {
		return K_APPLICATION;
	}

	public IPath getPath() {
		return containerPath;
	}
}