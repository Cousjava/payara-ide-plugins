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
