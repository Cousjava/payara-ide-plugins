/*
 * Copyright (c) 2011 Oracle and/or its affiliates. All rights reserved.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Oracle
 */

package com.sun.enterprise.jst.server.sunappsrv.serverview;

import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.jface.viewers.Viewer;

public class ServerViewViewerSorter extends ViewerSorter {
	
	@Override
	public int compare(Viewer viewer, Object e1, Object e2) {
		if( e1 instanceof ResourcesNode ){
			return -1;
		}
		if( e2 instanceof ResourcesNode ){
			return 1;
		}
		return super.compare(viewer, e1, e2);
	}
}
