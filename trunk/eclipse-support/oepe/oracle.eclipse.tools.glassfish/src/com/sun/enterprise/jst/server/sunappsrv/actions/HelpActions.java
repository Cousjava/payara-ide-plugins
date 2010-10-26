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


package com.sun.enterprise.jst.server.sunappsrv.actions;


/**
 *
 * @author Ludovic.Champenois@Sun.COM
 */
public class HelpActions {
	//V3 help menu item:
	
	public static class TopV3 extends ShowURLAction {
		public TopV3() { super("http://docs.sun.com/app/docs/coll/1343.9");}
	}	

	//V2.1 help menu item:
	
	public static class TopV21 extends ShowURLAction {
		public TopV21() { super("http://docs.sun.com/app/docs/coll/1343.4");}
	}	

	public static class JavaEE5JavaDoc extends ShowURLAction {
		public JavaEE5JavaDoc() { super("http://java.sun.com/javaee/5/docs/api/");}
	}
	public static class JavaEE6JavaDoc extends ShowURLAction {
		public JavaEE6JavaDoc() { super("http://javadoc.glassfish.org/javaee6/apidoc/");}
	}	
}
