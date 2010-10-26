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


package com.sun.enterprise.jst.server.sunappsrv.commands;

/**
 *
 * @author Peter Williams
 */
public class AppDesc {
    
    private final String name;
    private final String path;
    private final String contextRoot;
    
    public AppDesc(final String name, final String path, final String contextRoot) {
        this.name = name;
        this.path = path;
        this.contextRoot = contextRoot;
    }

    public String getName() {
        return name;
    }
    
    public String getPath() {
        return path;
    }
    
    public String getContextRoot() {
        return contextRoot;
    }
    
}
