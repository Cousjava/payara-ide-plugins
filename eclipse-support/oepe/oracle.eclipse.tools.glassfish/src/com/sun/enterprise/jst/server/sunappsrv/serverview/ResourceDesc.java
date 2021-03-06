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


package com.sun.enterprise.jst.server.sunappsrv.serverview;


/**
 *
 * @author Peter Williams
 */
public class ResourceDesc implements Comparable<ResourceDesc> {
    
    private final String name;
    private final String cmdSuffix;
    
    public ResourceDesc(final String name, final String cmdSuffix) {
        this.name = name;
        this.cmdSuffix = cmdSuffix;
    }

    public String getName() {
        return name;
    }

    public String getCommandSuffix() {
        return cmdSuffix;
    }

    public int compareTo(ResourceDesc o) {
        int result = name.compareTo(o.name);
        if(result == 0) {
            result = cmdSuffix.compareTo(o.cmdSuffix);
        }
        return result;
    }
    
}
