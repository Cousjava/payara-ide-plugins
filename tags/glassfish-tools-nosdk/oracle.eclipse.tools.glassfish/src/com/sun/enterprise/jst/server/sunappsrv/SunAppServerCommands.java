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

import org.eclipse.wst.server.core.IServerWorkingCopy;
import org.eclipse.core.commands.operations.AbstractOperation;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;

/* set all the different app server props using the generic do/undo framework
 *
 **/

public class SunAppServerCommands  extends AbstractOperation {
    
    protected String value;
    protected String oldValue;
    protected String commandName;
    
    SunAppServer sunServer;
    
    public SunAppServerCommands(IServerWorkingCopy server, String newValue, String commandName) {
        super( commandName);
        sunServer = SunAppServer.getSunAppServer(server);
        this.value = newValue;
        this.commandName = commandName;
    }
    
     

    
	public IStatus execute(IProgressMonitor monitor, IAdaptable adapt) {
        oldValue=  (String) sunServer.getProps().get(commandName);
        sunServer.getProps().put(commandName, value);
        return null;
	}



	public IStatus undo(IProgressMonitor monitor, IAdaptable adapt) {
        if (sunServer != null) {
            sunServer.getProps().put(commandName, oldValue);
        }
        return null;
	}
	
	public IStatus redo(IProgressMonitor monitor, IAdaptable adapt) {
		return execute(monitor, adapt);
	}    
    
    
}
