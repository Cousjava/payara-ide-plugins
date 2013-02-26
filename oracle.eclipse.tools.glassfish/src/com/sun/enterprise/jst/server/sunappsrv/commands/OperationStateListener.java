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

import com.sun.enterprise.jst.server.sunappsrv.commands.GlassfishModule.OperationState;

/**
*
* @author Peter Williams
*/
public interface OperationStateListener {

   public void operationStateChanged(OperationState newState, String message);
   
}

