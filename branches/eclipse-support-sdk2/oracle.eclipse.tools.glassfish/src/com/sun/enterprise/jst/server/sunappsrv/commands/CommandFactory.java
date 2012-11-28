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
package com.sun.enterprise.jst.server.sunappsrv.commands;

import com.sun.enterprise.jst.server.sunappsrv.commands.ServerCommand.SetPropertyCommand;

/**
 *
 * @author vkraemer
 */
public interface CommandFactory {

    public SetPropertyCommand getSetPropertyCommand(String name, String value);

}
