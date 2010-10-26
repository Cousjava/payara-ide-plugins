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
 * @author Nitya Doraisamy
 */
public class Utils {
    public static String simplifyModuleID(String candidateID) {
        String moduleID = null;

        moduleID = candidateID.replace(' ', '_');
        if (moduleID.startsWith("/")) { //$NON-NLS-1$
            moduleID = moduleID.substring(1);
        }

        // This moduleID will be later used to construct file path,
        // replace the illegal characters in file name
        //  \ / : * ? " < > | with _
        moduleID = moduleID.replace('\\', '_').replace('/', '_');
        moduleID = moduleID.replace(':', '_').replace('*', '_');
        moduleID = moduleID.replace('?', '_').replace('"', '_');
        moduleID = moduleID.replace('<', '_').replace('>', '_');
        moduleID = moduleID.replace('|', '_');

        // This moduleID will also be used to construct an ObjectName
        // to register the module, so replace additional special
        // characters , =  used in property parsing with -
        moduleID = moduleID.replace(',', '_').replace('=', '_');

        return moduleID;
    }
}
