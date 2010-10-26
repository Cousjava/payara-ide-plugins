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


import java.util.Map;
import java.util.concurrent.Callable;
import java.util.concurrent.TimeUnit;

import com.sun.enterprise.jst.server.sunappsrv.commands.GlassfishModule.OperationState;




/**
 *
 * @author Peter Williams
 */
public abstract class BasicTask<V> implements Callable<V> {

    /** Wait duration (ms) between server status checks.
     */
    public static final int DELAY = 250;
    
    /** Maximum amount of time (in ms) to wait for server to start.
     */
    public static final int START_TIMEOUT = 120000;
    
    /** Maximum amount of time (in ms) to wait for server to stop.
     */
    public static final int STOP_TIMEOUT = 10000;

    /** Unit (ms) for the DELAY and START_TIMEOUT constants
     */
    public static final TimeUnit TIMEUNIT = TimeUnit.MILLISECONDS;


    protected final Map<String, String> ip;
    protected OperationStateListener [] stateListener;
    protected String instanceName;

    public BasicTask(Map<String, String> properties, OperationStateListener... stateListener) {
        this.ip = properties;
        this.stateListener = stateListener;
        this.instanceName = "LUDO";//ip.get(GlassfishModule.DISPLAY_NAME_ATTR);
    }
    
    
    /** Implementation of command to execute goes here.
     *
     */
    public abstract V call();

    protected final OperationState fireOperationStateChanged(OperationState stateType, String resName, String... args) {
        if(stateListener != null && stateListener.length > 0) {
            String msg = resName;//TODO // ludoNbBundle.getMessage(BasicTask.class, resName, args);
            for(int i = 0; i < stateListener.length; i++) {
                if(stateListener[i] != null) {
                    stateListener[i].operationStateChanged(stateType, msg);
                }
            }
        }
        return stateType;
    }
}
