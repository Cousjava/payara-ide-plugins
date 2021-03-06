// <editor-fold defaultstate="collapsed" desc="CDDL+GPL License">
/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 * 
 * Copyright 1997-2009 Sun Microsystems, Inc. All rights reserved.
 * 
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common Development
 * and Distribution License("CDDL") (collectively, the "License").  You
 * may not use this file except in compliance with the License. You can obtain
 * a copy of the License at https://glassfish.dev.java.net/public/CDDL+GPL.html
 * or glassfish/bootstrap/legal/LICENSE.txt.  See the License for the specific
 * language governing permissions and limitations under the License.
 * 
 * When distributing the software, include this License Header Notice in each
 * file and include the License file at glassfish/bootstrap/legal/LICENSE.txt.
 * Sun designates this particular file as subject to the "Classpath" exception
 * as provided by Sun in the GPL Version 2 section of the License file that
 * accompanied this code.  If applicable, add the following below the License
 * Header, with the fields enclosed by brackets [] replaced by your own
 * identifying information: "Portions Copyrighted [year]
 * [name of copyright owner]"
 * 
 * Contributor(s):
 * 
 * If you wish your version of this file to be governed by only the CDDL or
 * only the GPL Version 2, indicate your decision by adding "[Contributor]
 * elects to include this software in this distribution under the [CDDL or GPL
 * Version 2] license."  If you don't indicate a single choice of license, a
 * recipient has the option to distribute your version of this file under
 * either the CDDL, the GPL Version 2 or to extend the choice of license to
 * its licensees as provided above.  However, if you add GPL Version 2 code
 * and therefore, elected the GPL Version 2 license, then the option applies
 * only if the new code is made subject to such option by the copyright
 * holder.
 */
// </editor-fold>

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
