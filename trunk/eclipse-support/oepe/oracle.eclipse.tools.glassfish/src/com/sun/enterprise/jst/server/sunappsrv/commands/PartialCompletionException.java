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
* @author vbk
*/
public class PartialCompletionException extends Exception {

   private String failedUpdates;

   PartialCompletionException(String itemsNotUpdated) {
       //throw new UnsupportedOperationException("Not yet implemented");
       failedUpdates = itemsNotUpdated;
   }

   @Override
   public String getMessage() {
       return "Failed to update: "+failedUpdates;
   }

}
