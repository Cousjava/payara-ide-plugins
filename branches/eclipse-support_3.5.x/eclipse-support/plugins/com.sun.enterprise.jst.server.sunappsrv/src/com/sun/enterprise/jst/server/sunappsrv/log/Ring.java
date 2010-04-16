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

package com.sun.enterprise.jst.server.sunappsrv.log;


import java.util.LinkedList;

/**
 * Ring buffer that cannot grow indefinitely when adding new elements
 * author: ludovic champenois
 */

public class Ring 
{

	private LinkedList list = new LinkedList();
	private int count = 0;
	private int maxItems = 0;

	/**
	 * Used to allocate string buffer.
	 */
	private static final int LINE_WIDTH = 80;

	public boolean isFull(){
		return (count >= maxItems);
	}

	public boolean isEmpty(){
		return (count == 0);
	}

	public Ring(int maxItems){
		this.maxItems = maxItems;
	}

	public synchronized void setMaxItems(int max){
		maxItems = max;
	}
	/**
	 * Return the contents of the ring with newlines for each line.
	 * 
	 * @return String
	 */
	public String getCompleteDocument(){
		StringBuffer sb = new StringBuffer(count * LINE_WIDTH);
		for (int i = 0; i < count; i++) {
			sb.append(list.get(i) + "\n");
		}
		return sb.toString();
	}
	
	public synchronized Object get(int i){
		return list.get(i);
	}

	public synchronized void clear(){
		list.clear();
		count = 0;
	}

	public synchronized boolean add(Object o){
		if (isFull()) {
			while (isFull()) {
				list.removeFirst();
				count--;
			}
			list.addLast(o);
			count++;
		}
		else {
			list.add(o);
			count++;
		}
		
		return true;
	}

	public int size(){
		return count;
	}




}
