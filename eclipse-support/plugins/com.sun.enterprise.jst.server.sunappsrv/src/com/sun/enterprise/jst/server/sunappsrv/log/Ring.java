// <editor-fold defaultstate="collapsed" desc="CDDL Licence">
/*
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * glassfishplugins/www/license/CDDLv1.0.txt or
 * https://glassfish.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * glassfishplugins/www/license/CDDLv1.0.txt.  If applicable,
 * add the following below this CDDL HEADER, with the
 * fields enclosed by brackets "[]" replaced with your
 * own identifying information: Portions Copyright [yyyy]
 * [name of copyright owner]
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
