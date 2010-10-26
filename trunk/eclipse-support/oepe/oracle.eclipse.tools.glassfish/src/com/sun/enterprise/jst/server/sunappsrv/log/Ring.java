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
