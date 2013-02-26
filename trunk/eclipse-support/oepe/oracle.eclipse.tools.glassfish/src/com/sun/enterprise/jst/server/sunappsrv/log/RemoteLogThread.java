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

package com.sun.enterprise.jst.server.sunappsrv.log;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Vector;

/**
 * author: ludovic champenois
 */
public class RemoteLogThread extends Thread {

	public static interface LogListener {
		public void logChanged(Ring list);
	}

	private InputStream log;
	private int sleepTime = 6000; // ms

	private int numLines = 100;
	private boolean isThreadActive = false;
	private Vector<LogListener> listeners = new Vector<LogListener>();
	private Ring ringBuffer = null;

	/**
	 * Create a LogThread.
	 */
	public RemoteLogThread(InputStream log, int interval, int numLines)
			throws IOException {
		setDaemon(true);
		this.log = log;
		sleepTime = interval;
		this.numLines = numLines;
	}

	/**
	 * Halt of the reader thread.
	 */
	public void halt() {
		isThreadActive = false;
		listeners = new Vector<LogListener>();
		interrupt();
	}

	public LogListener addListener(LogListener listener) {
		listeners.add(listener);
		return listener;
	}

	private static final String stripNewline(String s) {
		int len = s.length();
		if (len > 0 && '\n' == s.charAt(len - 1)) {
			s = s.substring(0, len - 1);
		}
		return s;
	}

	public void run() {
		BufferedReader reader = new BufferedReader(new InputStreamReader(log));
		// read from the input stream and put all the changes to the I/O window
        boolean updated = false;
		isThreadActive = true;
		ringBuffer = new Ring(numLines);
		V3LogFilter.Filter filter = new V3LogFilter.LogFileFilter(
				new V3LogFilter().getLevelMap());
		String line = null;
		while (isThreadActive) {
			 try {
				while (reader.ready()) {
					line = filter.process((char) reader.read());
					if (line!=null){
						line = stripNewline(line);
						updated = true;
						ringBuffer.add(line);								
					}
				 }
				
				if (updated)
					notifyListeners();
				
				sleep(sleepTime);
				ringBuffer.clear();
				
			} catch (IOException e) {
				e.printStackTrace();
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}

	}

	/**
	 * Notify the listeners
	 */
	protected synchronized void notifyListeners() {
		for (LogListener l : listeners) {
			l.logChanged(ringBuffer);
		}
	}

	public int getInterval() {
		return sleepTime;
	}

	public void setInterval(int interval) {
		sleepTime = interval;
	}

	public void clear() {
		ringBuffer.clear();
		notifyListeners();
	}

	public int getNumLines() {
		return numLines;
	}

	public void setNumLines(int n) {
		if (n == 0) {
			numLines = Integer.MAX_VALUE;
		} else {
			numLines = n;
		}
		ringBuffer.setMaxItems(numLines);
	}

}
