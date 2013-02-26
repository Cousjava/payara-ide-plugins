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

import java.io.IOException;
import java.util.Vector;
import java.util.concurrent.Future;

import com.sun.enterprise.jst.server.sunappsrv.SunAppServer;
import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;
import com.sun.enterprise.jst.server.sunappsrv.commands.CommandRunner;
import com.sun.enterprise.jst.server.sunappsrv.commands.Commands;
import com.sun.enterprise.jst.server.sunappsrv.commands.GlassfishModule.OperationState;

/**
 * author: ludovic champenois
 */
public class RemoteLogThread extends Thread {

	public static interface LogListener {
		public void logChanged(Ring list);
	}

	private SunAppServer server = null;
	private int sleepTime = 6000; // ms

	private int numLines = 100;
	private boolean isThreadActive = false;
	private Vector<LogListener> listeners = new Vector<LogListener>();
	private Ring ringBuffer = null;

	/**
	 * Create a LogThread.
	 */
	public RemoteLogThread(SunAppServer s, int interval, int numLines)
			throws IOException {
		setDaemon(true);
		server = s;
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

		isThreadActive = true;
		ringBuffer = new Ring(numLines);
		V3LogFilter.Filter filter = new V3LogFilter.LogFileFilter(
				new V3LogFilter().getLevelMap());
		String line = null;
		while (isThreadActive) {
			CommandRunner cmd = new CommandRunner(server);

			Commands.FetchLogData fld = new Commands.FetchLogData(null);
			Future<OperationState> result = cmd.execute(fld);
			OperationState state = null;
			try {
				state = result.get();

				if (state == OperationState.COMPLETED) {
					// now we start to actually put data into the pipe

					while (true) {
						try {
							Thread.sleep(sleepTime);
						} catch (InterruptedException e) {
							// nothing to do
						}
						String newQuery = fld.getNextQuery();
						fld = new Commands.FetchLogData(newQuery);
						result = cmd.execute(fld);
						state = result.get();
						if (state == OperationState.COMPLETED) {
							String s = fld.getLines();
							if (null != s && !"null\n".equals(s)) {
								for (int i = 0; i < s.length(); i++) {
									line = filter.process(s.charAt(i));
									if (line != null) {
										line = stripNewline(line);
										ringBuffer.add(line);
									}
								}

								notifyListeners();

							}
						} else {
							//SunAppSrvPlugin.logMessage(
							//		"error remotelogviewer state = " + state,
							//		null);

							break;
						}

					}
				} else {
					try {
						Thread.sleep(sleepTime);
					} catch (InterruptedException e) {
						// nothing to do
					}
				//	SunAppSrvPlugin.logMessage(
				//			"Error reading from log file, state = " + state,
				//			null);

				}
			} catch (Exception ex) {
				SunAppSrvPlugin.logMessage("Error reading from log file", ex);
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
