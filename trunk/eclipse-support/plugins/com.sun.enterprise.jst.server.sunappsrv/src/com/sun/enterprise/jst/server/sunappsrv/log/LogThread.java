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


import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.Vector;

import com.sun.enterprise.jst.server.sunappsrv.SunAppSrvPlugin;



/**
 * author: ludovic champenois
 */
public class LogThread extends Thread{


	public static interface LogListener 	{
		public void logChanged(Ring list);
	}

	private File logFile = null;
	private int sleepTime = 500; // ms

	private int numLines = 100;
	private boolean isThreadActive = false;
	private Vector<LogListener> listeners = new Vector<LogListener>();
	private Ring ringBuffer = null;



	/**
	 * Create a LogThread.
	 */
	public LogThread(File file, int interval, int numLines)
	throws FileNotFoundException, IOException	{
		setDaemon(true);
		logFile = file;
		sleepTime = interval;
		this.numLines = numLines;
	}

	/**
	 * Halt of the reader thread.
	 */
	public void halt()	{
		isThreadActive = false;
		listeners = new Vector<LogListener>();
		interrupt();
	}



	public LogListener addListener(LogListener listener){
		listeners.add(listener);
		return listener;
	}
	private static final String stripNewline(String s) {
		int len = s.length();
		if(len > 0 && '\n' == s.charAt(len-1)) {
			s = s.substring(0, len-1);
		}
		return s;
	}

	public void run()	{
		BufferedReader reader = null;

		isThreadActive = true;
		ringBuffer = new Ring(numLines);
		V3LogFilter.Filter filter =  new V3LogFilter.LogFileFilter(new V3LogFilter().getLevelMap()) ;
		long size = 0;
		String line = null;
		while (isThreadActive) {
			boolean updated = false;
			boolean truncated = false;
			while (reader==null){
				if (logFile.exists()){
					try {
						reader = new BufferedReader(new FileReader(logFile));
					} catch (FileNotFoundException e) {
						SunAppSrvPlugin.logMessage("Error reading from log file", e);
					}
					
				}
				try {
					sleep(sleepTime);
				} catch (InterruptedException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
	
			//Test if file is truncated...
			truncated = false;
			if (logFile.length() < size) {
				truncated = true;
			}
			size = logFile.length();
			try {
				if (truncated) {
					ringBuffer.add("log file rotation...");
					updated = true;
					reader.close();
					reader = new BufferedReader(new FileReader(logFile));

					// complete read
					while ((line = reader.readLine()) != null) {
					}
				}
				else if (!logFile.exists()) {
					ringBuffer.add("log file deleted");
					updated = true;
					isThreadActive = false;
				}
				else {
					while( reader.ready()) {
						line = filter.process((char) reader.read());
						if (line!=null){
							line = stripNewline(line);
							if (!line.startsWith("FINE: UTIL6049")){//strip useless log entry (v3 bug I think)
								updated = true;
								ringBuffer.add(line);								
							}
						}
					}
				}
				if (updated) {
					notifyListeners();
				}
				sleep(sleepTime);
				ringBuffer.clear();
			}
			catch (IOException e) {
				SunAppSrvPlugin.logMessage("Error reading from log file", e);
			}
			catch (InterruptedException e) {
			
			}
		}
		try {
			reader.close();
		}
		catch (Exception e) {
			SunAppSrvPlugin.logMessage("Error closing log file reader", e);
		}
	}

	/**
	 * Notify the listeners
	 */
	protected synchronized void notifyListeners()	{
		for (LogListener l: listeners) {
			l.logChanged(ringBuffer);
		}
	}

	public String getFilename() {
		return logFile.getAbsolutePath();
	}

	public int getInterval()	{
		return sleepTime;
	}

	public void setInterval(int interval)	{
		sleepTime = interval;
	}
	public void clear()	{
		ringBuffer.clear();
		notifyListeners();
	}

	public int getNumLines()	{
		return numLines;
	}

	public void setNumLines(int n)	{
		if (n == 0){
			numLines = Integer.MAX_VALUE;
		}
		else {
			numLines = n;
		}
		ringBuffer.setMaxItems(numLines);
	}



}