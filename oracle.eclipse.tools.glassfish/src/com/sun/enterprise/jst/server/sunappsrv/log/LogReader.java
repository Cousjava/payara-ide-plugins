package com.sun.enterprise.jst.server.sunappsrv.log;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

import org.eclipse.ui.console.MessageConsoleStream;
import org.glassfish.tools.ide.server.FetchLog;

public class LogReader implements Runnable {

	private static final int RESUME_LOOP_COUNT = 3;
	
	private FetchLog logFetcher;
	private volatile boolean shutDown = false;
	private int resumeLoopCount = 0;
	private MessageConsoleStream output;

	public LogReader(FetchLog logFetcher, MessageConsoleStream outputStream) {
		this.logFetcher = logFetcher;
		this.output = outputStream;
	}

	@Override
	public void run() {
		BufferedReader reader = new BufferedReader(new InputStreamReader(
				logFetcher.getInputStream()));
		V3LogFilter.Filter filter = new V3LogFilter.LogFileFilter(
				new V3LogFilter().getLevelMap());
		String line;
		try {
			while (true) {
				synchronized (this) {
					if (shutDown && !reader.ready() && (resumeLoopCount++ > RESUME_LOOP_COUNT)) {
						System.out.println("Shutdown true, break...");
						break;
					}
					while (reader.ready()) {
						line = filter.process((char) reader.read());
						if (line != null) {
							System.out.println("line ready..");
							//String message = reader.readLine();
							output.println(stripNewline(line));
							output.flush();
						}
					}
				}
				try {
					System.out.println("Sleep a little...");
					Thread.sleep(1000);
				} catch (InterruptedException e) {
					e.printStackTrace();
				}
			}
			System.out.println("end, closing streams...");
			logFetcher.close();
			try {
				output.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public synchronized void stop() {
		System.out.println("stop called...");
		shutDown = true;
	}
	
	private static final String stripNewline(String s) {
		int len = s.length();
		if(len > 0 && '\n' == s.charAt(len-1)) {
			s = s.substring(0, len-1);
		}
		return s;
	}
}
