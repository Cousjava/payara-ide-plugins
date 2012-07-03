package com.sun.enterprise.jst.server.sunappsrv.log;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

import org.eclipse.ui.console.MessageConsoleStream;
import org.glassfish.tools.ide.server.FetchLog;

public class LogReader implements Runnable {

	private FetchLog logFetcher;
	private volatile boolean shutDown = false;
	private MessageConsoleStream output;
	
	public LogReader(FetchLog logFetcher, MessageConsoleStream outputStream) {
		this.logFetcher = logFetcher;
		this.output = outputStream;
	}
	
	@Override
	public void run() {
		BufferedReader reader = new BufferedReader(new InputStreamReader(logFetcher.getInputStream()));
		try {
			while (true) {
				synchronized (this) {
					if (shutDown) {
						System.out.println("Shutdown true, break...");
						break;
					}
					while (reader.ready()) {
						System.out.println("reader ready, reading line...");
						String message = reader.readLine();
						output.println(message);
						output.flush();
					}
				}
				try {
					System.out.println("Sleep a little...");
					Thread.sleep(1000);
				} catch (InterruptedException e) {
					e.printStackTrace();
				}
			}
			System.out.println("end...");
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public synchronized void stop() {
		System.out.println("stop called, closing streams...");
		shutDown = true;
		logFetcher.close();
		try {
			output.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
}
