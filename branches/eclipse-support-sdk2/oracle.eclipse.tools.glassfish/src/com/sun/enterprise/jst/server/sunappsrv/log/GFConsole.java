package com.sun.enterprise.jst.server.sunappsrv.log;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.ui.console.MessageConsole;
import org.eclipse.ui.console.MessageConsoleStream;
import org.glassfish.tools.ide.server.FetchLog;

import com.sun.enterprise.jst.server.sunappsrv.handlers.AppServerContextAction;

public class GFConsole extends MessageConsole implements IGlassFishConsole {
	
	private List<LogReader> readers;

	public GFConsole(String name) {
		super(name, AppServerContextAction.getImageDescriptorFromlocalImage("icons/obj16/glassfishserver.gif"));
	}

	@Override
	public void startLogging(FetchLog... logFetchers) {
		stopLogging();
		readers = new ArrayList<LogReader>(logFetchers.length);
		MessageConsoleStream output = newMessageStream();
		for (FetchLog logFetcher : logFetchers) {
			LogReader reader = new LogReader(logFetcher, output);
			readers.add(reader);
			Thread t = new Thread(reader);
			t.start();
		}
	}

	@Override
	public void stopLogging() {
		if (readers == null)
			return;
		for (LogReader r : readers) {
			r.stop();
		}
		readers = null;
	}

	@Override
	protected void dispose() {
		super.dispose();
		stopLogging();
	}

	@Override
	public boolean isLogging() {
		return (readers != null) && (readers.size() > 0);
	}
	
}
