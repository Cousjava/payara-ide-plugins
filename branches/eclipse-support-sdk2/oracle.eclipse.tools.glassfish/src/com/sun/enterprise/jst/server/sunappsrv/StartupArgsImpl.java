package com.sun.enterprise.jst.server.sunappsrv;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.glassfish.tools.ide.data.StartupArgs;

public class StartupArgsImpl implements StartupArgs {

	private String javaHome;
	private ArrayList<String> javaArgs;
	private ArrayList<String> glassfishArgs;
	private HashMap<String, String> envVars;
	
	public StartupArgsImpl() {
		
	}
	
	void setJavaHome(String javaHome) {
		this.javaHome = javaHome;
	}
	
	/**
	 * Adds java arguments contained in <code>javaArgsString</code>.
	 * The parameter is parsed - the delimeter is defined as one or more
	 * whitespace characters followed by <code>-</code>.
	 * 
	 * @param javaArgsString
	 */
	void addJavaArgs(String javaArgsString) {
		String[] args = javaArgsString.split("\\s+(?=-)");
		if (javaArgs == null)
			javaArgs = new ArrayList<String>(args.length);
		Collections.addAll(javaArgs, args);
	}
	
	/**
	 * Adds single argument for bootstrap jar.
	 * No processing is done.
	 * 
	 * @param glassfishArgsString
	 */
	void addGlassfishArgs(String glassfishArgsString) {
		if (glassfishArgs == null)
			glassfishArgs = new ArrayList<String>();
		glassfishArgs.add(glassfishArgsString);
	}

	@Override
	public List<String> getGlassfishArgs() {
		return glassfishArgs;
	}

	@Override
	public List<String> getJavaArgs() {
		return javaArgs;
	}

	@Override
	public Map<String, String> getEnvironmentVars() {
		return envVars;
	}

	@Override
	public String getJavaHome() {
		return javaHome;
	}

}
