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


package com.sun.enterprise.jst.server.sunappsrv;

import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.ui.AbstractLaunchConfigurationTabGroup;
import org.eclipse.debug.ui.CommonTab;
import org.eclipse.debug.ui.EnvironmentTab;
import org.eclipse.debug.ui.ILaunchConfigurationDialog;
import org.eclipse.debug.ui.ILaunchConfigurationTab;
import org.eclipse.debug.ui.sourcelookup.SourceLookupTab;
import org.eclipse.jdt.debug.ui.launchConfigurations.JavaArgumentsTab;
import org.eclipse.jdt.debug.ui.launchConfigurations.JavaClasspathTab;
import org.eclipse.jdt.debug.ui.launchConfigurations.JavaJRETab;
import org.eclipse.wst.server.ui.ServerLaunchConfigurationTab;

// the fact that we need to provide our own tabgroup is a workaround for issue 238693
public class SunAppServerLaunchConfigurationTabGroup extends AbstractLaunchConfigurationTabGroup {

	public void createTabs(ILaunchConfigurationDialog dialog, String mode) {
		ILaunchConfigurationTab[] tabs = new ILaunchConfigurationTab[7];
		// workaround for issue 238692
		// tabs[0] = new ServerLaunchConfigurationTab(new String[] {"com.sun.enterprise.jst.server"});
		tabs[0] = new ComboServerLaunchConfigurationTab(new String[] {"com.sun.enterprise.jst.server"});
		// end workaround
		tabs[0].setLaunchConfigurationDialog(dialog);
		tabs[1] = new JavaArgumentsTab();
		tabs[1].setLaunchConfigurationDialog(dialog);
		tabs[2] = new JavaClasspathTab();
		tabs[2].setLaunchConfigurationDialog(dialog);
		tabs[3] = new SourceLookupTab();
		tabs[3].setLaunchConfigurationDialog(dialog);
		tabs[4] = new EnvironmentTab();
		tabs[4].setLaunchConfigurationDialog(dialog);
		tabs[5] = new JavaJRETab();
		tabs[5].setLaunchConfigurationDialog(dialog);	 
		tabs[6] = new CommonTab();
		tabs[6].setLaunchConfigurationDialog(dialog);
		setTabs(tabs);
	}

	// workaround for issue 238692
	private static class ComboServerLaunchConfigurationTab extends ServerLaunchConfigurationTab {
		public ComboServerLaunchConfigurationTab(String[] ids) {
			super(ids);
		}
		
		@Override
		public void initializeFrom(ILaunchConfiguration configuration) {
			super.initializeFrom(configuration);
			handleServerSelection();
		}
	}
	// end workaround
}

