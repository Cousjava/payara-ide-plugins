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

package com.sun.enterprise.jst.server.sunappsrv.actions;


/**
 *
 * @author Ludovic.Champenois@Sun.COM
 */
public class HelpActions {
	//V3 help menu items:
	
	public static class QuickStartGuideV3 extends ShowURLAction {
		public QuickStartGuideV3() { super("http://docs.sun.com/doc/820-4836");}
	}	
	public static class AdminGuideV3 extends ShowURLAction {
		public AdminGuideV3() { super("http://docs.sun.com/doc/820-4495");}
	}
	public static class DevGuideV3 extends ShowURLAction {
		public DevGuideV3() { super("http://docs.sun.com/doc/820-4496");}
	}
	public static class AppDeployGuideV3 extends ShowURLAction {
		public AppDeployGuideV3() { super("http://docs.sun.com/doc/820-4502");}
	}
	
	//V2 help menu items:
	
	public static class QuickStartGuideV2 extends ShowURLAction {
		public QuickStartGuideV2() { super("http://docs.sun.com/app/docs/doc/819-3193");}
	}	
	public static class AdminGuideV2 extends ShowURLAction {
		public AdminGuideV2() { super("http://docs.sun.com/app/docs/doc/819-3671");}
	}
	public static class DevGuideV2 extends ShowURLAction {
		public DevGuideV2() { super("http://docs.sun.com/app/docs/doc/819-3672");}
	}
	public static class AppDeployGuideV2 extends ShowURLAction {
		public AppDeployGuideV2() { super("http://docs.sun.com/app/docs/doc/819-3673");}
	}
	public static class DeployPlanningGuideV2 extends ShowURLAction {
		public DeployPlanningGuideV2() { super("http://docs.sun.com/app/docs/doc/819-3680");}
	}	
	
}
