// <editor-fold defaultstate="collapsed" desc="CDDL+GPL License">
/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 * 
 * Copyright 1997-2009 Sun Microsystems, Inc. All rights reserved.
 * 
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common Development
 * and Distribution License("CDDL") (collectively, the "License").  You
 * may not use this file except in compliance with the License. You can obtain
 * a copy of the License at https://glassfish.dev.java.net/public/CDDL+GPL.html
 * or glassfish/bootstrap/legal/LICENSE.txt.  See the License for the specific
 * language governing permissions and limitations under the License.
 * 
 * When distributing the software, include this License Header Notice in each
 * file and include the License file at glassfish/bootstrap/legal/LICENSE.txt.
 * Sun designates this particular file as subject to the "Classpath" exception
 * as provided by Sun in the GPL Version 2 section of the License file that
 * accompanied this code.  If applicable, add the following below the License
 * Header, with the fields enclosed by brackets [] replaced by your own
 * identifying information: "Portions Copyrighted [year]
 * [name of copyright owner]"
 * 
 * Contributor(s):
 * 
 * If you wish your version of this file to be governed by only the CDDL or
 * only the GPL Version 2, indicate your decision by adding "[Contributor]
 * elects to include this software in this distribution under the [CDDL or GPL
 * Version 2] license."  If you don't indicate a single choice of license, a
 * recipient has the option to distribute your version of this file under
 * either the CDDL, the GPL Version 2 or to extend the choice of license to
 * its licensees as provided above.  However, if you add GPL Version 2 code
 * and therefore, elected the GPL Version 2 license, then the option applies
 * only if the new code is made subject to such option by the copyright
 * holder.
 */
// </editor-fold>

package com.sun.enterprise.jst.server.sunappsrv;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStreamReader;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Properties;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants;
import org.eclipse.jst.server.generic.core.internal.GenericServerBehaviour;
import org.eclipse.jst.server.generic.core.internal.GenericServerRuntime;
import org.eclipse.osgi.util.NLS;
import org.eclipse.wst.common.componentcore.internal.util.ComponentUtilities;
import org.eclipse.wst.server.core.IModule;
import org.eclipse.wst.server.core.IServer;
import org.eclipse.wst.server.core.model.IModuleResource;
import org.eclipse.wst.server.core.util.PublishUtil;

import com.sun.enterprise.jst.server.sunappsrv.commands.CommandRunner;
import com.sun.enterprise.jst.server.sunappsrv.commands.Commands;
import com.sun.enterprise.jst.server.sunappsrv.commands.GlassfishModule.OperationState;
import com.sun.enterprise.jst.server.sunappsrv.commands.Utils;
import com.sun.enterprise.jst.server.sunappsrv.sunresource.wizards.ResourceUtils;


/**
 *
 *
 */
@SuppressWarnings("restriction")
public class SunAppServerBehaviour extends GenericServerBehaviour {
	private static final String DEFAULT_DOMAIN_DIR_NAME = "domains"; //$NON-NLS-N$
	private static final String DEFAULT_DOMAIN_NAME = "domain1"; //$NON-NLS-N$
//not used yet	private GlassFishV2DeployFacility gfv2depl=null;//lazy initialized
	private boolean needARedeploy = true; //by default, will be calculated..

	/** Creates a new instance of SunAppServerBehaviour */
	public SunAppServerBehaviour() {
		      SunAppSrvPlugin.logMessage("in SunAppServerBehaviour CTOR ");

	}
	protected void initialize(IProgressMonitor monitor){
		super.initialize(monitor);
		SunAppSrvPlugin.logMessage("in SunAppServerBehaviour initialize" );
		SunAppServer  sunserver = getSunAppServer();
		try {
			if (sunserver.isRunning()){
				SunAppSrvPlugin.logMessage("in SunAppServerBehaviour initialize is running!!" );

				if (isV3()) {
					if (sunserver.getV3ServerStatus()==SunAppServer.ServerStatus.DOMAINDIR_MATCHING){
						SunAppSrvPlugin.logMessage("in SunAppServerBehaviour initialize V3 DOMAINDIR_MATCHING"  );
						setStartedState();

                                                return;
					} else {
						SunAppSrvPlugin.logMessage("***in SunAppServerBehaviour initialize V3 DOMAINDIR_NOT_MATCHING, will reset to stop shortly"  );
					}
				} else { 
					if (sunserver.getV2ServerStatus()==SunAppServer.ServerStatus.DOMAINDIR_MATCHING){
						SunAppSrvPlugin.logMessage("in SunAppServerBehaviour initialize V2 DOMAINDIR_MATCHING"  );
						setStartedState();
						return;
					}
				}
			}
			SunAppSrvPlugin.logMessage("in SunAppServerBehaviour initialize STOP by Default..."  );
			setServerState(IServer.STATE_STOPPED);
			resetStatus(IServer.STATE_STOPPED);	
			} catch (CoreException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}

	/* get the correct adapter for  the GlassFish server
	 * 
	 */
	public  SunAppServer getSunAppServer(){
		//	return (SunAppServer)getServer().getAdapter(SunAppServer.class);
		SunAppServer  sunserver = (SunAppServer) getServer().getAdapter(SunAppServer.class);
		if (sunserver == null) {
			sunserver = (SunAppServer) getServer().loadAdapter(SunAppServer.class, new NullProgressMonitor());
		}
		return sunserver;
        }
        
	/* stub this public method from parent since we are not using <start> class definition in the serverdef.
	 * This was called and created a NPE for our special case.
	 * Note that this workaround was here before (added in plugin version 1.0.3) but
	 * caused issue 33, so it was removed.  It is now causing the NPE again sometimes, 
	 * and testing issue 33 is successful, so it is being put back.  We need to keep an
	 * eye on it, though.
	 * Not needed
	 *
       (non-Javadoc)
	 * @see org.eclipse.jst.server.generic.core.internal.GenericServerBehaviour#setupLaunchConfiguration(org.eclipse.debug.core.ILaunchConfigurationWorkingCopy, org.eclipse.core.runtime.IProgressMonitor)
	 */
	public void setupLaunchConfiguration(ILaunchConfigurationWorkingCopy workingCopy, IProgressMonitor monitor) throws CoreException {
		//do nothing...to prevent a NPE in wtp2.0.2 winter edition
		//SunAppSrvPlugin.logMessage("in SunAppServerBehaviour setupLaunchConfiguration doing nothing");

	}

	protected void setupLaunch(ILaunch launch, String launchMode, IProgressMonitor monitor) throws CoreException {
		int state = getServer().getServerState();
		SunAppSrvPlugin.logMessage("in SunAppServerBehaviour setupLaunch state=" +state);

		SunAppServer  sunserver = getSunAppServer();
		if (sunserver.isRunning()){
			SunAppSrvPlugin.logMessage("in SunAppServerBehaviour CTOR after sunserver it is running!!!");
			setMode(launchMode); //ILaunchManager.RUN_MODE);
			setStartedState(launchMode);


		} 
		state = getServer().getServerState();
		if (state!=IServer.STATE_STARTED){
			try {
				super.setupLaunch(launch, launchMode, monitor);
			} catch (CoreException ce) {


				SunAppSrvPlugin.logMessage("in SunAppServerBehaviour setupLaunch  ALREADY STARTED!!!!!!");
				setMode(launchMode); //ILaunchManager.RUN_MODE);
				setStartedState(launchMode);
				return;
			}
		}
		resetStatus(state);
	}

        public void setStartedState() {
            setStartedState(null);
    }

    public void setStartedState(String mode) {
        if (null != mode) {
            // setServerMode is a Galileo thing
            Method setServerMode;
            try {
                setServerMode = this.getClass().getMethod("setServerMode", String.class); //$NON-NLS-1$
                if (setServerMode != null) {
                    setServerMode.invoke(this, mode);
                }
            } catch (SecurityException e) {
                // TODO Auto-generated catch block
                SunAppSrvPlugin.logMessage("in SunAppServerBehaviour setStartedState : security exception"); //$NON-NLS-1$
            } catch (NoSuchMethodException e) {
                SunAppSrvPlugin.logMessage("in SunAppServerBehaviour setStartedState : no such method  exception"); //$NON-NLS-1$
            } catch (IllegalArgumentException e) {
                SunAppSrvPlugin.logMessage("in SunAppServerBehaviour setStartedState : illegal argument exception"); //$NON-NLS-1$
            } catch (IllegalAccessException e) {
                SunAppSrvPlugin.logMessage("in SunAppServerBehaviour setStartedState : illegal access exception"); //$NON-NLS-1$
            } catch (InvocationTargetException e) {
                SunAppSrvPlugin.logMessage("in SunAppServerBehaviour setStartedState : invocation target exception"); //$NON-NLS-1$
            }
        }
        setServerState(IServer.STATE_STARTED);
        resetStatus(IServer.STATE_STARTED);
    }

	/**
	 * Publish a single module to the server.
	 * <p>
	 * This method is called by the server core framework,
	 * in response to a call to <code>IServer.publish()</code>.
	 * Clients should never call this method directly.
	 * </p>
	 * <p>
	 * If the deltaKind is IServer.REMOVED, the module may have been completely
	 * deleted and does not exist anymore. In this case, a dummy module (with the
	 * correct id) will be passed to this method.
	 * </p>
	 * <p>
	 * It is recommended that clients implementing this method be responsible for
	 * setting the module state.
	 * </p>
	 * 
	 * @param kind one of the IServer.PUBLISH_XX constants. Valid values are:
	 *    <ul>
	 *    <li><code>PUBLISH_FULL</code>- indicates a full publish.</li>
	 *    <li><code>PUBLISH_INCREMENTAL</code>- indicates a incremental publish.
	 *    <li><code>PUBLISH_AUTO</code>- indicates an automatic incremental publish.</li>
	 *    <li><code>PUBLISH_CLEAN</code>- indicates a clean request. Clean throws
	 *      out all state and cleans up the module on the server before doing a
	 *      full publish.
	 *    </ul>
	 * @param module the module to publish
	 * @param deltaKind one of the IServer publish change constants. Valid values are:
	 *    <ul>
	 *    <li><code>ADDED</code>- indicates the module has just been added to the server
	 *      and this is the first publish.
	 *    <li><code>NO_CHANGE</code>- indicates that nothing has changed in the module
	 *      since the last publish.</li>
	 *    <li><code>CHANGED</code>- indicates that the module has been changed since
	 *      the last publish. Call <code>getPublishedResourceDelta()</code> for
	 *      details of the change.
	 *    <li><code>REMOVED</code>- indicates the module has been removed and should be
	 *      removed/cleaned up from the server.
	 *    </ul>
	 * @param monitor a progress monitor, or <code>null</code> if progress
	 *    reporting and cancellation are not desired
	 * @throws CoreException if there is a problem publishing the module
	 */	
	public void publishModule(int kind, int deltaKind, IModule[] module,
			IProgressMonitor monitor) throws CoreException {

		
		//first, test if the server is still existing 
		File serverloc=new File(getSunApplicationServerInstallationDirectory());
        if (!serverloc.exists()){
			SunAppSrvPlugin.logMessage(NLS.bind(Messages.serverDirectoryGone,serverloc.getAbsolutePath()),null);
			return;
       	
        }		
		
		needARedeploy = true; //by default
		
		//if (kind==IServer.PUBLISH_AUTO)
		SunAppSrvPlugin.logMessage("publishModule kind= " +kind+"  deltaKind=" +deltaKind+" "+module.length+module[0].getName(),null);
			
		if (isV3()){ //for V3, try to optimize the redeployment process by not using ANT, but V3 commands
			long t= System.currentTimeMillis();
			publishModuleForGlassFishV3( kind,  deltaKind, module,  monitor);
			SunAppSrvPlugin.logMessage("done publishModule in " +(System.currentTimeMillis()-t)+" ms");
		}
		else {
		/*not used yet	if (getSunAppServer().getFastDeploy().equals("true")){

				long t= System.currentTimeMillis();
				publishModuleForGlassFishV3( kind,  deltaKind, module,  monitor);
				SunAppSrvPlugin.logMessage("done V2.x publishModule in " +(System.currentTimeMillis()-t)+" ms");
			}
			else {*/
				super.publishModule(kind, deltaKind, module, monitor);
				
			//}
		}


	}
	
	protected synchronized void setServerStarted() {
		setServerState(IServer.STATE_STARTED);
	}

	/*
	 * If the server state is unknown, reset the status to OK
	 */
	private void resetStatus(int state) {
		if (state == IServer.STATE_UNKNOWN) {
			setServerStatus(null);
		}
	}


	protected void terminate() {
		SunAppSrvPlugin.logMessage("in SunAppServerBehaviour terminate");
		int state = getServer().getServerState();
		if (state == IServer.STATE_STOPPED)
			return;


		IProcess currentProcess = process;

		process = null;

		shutdown(state);

		try {
			if (currentProcess != null && !currentProcess.isTerminated()) {
				currentProcess.terminate();
				currentProcess = null;
			}
		} catch (Exception e) {
			SunAppSrvPlugin.logMessage("Error "+ e);
		}
	}

    public GenericServerRuntime getRuntimeDelegate() {
        return (GenericServerRuntime)getServer().getRuntime().loadAdapter(GenericServerRuntime.class,null);
     }
	public String getSunApplicationServerInstallationDirectory(){
		String path= (String)getRuntimeDelegate().getServerInstanceProperties().get(SunAppServer.ROOTDIR);
		/* SunAppServer  sunserver = (SunAppServer) getServer().getAdapter(SunAppServer.class);
        String d = sunserver.getRootDir();
              SunAppSrvPlugin.logMessage("getSunApplicationServerInstallationDirectory is :"+d);
        return d;*/
		return path;
	}
	/* return true if the current server is a V3 server
	 *  based on the modules dir existence for now
	 */
	public boolean isV3(){
		String loc=getSunApplicationServerInstallationDirectory();
		return new File(loc+"/modules").exists();
	}
	public String getDomainName(){
		SunAppServer  sunserver = getSunAppServer();
		String d = sunserver.getdomainName();
		if (isEmpty(d)) {
			d = DEFAULT_DOMAIN_NAME;
		}
		return d;
	}
	public String getDomainDir(){
		SunAppServer  sunserver = getSunAppServer();
		String d = sunserver.getDomainDir();
		if (isEmpty(d)) {
			d = getDefaultDomainDir();
		}
		return d;
	}
	private String getDefaultDomainDir() {
		return getSunApplicationServerInstallationDirectory() + File.separatorChar + 
		DEFAULT_DOMAIN_DIR_NAME;
	}

    public String getSampleDatabaseDir() {
		return getSunAppServer().getSampleDatabaseDir();
    }

	private boolean isEmpty(String testString) {
		return ((testString == null) || (testString.trim().length() == 0));
	}
	public String getSunApplicationServerAdminPort(){
		//String port= (String)getRuntimeDelegate().getServerInstanceProperties().get(SunAppServer.ADMINSERVERPORT);
		SunAppServer  sunserver = getSunAppServer();
		//	SunAppSrvPlugin.logMessage("sunappserver.adminserverportnumber we are looking for this prop value:"+sunserver.getAdminServerPort());
		return sunserver.getAdminServerPort();
	}    
	public String getServerPort(){
		SunAppServer  sunserver = getSunAppServer();
		//	SunAppSrvPlugin.logMessage("sunappserver.adminserverportnumber we are looking for this prop value:"+sunserver.getServerPort());
		return sunserver.getServerPort();
	}

	
	public void restart(final String launchMode) throws CoreException {
		SunAppSrvPlugin.logMessage("in SunAppServerBehaviour restart");
		stop(true);
		Thread thread = new Thread("Synchronous server start") {
			public void run() {
				try {
					//SunAppSrvPlugin.logMessage("in !!!!!!!SunAppServerBehaviour restart");
					getServer().start(launchMode, new NullProgressMonitor());
					SunAppSrvPlugin.logMessage("in SunAppServerBehaviour restart done");
					setServerState(IServer.STATE_STARTED);

				} catch (Exception e) {
					SunAppSrvPlugin.logMessage("in SunAppServerBehaviour restart",e);
				}
			}
		};
		thread.setDaemon(true);
		thread.start();

	    }	
	
	
	public void stop(boolean force) {
		SunAppSrvPlugin.logMessage("in SunAppServerBehaviour stop");

		resetStatus(getServer().getServerState());
		/*int state = getServer().getServerState();
		      if (state == IServer.STATE_STOPPED){
            SunAppSrvPlugin.logMessage("in SunAppServerBehaviour ALREADY STOPPED...Wierd");
                      return;
        }*/

		///shutdown(state);
		if (isV3()){
			stopV3();
		}else {
			stopSunServer();
			
		}
		setServerState(IServer.STATE_STOPPED);
	}
	
	
	 /**
     * 
     * @stop GlassFish v3 or v3 prelude via http command
     */
    public void stopV3() {

           try {
        	   CommandRunner mgr = new CommandRunner(getSunAppServer());
                Future<OperationState> result = mgr.execute(Commands.STOP);
                if(result.get(30, TimeUnit.SECONDS) == OperationState.COMPLETED) {
                } else  {
                	SunAppSrvPlugin.logMessage("Cannot stop in 30 seconds" );	//$NON-NLS-1$
                	return ;

                }
            } catch(Exception ex) {
                SunAppSrvPlugin.logMessage("stop-domain v3 is failing=",ex );	//$NON-NLS-1$
              	return ;
            }
      

    }	
	
	private String getScriptExtension(){
		String ret="";
		if (File.separator.equals("\\")) {
			ret= ".bat"; //NOI18N
		}
		return ret;
	}
	public String[] getStopCommand(){
		String asadminCmd =  getSunApplicationServerInstallationDirectory()+"/bin/asadmin"+ getScriptExtension();

		String stop[] = { asadminCmd,
				"stop-domain",
				"--domaindir",
				getDomainDir(), 
				getDomainName()
		};	
		return stop;
	}
	private  void stopSunServer() {
		// set arguments to be passed to Runtime.exec

		String arr[] = getStopCommand();
		// stop the SJSAS using Runtime.exec
		asyncExec(arr);
        int timeout = getServer().getStopTimeout();
		for (int i=0;i<timeout;i++){
			try {
				Thread.sleep(1000);
				SunAppSrvPlugin.logMessage("in SunAppServerBehaviour stopping...");

				SunAppServer  sunserver = getSunAppServer();
				if (sunserver.isRunning()==false){
					SunAppSrvPlugin.logMessage("in SunAppServerBehaviour really stopped");
					Thread.sleep(2000);//need an extra time to flush

					return;
				}
			} catch (Exception ex) {
				
				SunAppSrvPlugin.logMessage("Error in SunAppServerBehaviour being stopped",ex);
				
			}
		}
	}

	/*public void startPingingThread(){
		// ping server to check for startup
		try {
			setServerState(IServer.STATE_STARTING);
			String url = "http://"+getServer().getHost() +":" + getServerPort(); //8080 and not 4848 so we do not start admin gui
			SunAppSrvPlugin.logMessage("in SunAppServerBehaviour start Pinging " +url);

			ping = new PingThread(getServer(), url, this);
		} catch (Exception e) {
			SunAppSrvPlugin.logMessage( "Can't ping for server startup."); 
		}

	}
	*/
	private void asyncExec(final String[] arr) {
		new Thread(new Runnable() {
			public void run() {
				try {
					BufferedReader input = new BufferedReader(new InputStreamReader(Runtime.getRuntime().exec(arr).getInputStream()));
					String line = null;
					while ((line = input.readLine()) != null) SunAppSrvPlugin.logMessage(">>> " + line);
					input.close();
				} catch (Exception ex) {
					SunAppSrvPlugin.logMessage("Error starting/stopping integrated SJSAS:\n" + ex);
				}
			}
		}).start();
	}

	public String getDomainDirWithDomainName() {
		return getDomainDir().trim() + File.separatorChar + getDomainName();
	}

	protected String getConfigTypeID() {
		return SunAppSrvPlugin.SUNPLUGIN_ID + ".SunAppServerLaunchConfigurationType";
	}

	/**
	 * Returns the String name of the stop launch configuration.
	 * @return
	 */
	protected String getStopLaunchName() {
		return "ExternalLaunchStopper";
	}


    protected void setProcess(IProcess newProcess) {
        SunAppSrvPlugin.logMessage("in SunAppServerBehaviour setProcess");
        super.setProcess(newProcess);
    }


	/*
	 * Publishes for Web apps only in V3 prelude
	 */
	protected void publishModuleForGlassFishV3(int kind, int deltaKind, IModule[] module, IProgressMonitor monitor) throws CoreException {

		if (module.length > 1){// only publish root modules, i.e web modules
			setModulePublishState(module, IServer.PUBLISH_STATE_NONE);
			return ;
		}
		IPath path = getTempDirectory().append("publish.txt");
		//SunAppSrvPlugin.logMessage("in PATH" +path +"module length=======" +module.length);

		FileInputStream fis= null;
		Properties prop = new Properties();
		try {
			fis = new FileInputStream(path.toFile());
			prop.load(fis);
		} catch (Exception e) {
		} finally {
			try {
				fis.close();
			} catch (Exception ex) {
			}
		}

		publishDeployedDirectory(deltaKind,prop, module, monitor);

		setModulePublishState(module, IServer.PUBLISH_STATE_NONE);
		try {
			prop.store(new FileOutputStream(path.toFile()), "GlassFish v3 Prelude");
		} catch (Exception e) {
			SunAppSrvPlugin.logMessage(" error in PUBLISH_STATE_NONE",e );
		}		

	}
/*	private void publishJarFile(int kind, int deltaKind,Properties p,  IModule[] module, IProgressMonitor monitor) throws CoreException {
		//first try to see if we need to undeploy:
		
		if (deltaKind == ServerBehaviourDelegate.REMOVED ) {
			try {
				String publishPath = (String) p.get(module[1].getId());
				new File(publishPath).delete();
			} catch (Exception e) {
				throw new CoreException(new Status(IStatus.WARNING, SunAppSrvPlugin.SUNPLUGIN_ID, 0, "Could not remove module", e));
			}
		} else {
			IPath path = new Path (getDomainDirWithDomainName()+"/eclipseApps/"+module[0].getName());
			path = path.append("WEB-INF").append("lib");
			IPath jarPath = path.append(module[1].getName() + ".jar");
			if (!path.toFile().exists()) {
				path.toFile().mkdirs();
			} else {
				// force a delta publish
				if (jarPath.toFile().exists() && kind != IServer.PUBLISH_CLEAN && kind != IServer.PUBLISH_FULL) {

					IModuleResourceDelta[] delta = getPublishedResourceDelta(module);
					if (delta == null || delta.length == 0)
						return;
				}
			}

			IModuleResource[] mr = getResources(module);
			IStatus[] stat = PublishUtil.publishZip(mr, jarPath, monitor);
			SunAppSrvPlugin.logMessage("PublishUtil.publishZip called "+jarPath);
			analyseReturnedStatus(stat);
			p.put(module[1].getId(), jarPath.toOSString());
		}
	}
*/
	
/*not used yet	private GlassFishV2DeployFacility getV2DeploymentFacility(){
		if (gfv2depl==null){
			SunAppServer ss= getSunAppServer();
			SunAppSrvPlugin.logMessage("SunAppServer ss=" +ss);
			SunAppSrvPlugin.logMessage("SunAppServer ssgetAdminServerPort=" +ss.getAdminServerPort());
			SunAppSrvPlugin.logMessage("SunAppServer ssgetAdminName=" +ss.getAdminName());
			SunAppSrvPlugin.logMessage("SunAppServer ssgetAdminPassword=" +ss.getAdminPassword());
			gfv2depl = new GlassFishV2DeployFacility("localhost",Integer.parseInt(ss.getAdminServerPort()),ss.getAdminName(),ss.getAdminPassword(),false);
		}
		return gfv2depl;
	}
	
	*/
	private void publishDeployedDirectory(int deltaKind, Properties p,IModule module[], IProgressMonitor monitor) throws CoreException {
		if (deltaKind == REMOVED ) {
			String publishPath = (String) p.get(module[0].getId());
			SunAppSrvPlugin.logMessage("REMOVED in publishPath" +publishPath);
			if (isV3()){
				String name = Utils.simplifyModuleID(module[0].getName());
				Commands.UndeployCommand command = new Commands.UndeployCommand(name);
				try {
					Future<OperationState> result = getSunAppServer().execute(command);
					//wait 120 seconds max
					if(result.get(120, TimeUnit.SECONDS) == OperationState.RUNNING) {
						throw new CoreException(new Status(IStatus.ERROR, SunAppSrvPlugin.SUNPLUGIN_ID, 0,
								"cannot UnDeploy in less than 120 sec "+name, null));
					}
					if(result.get(120, TimeUnit.SECONDS) == OperationState.FAILED) {
						throw new CoreException(new Status(IStatus.ERROR, SunAppSrvPlugin.SUNPLUGIN_ID, 0,
								"Error during undeploy of module "+name+": "+command.message, null));
					}
				} catch(Exception ex) {
					SunAppSrvPlugin.logMessage("Undeploy is failing=",ex );
					throw new CoreException(new Status(IStatus.ERROR, SunAppSrvPlugin.SUNPLUGIN_ID, 0,
							"cannot UnDeploy "+name, ex));
				}
			}else { //v2
		/*not used yet		boolean ret= getV2DeploymentFacility().unDeploy(module[0].getName());
				SunAppSrvPlugin.logMessage("gfv2depl.unDeploy"+ret );
				if (ret==false){
					throw new CoreException(new Status(IStatus.WARNING, SunAppSrvPlugin.SUNPLUGIN_ID, 0,"cannot undeploy "+module[0].getName(), null));
					
				}
				*/
			}
			if (publishPath != null) {
				try {
					File pub = new File(publishPath);
					if (pub.exists()) {
						SunAppSrvPlugin.logMessage("PublishUtil.deleteDirectory called");
						IStatus[] stat = PublishUtil.deleteDirectory(pub, monitor);
						analyseReturnedStatus(stat);
					}
				} catch (Exception e) {
					throw new CoreException(new Status(IStatus.WARNING, SunAppSrvPlugin.SUNPLUGIN_ID, 0,"cannot remove "+module[0].getName(), e));
				}
			}
		}
		else {
			IPath path = new Path (getDomainDirWithDomainName()+"/eclipseApps/"+module[0].getName());
			IModuleResource[] moduleResource = getResources(module);
	//		SunAppSrvPlugin.logMessage("IModuleResource len="+moduleResource.length);
	//		for (int j=0;j<moduleResource.length ;j++)
	//			SunAppSrvPlugin.logMessage("IModuleResource n="+moduleResource[j].getName()+"-----"+moduleResource[j].getModuleRelativePath());

			IStatus[] stat = PublishUtil.publishSmart(moduleResource, path, monitor);
			SunAppSrvPlugin.logMessage("PublishUtil.publishSmart called");
			analyseReturnedStatus(stat);
			
			String contextRoot=null;
			AssembleModules assembler = new AssembleModules(module[0], path,getSunAppServer());
			//either ear or web.
			if(AssembleModules.isModuleType(module[0], "jst.web")) {
				SunAppSrvPlugin.logMessage("is WEB");
						assembler.assembleWebModule(monitor);
						
						needARedeploy = assembler.needsARedeployment();
						 String projectContextRoot = ComponentUtilities.getServerContextRoot(module[0].getProject());
						 contextRoot = (((projectContextRoot != null) && (projectContextRoot.length() > 0)) ? 
									projectContextRoot : module[0].getName());			
			}
			if(AssembleModules.isModuleType(module[0], "jst.ear")) {
				SunAppSrvPlugin.logMessage("is EAR");
				assembler.assembleDirDeployedEARModule(monitor);
				needARedeploy = assembler.needsARedeployment();
				
			}
			String spath =""+ path;
			///BUG NEED ALSO to test if it has been deployed once...isDeployed()
			if (needARedeploy ){
				String name = Utils.simplifyModuleID(module[0].getName());
	
				Boolean preserveSessions=getSunAppServer().getKeepSessions().equals("true");
				if (isV3()){
					Commands.DeployCommand command = new Commands.DeployCommand(spath,name,contextRoot,preserveSessions , getSunAppServer().isV3Prelude());
					try {
						Future<OperationState> result = getSunAppServer().execute(command);
						OperationState res=result.get(120, TimeUnit.SECONDS);
						SunAppSrvPlugin.logMessage("res="+res);
						if( res== OperationState.RUNNING) {
							throw new CoreException(new Status(IStatus.ERROR, SunAppSrvPlugin.SUNPLUGIN_ID, 0,
									"Timeout after 120s when trying to deploy "+name+". Please try again ", null));
						}
						if( res== OperationState.FAILED) {
							throw new CoreException(new Status(IStatus.ERROR, SunAppSrvPlugin.SUNPLUGIN_ID, 0,
									"Deployment Error for module: "+name+": "+command.message, null));
						}	
					} catch(Exception ex) {
						SunAppSrvPlugin.logMessage("deploy is failing=",ex );
						throw new CoreException(new Status(IStatus.ERROR, SunAppSrvPlugin.SUNPLUGIN_ID, 0,
								"cannot Deploy "+name, ex));	            
					}
				}else{
				/*not used yet	
					boolean ret= getV2DeploymentFacility().directoryDeploy(new File(spath),name,contextRoot);
					SunAppSrvPlugin.logMessage("gfv2depl.incrementalDeploy"+ret );
					if (ret==false){
						throw new CoreException(new Status(IStatus.WARNING, SunAppSrvPlugin.SUNPLUGIN_ID, 0,"cannot deploy "+name,null));
					}
					*/
					
				}
			}else {
				SunAppSrvPlugin.logMessage("optimal: NO NEED TO TO A REDEPLOYMENT, !!!");
				
			}
			
			//Get correct location for sun-resources.xml
			IProject project = module[0].getProject();
			String location = ResourceUtils.getRuntimeResourceLocation(project);
			if(location != null){
				if(location.trim().length() > 0){
					location = location + File.separatorChar + ResourceUtils.RESOURCE_FILE_NAME;
				}else {
					location = ResourceUtils.RESOURCE_FILE_NAME;
				}
			}
			File sunResource = new File(spath,location);
			if (sunResource.exists()){
				Commands.AddResourcesCommand register = new Commands.AddResourcesCommand(sunResource.getAbsolutePath());
				try {
					Future<OperationState> result = getSunAppServer().execute(register);
					if(result.get(120, TimeUnit.SECONDS) == OperationState.RUNNING) {
						throw new CoreException(new Status(IStatus.ERROR, SunAppSrvPlugin.SUNPLUGIN_ID, 0,
								"Timeout after 120s when trying to deploy the sun-resources.xml for "+module[0].getName()+". Please try again ", null));
					}
					if(result.get(120, TimeUnit.SECONDS) == OperationState.FAILED) {
						throw new CoreException(new Status(IStatus.ERROR, SunAppSrvPlugin.SUNPLUGIN_ID, 0,
								"Error when trying to deploy the sun-resources.xml for "+module[0].getName()+": "+register.message, null));
					}
				} catch(Exception ex) {
					SunAppSrvPlugin.logMessage("deploy of sun-resources is failing ",ex );
					throw new CoreException(new Status(IStatus.ERROR, SunAppSrvPlugin.SUNPLUGIN_ID, 0,
							"cannot register sun-resource.xml for "+module[0].getName(), ex));	            
				}				
			}			
			p.put(module[0].getId(), path.toOSString());
		}
	}



	protected  void analyseReturnedStatus(IStatus[] status) throws CoreException {

		if (status == null || status.length == 0){
			return;
		}
	/*	if (status.length == 1) {
			throw new CoreException(status[0]);
		}
		String message = "GlassFish: Error Deploying";
		MultiStatus ms = new MultiStatus(SunAppSrvPlugin.SUNPLUGIN_ID, 0, status, message, null);
		throw new CoreException(ms);*/
		for (IStatus s: status){
			SunAppSrvPlugin.logMessage("analyseReturnedStatus: "+s.getMessage() );
			
		}
	}




}

