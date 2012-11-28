package com.sun.enterprise.jst.server.sunappsrv.serverview;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.glassfish.tools.ide.admin.Command;
import org.glassfish.tools.ide.admin.CommandListComponents;
import org.glassfish.tools.ide.admin.CommandListResources;
import org.glassfish.tools.ide.admin.CommandListWebServices;
import org.glassfish.tools.ide.admin.ResultList;
import org.glassfish.tools.ide.admin.ResultMap;
import org.glassfish.tools.ide.admin.ServerAdmin;
import org.glassfish.tools.ide.admin.TaskState;
import org.glassfish.tools.ide.data.GlassFishServer;
import org.glassfish.tools.ide.data.IdeContext;

import com.sun.enterprise.jst.server.sunappsrv.commands.AppDesc;
import com.sun.enterprise.jst.server.sunappsrv.commands.ResourceDesc;
import com.sun.enterprise.jst.server.sunappsrv.commands.WSDesc;
import com.sun.enterprise.jst.server.sunappsrv.sunresource.wizards.PartialCompletionException;
import com.sun.enterprise.jst.server.sunappsrv.sunresource.wizards.ResourceUtils;

public class NodesUtils {

	public static List<ResourceDesc> getResources(GlassFishServer server, String type) {
        List<String> result = Collections.emptyList();
        List<ResourceDesc> retVal = Collections.emptyList();
        try {
        	Command command = new CommandListResources(CommandListResources.command(
                    type), null);
                Future<ResultList<String>> future = ServerAdmin.<ResultList<String>>
                exec(server, command, new IdeContext());
            ResultList<String> res = future.get(30, TimeUnit.SECONDS);
            if (TaskState.COMPLETED.equals(res.getState())) {
                result = res.getValue();
            }
            for (String rsc : result) {
            	retVal.add(new ResourceDesc(rsc, type));
            }
        } catch (InterruptedException ex) {
            Logger.getLogger("glassfish").log(Level.INFO, ex.getMessage(), ex);
        } catch (Exception ex) {
            Logger.getLogger("glassfish").log(Level.INFO, ex.getMessage(), ex);
        }
        return retVal;
    }
	
	
	public static Map<String, List<AppDesc>> getApplications(
			GlassFishServer server, String container) {
		Map<String, List<String>> apps = Collections.emptyMap();
		Command command = new CommandListComponents(null);
		Future<ResultMap<String, List<String>>> future = ServerAdmin
				.<ResultMap<String, List<String>>> exec(server, command,
						new IdeContext());
		ResultMap<String, List<String>> result = null;
		try {
			result = future.get(10, TimeUnit.SECONDS);
		} catch (InterruptedException e) {
			Logger.getLogger("glassfish").log(Level.INFO, e.getMessage(), e); // NOI18N
		} catch (ExecutionException e) {
			Logger.getLogger("glassfish").log(Level.INFO, e.getMessage(), e); // NOI18N
		} catch (TimeoutException e) {
			Logger.getLogger("glassfish").log(Level.INFO, e.getMessage(), e); // NOI18N
		}
		if (result.getState().equals(TaskState.COMPLETED)) {
			apps = result.getValue();
		}
		if (apps == null || apps.isEmpty()) {
			return Collections.emptyMap();
		}
		return processApplications(apps);
		// ServerCommand.GetPropertyCommand getCmd = new
		// ServerCommand.GetPropertyCommand("applications.application.*");
		// serverCmd = getCmd;
		// task = executor().submit(this);
		// state = task.get(30, TimeUnit.SECONDS);
		// if (state == OperationState.COMPLETED) {
		// result = processApplications(apps, getCmd.getData());
		// }
	}

	private static Map<String, List<AppDesc>> processApplications(
			Map<String, List<String>> appsList) {
		Map<String, List<AppDesc>> result = new HashMap<String, List<AppDesc>>();
		Iterator<String> appsItr = appsList.keySet().iterator();
		while (appsItr.hasNext()) {
			String engine = appsItr.next();
			List<String> apps = appsList.get(engine);
			for (int i = 0; i < apps.size(); i++) {
				String name = apps.get(i).trim();
				// String appname = "applications.application." + name; //
				// NOI18N
				// String contextKey = appname + ".context-root"; // NOI18N
				// String pathKey = appname + ".location"; // NOI18N

				List<AppDesc> appList = result.get(engine);
				if (appList == null) {
					appList = new ArrayList<AppDesc>();
					result.put(engine, appList);
				}
				appList.add(new AppDesc(name, null, null, engine));
			}
		}
		return result;
	}

	/**
	 * Sends list-web-services command to server (synchronous)
	 * 
	 * @return String array of names of deployed applications.
	 */
	public static List<WSDesc> getWebServices(GlassFishServer server) {
		List<String> wssList = Collections.emptyList();
		Command command = new CommandListWebServices();
		Future<ResultList<String>> future = ServerAdmin
				.<ResultList<String>> exec(server, command, new IdeContext());
		ResultList<String> result = null;
		try {
			result = future.get(10, TimeUnit.SECONDS);
		} catch (InterruptedException e) {
			Logger.getLogger("glassfish").log(Level.INFO, e.getMessage(), e); // NOI18N
		} catch (ExecutionException e) {
			Logger.getLogger("glassfish").log(Level.INFO, e.getMessage(), e); // NOI18N
		} catch (TimeoutException e) {
			Logger.getLogger("glassfish").log(Level.INFO, e.getMessage(), e); // NOI18N
		}
		if (result.getState().equals(TaskState.COMPLETED)) {
			wssList = result.getValue();
		}
		if (wssList == null || wssList.isEmpty()) {
			return Collections.emptyList();
		}
		return processWebServices(wssList);

	}

	private static List<WSDesc> processWebServices(List<String> wssList) {
		List<WSDesc> result = new ArrayList<WSDesc>();
		for (String a : wssList) {
			result.add(new WSDesc(a, a + "?wsdl", a + "?Tester")); // NOI18N
		}
		return result;
	}
	
	public static Map<String, String> getResourceData(GlassFishServer server, String name) {
        return ResourceUtils.getResourceData(server, name);
    }
	
	public static void putResourceData(GlassFishServer server, Map<String, String> data) throws PartialCompletionException {
        ResourceUtils.putResourceData(server, data);
    }

}
