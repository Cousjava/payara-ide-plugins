package com.sun.enterprise.datatools.enablement.mysql.internal.driver;

import java.io.File;
import java.io.FilenameFilter;

import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.datatools.connectivity.drivers.DefaultDriverValuesProvider;
import org.eclipse.datatools.connectivity.drivers.IDriverValuesProvider;

public class MySQLDriverValuesProvider extends DefaultDriverValuesProvider {

	public String createDefaultValue(final String key) {
		/**
		 * Check to see if the wrapper plug-in is in the Eclipse environment. If
		 * it is we'll use it and grab the driver jar from there.
		 */
		if (key.equals(IDriverValuesProvider.VALUE_CREATE_DEFAULT)) {
			File[] jars = getJars();
			return Boolean.toString(jars.length > 0);
		}
		if (key.equals(IDriverValuesProvider.VALUE_JARLIST)) {
			File[] jars = getJars();
			if (jars.length > 0) {
				return jars[0].getAbsolutePath();
			}
		}
		return super.createDefaultValue(key);
	}

	private File[] getJars() {
		String installLocation = new Path(Platform.getInstallLocation().getURL().getFile()).toPortableString();
		String driverLocation = installLocation + "mysql-driver";
		File file = new File(driverLocation);
		if (!file.exists() || !file.isDirectory()){ //test if it exists.
			return new File[0];
		}
		File[] jars = file.listFiles(new FilenameFilter() {
			public boolean accept(File dir, String name) {
				return name.endsWith(".jar");
			}
		});
		return jars;
	}

}
