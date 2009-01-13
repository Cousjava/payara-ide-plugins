package com.sun.enterprise.jst.server.sunappsrv.register.splashHandlers;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ProgressMonitorDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.splash.AbstractSplashHandler;

import com.sun.enterprise.jst.server.sunappsrv.register.Activator;

/**
 * @since 3.3
 * 
 */
public class InteractiveSplashHandler extends AbstractSplashHandler {

	// FIXME convert to property, so it could be given at runtime.
	private static final boolean workspace = false;

	public InteractiveSplashHandler() {
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.ui.splash.AbstractSplashHandler#init(org.eclipse.swt.widgets
	 * .Shell)
	 */
	public void init(final Shell splash) {
		super.init(splash);
		// showWizard(splash);

		final String glassfishLoc = getLocation();

		if (new File(glassfishLoc + File.separator + ".installed").exists())
			return;

		final String dir = V2InstallationConfigurer.getJDKDir();
		Display.getDefault().asyncExec(new Runnable() {

			public void run() {
				final Shell shell = new Shell(Display.getDefault());
				try {
					IRunnableWithProgress op = new IRunnableWithProgress() {
						public void run(IProgressMonitor progressMonitor) throws InvocationTargetException,
								InterruptedException {
							progressMonitor.setTaskName("Configuring Glassfish V2 installation ...");
							V2InstallationConfigurer.configureV2(dir, glassfishLoc);
						}
					};
					ProgressMonitorDialog pmd = new ProgressMonitorDialog(shell);
					pmd.run(true, false, op);
				} catch (Exception e) {
					org.eclipse.jface.dialogs.ErrorDialog.openError(shell, "Exception occurred", e
							.getLocalizedMessage(), new Status(IStatus.ERROR, Activator.PLUGIN_ID, e
							.getLocalizedMessage()));
				}

			}
		});

	}

	private String getLocation() {
		if (workspace) {
			// Overriding for testing purposes. Installation directory while
			// running from workspace is target platforms location. It's better
			// if we use some other dir for V2. Also this path is
			// suitable only for *nix platforms.
			return "/tmp/v2/glassfishv2";
		} else {
			try {
				// Get the eclipse installation location and from it V2
				// installation
				// directory.
				return FileLocator.toFileURL(Platform.getInstallLocation().getURL()).getFile() + File.pathSeparator
						+ "glassfishv2";
			} catch (IOException e1) {
				e1.printStackTrace();
			}
		}
		return null;
	}

	/**
	 * Show registration dialog.
	 * 
	 * @param splash
	 */
	public static void showWizard(final Shell splash) {
		RegistrationWizard wizard = new RegistrationWizard();
		WizardDialog dialog = new WizardDialog(splash, wizard);
		dialog.open();
	}
}
