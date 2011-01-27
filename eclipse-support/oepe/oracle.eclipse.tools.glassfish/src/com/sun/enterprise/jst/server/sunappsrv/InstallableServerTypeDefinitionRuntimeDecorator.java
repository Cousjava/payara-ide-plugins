/*
 * Copyright (c) 2005, 2007-2011 Oracle and/or its affiliates. All rights reserved.
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

import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.jst.server.generic.core.internal.GenericServerRuntime;
import org.eclipse.jst.server.generic.core.internal.Trace;
import org.eclipse.jst.server.generic.servertype.definition.Property;
import org.eclipse.jst.server.generic.servertype.definition.ServerRuntime;
import org.eclipse.jst.server.generic.ui.internal.GenericServerComposite;
import org.eclipse.jst.server.generic.ui.internal.GenericServerCompositeDecorator;
import org.eclipse.jst.server.generic.ui.internal.GenericServerUIMessages;
import org.eclipse.jst.server.generic.ui.internal.SWTUtil;
import org.eclipse.jst.server.generic.ui.internal.ServerTypeDefinitionRuntimeDecorator;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Text;
import org.eclipse.wst.server.core.TaskModel;
import org.eclipse.wst.server.core.internal.IInstallableRuntime;
import org.eclipse.wst.server.core.internal.ServerPlugin;
import org.eclipse.wst.server.ui.internal.wizard.TaskWizard;
import org.eclipse.wst.server.ui.internal.wizard.fragment.LicenseWizardFragment;
import org.eclipse.wst.server.ui.wizard.IWizardHandle;
import org.eclipse.wst.server.ui.wizard.WizardFragment;

// the fact that we need to provide our own runtime decorator is a workaround for issue 222688
// it is basically the code from the ServerTypeDefinitionDecorator + some from the InstallableRuntimeDecorator 
// (which would be used automatically if the bug wasn't there) with modifications to methods 
// which make use of private code.  This code combines the 2 decorators that are used by default
// so they can interact better b/w the text/browse/download controls and show the license
// There is also some additional directory checking, permission setting, and error messages
// Finally, we make use of the IProgressMonitor for task/progress bar reporting
@SuppressWarnings("restriction")
public class InstallableServerTypeDefinitionRuntimeDecorator extends ServerTypeDefinitionRuntimeDecorator implements
	GenericServerCompositeDecorator {
    private  String UNZIP_DIR_NAME = "glassfishv3-prelude"; //$NON-NLS-1$ //by default, v3 prelude otherwise v3 build
    private static final String BIN_DIR_NAME = "bin";  //$NON-NLS-1$
    private static final String ADMIN_SCRIPT_NAME = "asadmin"; //$NON-NLS-1$
	private String fInstallDirName;
	private ServerRuntime fDefinition;
    @SuppressWarnings("unchecked")
	private Map fProperties;
	private GenericServerRuntime fRuntime;
	private Property serverDirProperty;
	private Text path;
	private Button installButton;
	
	@SuppressWarnings("unchecked")
	public InstallableServerTypeDefinitionRuntimeDecorator(ServerRuntime definition, Map initialProperties, 
			IWizardHandle wizard, GenericServerRuntime runtime, String installDirName) {
		super(definition, initialProperties,wizard,runtime);
		//adding v3 build support for download. root dir is a bit different
		if (definition.getId().equals("com.sun.enterprise.jst.server.runtime.sunappsrv92")){
			UNZIP_DIR_NAME = "glassfishv3";
		}
		if (definition.getId().equals("org.glassfish.jst.server.runtime.glassfish31")){
			UNZIP_DIR_NAME = "glassfish3";
		}		fInstallDirName = installDirName;
		fDefinition = definition;
		fProperties = initialProperties;
		fRuntime = runtime;
		serverDirProperty = getServerDirProperty(fDefinition.getProperty());
	}

	@SuppressWarnings("unchecked")
	private Property getServerDirProperty(List fProperties) {
		// assume there's just one property we want and it is the only runtime context property
		for (int i = 0; i < fProperties.size(); i++) {
			Property property = (Property) fProperties.get(i);
			if (CONTEXT_RUNTIME.equals(property.getContext()))
				return property;
		}
		return null;
	}

	public void decorate(final GenericServerComposite composite) {
		String pathText;
		Dialog.applyDialogFont(composite);
		path = SWTUtil.createLabeledPath(serverDirProperty.getLabel(),getPropertyValue(serverDirProperty),composite);
		path.setData(serverDirProperty);
		path.addModifyListener(new PathModifyListener());
		pathText = path.getText();
		final IInstallableRuntime ir = ServerPlugin
				.findInstallableRuntime(fRuntime.getRuntime().getRuntimeType()
						.getId());
		installButton = SWTUtil.createButton(composite, GenericServerUIMessages.installServerButton);
		installButton.setEnabled(pathText.length() > 0);
		installButton.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent se) {
				final String selectedDirectory = path.getText();
				if (selectedDirectory != null) {
					String license = null;
					try {
						license = ir.getLicense(new NullProgressMonitor());
					} catch (CoreException e) {
						Trace.trace(Trace.SEVERE, "Error getting license", e);
					}
					TaskModel taskModel = new TaskModel();
					taskModel.putObject(LicenseWizardFragment.LICENSE, license);
					TaskWizard wizard2 = new TaskWizard(GenericServerUIMessages.installServerButton, new WizardFragment() {
						@SuppressWarnings("unchecked")
						protected void createChildFragments(List list) {
							list.add(new LicenseWizardFragment());
						}
					}, taskModel);
					
					WizardDialog dialog2 = new WizardDialog(composite.getShell(), wizard2);
					if (dialog2.open() == Window.CANCEL)
						return;
					IRunnableWithProgress runnable = new IRunnableWithProgress() {
						public void run(IProgressMonitor monitor)
								throws InvocationTargetException,
								InterruptedException {
							try {
								monitor.beginTask(Messages.downloadingServer, IProgressMonitor.UNKNOWN);
								ir.install(new Path(selectedDirectory),
										monitor);
                                // on mac (and maybe other OS's, exec bits not retained
                                correctPermissions(selectedDirectory);
                                monitor.done();
							} catch (CoreException e) {
								Trace.trace(Trace.SEVERE,
										"Error installing runtime", e); //$NON-NLS-1$
							}
						}
					};
					
					try {
						// NOTE: it would be best to make this cancelable by 
						// setting the second param to true, but due to the 
						// implementation of the installable runtime's install
						// being all one block, that is not really useful as of
						// now.  See wtp issue 274201
						fWizard.run(true, false, runnable);
						path.setText(getInternalDirectoryName(selectedDirectory));
						validate();
					} catch (Exception e) {
						Trace.trace(Trace.SEVERE,
								"Error installing runtime", e); //$NON-NLS-1$
					}
	
				}
			}
		});
		if (pathText.length() > 0) {
			// has an initial value, probably opened from edit dialog...
			// must do this after install button is created above
			validatePath(pathText);
		}
	}

    private String getInternalDirectoryName(String installDir) {
        return installDir + File.separatorChar +
			UNZIP_DIR_NAME + File.separatorChar + fInstallDirName;
    }

    private void correctPermissions(String installDir) {
        String osName = System.getProperty("os.name");

        if (!osName.startsWith("Windows")) {
            String binDir = getInternalDirectoryName(installDir) +
                File.separatorChar + BIN_DIR_NAME;
            String scriptName = binDir + File.separatorChar + ADMIN_SCRIPT_NAME;

            try {
				Runtime.getRuntime().exec("chmod a+x " + scriptName);
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
        }

    }

	// copy the one from the superclass hierarchy since it is private
    private String getPropertyValue(Property property){	
		if(fProperties!=null && fProperties.isEmpty()==false){
			//user properties exist use those
				return fDefinition.getResolver().resolveProperties((String)fProperties.get(property.getId()));
			}	
			if(Property.TYPE_SELECT_EDIT.equals(property.getType())){
				StringTokenizer tokenizer = new StringTokenizer(property.getDefault(),","); //$NON-NLS-1$
				if( tokenizer.hasMoreTokens()){
					return fDefinition.getResolver().resolveProperties(tokenizer.nextToken());
				}
				return ""; //$NON-NLS-1$
			}
			return fDefinition.getResolver().resolveProperties(property.getDefault());	
	}
   
    /**
     * Returns the property name/value pairs.  Replaces one in superclass because that depends on 
     * a private list of controls which we have no access to, and therefore our controls are not in there
     * @return Map containing the values collected from the user
     */
 	@SuppressWarnings("unchecked")
	public Map getValues(){
 		Map propertyMap = new HashMap<String, String>();
 		propertyMap.put(serverDirProperty.getId(), path.getText());
     	return propertyMap;
     }

 	private void validatePath(String path) {
 		if(path.length()<1)
 		{
 			fLastMessage = GenericServerUIMessages.emptyPath;
 			fWizard.setMessage(fLastMessage,IMessageProvider.ERROR);
 			installButton.setEnabled(false);
 		}
 		else if(!pathExist(path)){
 			fLastMessage = Messages.canInstallPath;
 			fWizard.setMessage(fLastMessage,IMessageProvider.ERROR);
 			installButton.setEnabled(true);
 		}else{
 			if(fLastMessage!=null && fLastMessage.equals(fWizard.getMessage())){
 				fLastMessage=null;
 				fWizard.setMessage(null,IMessageProvider.NONE);
 			}
 			boolean isInvalid = validate();
 			if (isInvalid) {
 				String glassfishDir = path + File.separatorChar + fInstallDirName;
 				String unzipDirName = getInternalDirectoryName(path);

 				if(pathExist(glassfishDir)){
 					fLastMessage = NLS.bind(Messages.possibleInstallExists,fInstallDirName);
 					fWizard.setMessage(fLastMessage,IMessageProvider.ERROR);
 					installButton.setEnabled(false);
 				} else if (pathExist(unzipDirName)) {
 					fLastMessage = NLS.bind(Messages.possibleInstallExists,UNZIP_DIR_NAME + File.separatorChar + fInstallDirName);
 					fWizard.setMessage(fLastMessage,IMessageProvider.ERROR);
 					installButton.setEnabled(false);
 				} else {
 					fLastMessage = Messages.canInstallPath;
 					fWizard.setMessage(fLastMessage,IMessageProvider.ERROR);
 					installButton.setEnabled(true);
 				}
 			} else {
 				installButton.setEnabled(false);
 			}
 		}
 	}
	private boolean pathExist(String path){
		File f = new File(path);
		return f.exists();
	}


	private final class PathModifyListener implements ModifyListener {
		public void modifyText(ModifyEvent e) {
			String path = ((Text) e.widget).getText();
			validatePath(path);
		}
	}
}
