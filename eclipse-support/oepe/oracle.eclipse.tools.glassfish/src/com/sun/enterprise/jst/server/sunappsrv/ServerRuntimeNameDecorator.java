package com.sun.enterprise.jst.server.sunappsrv;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jst.server.generic.core.internal.GenericServerRuntime;
import org.eclipse.jst.server.generic.ui.internal.GenericServerComposite;
import org.eclipse.jst.server.generic.ui.internal.GenericServerCompositeDecorator;
import org.eclipse.jst.server.generic.ui.internal.GenericServerUIMessages;
import org.eclipse.jst.server.generic.ui.internal.SWTUtil;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.widgets.Text;
import org.eclipse.wst.server.core.IRuntime;
import org.eclipse.wst.server.core.IRuntimeType;
import org.eclipse.wst.server.core.IRuntimeWorkingCopy;
import org.eclipse.wst.server.core.ServerCore;
import org.eclipse.wst.server.ui.wizard.IWizardHandle;

@SuppressWarnings("restriction")
public class ServerRuntimeNameDecorator implements GenericServerCompositeDecorator {

	private IRuntimeWorkingCopy wc;
	private GenericServerRuntime runtime;
	private IWizardHandle wizard;
	private Text runtimeNameText;
	
	public ServerRuntimeNameDecorator(GenericServerRuntime runtime, IWizardHandle handle) {
		wc = runtime.getRuntimeWorkingCopy();
		this.runtime = runtime;
		wizard = handle;
	}
	
	@Override
	public void decorate(GenericServerComposite composite) {
		// new runtime
		if ((wc.getName() == null) || wc.getName().isEmpty())
			wc.setName(createName());
		//candidateName = createName();
		runtimeNameText = SWTUtil.createLabeledText(Messages.runtimeNameLabel, wc.getName(), composite);
		runtimeNameText.addModifyListener(new ModifyListener() {
			
			@Override
			public void modifyText(ModifyEvent arg0) {
				wc.setName(runtimeNameText.getText());
				validate();
			}
		});
	}

	@Override
	public boolean validate() {
		IStatus status = wc.validate(null);
		if (status == null || status.isOK()){
			wizard.setMessage(null, IMessageProvider.NONE);
			wizard.update();
			return false;
		}
		
		wizard.setMessage(status.getMessage(), IMessageProvider.ERROR);
		wizard.update();
		return true; 
	}
	
	private String createName() {
        IRuntimeType runtimeType = runtime.getRuntime().getRuntimeType();
        String name = NLS.bind(
                GenericServerUIMessages.runtimeName, runtimeType.getName() );
        IRuntime[] list = ServerCore.getRuntimes();
        int suffix = 1;
        String suffixName = name;
        for( int i = 0; i < list.length; i++ )
        {
            if( (list[i].getName().equals( name ) || list[i].getName().equals(
                    suffixName ))
                    && !list[i].equals( runtime.getRuntime() ) )
                suffix++;
            suffixName = name + ' ' + suffix;
        }

        if( suffix > 1 )
            return suffixName;
        return name;
    }
	
}
