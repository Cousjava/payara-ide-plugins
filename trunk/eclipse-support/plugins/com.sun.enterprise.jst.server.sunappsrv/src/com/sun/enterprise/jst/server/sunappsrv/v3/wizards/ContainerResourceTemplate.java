package com.sun.enterprise.jst.server.sunappsrv.v3.wizards;

import java.util.*;
import org.eclipse.jst.j2ee.internal.common.operations.*;

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

@SuppressWarnings("restriction")
public class ContainerResourceTemplate {
  protected static String nl;
  public static synchronized ContainerResourceTemplate create(String lineSeparator)
  {
    nl = lineSeparator;
    ContainerResourceTemplate result = new ContainerResourceTemplate();
    nl = null;
    return result;
  }

  public final String NL = nl == null ? (System.getProperties().getProperty("line.separator")) : nl;
  protected final String TEXT_1 = "package ";
  protected final String TEXT_2 = ";";
  protected final String TEXT_3 = NL;
  protected final String TEXT_4 = NL + "import ";
  protected final String TEXT_5 = ";";
  protected final String TEXT_6 = NL + NL + "@Path(\"";
  protected final String TEXT_7 = "\")";
  protected final String TEXT_8 = NL + "public ";
  protected final String TEXT_9 = "abstract ";
  protected final String TEXT_10 = "final ";
  protected final String TEXT_11 = "class ";
  protected final String TEXT_12 = " extends ";
  protected final String TEXT_13 = " implements ";
  protected final String TEXT_14 = ", ";
  protected final String TEXT_15 = " {";
  protected final String TEXT_16 = NL + "    @SuppressWarnings(\"unused\")";
  protected final String TEXT_17 = NL + "    @Context" + NL + "    private UriInfo context;";
  protected final String TEXT_18 = NL + NL + "    /**" + NL + "     * Default constructor. " + NL + "     */" + NL + "    public ";
  protected final String TEXT_19 = "() {" + NL + "        // TODO Auto-generated constructor stub" + NL + "    }";
  protected final String TEXT_20 = NL + "       " + NL + "    /**" + NL + "     * @see ";
  protected final String TEXT_21 = "#";
  protected final String TEXT_22 = "(";
  protected final String TEXT_23 = ")" + NL + "     */" + NL + "    public ";
  protected final String TEXT_24 = "(";
  protected final String TEXT_25 = ") {" + NL + "        super(";
  protected final String TEXT_26 = ");" + NL + "        // TODO Auto-generated constructor stub" + NL + "    }";
  protected final String TEXT_27 = NL + NL + "\t/**" + NL + "     * @see ";
  protected final String TEXT_28 = "#";
  protected final String TEXT_29 = "(";
  protected final String TEXT_30 = ")" + NL + "     */" + NL + "    public ";
  protected final String TEXT_31 = " ";
  protected final String TEXT_32 = "(";
  protected final String TEXT_33 = ") {" + NL + "        // TODO Auto-generated method stub";
  protected final String TEXT_34 = NL + "\t\t\treturn ";
  protected final String TEXT_35 = ";";
  protected final String TEXT_36 = NL + "    }";
  protected final String TEXT_37 = NL + NL + "    /**" + NL + "     * Retrieves representation of an instance of ";
  protected final String TEXT_38 = NL + "     * @return an instance of ";
  protected final String TEXT_39 = NL + "     */" + NL + "    @GET" + NL + "    @Produces(\"";
  protected final String TEXT_40 = "\")" + NL + "    public ";
  protected final String TEXT_41 = " get";
  protected final String TEXT_42 = "() {" + NL + "        // TODO return proper representation object" + NL + "        throw new UnsupportedOperationException();" + NL + "    }" + NL;
  protected final String TEXT_43 = NL + "    /**" + NL + "     * POST method for creating an instance of ";
  protected final String TEXT_44 = NL + "     * @param content representation for the new resource" + NL + "     * @return an HTTP response with content of the created resource" + NL + "     */" + NL + "    @POST" + NL + "    @Consumes(\"";
  protected final String TEXT_45 = "\")" + NL + "    @Produces(\"";
  protected final String TEXT_46 = "\")" + NL + "    public Response post";
  protected final String TEXT_47 = "(";
  protected final String TEXT_48 = " content) {" + NL + "        // TODO" + NL + "        return Response.created(context.getAbsolutePath()).build();" + NL + "    }" + NL;
  protected final String TEXT_49 = NL + "\t/**" + NL + "     * Sub-resource locator method for ";
  protected final String TEXT_50 = NL + "     */" + NL + "    @Path(\"";
  protected final String TEXT_51 = "\")" + NL + "    public ";
  protected final String TEXT_52 = " get";
  protected final String TEXT_53 = "(";
  protected final String TEXT_54 = ") {" + NL + "        return ";
  protected final String TEXT_55 = ".getInstance(";
  protected final String TEXT_56 = ");" + NL + "    }" + NL + "}";

	public String generate(Object argument)
  {
    final StringBuffer stringBuffer = new StringBuffer();
    
	AddGenericResourceTemplateModel model = (AddGenericResourceTemplateModel) argument;
	String representationClass = model.getUnqualifiedContainerRepresentationClass();
	String mimeType = model.getProperty(AddGenericResourceDataModelProvider.MIME_TYPE).trim();
	String path = model.getProperty(AddGenericResourceDataModelProvider.PATH).trim();
	String containerPath = model.getProperty(AddGenericResourceDataModelProvider.CONTAINER_PATH).trim();
	String methodNameFromMimeType = model.getMethodNameSuffixFromMimeType();
	String originalClassName = model.getProperty(AddGenericResourceDataModelProvider.ORIGINAL_CLASS_NAME).trim();
	String[] paramListStrings = model.getParamList();
	String paramList = model.getCommaSeparatedParamList();
	boolean hasParam = (paramListStrings != null);
	String paramListNoTypes = (hasParam ? paramList : "");
	String pathParams = "";
	boolean isClientPattern = model.isClientControlledPattern();
	
	if (hasParam) { 
		for (int i=0;i<paramListStrings.length;i++) { 
			pathParams+="@PathParam(\"" + paramListStrings[i] + "\") String " + paramListStrings[i];
			if (i < paramListStrings.length - 1) {
				pathParams+=", ";
			}
		}
	}

     /* This Content is provided under the terms and conditions of the Eclipse Public License Version 1.0 
("EPL"). A copy of the EPL is available at http://www.eclipse.org/org/documents/epl-v10.php 
For purposes of the EPL, "Program" will mean the Content. 

Copied from org.eclipse.jst.j2ee.ejb plugin. */ 
    
	model.removeFlags(CreateJavaEEArtifactTemplateModel.FLAG_QUALIFIED_SUPERCLASS_NAME); 

     /* This Content is provided under the terms and conditions of the Eclipse Public License Version 1.0 
("EPL"). A copy of the EPL is available at http://www.eclipse.org/org/documents/epl-v10.php 
For purposes of the EPL, "Program" will mean the Content. 

Copied from org.eclipse.jst.j2ee.ejb plugin. */ 
    
	if (model.getJavaPackageName() != null && model.getJavaPackageName().length() > 0) {

    stringBuffer.append(TEXT_1);
    stringBuffer.append( model.getJavaPackageName() );
    stringBuffer.append(TEXT_2);
    
	}

    stringBuffer.append(TEXT_3);
     /* This Content is provided under the terms and conditions of the Eclipse Public License Version 1.0 
("EPL"). A copy of the EPL is available at http://www.eclipse.org/org/documents/epl-v10.php 
For purposes of the EPL, "Program" will mean the Content. 

Copied from org.eclipse.jst.j2ee.ejb plugin. */ 
     
	Collection<String> imports = model.getImports();
	for (String anImport : imports) { 

    stringBuffer.append(TEXT_4);
    stringBuffer.append( anImport );
    stringBuffer.append(TEXT_5);
     
	}

    stringBuffer.append(TEXT_6);
    stringBuffer.append( containerPath );
    stringBuffer.append(TEXT_7);
     /* This Content is provided under the terms and conditions of the Eclipse Public License Version 1.0 
("EPL"). A copy of the EPL is available at http://www.eclipse.org/org/documents/epl-v10.php 
For purposes of the EPL, "Program" will mean the Content. 

Copied from org.eclipse.jst.j2ee.ejb plugin. */ 
    
	if (model.isPublic()) { 

    stringBuffer.append(TEXT_8);
     
	} 

	if (model.isAbstract()) { 

    stringBuffer.append(TEXT_9);
    
	}

	if (model.isFinal()) {

    stringBuffer.append(TEXT_10);
    
	}

    stringBuffer.append(TEXT_11);
    stringBuffer.append( model.getClassName() );
    
	String superClass = model.getSuperclassName();
 	if (superClass != null && superClass.length() > 0) {

    stringBuffer.append(TEXT_12);
    stringBuffer.append( superClass );
    
	}

	List<String> interfaces = model.getInterfaces(); 
 	if ( interfaces.size() > 0) { 

    stringBuffer.append(TEXT_13);
    
	}
	
 	for (int i = 0; i < interfaces.size(); i++) {
   		String INTERFACE = (String) interfaces.get(i);
   		if (i > 0) {

    stringBuffer.append(TEXT_14);
    
		}

    stringBuffer.append( INTERFACE );
    
	}

    stringBuffer.append(TEXT_15);
     if (isClientPattern) { 
    stringBuffer.append(TEXT_16);
     } 
    stringBuffer.append(TEXT_17);
     /* This Content is provided under the terms and conditions of the Eclipse Public License Version 1.0 
("EPL"). A copy of the EPL is available at http://www.eclipse.org/org/documents/epl-v10.php 
For purposes of the EPL, "Program" will mean the Content. 

Copied from org.eclipse.jst.j2ee.ejb plugin. */ 
     
	if (!model.hasEmptySuperclassConstructor()) { 

    stringBuffer.append(TEXT_18);
    stringBuffer.append( model.getClassName() );
    stringBuffer.append(TEXT_19);
     
	} 

	if (model.shouldGenSuperclassConstructors()) {
		List<Constructor> constructors = model.getConstructors();
		for (Constructor constructor : constructors) {
			if (constructor.isPublic() || constructor.isProtected()) { 

    stringBuffer.append(TEXT_20);
    stringBuffer.append( model.getSuperclassName() );
    stringBuffer.append(TEXT_21);
    stringBuffer.append( model.getSuperclassName() );
    stringBuffer.append(TEXT_22);
    stringBuffer.append( constructor.getParamsForJavadoc() );
    stringBuffer.append(TEXT_23);
    stringBuffer.append( model.getClassName() );
    stringBuffer.append(TEXT_24);
    stringBuffer.append( constructor.getParamsForDeclaration() );
    stringBuffer.append(TEXT_25);
    stringBuffer.append( constructor.getParamsForCall() );
    stringBuffer.append(TEXT_26);
    
			} 
		} 
	} 

     /* This Content is provided under the terms and conditions of the Eclipse Public License Version 1.0 
("EPL"). A copy of the EPL is available at http://www.eclipse.org/org/documents/epl-v10.php 
For purposes of the EPL, "Program" will mean the Content. 

Copied from org.eclipse.jst.j2ee.ejb plugin. */ 
    
	if (model.shouldImplementAbstractMethods()) {
		for (Method method : model.getUnimplementedMethods()) { 

    stringBuffer.append(TEXT_27);
    stringBuffer.append( method.getContainingJavaClass() );
    stringBuffer.append(TEXT_28);
    stringBuffer.append( method.getName() );
    stringBuffer.append(TEXT_29);
    stringBuffer.append( method.getParamsForJavadoc() );
    stringBuffer.append(TEXT_30);
    stringBuffer.append( method.getReturnType() );
    stringBuffer.append(TEXT_31);
    stringBuffer.append( method.getName() );
    stringBuffer.append(TEXT_32);
    stringBuffer.append( method.getParamsForDeclaration() );
    stringBuffer.append(TEXT_33);
     
			String defaultReturnValue = method.getDefaultReturnValue();
			if (defaultReturnValue != null) { 

    stringBuffer.append(TEXT_34);
    stringBuffer.append( defaultReturnValue );
    stringBuffer.append(TEXT_35);
    
			} 

    stringBuffer.append(TEXT_36);
     
		}
	} 

    stringBuffer.append(TEXT_37);
    stringBuffer.append( model.getClassName() );
    stringBuffer.append(TEXT_38);
    stringBuffer.append( representationClass );
    stringBuffer.append(TEXT_39);
    stringBuffer.append( mimeType );
    stringBuffer.append(TEXT_40);
    stringBuffer.append( representationClass );
    stringBuffer.append(TEXT_41);
    stringBuffer.append( methodNameFromMimeType );
    stringBuffer.append(TEXT_42);
     if (!isClientPattern) { 
    stringBuffer.append(TEXT_43);
    stringBuffer.append( originalClassName );
    stringBuffer.append(TEXT_44);
    stringBuffer.append( mimeType );
    stringBuffer.append(TEXT_45);
    stringBuffer.append( mimeType );
    stringBuffer.append(TEXT_46);
    stringBuffer.append( methodNameFromMimeType );
    stringBuffer.append(TEXT_47);
    stringBuffer.append( representationClass );
    stringBuffer.append(TEXT_48);
     } 
    stringBuffer.append(TEXT_49);
    stringBuffer.append( path );
    stringBuffer.append(TEXT_50);
    stringBuffer.append( path );
    stringBuffer.append(TEXT_51);
    stringBuffer.append( originalClassName );
    stringBuffer.append(TEXT_52);
    stringBuffer.append( originalClassName );
    stringBuffer.append(TEXT_53);
    stringBuffer.append( pathParams );
    stringBuffer.append(TEXT_54);
    stringBuffer.append( originalClassName );
    stringBuffer.append(TEXT_55);
    stringBuffer.append( paramListNoTypes );
    stringBuffer.append(TEXT_56);
    return stringBuffer.toString();
  }
}
