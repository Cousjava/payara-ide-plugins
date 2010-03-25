<%@page contentType="text/html"%>
<%@page pageEncoding="UTF-8"%>
<%@taglib uri="http://java.sun.com/jsf/core" prefix="f" %>
<%@taglib uri="http://java.sun.com/jsf/html" prefix="h" %>
<f:view>
    <html>
        <head>
            <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
            <title>Customer Detail</title>
            <link rel="stylesheet" type="text/css" href="/JPAJSF/faces/jsfcrud.css" />
        </head>
        <body>
        <h:panelGroup id="messagePanel" layout="block">
            <h:messages errorStyle="color: red" infoStyle="color: green" layout="table"/>
        </h:panelGroup>
        <h1>Customer Detail</h1>
        <h:form>
            <h:panelGrid columns="2">
                <h:outputText value="CustomerId:"/>
                <h:outputText value="#{customer.customer.customerId}" title="CustomerId" />
                <h:outputText value="Zip:"/>
                <h:outputText value="#{customer.customer.zip}" title="Zip" />
                <h:outputText value="Name:"/>
                <h:outputText value="#{customer.customer.name}" title="Name" />
                <h:outputText value="Addressline1:"/>
                <h:outputText value="#{customer.customer.addressline1}" title="Addressline1" />
                <h:outputText value="Addressline2:"/>
                <h:outputText value="#{customer.customer.addressline2}" title="Addressline2" />
                <h:outputText value="City:"/>
                <h:outputText value="#{customer.customer.city}" title="City" />
                <h:outputText value="State:"/>
                <h:outputText value="#{customer.customer.state}" title="State" />
                <h:outputText value="Phone:"/>
                <h:outputText value="#{customer.customer.phone}" title="Phone" />
                <h:outputText value="Fax:"/>
                <h:outputText value="#{customer.customer.fax}" title="Fax" />
                <h:outputText value="Email:"/>
                <h:outputText value="#{customer.customer.email}" title="Email" />
                <h:outputText value="CreditLimit:"/>
                <h:outputText value="#{customer.customer.creditLimit}" title="CreditLimit" />
                <h:outputText value="DiscountCode:"/>
                <h:panelGroup>
                    <h:outputText value="#{customer.customer.discountCode}"/>
                    <h:panelGroup rendered="#{customer.customer.discountCode != null}">
                        <h:outputText value=" ("/>
                        <h:commandLink value="Show" action="#{discountCode.detailSetup}">
                            <f:param name="jsfcrud.currentCustomer" value="#{jsfcrud_class['e.util.JsfUtil'].jsfcrud_method['getAsConvertedString'][customer.customer][customer.converter].jsfcrud_invoke}"/>
                            <f:param name="jsfcrud.currentDiscountCode" value="#{jsfcrud_class['e.util.JsfUtil'].jsfcrud_method['getAsConvertedString'][customer.customer.discountCode][discountCode.converter].jsfcrud_invoke}"/>
                            <f:param name="jsfcrud.relatedController" value="customer"/>
                            <f:param name="jsfcrud.relatedControllerType" value="e.CustomerController"/>
                        </h:commandLink>
                        <h:outputText value=" "/>
                        <h:commandLink value="Edit" action="#{discountCode.editSetup}">
                            <f:param name="jsfcrud.currentCustomer" value="#{jsfcrud_class['e.util.JsfUtil'].jsfcrud_method['getAsConvertedString'][customer.customer][customer.converter].jsfcrud_invoke}"/>
                            <f:param name="jsfcrud.currentDiscountCode" value="#{jsfcrud_class['e.util.JsfUtil'].jsfcrud_method['getAsConvertedString'][customer.customer.discountCode][discountCode.converter].jsfcrud_invoke}"/>
                            <f:param name="jsfcrud.relatedController" value="customer"/>
                            <f:param name="jsfcrud.relatedControllerType" value="e.CustomerController"/>
                        </h:commandLink>
                        <h:outputText value=" "/>
                        <h:commandLink value="Destroy" action="#{discountCode.destroy}">
                            <f:param name="jsfcrud.currentCustomer" value="#{jsfcrud_class['e.util.JsfUtil'].jsfcrud_method['getAsConvertedString'][customer.customer][customer.converter].jsfcrud_invoke}"/>
                            <f:param name="jsfcrud.currentDiscountCode" value="#{jsfcrud_class['e.util.JsfUtil'].jsfcrud_method['getAsConvertedString'][customer.customer.discountCode][discountCode.converter].jsfcrud_invoke}"/>
                            <f:param name="jsfcrud.relatedController" value="customer"/>
                            <f:param name="jsfcrud.relatedControllerType" value="e.CustomerController"/>
                        </h:commandLink>
                        <h:outputText value=" )"/>
                    </h:panelGroup>
                </h:panelGroup>


            </h:panelGrid>
            <br />
            <h:commandLink action="#{customer.destroy}" value="Destroy">
                <f:param name="jsfcrud.currentCustomer" value="#{jsfcrud_class['e.util.JsfUtil'].jsfcrud_method['getAsConvertedString'][customer.customer][customer.converter].jsfcrud_invoke}" />
            </h:commandLink>
            <br />
            <br />
            <h:commandLink action="#{customer.editSetup}" value="Edit">
                <f:param name="jsfcrud.currentCustomer" value="#{jsfcrud_class['e.util.JsfUtil'].jsfcrud_method['getAsConvertedString'][customer.customer][customer.converter].jsfcrud_invoke}" />
            </h:commandLink>
            <br />
            <h:commandLink action="#{customer.createSetup}" value="New Customer" />
            <br />
            <h:commandLink action="#{customer.listSetup}" value="Show All Customer Items"/>
            <br />
            <br />
            <h:commandLink value="Index" action="welcome" immediate="true" />

        </h:form>
        </body>
    </html>
</f:view>
