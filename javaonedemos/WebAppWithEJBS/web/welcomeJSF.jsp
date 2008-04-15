<%@page contentType="text/html"%>
<%@page pageEncoding="MacRoman"%>

<%@taglib prefix="f" uri="http://java.sun.com/jsf/core"%>
<%@taglib prefix="h" uri="http://java.sun.com/jsf/html"%>

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"
   "http://www.w3.org/TR/html4/loose.dtd">

<%--
    This file is an entry point for JavaServer Faces application.
--%>

<html>
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=MacRoman">
        <title>JSP Page</title>
    </head>
    <body>
        <f:view>
            <h:form>
<h1><h:outputText value="JavaServer Faces" /></h1>
    <br/>
<h:commandLink action="#{book.listSetup}" value="Show All Book Items"/>

    <br/>
<h:commandLink action="#{author.listSetup}" value="Show All Author Items"/>
</h:form>

        </f:view>
    </body>
</html>
