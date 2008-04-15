<%@page contentType="text/html"%>
<%@page pageEncoding="MacRoman"%>
<%@taglib uri="http://java.sun.com/jsf/core" prefix="f" %>
<%@taglib uri="http://java.sun.com/jsf/html" prefix="h" %>
<html>
<head>
 <meta http-equiv="Content-Type" content="text/html; charset=MacRoman" />
<title>Editing Book</title>
</head>
<body>
<f:view>
  <h:messages errorStyle="color: red" infoStyle="color: green" layout="table"/>
 <h1>Editing Book</h1>
<h:form>
<h:panelGrid columns="2">
<h:outputText value="BookId:"/>
 <h:outputText value="#{book.book.bookId}" title="BookId" />
<h:outputText value="Isbn:"/>
<h:inputText id="isbn" value="#{book.book.isbn}" title="Isbn" />
<h:outputText value="Authorid:"/>
<h:inputText id="authorid" value="#{book.book.authorid}" title="Authorid" />
<h:outputText value="Publisheddate (MM/dd/yyyy):"/>
<h:inputText id="publisheddate" value="#{book.book.publisheddate}" title="Publisheddate" >
<f:convertDateTime type="DATE" pattern="MM/dd/yyyy" />
</h:inputText>
<h:outputText value="Title:"/>
<h:inputText id="title" value="#{book.book.title}" title="Title" />
</h:panelGrid>
<br />
<h:commandLink action="#{book.edit}" value="Save">
<f:param name="jsfcrud.currentBook" value="#{book.asString[book.book]}"/>
</h:commandLink>
<br />
<br />
<h:commandLink action="#{book.detailSetup}" value="Show" immediate="true">
<f:param name="jsfcrud.currentBook" value="#{book.asString[book.book]}"/>
</h:commandLink>
<br />
<h:commandLink action="#{book.listSetup}" value="Show All Book Items" immediate="true"/>
<br />
<a href="/WebAppWithEJBS/faces/welcomeJSF.jsp">Index</a>
</h:form>
 </f:view>
</body>
</html>
