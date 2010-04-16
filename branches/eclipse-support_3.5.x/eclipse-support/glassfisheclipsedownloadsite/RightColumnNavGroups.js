function writeRHSColumn() {
      Date.prototype.getWeek = function() 
      {
        var onejan = new Date(this.getFullYear(),0,1);
        return Math.ceil((((this - onejan) / 86400000) + onejan.getDay())/7);
      }

	if (typeof oenodownloadsbox == "undefined" || !oenodownloadsbox) {
		document.write('<div class="orangesidebar">');
		document.write('	<div class="cornerTL"><div class="cornerTR"></div></div>');
		document.write('	<div class="title">Download GlassFish&nbsp;ESB</div>');
		document.write('	<div class="copy">Be up and running in mere minutes...<br>');
		document.write('	<a href="https://open-esb.dev.java.net/Downloads.html">Download GlassFish&nbsp;ESB</a></div>');
		document.write('	<div class="cornerBL"><div class="cornerBR"></div></div>');
		document.write('</div>');
	}
	
	
	document.write('<div class="bluesidebar2">');
	document.write('	<div class="cornerTL"><div class="cornerTR"></div></div>');
	document.write('	<div class="title"><font style="font-size:16pt; color: #ED9B4F">Now</font> <font style="font-size:40pt; color: #3E6A8F">45</font></div>');
	document.write('	<div class="copy"><font style="font-size:16pt; font-weight: bold; color: #ED9B4F">components</font> <br>(and counting!)</div>');
	document.write('	<div class="copy">See all 45 components<br> in the <a href="https://open-esb.dev.java.net/Components.html">components catalog</a></div>');
	document.write('	<div class="cornerBL"><div class="cornerBR"></div></div>');
	document.write('</div>');
	
	
	document.write('<div class="orangesidebar">');
	document.write('	<div class="cornerTL"><div class="cornerTR"></div></div>');
	document.write('	<div class="title">Commercial support</div>');
	document.write('	<div class="copy">Read more about <a href="http://www.sun.com/glassfishesb/">commercial support by Sun&nbsp;Microsystems</a>');
	document.write('	</div>');
	document.write('	<div class="cornerBL"><div class="cornerBR"></div></div>');
	document.write('</div>');
	
	
	var today       = new Date();
	var week_number = today.getWeek();
	var mod_result  = week_number % 7;
	
	document.write('<div class="bluesidebar">');
	document.write('	<div class="cornerTL"><div class="cornerTR"></div></div>');
	document.write('	<div class="pad">');
	document.write('		<span style="font-weight: bold; color: #ED9B4F">Featured partner:</span>');
	document.write('		<span style="font-size: 0.85em; "><br>');
	switch (mod_result)
	{
	  case 0:
		document.write("            <a href='http://www.imolinfo.it/index_en.php'><img src='https://open-esb.dev.java.net/images/partners/Imola-small-gray-background.gif'></a> ");
		document.write('		<br>');
		document.write("              <strong>Imola Informatica Srl</strong> uses Open ESB technology to develop open-source plugins for jbi4cics and jbi4corba connectors.<br><a href='http://www.imolinfo.it/index_en.php'>Read more...</a><br/>");
	    break;
	
	  case 1:
		document.write("            <a href='http://www.bostechcorp.com'><img src='https://open-esb.dev.java.net/images/partners/BostechCorpSmallLogo.gif'></a> ");
		document.write('		<br>');
		document.write("              <strong>Bostech Corporation</strong> helps businesses seamlessly share, exchange, and transact critical information with their customers, supliers, partners and employees.<br><a href='http://www.bostechcorp.com'>Read more...</a><br/>");
	    break;
	    
	  case 2:
		document.write("            <a href='http://www.advantech.co.il/en/inner_Java.html'><img src='https://open-esb.dev.java.net/images/partners/Advantech_Small.gif'></a> ");
		document.write('		<br>');
		document.write("              <strong>AdvanTech</strong> Java division develops Java/J2EE based solutions and implements middleware products for mediating business processes, services and applications.<br><a href='http://www.advantech.co.il/en/inner_Java.html'>Read more...</a><br/>");
	    break;      
	
	  case 3:
		document.write("            <a href='http://www.Eviware.com'><img src='https://open-esb.dev.java.net/images/partners/Eviware-small.gif'></a> ");
		document.write('		<br>');
		document.write("              <strong>Eviware</strong> is the company behind soapUI, the popular Open Source Web Service testing tool.<br><a href='http://www.Eviware.com'>Read more...</a><br/>");
	    break;
	
	  case 4:
		document.write("            <a href='http://www.gestalt-llc.com'><img src='https://open-esb.dev.java.net/images/partners/Gestalt-small-gray-background.gif'></a> ");
		document.write('		<br>');
		document.write("              <strong>Gestalt, LLC</strong> helps its clients develop practical solutions that drive competitive advantage in complex decision environments.<br><a href='http://www.gestalt-llc.com'>Read more...</a><br/>");
	    break;
	    
	  case 5:
		document.write("            <a href='http://www.pymma.com/'><img src='https://open-esb.dev.java.net/images/partners/pymma117.png'></a> ");
		document.write('		<br>');
		document.write("              <strong>Pymma Consulting</strong> helps customers design scalable enterprise architectures, increase their applications performance, solve system integration issues and reduce their time-to-market.<br><a href='http://www.pymma.com/'>Read more...</a><br/>");
	    break;
	    
	  
	  default:
		document.write("            <a href='http://www.imolinfo.it/index_en.php'><img src='https://open-esb.dev.java.net/images/partners/Imola-small-gray-background.gif'></a> ");
		document.write('		<br>');
		document.write("              <strong>Imola Informatica Srl</strong> uses Open ESB technology to develop open-source plugins for jbi4cics and jbi4corba connectors.<br><a href='http://www.imolinfo.it/index_en.php'>Read more...</a><br/>");
	    break;
	}
	document.write('</span>');
	document.write('	</div>');
	document.write('	<div class="cornerBL"><div class="cornerBR"></div></div>');
	document.write('</div>');

}

// Write it by default unless there's a variable dontWriteRHSColumn defined before the script was imported
if (typeof dontWriteRHSColumn == "undefined" || dontWriteRHSColumn == undefined || dontWriteRHSColumn == false) {
	writeRHSColumn();
}
