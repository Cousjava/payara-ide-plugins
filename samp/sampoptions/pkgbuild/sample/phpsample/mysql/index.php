<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<html>
<head>
  <title>Web Stack Sample Application</title>
  <meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
  <link rel="stylesheet" href="style.css">
  </head>
  <body>

<?php
require_once("dbname.php");
?>


<?php
if ( !mysql_select_db($database, $mysql_connection) ) {
	?>
         
	<p>The database for this sample  does not exist.</p>
	<p>Please, run the <a href="setup.php>setup page.</a></p>
 
 <?php
	exit();
	}



@mysql_select_db($database);
$query="SELECT * FROM products";
$result=mysql_query($query);

$num=mysql_numrows($result); 

mysql_close();

echo "<b><center>Products</center></b><br><br>";

?>
<table border="0" cellspacing="2" cellpadding="2">
<tr> 
<th><font face="Arial, Helvetica, sans-serif">Name</font></th>
<th><font face="Arial, Helvetica, sans-serif">Category</font></th>
<th><font face="Arial, Helvetica, sans-serif">prices</font></th>

</tr>

<?php
$i=0;
while ($i < $num) {
$name=mysql_result($result,$i,"name");
$category=mysql_result($result,$i,"category");
$price=mysql_result($result,$i,"price");

?>

<tr> 
<td><font face="Arial, Helvetica, sans-serif"><?php echo "$name"; ?></font></td>
<td><font face="Arial, Helvetica, sans-serif"><?php echo "$category"; ?></font></td>
<td><font face="Arial, Helvetica, sans-serif"><?php echo "$price"; ?></font></td>

</tr>
<?php
++$i;
} 
echo "</table>";


?>
	<p><a href="add.html">Add a Product...</a></p>
    </body>
</html>
