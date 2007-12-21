<?php
include("dbname.php");
mysql_connect($dbhost,$username,$password);
@mysql_select_db($database) or die( "Unable to select database"); 
$name=$_POST["name"];
$category=$_POST["category"];
$price=$_POST["price"];
$query = "INSERT INTO products VALUES ('','$name','$category','$price')";
mysql_query($query);

mysql_close();

header ("Location: ./index.php");
?> 
