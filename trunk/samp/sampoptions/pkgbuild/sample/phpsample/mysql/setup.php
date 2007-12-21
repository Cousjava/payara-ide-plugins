<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<html>
<head>
  <title>Web Stack Sample Application</title>
  <meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
  <link rel="stylesheet" href="style.css">
  </head>
  <body>

<br>

<?php
include("dbname.php");

?>
Setting up database...

<?php


// Create database
if (mysql_query("CREATE DATABASE phpwebstacksample",$mysql_connection))
  {
  echo "<p>Database created</p>";
  echo "<p>Please, run the <a href=\"index.php\">index page</a> to see the database content.</p>";
  }
else
  {
  echo "<p>Error creating database: " . mysql_error() . "</p>";
  }

// Create table in my_db database
// TODO: this shouldnt run if the database creation fails, will not hurt though
mysql_select_db($database, $mysql_connection);

$query="CREATE TABLE products (
    id int(6) NOT NULL auto_increment,
    name varchar(15) NOT NULL,
    category varchar(20) NOT NULL,
    price varchar(20) NOT NULL,
    PRIMARY KEY (id),
    UNIQUE id (id),
    KEY id_2 (id))";

mysql_query($query,$mysql_connection);

mysql_close($mysql_connection);

?>
    </body>
</html>
        
