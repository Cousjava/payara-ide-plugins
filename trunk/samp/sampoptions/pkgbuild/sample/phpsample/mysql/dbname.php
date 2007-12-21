<?php
$username="root";
$password="";
$database="phpwebstacksample";
$dbhost="localhost";
$mysql_connection = mysql_connect($dbhost, $username, $password);
if (!$mysql_connection)
  {
  die('Could not connect: ' . mysql_error());
  }?> 
