<?php

$dateStr=date("Y-m-d-H-i-s");

$logHome = "../../data/cl2";

$f=fopen($logHome . "/uploadlog.txt","a");
dbgPrint("\n".$dateStr."------------------------------------------",$f);

function dbgPrint($obj,$f) {
  echo "\n";
  print_r($obj);
  echo "\n";

  if ($f) { 
    fwrite($f,print_r($obj,true)."\n");
  }
}

function statusWrite($text) {
  global $dateStr;
  $sf = fopen("../../data/cl2/status.txt", "w");
  fwrite($sf, "LAST UPLOAD STATUS\n");
  fwrite($sf, "Time: " . $dateStr . "\n");
  fwrite($sf, "Status: " . $text . "\n");
  fclose($sf);
}

if ( isset($_FILES) &&
     isset($_FILES["logdata"]) &&
     isset($_FILES["logdata"]["tmp_name"]) &&
     isset($_FILES["logdata"]["name"]) ) {

  dbgPrint("HTTP upload fields in \$_FILES were correct",$f);
  
  if($_FILES["logdata"]["error"] == UPLOAD_ERR_OK) {
    $logTmpName=$_FILES['logdata']['tmp_name'];
    $logName=$_FILES['logdata']['name'];

    dbgPrint("HTTP upload status was OK",$f);
    
    $matches=array();
    if ( preg_match("/^([_a-zA-Z0-9]{1,32})[.]db$/", $logName, $matches)==1 ) {
      dbgPrint("Uploading user's identity analyzed (".$matches[1].")",$f);

      $logFileName="log--" . $matches[1] . "--".$dateStr.".db";
      $logFullPath = $logHome."/".$logFileName;
      move_uploaded_file($logTmpName, $logFullPath)
        or die("mv");

      dbgPrint("Raw log file stored successfully in " . $logFullPath, $f);
      statusWrite("success: file stored as " . $logFileName);
    }
    else {
      header("Status: 400 Bad request",true,400);
      dbgPrint("ERROR: could not identify username from logName=".$logName.". Matches were:\n".print_r($matches,true),$f);      
      statusWrite("error: could not identify user");
    }
  }
  else {
    header("Status: 400 Bad request",true,400);
    dbgPrint("ERROR in upload, error code=".$_FILES["logdata"]["error"].
             "\n\$_FILES=".print_r($_FILES,true),$f);
    statusWrite("bad request");
  }
}
else {
  header("Status: 400 Bad request",true,400);
  dbgPrint("ERROR in \$_FILES. \$_SERVER=".print_r($_SERVER,true).", \$_FILES=".print_r($_SERVER,true),$f);
  statusWrite("bad request");
}

dbgPrint("Closing the log file",$f);
fclose($f);

/**

Copyright 2010 Helsinki Institute for Information Technology (HIIT)
and the authors. All rights reserved.

Authors: Antti Salovaara <antti.salovaara@hiit.fi>
         Tero Hasu <tero.hasu@hut.fi>

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation files
(the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge,
publish, distribute, sublicense, and/or sell copies of the Software,
and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

 **/

?>
