<?php
// the $_POST[] array will contain the passed in filename and data
// the directory "data" is writable by the server (chmod 777)
$filename = "data/".$_POST['filename'];
$data = $_POST['filedata'];
// write the file to disk
file_put_contents($filename, $data);


//move completed file into used directory
$csv = $_POST['csv'];

if (strpos($csv, 'inuse') !== false) {
	$newPath = str_replace('inuse', 'used', $csv);
	rename($csv, $newPath);
}


?>
