<?php 

$files  = array();

//create an array of all the files in unused directory
foreach(glob('Stimlists/unused/*.*') as $filename){
    $files[] = $filename; 
}
//randomize order of files
shuffle($files);

//update the file's path location string 
$newPath = str_replace('unused', 'inuse', $files[0]);

//move file from current location to new location
rename($files[0], $newPath);

echo $newPath;

?>