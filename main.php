#!/usr/bin/env php
<?php

$shortopts  = "";
$shortopts .= "f:t:";  // Required value
$shortopts .= "v::"; // Optional value
$shortopts .= "rpo"; // These options do not accept values

$longopts  = array(
    "required:",     // Required value
    "optional::",    // Optional value
    "option",        // No value
    "opt",           // No value
);

$options = getopt($shortopts, $longopts);
var_dump($options);

