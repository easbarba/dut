#!/usr/bin/env php
<?php
use function PHPSTORM_META\override;

$shortopts = "";
$shortopts .= "t:";
$shortopts .= "f:";
$shortopts .= "i::";
$shortopts .= "crpo";

$longopts = [
    "to:",
    "from:",
    "create",
    "remove",
    "pretend",
    "overwrite",
    "info",
    "info::",
];

// class Core {}

class Actions
{
    // function __construct() {}

    public function create(): void
    {
        print_r("In the beginning, there was nothing! Let there be links!");
    }

    public function remove(): void
    {
        print_r('No, no, no, no, no, no, Aren\'t you something');
    }

    public function pretend(): void
    {
        print_r("If you dont say, I wont say too, and no one will know!");
    }

    public function overwrite(): void
    {
        print_r(
            "oh no, something is wrong, be a dear and overwrite it for me?!"
        );
    }

    public function info(): void
    {
        print_r("Of course my heart, whatever you want!");
    }
}

# Parsing!
$options = getopt($shortopts, $longopts);

# Run for it!
$actions = new Actions();
if (array_key_exists("create", $options)) {
    $actions->create();
}
if (array_key_exists("remove", $options)) {
    $actions->remove();
}
if (array_key_exists("pretend", $options)) {
    $actions->pretend();
}
if (array_key_exists("overwrite", $options)) {
    $actions->overwrite();
}
if (array_key_exists("info", $options)) {
    $actions->info();
}
