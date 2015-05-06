<?php

function strip_whitespace($string) {
    return preg_replace("/[\s\t]/", "", $string);
}

function compare($test, $content, $compare_file) {
    $compare = strip_whitespace(file_get_contents($compare_file));
    $content = strip_whitespace($content);
    
    $test->assertEquals($compare, $content);
}

?>