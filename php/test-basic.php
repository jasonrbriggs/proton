<?php

include_once('proton/template.php');

$t = new Template('../python/test/basic.xhtml');

$t->set_element('title', 'An Xhtml Page', '*');
$t->set_element('content', 'Content goes here');
$t->set_element('link', 'Link goes here');
$t->set_attribute('link', 'href', 'http://www.google.com');

echo $t;

?>