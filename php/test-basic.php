<?php

include_once('proton/template.php');

$t = new Template('../python/test/basic.xhtml');
$t->set_no_content_names(array('br', 'hr', 'img', 'input'));

$t->set_element('title', 'An Xhtml Page', '*');
$t->set_element('content', 'Content replaced here');
$t->set_element('link', 'Link text replaced here');
$t->set_attribute('link', 'href', 'http://www.google.com');

echo $t;

?>