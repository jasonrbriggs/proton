<?php

include_once('proton/template.php');

$t = new Template('../python/test/repeat.xhtml');
$t->set_no_content_names(array('br', 'hr', 'img', 'input'));

$t->set_element('title', 'Repeating Xhtml Page', '*');
$t->set_element('link', 'This is a link to Google');
$t->set_attribute('link', 'href', 'http://www.google.com');

$t->repeat('list-item', 5);

for ($i = 0; $i < 5; $i++) {
    $t->set_element('list-item', 'test' . $i, $i);
}

echo $t;
?>