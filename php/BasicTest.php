<?php

class BasicTest extends PHPUnit_Framework_TestCase {
    public function test() {
    
        $t = new Template('../resources/basic.xhtml');
        $t->set_no_content_names(array('br', 'hr', 'img', 'input'));

        $t->set_element('title', 'Basic Xhtml Page', '*');
        $t->set_element('content', 'Content goes here');
        $t->set_element('link', 'Link goes here');
        $t->set_attribute('link', 'href', 'http://www.duckduckgo.com');
    
        compare($this, $t, '../resources/basic-output.xhtml');
    }
}

?>