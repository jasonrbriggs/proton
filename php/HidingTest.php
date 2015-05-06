<?php

class HidingTest extends PHPUnit_Framework_TestCase {
    public function test() {

        $t = new Template('../python/test/hiding.xhtml');
        $t->set_no_content_names(array('br', 'hr', 'img', 'input'));

        $t->set_element('title', 'Hiding Xhtml Page', '*');
        $t->hide('hidden-element');

        // echo $t;

        //echo "\n\n------------------------------------------------\n\n";

        $t = new Template('../python/test/hiding2.xhtml');
        $t->set_no_content_names(array('br', 'hr', 'img', 'input'));

        $t->set_element('title', 'Navigation Example', '*');
        $t->hide('autopayments');
        $t->hide('exchange');
        $t->hide('transactions');

    }
}
?>