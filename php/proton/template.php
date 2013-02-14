<?php
class KeyVal {
    var $key;
    var $val;
    function KeyVal($key, $val) {
        $this->key = $key;
        $this->val = $val;
    }
}

class Template {
    var $eid = array();
    
    var $rid = array();
    var $aid = array();
    
    var $no_content_names = array();
    var $current_parent;
    
    function Template($filename) {
        $this->dom = new DOMDocument();
        $this->dom->load($filename);
        $this->doc = $this->dom->documentElement;
    }
    
    function set_no_content_names($no_content_names) {
        $this->no_content_names = $no_content_names;
    }
    
    function set_element($name, $value, $index = 0) {
        if (!is_numeric($index) && $index == '*') {
            $this->eid[$name] = $value;
        }
        else {
            if (!array_key_exists($name, $this->eid)) {
                $this->eid[$name] = array();
            }
            $this->eid[$name][$index] = $value;
        }
    }
    
    function set_attribute($name, $attr_name, $attr_value, $index = 0) {
        if (!is_numeric($index) && $index == '*') {
            $this->aid[$name] = new KeyVal($attr_name, $attr_value);
        }
        else {
            if (!array_key_exists($name, $this->aid)) {
                $this->aid[$name] = array();
            }
            $this->aid[$name][$index] = new KeyVal($attr_name, $attr_value);
        }
    }
    
    function repeat($name, $count, $index = 0) {
        if (!is_numeric($index) && $index == '*') {
            $this->rid[$name] = $count;
        }
        else {
            if (!array_key_exists($name, $this->rid)) {
                $this->rid[$name] = array();
            }
            $this->rid[$name][$index] = $count;
        }
    }
    
    function hide($name, $index = 0) {
        $this->set_element($name, '@@@@@HIDE@@@@@', $index);
    }
    
    private function parse($node) {
        if ($node->nodeType == XML_TEXT_NODE) {
            echo $node->nodeValue;
            return;
        }
        
        $close = true;
        $attrs = array();
        $content = null;

        if ($node->hasAttributes()) {
            foreach ($node->attributes as $attr) { 
                if ($attr->name == 'eid') {
                    $content = $this->process_eid($attr->value);
                    if ($content == '@@@@@HIDE@@@@@') {
                        $node->parentNode->removeChild($node);
                        return;
                    }
                    continue;
                }
                else if ($attr->name == 'aid') {
                    $this->process_aid($attr->value, $attrs);
                    continue;
                }
                else if ($attr->name == 'rid') {
                    $this->process_rid($attr->value, $node);
                    continue;
                }
                
                if (!array_key_exists($attr->name, $attrs)) {
                    $attrs[$attr->name] = $attr->value;
                }
            }
        }
        
        echo '<' , $node->tagName;
        
        if (count($attrs) > 0) {
            foreach ($attrs as $key => $val) {
                echo ' ' , $key , '="' , $val , '"';
            }
        }
        
        if ($content == null && !$node->hasChildNodes() && in_array($node->tagName, $this->no_content_names)) {
            echo ' />';
            $close = false;
        }
        else {
            echo '>';
        }
        
        if ($content != null) {
            echo $content;
        }
        else if ($node->hasChildNodes()) {
            $children = $node->childNodes; 
            for ($i=0; $i < $children->length; $i++) { 
                $child = $children->item($i);
                $this->parse($child);
            }
        }
        
        if ($close) {
            echo '</' , $node->tagName , '>';    
        }
    }
    
    private function process_eid($value) {
        if (array_key_exists($value, $this->eid)) {
            if (is_array($this->eid[$value])) {
                $rtn = current($this->eid[$value]);
                next($this->eid[$value]);
            }
            else {
                $rtn = $this->eid[$value];
            }
            return $rtn;
        }
        return null;
    }
    
    private function process_aid($value, &$attrs) {
        if (array_key_exists($value, $this->aid)) {
            if (is_array($this->aid[$value])) {
                $keyval = current($this->aid[$value]);
                next($this->aid[$value]);
            }
            else {
                $keyval = $this->aid[$value];
            }

            $attrs[$keyval->key] = $keyval->val;
        }
    }
    
    private function process_rid($value, &$node) {
        if (array_key_exists($value, $this->rid)) {
            if (is_array($this->rid[$value])) {
                $count = current($this->rid[$value]);
                next($this->rid[$value]);
            }
            else {
                $count = $this->rid[$value];
            }
            
            $parent = $node->parentNode;
            $children = array();
            for ($i = 0; $i < $count-1; $i++) {
                $child = $node->cloneNode(true);
                $child->removeAttribute('rid');
                $children[] = $child;
            }
            
            foreach ($children as $child) {
                $parent->appendChild($child);
            }
        }
    }
    
    function __toString() {
        ob_start();
        
        $this->parse($this->doc);
    
        $rtn = ob_get_contents();
        ob_end_clean();
    
        return $rtn;
    }
}


?>