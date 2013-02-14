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
    
    var $chardata = null;
    
    function Template($filename) {
        $this->parser = xml_parser_create();
        
        xml_parser_set_option($this->parser, XML_OPTION_CASE_FOLDING, 0);

        //Specify element handler
        xml_set_element_handler($this->parser, array(&$this, "start"), array(&$this, "stop"));

        //Specify data handler
        xml_set_character_data_handler($this->parser, array(&$this, "char"));

        //Open XML file
        $this->fp = fopen($filename, "r");
    }
    
    private function start($parser, $element_name, $element_attrs) {
        echo '<' , $element_name;
        
        $attrs = array();
        if (count($element_attrs) > 0) {
            foreach ($element_attrs as $key => $value) {
                if ($key == 'eid') {
                    $this->process_eid($value);
                    continue;
                }
                else if ($key == 'aid') {
                    $this->process_aid($value, &$attrs);
                    continue;
                }
                else if ($key == 'rid') {
                    $this->process_rid($value);
                    continue;
                }
                
                if (!array_key_exists($key, $attrs)) {
                    $attrs[$key] = $value;
                }
            }
        }
        
        if (count($attrs) > 0) {
            foreach ($attrs as $key => $val) {
                echo ' ' , $key , '="' , $val , '"';
            }
        }
        
        echo '>';
    }

    private function stop($parser, $element_name) {
        echo '</' , $element_name , '>';
    }

    private function char($parser, $data) {
        if ($this->chardata != null) {
            echo $this->chardata;
            $this->chardata = null;
        }
        else {
            echo $data;
        }
    }
    
    private function process_eid($value) {
        if (array_key_exists($value, $this->eid)) {
            if (is_array($this->eid[$value])) {
                $this->chardata = current($this->eid[$value]);
                next($this->eid[$value]);
            }
            else {
                $this->chardata = $this->eid[$value];
            }
        }
    }
    
    private function process_aid($value, $attrs) {
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
    
    private function process_rid($value) {
        if (array_key_exists($value, $this->rid)) {
            if (is_array($this->rid[$value])) {
                $count = current($this->rid[$value]);
                next($this->rid[$value]);
            }
            else {
                $count = $this->rid[$value];
            }
            
        }
    }
    
    function set_element($name, $value, $index = 0) {
        if ($index == '*') {
            $this->eid[$name] = $value;
        }
        else {
            if (!array_key_exists($name, $this->eid)) {
                $this->eid[$name] = array_fill(0, $index, null);
            }
            $this->eid[$name][$index] = $value;
        }
    }
    
    function set_attribute($name, $attr_name, $attr_value, $index = 0) {
        if ($index == '*') {
            $this->aid[$name] = new KeyVal($attr_name, $attr_value);
        }
        else {
            if (!array_key_exists($name, $this->aid)) {
                $this->aid[$name] = array_fill(0, $index, null);
            }
            $this->eid[$name][$index] = new KeyVal($attr_name, $attr_value);
        }
    }
    
    function __toString() {
        ob_start();
        while ($data = fread($this->fp, 4096)) {
            xml_parse($this->parser, $data, feof($this->fp)) or 
                die (sprintf("XML Error: %s at line %d", 
                xml_error_string(xml_get_error_code($parser)),
                xml_get_current_line_number($parser)));
        }

        $rtn = ob_get_contents();
        ob_end_clean();

        // Free the XML parser
        xml_parser_free($this->parser);
    
        return $rtn;
    }
}


?>