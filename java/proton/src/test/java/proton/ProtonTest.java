package proton;

import java.io.File;
import java.io.StringReader;

import junit.framework.TestCase;

import nu.xom.Builder;
import nu.xom.Document;
import nu.xom.Element;
import nu.xom.Nodes;

import org.xml.sax.XMLReader;
import org.xml.sax.helpers.XMLReaderFactory;

/**
 *
 * @author Jason R Briggs
 */
public abstract class ProtonTest extends TestCase {

    protected String baseDir;
    protected Templates templates;

    @Override
    public void setUp() throws Exception {
        //this.baseDir = "target" + File.separator + "test-classes";
        this.baseDir = "";
        this.templates = new Templates(baseDir);
    }

    public Document loadXml(Template tmp) throws Exception {
        String out = tmp.toString();

        XMLReader xmlReader = XMLReaderFactory.createXMLReader();
        xmlReader.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false);
        Builder parser = new Builder(xmlReader);
        return parser.build(new StringReader(out));
    }

    public String find(Document doc, String xpath) {
        Nodes nodes = doc.query(xpath);
        if (nodes == null || nodes.size() < 1) {
            return null;
        }
        else {
            return nodes.get(0).getValue();
        }
    }

    public String findAttribute(Document doc, String xpath, String attr) {
        Nodes nodes = doc.query(xpath);
        if (nodes == null || nodes.size() < 1) {
            return null;
        }

        Element node = (Element) nodes.get(0);
        if (node.getAttribute(attr) == null) {
            return null;
        }
        else {
            return node.getAttributeValue(attr);
        }
    }
}
