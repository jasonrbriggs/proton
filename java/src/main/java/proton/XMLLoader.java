package proton;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import nu.xom.Builder;
import nu.xom.Document;
import nu.xom.ParsingException;

import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.XMLReaderFactory;

/**
 * Loader for returning a copy of specified xml files.
 * 
 * @author Jason R Briggs
 */
public class XMLLoader {

    private String basePath = "";

    private Map<String, Document> loaded = new HashMap<String, Document>();

    /**
     * Default constructor
     */
    public XMLLoader(String basePath) {
        if (!basePath.equals("")) {
            basePath = basePath.replace('\\', '/');
            this.basePath = basePath + "/";
        }
    }

    /**
     * Load the specified xml and return a deep copy (storing the loaded file for next time)
     */
    public Document load(String path) throws SAXException, IOException, ParsingException {
        String fpath = basePath + path;

        if (!loaded.containsKey(fpath)) {
            XMLReader xmlReader = XMLReaderFactory.createXMLReader();
            xmlReader.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false);
            Builder parser = new Builder(xmlReader);
            Document doc = parser.build(getClass().getClassLoader().getResourceAsStream(fpath));

            loaded.put(fpath, doc);
        }

        return (Document) loaded.get(fpath).copy();
    }
}