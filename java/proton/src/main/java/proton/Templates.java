package proton;

import java.io.IOException;
import java.util.HashMap;

import nu.xom.ParsingException;
import org.xml.sax.SAXException;

/**
 * Factory class for {@link Template} objects.
 * 
 * @author Jason R Briggs
 */
public class Templates {

    private HashMap<String, Template> templates;

    private XMLLoader xmlLoader;

    public Templates() {
        this.templates = new HashMap<String, Template>();
        setPath(".");
    }

    /**
     * Construct the factory with the specified base path.
     */
    public Templates(String path) {
        this.templates = new HashMap<String, Template>();
        setPath(path);
    }

    public void setPath(String path) {
        this.xmlLoader = new XMLLoader(path);
    }

    /**
     * Return a template with the specified path.
     *
     * @param templatePath the location of the template file.
     */
    public Template get(String templatePath) throws ParsingException, IOException, SAXException {
        if (!templates.containsKey(templatePath)) {
            Template template = new Template(this, templatePath);
            templates.put(templatePath, template);
        }
        return templates.get(templatePath).copy();
    }

    /**
     * Return the {@link XMLLoader} to use with all child templates.
     */
    protected XMLLoader getLoader() {
        return xmlLoader;
    }
}