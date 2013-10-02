package proton;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.lang.reflect.InvocationTargetException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.Set;

import nu.xom.Attribute;
import nu.xom.Document;
import nu.xom.Element;
import nu.xom.Node;
import nu.xom.ParentNode;
import nu.xom.ParsingException;

import nu.xom.Serializer;
import org.xml.sax.SAXException;

/**
 * Object encapsulating the logic for an XML template.
 * 
 * @author Jason R Briggs
 */
public class Template {

    /**
     * Hidden element marker
     */
    private static final HiddenElement HIDDEN = new HiddenElement();

    /**
     * The basic list of translated elements
     */
    private static final Set<String> TRANSLATED_ELEMENTS = Utils.toSet("h1", "h2", "h3", "h4", "h5", "p", "span", "th", "td", "button", "title");

    private static final Set<String> BODY_REQUIRED_ELEMENTS = Utils.toSet("textarea", "iframe", "a", "p", "div", "td", "th");

    private XMLLoader xmlLoader;                                        // reference to the common xml loader
    private Document doc;
    private Element root;                                               // root xml element

    private HashMap<String, IndexedValue> elements;                     // a map of elements to be processed
    private HashMap<String, Map<String, IndexedValue>> attributes;      // a map of xml attributes to be processed
    private HashMap<String, IndexedValue> repeats;                      // a map of elements that need to be repeated

    private Set<String> translatedElements;                             // the set of elements that should be translated
    private ResourceBundle resources = null;                            // the resource bundle to use for translation

    private boolean processed = false;                                  // one-shot use, to stop the 'process' method being called more than once

    /**
     * Default constructor, passed a reference to the {@link XMLLoader}
     */
    private Template(XMLLoader xmlLoader) {
        this.xmlLoader = xmlLoader;
        this.elements = new HashMap<String, IndexedValue>();
        this.attributes = new HashMap<String, Map<String, IndexedValue>>();
        this.repeats = new HashMap<String, IndexedValue>();
        this.translatedElements = TRANSLATED_ELEMENTS;
    }

    /**
     * Construct a new template with a specific path.
     *
     * @param templates reference to the parent {@link Templates} holder.
     * @param templatePath the path to the template file
     */
    public Template(Templates templates, String templatePath) throws ParsingException, IOException, SAXException {
        this(templates.getLoader());
        this.doc = xmlLoader.load(templatePath);
        this.root = doc.getRootElement();
    }

    /**
     * Construct a new template with a specific {@link Document}.
     */
    private Template(Templates templates, Document doc) {
        this(templates.getLoader());
        this.doc = doc;
        this.root = doc.getRootElement();
    }

    /**
     * Return a copy of this template.
     */
    public Template copy() {
        Template tmp = new Template(xmlLoader);
        tmp.doc = (Document) doc.copy();
        tmp.root = tmp.doc.getRootElement();
        return tmp;
    }

    /**
     * Set the {@link ResourceBundle} to use for translating this template.
     */
    public void setResources(ResourceBundle resources) {
        this.resources = resources;
    }

    /**
     * Set the first occurence of an element to a specified value.
     *
     * @param eid the eid of the element to set
     * @param content the data to set as the value of this element
     */
    public void setElement(String eid, Object content) {
        try {
            setElement(eid, content, 0, false);
        } catch (IllegalAccessException iae) {
        } catch (InvocationTargetException ite) { }
    }

    public void setElement(String eid, Object content, int index) {
        try {
            setElement(eid, content, index, false);
        } catch (IllegalAccessException iae) {
        } catch (InvocationTargetException ite) { }
    }

    /**
     * Set an element to a specified value.
     *
     * @param eid the eid of the element to set
     * @param content the data to set as the value of this element
     * @param the specific occurrence to set (from 0-n, if less than 0 then set all elements with matching eid)
     */
    public void setElement(String eid, Object content, int index, boolean introspect) throws IllegalAccessException, InvocationTargetException {
        if (content == null) {
            return;
        }
        
        if (content instanceof Collection) {
            Object[] arr = ((Collection) content).toArray();
            repeatElement(eid, arr.length);
            for (int i = 0; i < arr.length; i++) {
                setElement(eid, arr[i], index + i, introspect);
            }
            return;
        }

        if (!elements.containsKey(eid)) {
            elements.put(eid, new IndexedValue());
        }

        IndexedValue value = elements.get(eid);
        value.set(content, index);

        if (introspect) {
            Map<String, Object> props = Utils.invokeGetters(content);
            for (Map.Entry<String, Object> entry : props.entrySet()) {
                setElement(eid + ":" + entry.getKey(), entry.getValue(), index, false);
                setAttribute(eid, entry.getKey(), entry.getValue().toString(), index);
            }
        }
    }

    /**
     * Set the first occurence of an attribute to a specified value.
     *
     * @param aid the aid of the element to set an attribute on
     * @param attrName the name of the attribute to set
     * @param content the attribute value -- if an empty string, this is equivalent to removing the attribute
     */
    public void setAttribute(String aid, String attrName, String content) {
        setAttribute(aid, attrName, content, 0);
    }

    /**
     * Set an attribute to a specified value.
     * 
     * @param aid the aid of the element to set an attribute on
     * @param attrName the name of the attribute to set
     * @param content the attribute value -- if an empty string, this is equivalent to removing the attribute
     * @param index the specific occurrence to set (from 1-n, if less than 1 then set all elements with the matching aid)
     */
    public void setAttribute(String aid, String attrName, String content, int index) {
        if (!attributes.containsKey(aid)) {
            attributes.put(aid, new HashMap<String, IndexedValue>());
        }

        Map<String, IndexedValue> attribs = attributes.get(aid);
        if (!attribs.containsKey(attrName)) {
            attribs.put(attrName, new IndexedValue());
        }

        IndexedValue value = attribs.get(attrName);
        value.set(content, index);
    }

    /**
     * Hide all elements with the matching eid.
     */
    public void hideElement(String eid) {
        hideElement(eid, -1);
    }

    /**
     * Hide the specific occurrence of an element with the matching eid.
     *
     * @param eid the eid of the attribute to hide
     * @param index the specific occurrence (from 0-n)
     */
    public void hideElement(String eid, int index) {
        try {
            setElement(eid, HIDDEN, index, false);
        }
        catch (IllegalAccessException iae) {
        }
        catch (InvocationTargetException ite) {
        }
    }

    /**
     * Repeat the first occurrence of an attirbute with the matching eid.
     *
     * @param eid the eid of the element to repeat
     * @param count the number of times this element should be repeated
     */
    public void repeatElement(String eid, int count) {
        repeatElement(eid, count, 0);
    }

    /**
     * Repeat a specified occurrence of an attribute with the matching eid.
     *
     * @param eid the eid of the element to repeat
     * @param count the number of times this element should be repeated
     * @param index the specific occurrence (from 0-n)
     */
    public void repeatElement(String eid, int count, int index) {
        if (!repeats.containsKey(eid)) {
            repeats.put(eid, new IndexedValue());
        }

        IndexedValue value = repeats.get(eid);
        value.set(new RepeatedElement(count), index);
    }

    /**
     * Include a template at the specified element location.
     *
     * @param eid the eid of the element to replace with a new template
     * @param templatePath location of the template file
     */
    public void include(String eid, String templatePath) throws ParsingException, IOException, SAXException {
        Document newDoc = xmlLoader.load(templatePath);
        setElement(eid, newDoc.getRootElement().copy());
    }

    /**
     * Process the template, checking each element for "rid" (repeated elements), "eid" (content, hidden), and "aid" (attributes).
     */
    private void process(Element node) {
        // repeated elements
        if (node.getAttribute("rid") != null) {
            Attribute attr = node.getAttribute("rid");
            node.removeAttribute(attr);

            String rid = attr.getValue();

            if (repeats.containsKey(rid)) {
                IndexedValue value = repeats.get(rid);

                RepeatedElement val = (RepeatedElement) value.pop();
                for (int i = 0; i < val.getCount() - 1; i++) {
                    ParentNode parent = node.getParent();
                    parent.appendChild(node.copy());
                }
                process((Element) node.getParent());
                return;
            }
        }

        // translation
        if (resources != null && node.getValue() != null && translatedElements.contains(node.getLocalName())) {
            String val = node.getValue();
            try {
                val = resources.getString(val.replace(' ', '+'));
                node.removeChildren();
                node.appendChild(val);
            }
            catch (MissingResourceException mre) { }
        }

        // element processing
        if (node.getAttribute("eid") != null) {
            Attribute attr = node.getAttribute("eid");
            node.removeAttribute(attr);

            String eid = attr.getValue();

            if (elements.containsKey(eid)) {
                IndexedValue value = elements.get(eid);

                Object val = value.pop();
                if (val != null) {
                    if (val instanceof HiddenElement) {
                        node.getParent().removeChild(node);
                    }
                    else if (val instanceof Element) {
                        ParentNode parent = node.getParent();
                        Element elem = (Element) val;
                        parent.replaceChild(node, elem);
                        process((Element) elem);
                        return;
                    }
                    else {
                        node.removeChildren();
                        node.appendChild(val.toString());
                    }
                }
            }
        }

        // attribute processing
        if (node.getAttribute("aid") != null) {
            Attribute attr = node.getAttribute("aid");
            node.removeAttribute(attr);

            String aid = attr.getValue();

            if (attributes.containsKey(aid)) {
                Map<String, IndexedValue> attribs = attributes.get(aid);

                for (int i = 0; i < node.getAttributeCount(); i++) {
                    attr = node.getAttribute(i);

                    if (!attribs.containsKey(attr.getLocalName())) {
                        continue;
                    }

                    IndexedValue value = attribs.get(attr.getLocalName());
                    String val = (String) value.pop();

                    if (val == null) {
                        continue;
                    }
                    else if (val.equals("")) {
                        node.removeAttribute(attr);
                    }
                    else {
                        attr.setValue(val);
                    }
                }
            }
        }

        // do this at the moment, because I can't figure out how to get XOM to serialize xhtml properly
        if (node.getValue() == null || node.getValue().equals("") && BODY_REQUIRED_ELEMENTS.contains(node.getLocalName())) {
            node.appendChild("\n");
        }

        // process the children of this node
        for (int i = 0; i < node.getChildCount(); i++) {
            Node child = node.getChild(i);
            if (child instanceof Element) {
                process((Element) node.getChild(i));
            }
        }
    }


    @Override
    public String toString() {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        toString(baos);
        return baos.toString();
    }

    public void toString(OutputStream outputStream) {
        if (!processed) {
            process(root);
            processed = true;
        }

        try {
            Serializer serializer = new Serializer(outputStream, "utf-8");
            serializer.write(doc);
        }
        catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Marker class for hidden elements.
     */
    private static class HiddenElement {
    }

    /**
     * Wrapper class for repeated elements.
     */
    private class RepeatedElement {
        private int count;

        public RepeatedElement(int count) {
            this.count = count;
        }

        public int getCount() {
            return count;
        }
    }

}