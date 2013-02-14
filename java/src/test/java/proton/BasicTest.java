package proton;

import proton.Template;
import nu.xom.Document;
import nu.xom.Element;
import nu.xom.Nodes;

/**
 * Basic unit test for Clover.
 */
public class BasicTest extends ProtonTest {

    public void testBasic() throws Exception {
        Template tmp = templates.get("basic.xhtml");
        tmp.setElement("title", "Basic Xhtml Page", -1);
        tmp.setElement("content", "Content goes here");
        tmp.setElement("link", "Link goes here");
        tmp.setAttribute("link", "href", "http://www.google.com");

        System.out.println(tmp.toString());
        Document doc = loadXml(tmp);
        
        assertEquals("Basic Xhtml Page", find(doc, "//head/title"));
        assertEquals("Basic Xhtml Page", find(doc, "//body/h1"));
        assertEquals("Content goes here", find(doc, "//body/div"));
        assertEquals("Link goes here", find(doc, "//body/a"));
        assertEquals("http://www.google.com", findAttribute(doc, "//body/a", "href"));
    }

    public void testHiding1() throws Exception {
        Template tmp = templates.get("hiding.xhtml");
        tmp.setElement("title", "Hiding Xhtml Page");
        tmp.hideElement("hidden-element");

        Document doc = loadXml(tmp);
        System.out.println(tmp.toString());

        assertTrue(doc.query("//body/div").size() == 0);
    }

    public void testHiding2() throws Exception {
        Template tmp = templates.get("hiding2.xhtml");
        tmp.setElement("title", "Hiding Example 2");
        tmp.hideElement("autopayments");
        tmp.hideElement("exchange");
        tmp.hideElement("transactions");

        Document doc = loadXml(tmp);
        System.out.println(tmp.toString());

        Nodes anchorNodes = doc.query("//body/ul/li");
        assertEquals(3, anchorNodes.size());

        anchorNodes = doc.query("//a");
        Element anchor1 = (Element) anchorNodes.get(0);
        assertEquals("/accounts", anchor1.getAttribute("href").getValue());

        Element anchor2 = (Element) anchorNodes.get(1);
        assertEquals("/transfer", anchor2.getAttribute("href").getValue());

        Element anchor3 = (Element) anchorNodes.get(2);
        assertEquals("/bills", anchor3.getAttribute("href").getValue());
    }

    public void testRepetition() throws Exception {
        Template tmp = templates.get("repeat.xhtml");
        tmp.setElement("title", "Repeating Xhtml Page");
        tmp.setElement("link", "This is a link to Google");
        tmp.setAttribute("link", "href", "http://www.google.com");

        tmp.repeatElement("list-item", 5);
        for (int i = 1; i < 6; i++) {
            tmp.setElement("list-item", "test" + i, i);
        }

        Document doc = loadXml(tmp);
        System.out.println(tmp.toString());

        Nodes listNodes = doc.query("//body/ul/li");
        assertEquals(5, listNodes.size());

        for (int i = 0; i < listNodes.size(); i++) {
            Element listNode = (Element) listNodes.get(i);
            assertEquals("test" + (i+1), listNode.getValue());
        }
    }

    public void testEmpty() throws Exception {
        Template tmp = templates.get("empty.xhtml");
        tmp.setElement("title", "Empty Element Test");

        Document doc = loadXml(tmp);
        System.out.println(tmp.toString());
    }
}
