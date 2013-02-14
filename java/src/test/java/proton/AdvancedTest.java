package proton;

import proton.Template;
import java.util.Arrays;
import java.util.List;
import nu.xom.Document;
import nu.xom.Element;
import nu.xom.Nodes;

/**
 *
 * @author Jason R Briggs
 */
public class AdvancedTest extends ProtonTest {

    public void testInclude() throws Exception {
        Template tmp = templates.get("include1.xhtml");
        tmp.setElement("title", "Include Test");
        tmp.include("include-content", "include2.xhtml");
        tmp.setElement("para1", "First paragraph of text");
        tmp.setElement("para2", "Second paragraph of text");

        String out = tmp.toString();
        System.out.println(out);

        Document doc = loadXml(tmp);

        assertEquals("Include Test", find(doc, "//body/h1"));
        Nodes paraNodes = doc.query("//body/div/p");
        assertEquals(2, paraNodes.size());
        assertEquals("First paragraph of text", paraNodes.get(0).getValue());
        assertEquals("Second paragraph of text", paraNodes.get(1).getValue());
    }

    private void applyTwoTemplatesData(Template tmp) {
        tmp.repeatElement("list", 2);
        for (int i = 1; i < 3; i++) {
            tmp.setElement("listid", Integer.toString(i), i);
            tmp.setAttribute("listid", "id", Integer.toString(i), i);
            tmp.setElement("listval", "my item " + i, i);
        }
    }

    public void testTwoTemplates() throws Exception {
        Template tmp1 = templates.get("twotemplates.xhtml");
        applyTwoTemplatesData(tmp1);

        String out1 = tmp1.toString();
        System.out.println(out1);

        Document doc1 = loadXml(tmp1);
        Nodes tdNodes = doc1.query("//body/table/tr/td");
        assertEquals("1", tdNodes.get(1).getValue());
        assertEquals("my item 1", tdNodes.get(3).getValue());
        assertEquals("2", tdNodes.get(5).getValue());
        assertEquals("my item 2", tdNodes.get(7).getValue());

        Template tmp2 = templates.get("twotemplates.xml");
        applyTwoTemplatesData(tmp2);

        String out2 = tmp2.toString();
        System.out.println(out2);

        Document doc2 = loadXml(tmp2);
        Nodes itemNodes = doc2.query("//item");
        Element item1 = (Element) itemNodes.get(0);
        assertEquals("1", item1.getAttribute("id").getValue());
        assertEquals("my item 1", item1.getValue());

        Element item2 = (Element) itemNodes.get(1);
        assertEquals("2", item2.getAttribute("id").getValue());
        assertEquals("my item 2", item2.getValue());
    }

    
    public void testMagicProps() throws Exception {
        Template tmp = templates.get("magic-props.xhtml");

        PropTest propTest = new PropTest("100", "500");
        tmp.setElement("prop", propTest, 1, true);

        String out = tmp.toString();
        System.out.println(out);

        Document doc = loadXml(tmp);
        Nodes ddNodes = doc.query("//body/dl/dd");
        assertEquals("100", ddNodes.get(0).getValue());
        assertEquals("500", ddNodes.get(1).getValue());
    }

    public void testMagicList() throws Exception {
        Template tmp = templates.get("magic-list.xhtml");
        String[] d = new String[]{ "a", "b", "c", "d", "e", "f", "g" };
        List<String> data = Arrays.asList(d);

        tmp.setElement("list-item", data);

        String out = tmp.toString();
        System.out.println(out);

        Document doc = loadXml(tmp);
        Nodes liNodes = doc.query("//body/ul/li");
        for (int i = 0; i < d.length; i++) {
            assertEquals(d[i], liNodes.get(i).getValue());
        }
    }

    class PropTest {
        private String x;
        private String y;

        public PropTest(String x, String y) {
            this.x = x;
            this.y = y;
        }

        public String getX() {
            return x;
        }

        public String getY() {
            return y;
        }
    }
}