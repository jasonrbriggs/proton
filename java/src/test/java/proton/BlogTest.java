package proton;

import proton.Template;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import nu.xom.Document;
import nu.xom.Element;
import nu.xom.Nodes;

/**
 *
 * @author Jason R Briggs
 */
public class BlogTest extends ProtonTest {

    public void testBlog() throws Exception {
        Template tmp = templates.get("blogexample.xhtml");

        Date now = new Date();

        tmp.setElement("title", "My Test Blog");
        tmp.setElement("menu", getMenu(), 1, true);
        tmp.setElement("blogroll", getBlogRoll(), 1, true);

        Post post = new Post("My First Post", "joe@bloggs.com", now, "This is my first post...\n...and what a great post it is.");

        tmp.setElement("post", post, 1, true);

        String out = tmp.toString();
        System.out.println(out);

        Document doc = loadXml(tmp);

        assertEquals("My First Post", find(doc, "//body/div/h1"));

        Nodes spanNodes = doc.query("//body/div/p/span");
        assertTrue(spanNodes.size() == 2);
        assertEquals(now.toString(), spanNodes.get(0).getValue());
        assertEquals("joe@bloggs.com", spanNodes.get(1).getValue());

        Nodes pNodes = doc.query("//body/div/div/p");
        assertEquals("This is my first post...", pNodes.get(0).getValue());
        assertEquals("...and what a great post it is.", pNodes.get(1).getValue());

        Nodes anchorNodes = doc.query("//body/div/ul/li/a");

        Element anchor1 = (Element) anchorNodes.get(0);
        Element anchor2 = (Element) anchorNodes.get(1);
        Element anchor3 = (Element) anchorNodes.get(2);

        assertEquals("Home", anchor1.getValue());
        assertEquals("/home", anchor1.getAttribute("href").getValue());
        assertEquals("About", anchor2.getValue());
        assertEquals("/about", anchor2.getAttribute("href").getValue());
        assertEquals("Admin", anchor3.getValue());
        assertEquals("/admin", anchor3.getAttribute("href").getValue());

        anchorNodes = doc.query("//body/div/ol/li/a");

        anchor1 = (Element) anchorNodes.get(0);
        anchor2 = (Element) anchorNodes.get(1);

        assertEquals("Some blog", anchor1.getValue());
        assertEquals("http://www.someblog.com", anchor1.getAttribute("href").getValue());
        assertEquals("Another blog", anchor2.getValue());
        assertEquals("http://www.anotherblog.com", anchor2.getAttribute("href").getValue());
    }

    private List<Ref> getMenu() {
        Ref[] ref = new Ref[]{
            new Ref("/home", "Home"),
            new Ref("/about", "About"),
            new Ref("/admin", "Admin")
        };

        return Arrays.asList(ref);
    }

    private List<Ref> getBlogRoll() {
        Ref[] ref = new Ref[]{
            new Ref("http://www.someblog.com", "Some blog"),
            new Ref("http://www.anotherblog.com", "Another blog")
        };

        return Arrays.asList(ref);
    }

    class Ref {
        private String href;
        private String text;

        public Ref(String href, String text) {
            this.href = href;
            this.text = text;
        }

        public String getHref() {
            return href;
        }

        public String getText() {
            return text;
        }
    }

    class Post {

        private String title;
        private String author;
        private Date date;
        private List<String> content;

        public Post(String title, String author, Date date, String content) {
            this.title = title;
            this.author = author;
            this.date = date;
            this.content = Arrays.asList(content.split("\n"));
        }

        public String getAuthor() {
            return author;
        }

        public List<String> getContent() {
            return content;
        }

        public Date getDate() {
            return date;
        }

        public String getTitle() {
            return title;
        }
    }
}