package proton;

import proton.Template;
import java.util.Locale;
import java.util.ResourceBundle;
import nu.xom.Document;
import nu.xom.Nodes;

/**
 *
 * @author Jason R Briggs
 */
public class I18NTest extends ProtonTest {

    public void testI18N() throws Exception {
        Template tmp = templates.get("i18n.xhtml");
        ResourceBundle resources = ResourceBundle.getBundle("test", Locale.FRENCH);
        tmp.setResources(resources);

        Document doc = loadXml(tmp);
        String out = tmp.toString();

        System.out.println(out);

        assertEquals("Titre de la page", find(doc, "//head/title"));
        assertEquals("Titre de la page", find(doc, "//body/h1"));

        Nodes nodes = doc.query("//body/p");
        assertEquals("Certains textes traduits", nodes.get(0).getValue());
        assertEquals("Not translated text", nodes.get(1).getValue());
    }

}
