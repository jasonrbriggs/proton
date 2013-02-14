package proton;

import proton.Template;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author Jason R Briggs
 */
public class BenchmarkTest extends ProtonTest {

    private List<Datum> data;

    @Override
    public void setUp() throws Exception {
        super.setUp();

        BufferedReader br = new BufferedReader(new InputStreamReader(getClass().getClassLoader().getResourceAsStream("benchmark-data.csv")));

        data = new ArrayList<Datum>();
        String line;
        int i = 1;
        while ((line = br.readLine()) != null && !line.equals("")) {
            Datum datum = new Datum(line.split("\\t"));

            datum.setId(Integer.toString(i));
            datum.setClazz(i % 2 == 1 ? "odd" : "even");

            data.add(datum);
            i++;
        }

        br.close();
    }

    private void run(int times) throws Exception {
        for (int i = 0; i < times; i++) {
            Template tmp = templates.get("benchmark.xhtml");
            tmp.repeatElement("data", data.size());
            for (int j = 0; j < data.size(); j++) {
                Datum d = data.get(j);
                int j1 = j + 1;
                tmp.setAttribute("clazz", "class", d.getClazz(), j1);
                tmp.setElement("id", d.getId(), j1);
                tmp.setAttribute("symbol", "href", "/stock/" + d.getSymbol(), j1);
                tmp.setElement("symbol", d.getSymbol(), j1);
                tmp.setAttribute("url", "href", d.getUrl(), j1);
                tmp.setElement("name", d.getName(), j1);
                tmp.setElement("price", d.getPrice(), j1);
                if (d.getChange() < 0) {
                    tmp.hideElement("azchange", j1);
                    tmp.hideElement("azratio", j1);
                    tmp.setElement("bzchange", Float.toString(d.getChange()), j1);
                    tmp.setElement("bzratio", d.getRatio(), j1);
                } else {
                    tmp.hideElement("bzchange", j1);
                    tmp.hideElement("bzratio", j1);
                    tmp.setElement("azchange", Float.toString(d.getChange()), j1);
                    tmp.setElement("azratio", d.getRatio(), j1);
                }
            }
            tmp.toString();
        }
    }

    public void testPerformance() throws Exception {
        int times = 10000;

        long start_t = System.currentTimeMillis();

        run(times);

        long end_t = System.currentTimeMillis();

        float real  = end_t - start_t;
        System.out.println("Time taken " + real);

    }
    
}