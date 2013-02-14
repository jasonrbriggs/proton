package proton;

/**
 *
 * @author Jason R Briggs
 */
public class Datum {
    private String id;
    private String clazz;
    private String name;
    private String description;
    private String url;
    private String symbol;
    private String price;
    private float change;
    private String ratio;

    public Datum(String... data) {
        assert(data.length >= 7);
        setName(data[0]);
        setDescription(data[1]);
        setUrl(data[2]);
        setSymbol(data[3]);
        setPrice(data[4]);
        setChange(data[5]);
        setRatio(data[6]);
    }

    public float getChange() {
        return change;
    }

    public void setChange(String change) {
        setChange(Float.parseFloat(change));
    }

    public void setChange(float change) {
        this.change = change;
    }

    public String getClazz() {
        return clazz;
    }

    public void setClazz(String clazz) {
        this.clazz = clazz;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getPrice() {
        return price;
    }

    public void setPrice(String price) {
        this.price = price;
    }

    public String getRatio() {
        return ratio;
    }

    public void setRatio(String ratio) {
        this.ratio = ratio;
    }

    public String getSymbol() {
        return symbol;
    }

    public void setSymbol(String symbol) {
        this.symbol = symbol;
    }

    public String getUrl() {
        return url;
    }

    public void setUrl(String url) {
        this.url = url;
    }


}