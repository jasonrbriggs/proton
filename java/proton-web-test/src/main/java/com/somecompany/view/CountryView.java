package com.somecompany.view;

import com.somecompany.model.Country;
import proton.utils.annotation.TemplateInfo;
import proton.utils.annotation.TemplateView;

/**
 *
 * @author Jason R Briggs
 */
@TemplateView
public class CountryView {
    private String country;
    private String selected = "";

    public CountryView(Country country) {
        this.country = country.getCode();
    }

    @TemplateInfo(eid = "country", aid="country", attr = "value")
    public String getCountry() {
        return country;
    }

    public void setCountry(String country) {
        this.country = country;
    }

    @TemplateInfo(aid = "country", attr = "selected")
    public String getSelected() {
        return selected;
    }

    public void setSelected(String selected) {
        this.selected = selected;
    }


}
