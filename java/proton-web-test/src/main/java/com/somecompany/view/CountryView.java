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
    private Country country;
    private String selected = "";

    public CountryView(Country country) {
        this.country = country;
    }

    @TemplateInfo(eid = "country", aid="country", attr = "value")
    public String getCountry() {
        return country.getCode();
    }

    @TemplateInfo(aid = "country", attr = "selected")
    public String getSelected() {
        return selected;
    }

    public void setSelected(String selected) {
        this.selected = selected;
    }
}
