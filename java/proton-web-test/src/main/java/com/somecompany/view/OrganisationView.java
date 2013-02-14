package com.somecompany.view;

import java.util.ArrayList;
import java.util.List;

import com.somecompany.model.Country;
import com.somecompany.model.Organisation;

import proton.utils.annotation.TemplateInfo;
import proton.utils.annotation.TemplateView;

/**
 *
 * @author Jason R Briggs
 */
@TemplateView
public class OrganisationView extends OrganisationSummaryView {

    private AddressView address;
    private List<CountryView> countryViews;
    private String countryCode;

    public OrganisationView(Organisation org) {
        super(org);
        this.address = new AddressView(org.getAddress());
        this.countryCode = org.getAddress().getCountry().getCode();
    }

    public void apply(Organisation organisation) {
        super.apply(organisation);
        address.apply(organisation.getAddress());
    }

    @TemplateInfo
    public AddressView getAddress() {
        return address;
    }

    @TemplateInfo(rid = "countries")
    public List<CountryView> getCountries() {
        return countryViews;
    }

    public void setCountries(List<Country> countries) {
        countryViews = new ArrayList<CountryView>();
        for (Country country : countries) {
            CountryView cv = new CountryView(country);
            if (cv.getCountry().equals(countryCode)) {
                cv.setSelected("selected");
            }
            countryViews.add(cv);
        }
    }
}