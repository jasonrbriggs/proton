package com.somecompany.view;

import com.somecompany.model.Address;

import proton.utils.annotation.TemplateInfo;
import proton.utils.annotation.TemplateView;

/**
 *
 * @author Jason R Briggs
 */
@TemplateView
public class AddressView {

    private Address address;

    public AddressView(Address address) {
        this.address = address;
    }

    @TemplateInfo(eid = "addr1", aid="addr1", attr = "value")
    public String getAddrLine1() {
        return address.getAddrLine1();
    }

    @TemplateInfo(eid = "addr2", aid="addr2", attr = "value")
    public String getAddrLine2() {
        return address.getAddrLine2();
    }

    @TemplateInfo(eid = "city", aid="city", attr = "value")
    public String getCity() {
        return address.getCity();
    }

    @TemplateInfo(eid = "postcode", aid="postcode", attr = "value")
    public String getPostcode() {
        return address.getPostcode();
    }

    @TemplateInfo(eid = "countryCode")
    public String getCountryCode() {
        return address.getCountry().getCode();
    }

}