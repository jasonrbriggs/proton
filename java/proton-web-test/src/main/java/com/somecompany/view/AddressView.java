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

    private String addrLine1;
    private String addrLine2;
    private String city;
    private String postcode;
    private String countryCode;

    public AddressView(Address address) {
        this.addrLine1 = address.getAddrLine1();
        this.addrLine2 = address.getAddrLine2();
        this.city = address.getCity();
        this.postcode = address.getPostcode();
        this.countryCode = address.getCountry().getCode();
    }

    public void apply(Address address) {
        address.setAddrLine1(getAddrLine1());
        address.setAddrLine2(getAddrLine2());
        address.setCity(getCity());
        address.setPostcode(getPostcode());
    }

    @TemplateInfo(eid = "addr1", aid="addr1", attr = "value")
    public String getAddrLine1() {
        return addrLine1;
    }

    public void setAddrLine1(String addrLine1) {
        this.addrLine1 = addrLine1;
    }

    @TemplateInfo(eid = "addr2", aid="addr2", attr = "value")
    public String getAddrLine2() {
        return addrLine2;
    }

    @TemplateInfo(eid = "city", aid="city", attr = "value")
    public String getCity() {
        return city;
    }

    @TemplateInfo(eid = "postcode", aid="postcode", attr = "value")
    public String getPostcode() {
        return postcode;
    }

    @TemplateInfo(eid = "countryCode")
    public String getCountryCode() {
        return countryCode;
    }

}