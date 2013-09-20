package com.somecompany.model;

/**
 *
 * @author Jason R Briggs
 */
public class Address {

    private String addrLine1;
    private String addrLine2;
    private String city;
    private Country country;
    private String postcode;

    public String getAddrLine1() {
        return addrLine1;
    }

    public String getAddrLine2() {
        return addrLine2;
    }

    public String getCity() {
        return city;
    }

    public Country getCountry() {
        return country;
    }

    public String getPostcode() {
        return postcode;
    }

    public void setAddrLine1(String addrLine1) {
        this.addrLine1 = addrLine1;
    }

    public void setAddrLine2(String addrLine2) {
        this.addrLine2 = addrLine2;
    }

    public void setCity(String city) {
        this.city = city;
    }

    public void setCountry(Country country) {
        this.country = country;
    }

    public void setPostcode(String postcode) {
        this.postcode = postcode;
    }
}