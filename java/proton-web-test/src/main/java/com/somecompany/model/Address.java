package com.somecompany.model;

/**
 *
 * @author Jason R Briggs
 */
public interface Address {

    String getAddrLine1();

    String getAddrLine2();

    String getCity();

    Country getCountry();

    String getPostcode();

    void setAddrLine1(String addrLine1);

    void setAddrLine2(String addrLine2);

    void setCity(String city);

    void setPostcode(String postcode);
}