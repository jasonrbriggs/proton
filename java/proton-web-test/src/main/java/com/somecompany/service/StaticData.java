package com.somecompany.service;

import java.util.HashMap;
import java.util.Map;

import com.somecompany.model.Address;
import com.somecompany.model.Country;
import com.somecompany.model.Organisation;

/**
 * Dummy in memory data.
 *
 * @author Jason R Briggs
 */
public class StaticData {

    public static Map<String, Country> countryData = new HashMap<String, Country>();
    public static Map<String, Organisation> orgData = new HashMap<String, Organisation>();

    static {
        countryData.put("UK", new Country("UK", "United Kingdom"));
        countryData.put("USA", new Country("USA", "USA"));

        for (int i = 0; i < 5; i++) {
            Organisation org = new Organisation();
            org.setId(i + 1);
            org.setOrgUnitId("orgUnitId" + i);
            org.setName("orgName" + i);

            Address addr = new Address();
            addr.setAddrLine1("org" + i + " address line 1");
            addr.setAddrLine2("org" + i + " address line 2");
            addr.setCity("city" + i);
            if (i % 2 == 0) {
                addr.setCountry(countryData.get("UK"));
            }
            else {
                addr.setCountry(countryData.get("USA"));
            }
            addr.setPostcode("12345" + i);

            org.setAddress(addr);

            orgData.put(org.getOrgUnitId(), org);
        }
    }
}
