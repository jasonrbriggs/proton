package com.somecompany.service;

import java.util.ArrayList;
import java.util.List;

import com.somecompany.model.Country;

/**
 *
 * @author Jason R Briggs
 */
public class RefDataService {

    public List<Country> getCountries() {
        ArrayList<Country> rtn = new ArrayList<Country>(StaticData.countryData.values());
        return rtn;
    }
    
    public Country getCountry(String code) {
        return StaticData.countryData.get(code);
    }

}
