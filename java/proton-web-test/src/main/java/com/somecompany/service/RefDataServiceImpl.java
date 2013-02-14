package com.somecompany.service;

import java.util.ArrayList;
import java.util.List;

import com.somecompany.model.Country;

/**
 *
 * @author Jason R Briggs
 */
public class RefDataServiceImpl implements RefDataService {

    public List<Country> getCountries() {
        ArrayList<Country> rtn = new ArrayList<Country>(StaticData.countryData.values());
        return rtn;
    }



}
