package com.somecompany.model.impl;

import com.somecompany.model.Country;

/**
 *
 * @author Jason R Briggs
 */
public class CountryImpl implements Country {

    private String code;
    private String name;

    public CountryImpl(String code, String name) {
        this.code = code;
        this.name = name;
    }

    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    @Override
    public int hashCode() {
        return code.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null || !(obj instanceof Country)) {
            return false;
        }
        else {
            Country that = (Country) obj;
            return this.getCode().equals(that.getCode());
        }
    }
}
