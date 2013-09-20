package com.somecompany.service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.somecompany.model.Organisation;

/**
 *
 * @author Jason R Briggs
 */
public class OrganisationService {

    public Organisation getOrganisation(String orgUnitId) {
        if (StaticData.orgData.containsKey(orgUnitId)) {
            return StaticData.orgData.get(orgUnitId);
        }
        
        return null;
    }

    public List<Organisation> getOrganisations() {
        ArrayList<Organisation> rtn = new ArrayList<Organisation>(StaticData.orgData.values());
        Collections.sort(rtn);
        return rtn;
    }

    public void updateOrganisation(Organisation org) {
        StaticData.orgData.put(org.getOrgUnitId(), org);
    }

}
