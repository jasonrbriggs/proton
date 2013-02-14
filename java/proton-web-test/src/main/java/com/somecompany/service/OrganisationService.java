package com.somecompany.service;

import java.util.List;

import com.somecompany.model.Organisation;

/**
 *
 * @author Jason R Briggs
 */
public interface OrganisationService {

    List<Organisation> getOrganisations();

    Organisation getOrganisation(String orgUnitId);

    void updateOrganisation(Organisation org);

}
