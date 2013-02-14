package com.somecompany.view;

import java.util.ArrayList;
import java.util.List;

import proton.utils.annotation.TemplateInfo;
import proton.utils.annotation.TemplateView;

/**
 *
 * @author Jason R Briggs
 */
@TemplateView
public class OrgsView {

    private List<OrganisationSummaryView> orgs;

    private OrganisationView currentOrg = null;

    public OrgsView() {
        orgs = new ArrayList<OrganisationSummaryView>();
    }

    @TemplateInfo(rid = "organisations")
    public List<OrganisationSummaryView> getOrgSummaries() {
        return orgs;
    }

    public void addOrgSummary(OrganisationSummaryView orgSummary) {
        orgs.add(orgSummary);
    }

    public void setOrgSummaries(List<OrganisationSummaryView> orgs) {
        this.orgs = orgs;
    }

    @TemplateInfo
    public OrganisationView getCurrentOrg() {
        return currentOrg;
    }

    public void setCurrentOrg(OrganisationView currentOrg) {
        this.currentOrg = currentOrg;
    }

}