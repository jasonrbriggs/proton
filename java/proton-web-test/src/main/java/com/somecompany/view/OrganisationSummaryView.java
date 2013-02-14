package com.somecompany.view;

import java.util.ArrayList;
import java.util.List;

import com.somecompany.model.Organisation;
import proton.utils.annotation.TemplateInfo;
import proton.utils.annotation.TemplateView;

/**
 *
 * @author Jason R Briggs
 */
@TemplateView(suffix = "Summary")
public class OrganisationSummaryView {

    private String name;
    private String orgUnitId;
    private String viewHref;
    private String editHref;

    public OrganisationSummaryView(Organisation organisation) {
        this.name = organisation.getName();
        this.orgUnitId = organisation.getOrgUnitId();
    }

    public void apply(Organisation organisation) {
        organisation.setName(getName());
    }

    @TemplateInfo(eid = "orgUnitName", aid="orgUnitName", attr = "value")
    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    @TemplateInfo(eid = "orgUnitId", aid = "orgUnitId", attr = "value")
    public String getOrgUnitId() {
        return orgUnitId;
    }

    public void setViewHref(String viewHref) {
        this.viewHref = viewHref;
    }

    @TemplateInfo(aid = "orgUnitViewHref", attr = "href")
    public String getViewHref() {
        return viewHref;
    }

    @TemplateInfo(aid = "orgUnitEditHref", attr = "href")
    public String getEditHref() {
        return editHref;
    }

    public void setEditHref(String editHref) {
        this.editHref = editHref;
    }

    public static List<OrganisationSummaryView> create(List<Organisation> orgs) {
        List<OrganisationSummaryView> orgSummaries = new ArrayList<OrganisationSummaryView>();
        for (Organisation org : orgs) {
            orgSummaries.add(new OrganisationSummaryView(org));
        }
        return orgSummaries;
    }
}