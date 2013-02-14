package com.somecompany;

import java.util.List;
import java.io.IOException;

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.somecompany.model.Country;
import com.somecompany.model.Organisation;
import com.somecompany.service.OrganisationService;
import com.somecompany.service.RefDataService;
import com.somecompany.view.MenuItem;
import com.somecompany.view.OrganisationView;
import com.somecompany.view.OrganisationSummaryView;
import com.somecompany.view.OrgsView;
import com.somecompany.view.Page;

import org.apache.commons.lang.StringUtils;

import proton.Template;
import proton.utils.TemplateHelper;
import proton.utils.WebUtils;
import proton.utils.exception.HttpException;

/**
 *
 * @author Jason R Briggs
 */
public class MasterDetailServlet extends ContextAwareServlet {

    private static final long serialVersionUID = 1L;

    private OrganisationService organisationService;
    private RefDataService refDataService;

    @Override
    public void init(ServletConfig config) throws ServletException {
        super.init(config);

        organisationService = (OrganisationService) context.getBean("organisationService");
        refDataService = (RefDataService) context.getBean("refDataService");
    }

    public void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        try {
            Organisation org = getOrganisation(request);
            OrganisationView orgView = null;
            if (org != null) {
                orgView = new OrganisationView(org);
            }

            String edit = request.getParameter("edit");
            String raw = request.getParameter("raw");

            Template tmp;

            if (orgView != null && (edit != null || raw != null)) {
                tmp = getSingleDisplay(request, response, orgView, raw != null);
            }
            else {
                tmp = getListDisplay(request, response, orgView);
            }

            response.getWriter().write(tmp.toString());

        }
        catch (HttpException he) {
            response.sendError(he.getCode(), he.getMessage());
        }
        catch (Exception e) {
            throw new ServletException(e);
        }
    }

    public void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        Organisation org = getOrganisation(request);

        try {
            TemplateHelper templateHelper = new TemplateHelper();

            OrganisationView orgView = new OrganisationView(org);

            orgView.apply(org);

            Template tmp = getSingleDisplay(request, response, orgView, false);
            response.getWriter().write(tmp.toString());
        }
        catch (HttpException he) {
            response.sendError(he.getCode(), he.getMessage());
        }
        catch (Exception e) {
            throw new ServletException(e);
        }
    }

    private Organisation getOrganisation(HttpServletRequest request) {
        Organisation org = null;

        String pathinfo = WebUtils.getPathInfo(request);

        if (!StringUtils.isEmpty(pathinfo)) {
            org = organisationService.getOrganisation(pathinfo);

            if (org == null) {
                throw new HttpException(HttpServletResponse.SC_NOT_FOUND, "Org unit " + pathinfo + " was not found");
            }
        }

        return org;
    }

    private Template getListDisplay(HttpServletRequest request, HttpServletResponse response, OrganisationView org) throws Exception {
        Template tmp;
        
        if (request.getParameter("raw") != null) {
            response.setContentType("text/xml");
            tmp = templates.get("raw-list.xml");
        }
        else {
            response.setContentType("text/html");
            tmp = templates.get("list.xhtml");
        }

        Page page = getPage("Organisations", tmp);

        OrgsView orgsView = new OrgsView();
        List<Organisation> organisations = organisationService.getOrganisations();
        orgsView.setOrgSummaries(OrganisationSummaryView.create(organisations));

        for (OrganisationSummaryView orgSummary : orgsView.getOrgSummaries()) {
            orgSummary.setViewHref(WebUtils.getPath(request, orgSummary.getOrgUnitId()));
            orgSummary.setEditHref(WebUtils.getPath(request, orgSummary.getOrgUnitId()) + "?edit");
        }

        if (org != null) {
            orgsView.setCurrentOrg(org);
        }
        else {
            tmp.hideElement("organisation");
        }

        TemplateHelper templateHelper = new TemplateHelper();
        templateHelper.decorate(tmp, page, orgsView);

        return tmp;
    }

    private Template getSingleDisplay(HttpServletRequest request, HttpServletResponse response, OrganisationView org, boolean raw) throws Exception {
        Template tmp;
        
        if (raw) {
            tmp = templates.get("raw.xml");
            response.setContentType("text/xml");
        }
        else {
            response.setContentType("text/html");
            tmp = templates.get("edit.xhtml");
        }

        Page page;
        if (org == null) {
            page = getPage("Add Organisation", tmp);
        }
        else {
            page = getPage("Edit Organisation", tmp);
        }

        List<Country> countries = refDataService.getCountries();
        org.setCountries(countries);

        TemplateHelper templateHelper = new TemplateHelper();
        templateHelper.decorate(tmp, page, org);

        return tmp;
    }

    private Page getPage(String title, Template tmp) throws Exception {
        Page page = new Page(title);
        page.addMenuItem(new MenuItem("/test1", "Test 1"));
        page.addMenuItem(new MenuItem("/test2", "Test 2"));

        tmp.include("menu", "menu.xhtml");

        return page;
    }
}