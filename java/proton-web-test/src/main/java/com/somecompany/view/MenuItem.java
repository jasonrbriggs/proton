package com.somecompany.view;

import proton.utils.annotation.TemplateInfo;
import proton.utils.annotation.TemplateView;

/**
 *
 * @author Jason R Briggs
 */
@TemplateView
public class MenuItem {

    private String href;
    private String text;

    public MenuItem(String href, String text) {
        this.href = href;
        this.text = text;
    }

    @TemplateInfo(aid = "menu-item", attr = "href")
    public String getHref() {
        return href;
    }

    @TemplateInfo(eid = "menu-item")
    public String getText() {
        return text;
    }

}
