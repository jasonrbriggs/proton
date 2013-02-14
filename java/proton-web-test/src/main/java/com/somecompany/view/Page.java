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
public class Page {

    private String title;

    private List<MenuItem> menuItems = new ArrayList<MenuItem>();

    public Page(String title) {
        this.title = title;
    }

    @TemplateInfo(eid = "title", all = true)
    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    @TemplateInfo(rid = "menu")
    public List<MenuItem> getMenuItems() {
        return menuItems;
    }

    public void addMenuItem(MenuItem menuItem) {
        menuItems.add(menuItem);
    }
}
