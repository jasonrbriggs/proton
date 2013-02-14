package com.somecompany;

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;

import org.springframework.context.support.ClassPathXmlApplicationContext;

import proton.Templates;

/**
 * Base class for servlets requiring access to the Spring context.
 * 
 * @author Jason R Briggs
 */
public abstract class ContextAwareServlet extends HttpServlet {

    protected ClassPathXmlApplicationContext context;

    protected Templates templates;

    /**
     * Initialise the Spring context, and load the jdbc templates.
     */
    public void init(ServletConfig config) throws ServletException {
        super.init(config);
        context = new ClassPathXmlApplicationContext(new String[]{"applicationContext.xml"});

        templates = (Templates) context.getBean("templates");
    }

}