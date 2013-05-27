proton
======

Proton is a simple, "code-less" engine for xml/xhtml templates. Code-less, because it uses 3 types of ID (attribute) in a template file, rather than snippets of code — this moves the complexity out of the template and into the application.

A simple Proton template looks like this:

    &lt;html&gt;
        &lt;head&gt;
            &lt;title eid="title"&gt;PAGE TITLE&lt;/title&gt;
        &lt;/head&gt;
        &lt;body&gt;
            &lt;h1 eid="title"&gt;PAGE TITLE&lt;/h1&gt;

            &lt;p&gt;&lt;a eid="link" aid="link" href=""&gt;LINK ITEM&lt;/a&gt;&lt;/p&gt;

            &lt;ul&gt;
                &lt;li rid="list-item" eid="list-item"&gt;LIST ITEMS&lt;/li&gt;
            &lt;/ul&gt;
        &lt;/body&gt;
    &lt;/html&gt;

And to render this template, you might do something like this:

    from proton import template
    tmp = template.get_template('test.xhtml')

    tmp.set_value('title', 'An Xhtml Page', '*')
    tmp.set_value('link', 'This is a link to Google')
    tmp.set_attribute('link', 'href', 'http://www.google.com')

    tmp.repeat('list-item', 5)
    for x in range(0, 5):
        tmp.set_value('list-item', 'test%s' % x, x)

    print(str(tmp))

