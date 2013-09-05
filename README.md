proton
======

Proton is a simple, "code-less" engine for xml/xhtml templates. Code-less, because it uses 3 types of ID (attribute) in a template file, rather than snippets of code — this moves the complexity out of the template and into the application.

A simple Proton template looks like this:

    <html>
        <head>
            <title eid="title">PAGE TITLE</title>
        </head>
        <body>
            <h1 eid="title">PAGE TITLE</h1>

            <p><a eid="link" aid="link" href="">LINK ITEM</a></p>

            <ul>
                <li rid="list-item" eid="list-item">LIST ITEMS</li>
            </ul>
        </body>
    </html>

And to render this template in Python, you might do something like this:

    from proton import template
    tmp = template.get_template('test.xhtml')

    tmp.set_value('title', 'An Xhtml Page', '*')
    tmp.set_value('link', 'This is a link to Google')
    tmp.set_attribute('link', 'href', 'http://www.google.com')

    tmp.repeat('list-item', 5)
    for x in range(0, 5):
        tmp.set_value('list-item', 'test%s' % x, x)

    print(str(tmp))
    
Resulting in the following output:

    <html>
        <head>
            <title>An Xhtml Page</title>
        </head>
        <body>
            <h1>An Xhtml Page</h1>

            <p><a href="http://www.google.com">This is a link to Google</a></p>

            <ul>
                <li>test0</li>
                <li>test1</li>
                <li>test2</li>
                <li>test3</li>
                <li>test4</li>
            </ul>
        </body>
    </html>

Similarly in Haskell, you could do the following:

    tmps <- loadTemplates "templates"
    tmp <- getTemplate tmps "templates/test.xhtml"
    tmp <- setElementValue tmp "title" "An Xhtml Page" 0
    tmp <- setElementValue tmp "link" "This is a link to Google" 0
    tmp <- setAttributeValue tmp "link" "href" "http://www.google.com" 0
    tmp <- setElementValues tmp "list-item" (map (\x -> "test" ++ (show x)) [0..4])
    
    s <- renderTemplate tmp


