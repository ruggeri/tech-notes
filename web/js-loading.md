Script tags typically stop HTML parsing. You might do this if you
really thought nothing else should load until you've run the script.

In particular, if you have inline JS, you may want to wait until
jQuery is loaded in the head.

You can set the async attribute, which will load the JS while you
parse the HTML, and then execute it (stopping HTML parsing) when the
script is ready.

You can also set defer, which will download in parallel and run the
script after the HTML is parsed. This will also make sure scripts are
run in order.

If you're using async with dependencies, I think you use something
like RequireJS.

There's another suggestion: dynamically add (not async) script tags to
the head with a short inline script in the head. Adding a script tag
dynamically will cause parallelized downloading even without the async
tag. However, running of the scripts will be done in order. So you get
a mix of behaviors not acheivable with just an HTML script tag.

Finally: you can put the tags at the bottom of the page. But that
means you can't begin downloading JS until everything is parsed.

Typical advice used to be put at the bottom: defer basically
accomplishes this but starts the fetching before the document is
parsed. Is that worth it? NB: old browsers not supporting defer will
be slow, but maybe fuck them.

One advantage of async might be that you don't freeze to execute all
your JS serially, thus spreading out pauses? I guess you get JS
started going earlier; is that important to you? I guess it could be.

Basically: defer would be fine, except sometimes you really do want
your JS to run as soon as it possibly can.

Source: http://www.growingwiththeweb.com/2014/02/async-vs-defer-attributes.html
Source: http://www.html5rocks.com/en/tutorials/speed/script-loading/
Source: http://stackoverflow.com/questions/436411/where-is-the-best-place-to-put-script-tags-in-html-markup
