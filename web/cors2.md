## Same Origin

We don't want evil.com to be able to access
facebook.com/api/friends. If it sent your cookies there, it could then
download all your friends. Therefore, not even GET requests are by
default allowed cross origin.

This is called:
[same-origin policy](https://en.wikipedia.org/wiki/Same-origin_policy).

## JSONP

In the old days, before CORS, one possible solution was
[JSONP](https://en.wikipedia.org/wiki/JSONP). This took advantage of
the fact that *some* resources *could* be loaded cross origin.

So you would add a script tag with `src` equal to something like:

   facebook.com/api/friends.js

this would return some **JavaScript** like:

```
window.facebookResponse = [
  { id: 123, name: "markov" },
  { id: 456, name: "curie" },
]
```

Notice this isn't exactly JSON. This is **JavaScript**. But the caller
who started the script tag could now get the result at
`window.facebookResponse`. The way JSONP works is a little more
clever, but this is the idea.

## JSON Hijacking

Without JSONP, if you tried to make a script tag with `src` equal to
`facebook.com/api/friends.json` (request for JSON format), you would
get JSON, but it wouldn't be stored anywhere.

There is however a subtle attack called
[JSON Hijacking](https://haacked.com/archive/2008/11/20/anatomy-of-a-subtle-json-vulnerability.aspx/).

Basically, you hijack the Array constructor function:

```
Array = function() {
  window.secrets = this;
};
```

Then, if evil.com makes a request for `friends.json` via a script tag,
it gets the response:

```
[
  { id: 123, name: "markov" },
  { id: 456, name: "curie" },
]
```

which *looks* harmless, because this invokes no code; so the result
should just be "lost". But the reassigned Array constructor steals the
data by saving it to a global variable.

I belive new versions of JS don't let you reassign the Array
constructor like this. In the old days, the mitigation was to make
sure that your JSON responses weren't actually quite valid JSON:

https://stackoverflow.com/questions/2669690/why-does-google-prepend-while1-to-their-json-responses

The other solution is to require CSRF tokens when making API
requests. But this can be operationally difficult (CSRF tokens need to
be verified, which is extra work).

## CORS

When a service does want to let some domains use it cross origin, now
we have CORS. CORS is a better answer than JSONP which was a hack.

CORS is described
[here](https://en.wikipedia.org/wiki/Cross-origin_resource_sharing).

Basically: when you make a request cross-origin, your browser will
include a `Origin: http://www.example.com` header. And importantly, a
CORS enabled server will include a `Access-Control-Allow-Origin:
http://www.example.com` header.

The browser will *only* deliver the response from the server *if* the
requesting domain is a match.

## OPTIONS

The above method works for GET and POST requests with format
`application/x-www-form-urlencoded` (and a few other cases). In fact,
via `form` tag a site could always trick you into making a foreground
request cross-origin, but not a *background* request.

Merely allowing cross-origin POST requests without delivering the
response to evil.com is itself bad. That's why we have CSRF tokens.

So CORS just gives us the ability to tell the browser: hey, this
response is okay to deliver as an AJAX response.

**On the other hand**: no browser ever allowed evil.com to make a PUT,
DELETE, et cetera request. The fear is that maybe some old servers
*assume* and *rely* on an assumption that no browser will initiate
those requests cross origin.

Therefore, the browser sends an OPTIONS request before making a PUT or
DELETE request. This is kinda paranoid. The OPTIONS request says: "Is
it okay if I send you a PUT or DELETE request cross origin?"

The answer can be "No", "Yes", or "I don't understand an OPTIONS
request!" In the last case, the browser assumes it is not safe to send
the request, because maybe the service won't have bothered to check a
CSRF token because it relied on the old behavior of no cross origin
request ever being initiated.

Note: the OPTIONS is not about knowing whether to deliver the response
or not. If the `Access-Control-Allow-Origin` header is not in the
repsonse, the response won't be delivered. But the problem is that the
*request* may have already made a change to user data.
