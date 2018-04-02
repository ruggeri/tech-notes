## JSONP

Of course, you're supposed to use CSRF tokens for non-GET requests;
that prevents a CSRF attack.

Moreover, the browser doesn't allow you to make cross-origin
XMLHttpRequest requests. This is because it doesn't want to let one
domain use another's domain cookies. In particular, it won't even let
you make GET requests, since you could try to steal API data using the
user's cookies.

An exception to the same-origin policy is script tags; that's because
it's so established that people use JS hosted elsewhere: for instance,
for pixel tags.

JSONP is a technique where with you use a script tag to make a
cross-origin request. You make the request by setting the src of a
script tag. You can obviously only perform GET requests this way.

A JSONP API will return a bit of JS code that calls a callback to
receive the data. However, there is danger of a CSRF attack. One way
to prevent this is to check the referer against a whitelist, but it
seems that this can be spoofed using Flash.

You can also require the user to upload a CSRF token. It would
probably take some coordination to get this token on the application's
pages.

## JSON Hijacking

We use CSRF tokens for all non-GET requests. However, GET requests can
be made cross-origin, as seen with JSONP. For a non-JSONP API, the
script will just contain some data, if JSON was returned.

This can be dangerous: a malicious website can rewrite the
array/object constructor functions to store the data so that they can
later retrieve it. This is called **JSON hijacking**.

Modern browsers don't allow this. But to prevent hijacking in older
browsers (including IE<10), you can:

* Do what google does, and prepend `throw 1;`; before API
  responses.
* Require a CSRF token even in a GET request.
    * But that would mess with caching and cause additional CPU load.

http://stackoverflow.com/questions/3146798/why-do-people-put-code-like-throw-1-dont-be-evil-and-for-in-front-of

## CORS

An alternative to JSONP is to use CORS.

A browser making a cross-site GET request will include an Origin
header telling where the request came from. The server can return
Access-Control headers to tell whether this request should be allowed.

If the cross site request should not be allowed, or if the server is
otherwise just not configured to return the Access-Control headers,
the browser will not make the response available.

For POST/PUT/DELETE requests, the browser sends a "preflight" OPTIONS
request first. This request contains the Origin, and the
Access-Control headers are returned. If the request is allowed, the
browser makes the real request.

The reason for this preflight instead of just blindly making the
request is to avoid pushing more responsibility for sandboxing onto
the server. Only if the server has opted into CORS will the modifying
request be made.

http://en.wikipedia.org/wiki/Cross-origin_resource_sharing

Actually, why should you need the preflight? This describes why.

https://stackoverflow.com/questions/15381105/cors-what-is-the-motivation-behind-introducing-preflight-requests

## Content-Security-Policy

Content-Security-Policy is an HTTP header which can tell the browser
not to (a) execute inline scripts or (b) use eval. This helps mitigate
XSS attacks.

You can add a whitelist of places you're allowed to make JS requests
to. CSP also allows you to whitelist frame sources, etc.

http://www.html5rocks.com/en/tutorials/security/content-security-policy

## Preventing API Abuse

* The service provides tokens to the application.
* The user makes requests to the application, which then makes
  requests to service.
* The application can issue a token to the user which allows them to
  rate limit the user to prevent abuse.
* What if the application wants to do client-side JS directly to the
  service? Don't want to give out the token in the JS code; it will be
  stolen.
* Comes up for direct AWS uploads, too.
* Could work kinda like OAuth, but maybe service allows you to place
  constraints? Maybe the token is only active for a short duration?

The way to do this is for the user to first request a policy from
you. You can specify a max size of file they can transfer. Your server
signs this with a public key that AWS verifies. You set an
expiration. Typically only a few minutes.
