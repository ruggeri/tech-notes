Features of HTTP2 include:

* Data compression of HTTP headers.
* Server push of resources it thinks you need.
* Asking for multiple resources over same connection (pipelining)
    * I think HTTP 1.1 also offered this, but no "async" responding.
    * That is, the server had to respond in same order as resources
      were requested.
    * This is an example of head-of-line blocking.
