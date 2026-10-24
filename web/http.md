# Evolution of HTTP

## HTTP/0.9

- There was only GET request.
- No headers. Server replies with raw HTML document.
- Starts around 1991.

## HTTP/1

- Adopted 1996.
- In HTTP/1.0, to make a request, you open a TCP socket. You send the
  request for a single resource. You get the response. Then the
  connection is done.
- Adds methods beyond GET, adds response status codes, adds
  request/response headers, `Content-Type` allows specification of
  format of resource returned,
- HTTP/1.0 actually doesn't require a TCP socket, I think just a
  reliable transport mechanism where packets don't drop.
- The server *can* stream a series of responses (it's the client's to
  interpret the series). It's just a byte stream. So you can serialize a
  series of JSON objects.

# HTTP/1.1

- HTTP/1.1 was introduced in 1997.
- In HTTP/1.1, client gained ability to send *multiple* requests on the
  same socket. This is called **pipelining**.
  - However, the server is required to complete responding to the first
    request before it starts sending the response to the second request.
  - The client requests can be made at any time. They might be made all
    up front. Or, client might request HTTP, then while parsing learn
    JavaScript and CSS resources it needs to fetch, and fetch those.
  - Server will leave open the connection for the client to close,
    allowing client to make subsequent requests. Server can still impose
    a timeout and close the connection when it wants.
  - You have a "head of line blocking" problem, where if an early
    request is responded to slowly, subsequent requests cannot be
    processed.
- Pipelining never really took off. Browsers preferred to open multiple
  sockets simultaneously. Server and proxy implementation was slow,
  which hampered thing. And the head of line blocking was a fundamental
  problem.
  - In HTTP/1.1 era, browsers might open a max of six sockets per
    origin, but only fetch one resources at a time.
- Even though pipelining wasn't widely used to its fullest, "persistent"
  connection was often used. After receiving response to a request,
  client could follow up with a second request on the same connection.
- HTTP/1.1 also introduces the mandatory `Host` header, which allows the
  same IP address to host multiple services.

## HTTP/2

- In 2015, HTTP/2 arrived.
  - My first notes on HTTP/2 were maybe from 2018??
  - Client can still fire multiple HTTP requests.
  - But now server responses can be multiplexed. Each chunk sent by
    server can be addressed to a specific request. So a slow HTTP
    request made earlier in the stream doesn't block subsequent
    requests.
  - HTTP/2 is more tied to TCP. Which means that you now have a **TCP**
    head-of-line blocking problem. A delayed TCP packet blocks the
    stream.
  - HTTP/2 also introduced "HPACK". Basically, this is a form of header
    compression. The client will fire many HTTP/2 requests, often with
    very similar/repetitious headers, which can be quite verbose. HPACK
    means that a dynamic table will be used to store header values, and
    subsequent requests can refer to prior header values.
  - HTTP/2 had a "flow control" which allows client to specify how fast
    it can consume various of the multiplexed responses. For instance,
    if on fetched resources (maybe a download of a big image) is
    progressing slowly, the server should use more bandwidth on
    responding to API requests that maybe can be consumed at a faster
    rate. This basically avoids the buffer filling up with a bunch of
    data that can't be consumed quickly, blocking other requests.
  - There was also an ability for the client to prioritize and cancel
    streams/requests.
- Connection Coalescing
  - Basically, server can send an `ORIGIN` frame to advertise *other
    origins* that it can serve.
  - So if you connect to `www.youtube.com`, server might say that it
    also can serve `mobile.youtube.com`, which means you can use the
    same connection.
  - The TLS certificate needs to check out also for the origin.
  - In theory, you can do coalescing even without TLS, if browser can
    verify that DNS resolves to same IP address.
- Server Push
  - There is a feature where server could push resources it anticipated
    the client would need even without the client asking.
  - But browsers dropped support and feature was abandoned. The problem
    is that servers didn't know what resources you might already have
    cached, and it didn't always know how to prioritize resources.
  - In 2017 an alternative was added (amending both HTTP/2 and
    HTTP/1.1). The server can send an `103 Early Hints` response which
    basically says informs client of several resources that might be
    required in case client wants to fetch those. But this is still not
    widely used by servers as of 2026 (widely implemented by browsers
    though).
- Both HTTP/1.1 and HTTP/2 need to run on a "reliable" transport layer.
  - HTTP/1.1 has basically just one protocol stream. HTTP/2 has multiple
    protocol streams.
  - But within a protocol stream, data must be delivered in order. The
    HTTP protocol isn't able to re-order data that comes in
    out-of-order.
  - That is TCP's job. You can't just run HTTP on UDP.
  - There was no point in trying to run HTTP/1.1 on UDP, because at HTTP
    protocol level, there is no ability for the client to process any
    later data until all prior data has been sent to client.
  - But with appearance of HTTP/2, a delayed HTTP/2 protocol frame on
    stream 1 doesn't *need* to block a frame on stream 2. But if the
    *underlying TCP packet* for the stream 1 protocol frame is sequenced
    before the TCP packet for the stream 2 protocol frame, it will
    *still* delay.
  - Basically: head-of-line blocking continues at the TCP transport
    layer.

**SPDY**

- SPDY was Google's prototype for HTTP/2. It basically prototyped the
  ideas that came to be HTTP/2. It was implemented by Chrome.
- It was announced 2009 and Chrome had it by 2010.
- Firefox and Opera did implement it.
- SPDY basically started to work on all the facets that HTTP/2
  eventually standardized, but HTTP/2 isn't just a standardization of
  SPDY. But it is very much based on the ideas trialed in SPDY.

## QUIC

- This is the point of **QUIC**.
  - This is introduced to IETF around 2015 by Google. But maybe only
    standardized 2021?
  - Basically, you multiplex multiple protocol streams over the same UDP
    connection.
    - Flow control/congestion logic mostly follows TCP.
  - It also incorporates TLS 1.3 and requires encryption of the stream.
  - Also, connections get a "Connection ID". That means a client can
    open a connection on WiFi, but then continue to use it after
    switching to cell network. Previously, connection relied on IP
    address.
    - Granted, you will need to re-establish a UDP connection. But you
      can pickup where you left off; you don't need to throw the whole
      stream away.
  - Last, I think the initial connection setup is optimized.
    - Normal TCP + TLS 1.3 involves (1) TCP setup, (2) TLS handshake,
      (3) client gets to start HTTP request.
    - But QUIC pushes TLS handshake into the initial setup round-trip.
      So you have one round trip, rather than two, before you can start
      making your actual request.
    - So this is a latency reduction idea.
  - Basically, this is a **modern alternative to TCP**.
- TCP vs QUIC
  - TCP is simpler than QUIC. And if you don't have multiple requests to
    multiplex, QUIC may not offer you much. It doesn't inherently give
    you higher throughput.
  - Since QUIC requires encryption, server must have TLS certificates to
    present to client. Client must have some way of deciding whether to
    accept: will it accept self-signed? Will it consult a certificate
    authority? Does it have a value already pinned?
  - Accepting self-signed certificates without checking with a
    certificate authority means client is susceptible to server
    impersonation.
    - Self-signed here means just that it's a certificate for a public
      key, but it isn't signed by a chain back to a certificate
      authority.
    - It is still a valid certificate for establishment of an encrypted
      channel.
    - And it still can be pinned by the client so that it can't change
      behind the scenes.
  - For simple inter-process communication, this is probably overkill.
  - TLS Note: a server certificate and certificate chain might be
    1.5-4KB. But a typical UDP datagram size is maybe 1.2KB. That means
    you might need multiple UDP datagrams sent by the server. But these
    of course can sent in parallel on UDP.

## HTTP/3

- Standardized in 2022. Basically it's HTTP over QUIC.
- The independent streams are now pushed into QUIC layer, so there is no
  head-of-line blocking across streams.
- Stream flow control/cancellation now built in to underlying QUIC
  transport, so HTTP/3 is simpler there.
- Basically, each HTTP request is one QUIC stream.
- HTTP/3 replaces HPACK header compression with QPACK. Basically, it
  needs to account for the possibility that data from streams
  arrives out-of-order.

# TODO: TLS

- 1994-1999: SSL comes out of netscape for passwords, banking,
  purchases.
- What is TLS vs SSL?
- Firesheep (Facebook session hijacking) occurs around 2010.
- What is HSTS: basically a server can tell a browser that it *must* use
  HTTPS for future connections (at least for a specified period). So
  that future connections don't "downgrade" to plain HTTP.
- 2015: Let's Encrypt makes it easier to get certificates.
  - It's a certificate authority that is a non-profit. Issues TLS
    certificates for free.
  - Entirely automated.
  - You prove control of a domain by adding a DNS record (at least
    temporarily) with their challenge token.
  - Owned by non-profit started by Mozilla and EFF, amongst others.
  - What do for-profit certificate authorities offer? They offer
    *organizational validation*. Basically, they not only show you have
    control over a domain (domain validation), but they also record in
    the certificate *who you are*. What organization are you. They can
    do this, but not in an *automated, low-cost way* like Let's encrypt
    does for domain validation.
  - So: Let's Encrypt can issue certificate for `bankofamerica.com` to
    someone who controls that domain, but they don't say in the
    certificate that "Bank of America, INC" controls the domain.
  - In the past, browsers would display the organization name of a
    certificate to show that the webiste you were visiting is really
    from Bank of America, INC.
    - You could imagine a bad guy sets up `bankofamerika.com` to pretend
      to be the true BoA. They even get a Let's Encrypt certificate.
    - But browser doesn't show "Bank of America, INC" in the browser
      bar, because Let's Encrypt *didn't* certify the organization!
  - There may be some special use cases where organization validation is
    important, but often not on the typical consumer web...
- 2018: Chrome begins labeling every non-HTTPS page as "not secure".
- 2021/2022: QUIC and HTTP/3 make TLS mandatory.
- 2021: Chrome always tries HTTPS first.
