# DNS Records

* A: this maps a hostname to a 32bit IP address.
* AAAA: this maps a hostname to a 64bit IP address.
* CNAME: this maps a hostname to another host name.

We know that fetching the DNS records for a domain can be done
recursively. You ask one DNS server, and this can in turn ask another,
until some server actually knows the answer. The answer may be either an
A or a CNAME record. If the answer is a CNAME record, the process is
repeated for the second name.

A server that is configured to give a response to a domain name lookup
is an *authoritative name server* for the specified domain. A domain
name server that merely replicates DNS information fetched from other
servers is a *caching name server*.

When you query DNS records for `x.y.z`, a domain name server wants to
ask: what is the authoritative name server for `x.y.z`? It may have
already cached this information. But, if not, it may ask: what is the
name server for `y.z`. If it doesn't know this, it will ask what the
domain name server is for `z`, which is a TLD.

Okay. The root zone, `.` has 13 root name servers. (Actually, some of
these are 'anycasted', which is a way of creating more redundancy. But
more later.) 13 is the maximum practical number of servers, because of
IP packet size. The 13 are run by various organizations: Verisign,
University of Maryland, NASA Ames, et cetera.

Here they are (notice permuted order for balancing):

```
>> host -t ns .
. name server b.root-servers.net.
. name server c.root-servers.net.
. name server d.root-servers.net.
. name server e.root-servers.net.
. name server f.root-servers.net.
. name server g.root-servers.net.
. name server h.root-servers.net.
. name server i.root-servers.net.
. name server j.root-servers.net.
. name server k.root-servers.net.
. name server l.root-servers.net.
. name server m.root-servers.net.
. name server a.root-servers.net.
```

The root name servers know who are the authoritative name
servers for the various TLDs. Some examples:

```
# All are run by Verisign.
>> host -t ns com
com name server k.gtld-servers.net.
com name server c.gtld-servers.net.
com name server h.gtld-servers.net.
com name server d.gtld-servers.net.
com name server f.gtld-servers.net.
com name server e.gtld-servers.net.
com name server j.gtld-servers.net.
com name server i.gtld-servers.net.
com name server b.gtld-servers.net.
com name server g.gtld-servers.net.
com name server a.gtld-servers.net.
com name server l.gtld-servers.net.
com name server m.gtld-servers.net.
```

And for the ru TLD.

```
>> host -t ns ru
ru name server e.dns.ripn.net.
ru name server f.dns.ripn.net.
ru name server d.dns.ripn.net.
ru name server b.dns.ripn.net.
ru name server a.dns.ripn.net.
```

Per Stack Overflow, the TLD authoritative name servers know the name
servers of every domain name registered in that TLD.

When you register a name with GoDaddy, GoDaddy communicates with
Verisign. One of the key pieces of information for GoDaddy to tell
Verisign is to specify the nameservers for your domain.

Source: https://superuser.com/questions/271428/does-a-gtld-servers-net-have-a-list-of-all-com-domains

Registrars like GoDaddy may do other things. For instance, keep track of
who owns what domains. But for our purposes, we only care that they are
responsible for telling Verisign about the name servers for a registered
domain.

Take AWS. AWS Route53 is a registrar for .com. You can buy domain names
and designate name servers. Route53 will also run nameservers for you.
This is the default. You can configure their domain name servers.
Responses can be static, or dynamic. It can use information like the
requester's geographic location. If desired, you can tell the Route53
registrar service to point to use another service's DNS. This is what I
needed to set to point `kate-ruggeri.com` to Cargo's DNS.

If I run `uchicago.edu`, then I might want to federate further the DNS
of `math.uchicago.edu` and `physics.uchicago.edu`. I want the two
departments to be able to configure their own mail servers, subdomains,
et cetera.

For this reason, as the administrator of `uchicago.edu`, I will set up
NS records for `math.uchicago.edu` and `physics.uchicago.edu`. I will
point to the name servers that these departments specify. An NS record
can list multiple (up to 13, presumably). I may also set a very high TTL
on this record, since I do not expect these name servers to change very
often.

Now the `math.uchicago.edu` people can use AWS Route53 name servers and
the `physics.uchicago.edu` people can use Cloudflare and everyone is
very happy.

Fundamentally, when we are doing a DNS query for `uchicago.edu`, an NS
lookup is going to the DNS servers for `.edu`.

## TODO

* What is the purpose of apex NS records.
* Why can we not have a CNAME at the apex?
* What is the point of SOA records?
