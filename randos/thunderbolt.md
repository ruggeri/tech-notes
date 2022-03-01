## Display Connectors

First you had VGA. This was typically used to send analog signal to a
CRT monitor.

You started to have DVI (Digital Visual Interface). This was developed
to support digital monitors. Interestingly, DVI to VGA conversion was
possibly with just a converter cable because DVI carries an analog
VGA signal as well.

HDMI supersedes DVI. It drops the analog signal, supports some other
color modes used outside computing, and can also send audio and remote
control data. I believe also that HDCP (copy protection for HD content)
is incorporated into HDMI. HDMI is most common in

An alternative to HDMI is DisplayPort. HDMI and DisplayPort are
basically even competitors. HDMI is used more for home theater, and
DisplayPort for computing. But HDMI is very prevalent on the computer
side as well.

## DisplayPort and Mini DisplayPort Connectors

You have your usual DisplayPort connector, but this is a little chunky
for a laptop. Thus, Apple created **Mini DisplayPort**. It's just a
different shape for the same connectors. It's not a different protocol.

Apple has done this a number of times: Mini-VGA on the iBook, Mini-DVI
on the PowerBook G4.

## Thunderbolt 1 and 2

Apple wanted to combine both video and data over a single cable. In 2011
it produced Thunderbolt using Mini DisplayPort as the cable/receptacle.

DisplayPort at that time could do 5.4 Gbps on each of its four lanes.
That was about 20 Gbps. This was much more more than the 5 Gbps of USB
3.0 which arrived 2008.

Thus, DisplayPort made sense. On Thunderbolt 1, they split up the
DisplayPort lanes into two channels, each doing 10 Gbps. One channel can
run video and the other data.

Thunderbolt 2 simply allowed for more flexible use of the two channels.
For instance, you can use all 20 Gbps for one of the channels. That
would allow 4K@60Hz video.

Thunderbolt 1/2 receptacles are (typically) distinguished from regular
Mini DisplayPort receptacles with a Thunderbolt icon.

Note that Thunderbolt 1 and 2 does not deliver power. That's why my
Thunderbolt display needed a second cable to deliver power. Note that
the Thunderbolt Display is QHD @ 60HZ which needs 5.6 Gbps, so they
couldn't have just used USB 3.0 speeds and had anything left for
high-speed data.

## Thunderbolt 3 and USB 4

Eventually USB 3.2 Gen 2x2 offered 20 Gbps in each direction.

Thunderbolt 3 takes the same connector but doubles the bandwidth to 40
Gbps in each direction. With 40 Gbps, it can run two 4K@60Hz monitors
(without DSC).

Thunderbolt 3 will basically become USB 4. One distinction is that Intel
controls certification and testing of all Thunderbolt 3 controllers and
cables. At this point, Thunderbolt is becoming a brand-name more than a
standard, since USB4 has all the same capabilities (though not the same
minimum requirements).

## Thunderbolt 4

Thunderbolt 4 adds some features, but doesn't change the speed.
Thunderbolt 4 will require cables to always support 40Gbps.

Source: https://www.cablematters.com/blog/Thunderbolt/usb-c-vs-thunderbolt-3-which-one-is-better
