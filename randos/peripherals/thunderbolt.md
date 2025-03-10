## Display Connectors

First you had VGA. This was typically used to send analog signal to a
CRT monitor.

You started to have DVI (Digital Visual Interface). This was developed
to support digital monitors. Interestingly, DVI to VGA conversion was
possible with just a converter cable because DVI carries an analog
VGA signal as well.

HDMI supersedes DVI. It drops the analog signal, supports some other
color modes used outside computing, and can also send audio and remote
control data. I believe also that HDCP (copy protection for HD content)
is incorporated into HDMI.

An alternative to HDMI is DisplayPort. HDMI and DisplayPort are
basically even competitors. HDMI is used more for home theater, and
DisplayPort for computing. But HDMI is very prevalent on the computer
side as well.

## DisplayPort and Mini DisplayPort Connectors

You have your usual DisplayPort connector, but this is a little chunky
for a laptop. Thus, Apple created **Mini DisplayPort**. It's just a
different shape for the same connectors. It's not a different protocol.

HDMI also comes in several flavors. Type A is the standard plug size.
Type B has never been implemented. There is Type C, which I don't
believe I ever see. And there is Type D, which is about the size of
micro USB; I have a converter for this type, though I don't know that
I've used it?

Apple has done this a number of times: Mini-VGA on the iBook, Mini-DVI
on the PowerBook G4.

## HDMI to DisplayPort and DispalyPort to HDMI

Say you have a device that outputs DisplayPort, but a monitor that
accepts HDMI. Then some DisplayPort output ports (if they support
Dual-Mode, also called DP++), can connect to an HDMI display with a
_passive_ adapter. The DisplayPort output device will do the necessary
conversion.

However, it is very important to note this does _not_ work in the
opposite direction. You cannot plug an HDMI output into a DisplayPort
input. Dual-Mode is _unidirectional_. Wow, what a terrible cable to
exist! You _think_ it might work HDMI -> DP, but it won't!

They do make _active_ adapters that can do this, but I don't have one. I
_do_ have a passive DP -> HDMI adapter, but it didn't work from my Sony
A7RV HDMI output to my Dell monitor DP input.

Source: https://en.wikipedia.org/wiki/DisplayPort#DisplayPort_Dual-Mode_(DP++)
Source: https://www.makeuseof.com/difference-between-converting-hdmi-displayport/

## Thunderbolt 1 and 2

Apple wanted to combine both video and data over a single cable. In 2011
it produced Thunderbolt using Mini DisplayPort as the physical
cable/interconnect. How?

Remember: in 2011 we only had USB 3.0 which was 5Gbps. USB 3.1 Gen 2
(10Gbps) wouldn't be specified until 2013. So USB wasn't a solution.

Thunderbolt 1 supported up to 20Gbps. DisplayPort cables could do 5.4
Gbps on each of four lanes. Thunderbolt 1 used two lanes for video, and
two lanes for data. So you had 10.8 Gbps for video, which could drive
the 2011 QHD Thunderbolt Display at 60Hz (requires 6.64 Gbps). And you
had 10.8 Gbps for data. That's about equivalent to USB 3.0, which
offered 5 Gbps in each direction.

**Thunderbolt 2**

Thunderbolt 2 came out in 2013 and simply allowed for more flexible use
of the lanes. For instance, you can use all 20 Gbps for one of the
channels. That would allow 4K@60Hz video (requires about ~18 Gbps).

However, then you couldn't do USB data at the same time. Wasn't the
ability to do video and data simultaneously the whole point of
Thunderbolt 1? Indeed: I don't think I ever used Thunderbolt 2 for
video, since I didn't have a UHD monitor at that time.

Thunderbolt 2 did have the advantage in 2013 that it had higher
bandwidth than USB 3.1 Gen 2. Thus, it was a superior bandwidth choice
for peripherals. But in this market, it was fighting against USB, which
was very entrenched. I'm not sure there were many Thunderbolt 2
peripherals manufactured.

**Power**

Note that neither Thunderbolt 1 and 2 deliver power. That's why my
Thunderbolt display needed a second cable to deliver power.

## Thunderbolt 3 and USB 4

Eventually in 2017 USB 3.2 Gen 2x2 offered 20 Gbps up and 20 Gbps down
by using 2 10 Gbps SuperSpeed+ differential pairs for each direction.
Note that this requires USB-C cables. That matches the performance of
Thunderbolt 2.

But in 2016, Apple was already producing MacBookPros with up to 4
Thunderbolt 3 ports. Thunderbolt 3 also runs on USB-C. However, it
manages to do 20 Gbps on each differential pair. That means 40 Gbps up
and 40 Gbps down.

Because it doubles the bandwidth, Thunderbolt 3 can run _two_ 4K@60Hz
monitors simultaneously. As usual, nothing is left for fast peripherals.

In 2019, USB4 is released, based on Thunderbolt 3. It brings USB and
Thunderbolt 3 to parity. At this point, Thunderbolt is becoming a
brand-name more than a standard, since USB4 has all the same
capabilities (though not the same minimum requirements).

## Thunderbolt 4 and 5

Thunderbolt 4 adds some features, but doesn't change the speed.
Thunderbolt 4 will require cables to always support 40Gbps. It sounds
like Thunderbolt 4 is basically just raising the minimum requirements to
be Thunderbolt 4 certified. USB4 should already be capable of this.

As of 2025-03-XX, it appears Thunderbolt 5 means USB4 v2. So 80Gbps
bidirectional (or 120GBps/40Gbps asymmetric). I believe USB4 v2 is
already supposed to support DisplayPort 2.1, but this is definitely part
of Thunderbolt 5.

Thunderbolt, as described, basically just _mandates_ some features which
are otherwise _optional_ for USB4 v2. And to use Intel's Thunderbolt
logo, you have to get your equipment tested in a laboratory. The main
difference may be that Thunderbolt mandates 140W vs 100W power.

Source: https://www.cablematters.com/blog/Thunderbolt/usb-c-vs-thunderbolt-3-which-one-is-better
Source: https://www.cablematters.com/Blog/Thunderbolt/thunderbolt-5-vs-usb4
Source: https://www.graniteriverlabs.com/en-us/technical-blog/a-complete-overview-of-differences-between-tbt5-tbt4-and-usb4
