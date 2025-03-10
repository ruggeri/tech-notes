## USB Connectors

**USB-A and USB-B**

First, we need to talk about USB connectors. Originally, you had USB-A
(the flat plug we all know), and USB-B (the "house" type plug). The
plugs were designed so that USB-A plugged into your computer, and USB-B
(if needed), plugged into the device. USB-B might be plugged into a
monitor for instance.

**SuperSpeed Connectors/Pins**

USB-A and USB-B both started with four pins. Eventually, in later USB
revisions, they wanted to support higher transfer speeds which meant
more pins. Thus, USB 3.0 and 3.1 variants of USB-A and USB-B have 9
pins: the original four pins plus five new ones. These are called USB
A/B **SuperSpeed** connectors.

We will learn later that the four pins consist of two **differential
pairs**. A differential pair is like a balanced line: the two members of
the pair sends positive and negative images of the same signal. Each
pair is used to send one (logical) signal: one pair is used for
transmission and the other pair is used for receiving. I believe that
the differential pair is able to allow longer cable runs since EM
interference can be cancelled.

The USB-A connector had identical shape, so that a USB-A 3.0 (AKA
SuperSpeed) cable could be plugged into any USB-A receptacle. The USB-B
shape was somewhat changed, to add a "roof" to the house. A USB-B 3.0
connector cannot be plugged into a USB-B 2.0 port. However, a USB-B 2.0
connector _can_ be plugged into a USB-B 3.0 port (and protocol
downgrades to USB 2.0 of course).

You can tell a SuperSpeed USB-A connector or receptacle because it is
colored **blue**. This is for reference only; you can use a
non-SuperSpeed cable in a SuperSpeed receptacle, or a SuperSpeed cable
in a non-SuperSpeed receptacle. You just won't get the SuperSpeed. The
original four pin connection will be used.

USB 3.1 Gen 2 USB-A ports (also called **SuperSpeed+**, and capable of
10Gbps bidirectional) are colored **teal**.

**Micro-USB and Mini-USB**

They also wanted to connect smaller devices like cellphones or portable
rechargeable items. These are called Mini and Micro USB.

I don't believe I have ever seen USB Mini-A and Micro-A. Normally Mini-B
and Micro-B have full-sized USB-A on the other end for plugging into a
full-sized USB hub.

Mini-B is not that commonly used: I have seen it on some medium sized
devices only. I think I might have seen it on an old external hard
drive? Mini B was clearly unloved because it never even got SuperSpeed
variants.

Much more common (almost universal) is USB Micro-B. I presume that
durability of Mini-B is superior, but Micro-B is a more convenient
(small) size. Tons of rechargeable things use Micro-B.

There is a SuperSpeed versions of Micro-B. This have an extended shape
to support the extra pins. I have seen these only _exceedingly rarely_.
I think it might have been on a smartphone?

**USB-C**

At some point, it was decided to begin to transition to USB-C. USB-C has
a number of advantages: it has the same connector on both ends. Second,
it can be inserted in either orientation.

It has EVEN more pins, allowing for a total of four SuperSpeed
differential pairs. As we will learn, not all USB-C controllers will use
all the available differential pairs.

USB-C continues to have four pins dedicated to the same purpose as the
USB-A/USB-B connectors. This allows USB-C cable to be used to be
backward compatible and deliver the original USB protocol.

## USB Protocols

**USB 1.0 and USB 2.0**

So, we have a story of increasing bandwidth. USB 1.0 started out with 12
Mbps. In 2001, USB 2.0 was released and it can do 480 Mbps. This
increase in bandwidth must have been achieved either by higher
signalling rate or better protocol efficiencies.

These low bandwidths could be supported simply with the original USB-A
and USB-B connectors.

**USB 3.0**

In 2008, we got USB 3.0. It can do 5 Gbps.

To support this, they added the five new pins of the **SuperSpeed**
connectors. These added two (differential pair) _lanes_ (one transmit,
one receive). As mentioned, you can use USB-A 3.0 cables with USB-A 2.0
ports (and USB-A 2.0 cables with USB-A 3.0 ports); you'll just downgrade
to USB 2.0. The connectors are physically compatible. But the USB-B 3.0
connector cannot be plugged into a USB-B 2.0 port, though a USB-B 2.0
connector _can_ be plugged into USB-B 3.0 port.

Note that USB 3.0 protocol/cables are **full duplex**: they support 5
Gbps in each direction (i.e., on each lane). That gives them total
bidirectional bandwidth of 10 Gbps.

Because USB-A 3.0 receptacles are physically compatible with USB 1.0/2.0
cable, we can fall back to the original standards and run at lower
speeds. Likewise, even though USB-C connectors are not physically
compatible with USB-A or USB-B, some of USB-C's pins are dedicated to
the same old purposes, so we can run USB 1.0/2.0 on USB-C.

Note that since mini-USB didn't get SuperSpeed variants, it was
effectively a dead-end as early as 2008.

**USB 3.1 (Gen 2)**

In 2013, USB 3.1 **Gen 2** does 10 Gbps (in each direction). This is
called **SuperSpeed+**. This gets double the bandwidth out of each
differential pair lane, presumably by increased clock rate and lower
protocol overhead.

Somewhat confusingly, the USB 3.0 standard is incorporated into the USB
3.1 standard as USB 3.1 **Gen 1**. This means that USB 3.1 Gen 2
controllers must support the old USB 3.0 mode (now called USB 3.1 Gen 1)
and its 5 Gbps protocol.

Of course, there is no needed change in cabling, because we are just
squeezing more Gbps out of the same physical format.

**USB 3.2 Gen 2x2**

In 2017, USB 3.2 wants to do 20 Gbps. However, it can no longer squeeze
more bandwidth out of a single lane.

The solution is with the USB-C connector. It actually has eight
differential pair pins that allow for **four lanes** (two in each
direction). When a USB-C cable is used for USB 3.1 Gen 1 or USB 3.1 Gen
2, only two of these lanes were used (one in each direction). I think
which of the two transmit lanes (and which of the two receive lanes)
gets used depends on the insertion orientation.

When USB3.2 Gen 2x2 is used then all four lanes (two in each direction)
are used.

Thus, without increasing bandwidth per lane, you can increase overall
transmission/receiving bandwidth by using two lanes each for
transmission and receiving. This is called **USB 3.2 Gen 2x2**. Thus you
get 2\*10Gbps=20Gpbs bandwidth in each direction.

As before, USB 3.2 Gen 1 is just USB 3.0 (5Gbps) and USB 3.2 Gen 2 is
just USB 3.1 Gen 2 (10Gbps). When running at these reduced levels, you
use just the original two lanes.

A USB 3.2 2x2 receptacle must be a USB-C receptacle, since only USB-C
has the pins for the four data lanes.

**USB 4**

In 2019, USB 4 is released. This supports 20GBps on a single lane! Thus
you can do 40Gbps bidirectional. And in 2022, USB4 2.0 (really!)
supports 40GBps per lane, for 80Gbps bidirectional.

This incorporates all the old USB standards as follows:

- Note: there is no USB4 Gen 1; they don't bother incorporating a 5Gbps
  mode as part of USB4.
- USB4 Gen 2x1: 10Gbps symmetric using a single lane.
- USB4 Gen 2x2: 20Gbps symmetric using both lanes.
- USB4 Gen 3x1: 20Gbps symmetric using a single lane at double the USB
  3.2 Gen 2 data rate.
  - I wonder if this is widely implemented. Why not just use USB4 Gen
    2x2, which has wider
- USB4 Gen 3x2: 40Gbps symmetric using both lanes at double the USB 3.2
  Gen 2 data rate.
- USB4 Gen 4x1: 40Gbps symmetric using a single data lane at 4x the
  Gen 2x2 data rate!
  - Again, I wonder if this is widely implemented; USB4 Gen 3x2 offers
    the same Gbps.
- USB4 Gen 4x2: 80Gbps symmetric using both data lanes at 4x the Gen 2x2
  data rate.
- USB4 Gen 4 Asymmetric
  - Uses three data lanes, allocating only one for upstream.
  - Thus, can do 120Gbps in one direction.

Note, USB4 was based on Thunderbolt 3.

Source: https://en.wikipedia.org/wiki/USB4

**USB Naming**

USB naming has been a mess. There is a desire to name USB by its
unidirectional throughput. Thus, the following marketing names will be
used:

- USB 1.0 and USB 2.0 stay named the same.
- USB 5Gbps is used instead of USB 3.0, USB 3.1 Gen 1, USB 3.2 Gen 1.
- USB 10Gbps is used instead of USB 3.1 Gen 2, USB 3.2 Gen 2.
- USB 20Gbps is used instead of USB 3.2 Gen 2x2.
- USB 40Gbps is used instead of USB4 Gen 3x2.
- USB 80Gbps is used instead of USB4 Gen 4x2.

This might bring some sense to the nonsense of the naming...

Source: https://en.wikipedia.org/wiki/USB#/media/File:USB_2022_September_naming_scheme.svg

## Cables

USB-C cables seem to frequently leave out pins. In particular, many
USB-C cables only support USB 2 by leaving out the USB3 data pins. This
is especially popular for USB-C cables intended for charging. Sadly,
about half the USB-C cables in my household are for charging only.

Now, let's talk about the _length_ of cables. Cables cannot be
infinitely long, because of signal degradation.

For USB 2.0 (480Mbps), cables of 5m (16.4ft) are the maximum recommended
(by the standard?). For USB 3.0 (5Gbps) and USB 3.1 cables (10Gbps), the
maximum cable length is 3m or 9.8ft. That stayed the same in USB 3.2 Gen
2x2 (20Gbps): remember, that just added a second data lane in each
direction so didn't change the datarate over the lane (stayed at 10Gbps
per lane). Thus this shouldn't change sensitivity to noise. (Anker says
3.2 2x2 should be max 0.8m? But Cable Matters says 3m. I think Cable
Matters is probably right?)

I will note that TetherTools sells a 15ft USB-C cable that does USB
3.0/USB3.1 Gen 1/USB 3.2 Gen 1 5Gbps. It does not use any kind of active
cable. I guess if USB 3.1 Gen 2 cables can do 10Gbps per lane and 3m,
then it feels like 5Gbps should be doable above 3m. And maybe the
TetherTools cable rejects more interference than usual?

Source: https://www.anker.com/blogs/cables/usb-cable-max-length
Source: https://www.cablematters.com/Blog/USB-C/usb-cable-max-length
TetherTools Cable: https://www.bhphotovideo.com/c/product/1387534-REG

USB-C cables can be longer (up to 4m) if they are only used for power
delivery!

But now you want USB4 cables, which are supposed to do 20Gbps (Gen 3) or
even 40Gbps (Gen 4) _per_ lane for up to 80Gbps of symmetric bandwidth!
That restricts cable length to 0.8 meter (aka 2.6 feet). That's a pretty
short cable!

**Active Cables**

These use chips to deal with signal attenuation. Apparently there are
USB 2.0 active cables of about 33 feet. That's crazy!

Cable Matters does 20Gbps at 5m/16.4ft for $97. Compare to $19.99 for
Cable Matters 2m/6.6ft 20Gbps cable.

If you want to do USB4 at 3 meters/9 feet, you need an active cable.
I've linked one sold by Apple for $159; it does 40Gbps (presumably USB4
Gen3x2) at 3m (9.8ft)! OWC does a 15ft optical 40Gbps cable for $130.

Source: https://www.cablematters.com/Blog/USB-C/usb-cable-max-length
Source: https://blog.tripplite.com/usb-cable-max-length
Source: https://www.amazon.com/Cable-Matters-Charging-Compatible-Thunderbolt/dp/B0B5JJ55MB
Source: https://www.apple.com/shop/product/MW5H3AM/A/thunderbolt-4-usb%E2%80%91c-pro-cable-3-m
Source: https://www.owc.com/solutions/usb4-cables

## USB Power

USB cables can deliver power as well as data. Originally I think they
could do about 7.5W. Now-a-days they can do up to 100W. This is plenty
for charging a laptop, I guess.

USB-C can deliver up to 20V at 5A: 100W. Traditionally USB gave 5V on
the power pin, but later specifications like USB Power Delivery made the
voltage negotiable somehow.

I'm not sure how that works. I don't know if you can use USB Power
Delivery (up to 100W power) at the same time as using USB data.

**Weird Charging**

You may notice that some devices need specific charging cables to work.
This commonly happens when the supplied cable has USB-C as an input to
the device, but plug into a charger with USB-A.

Why? The reason is that USB-C charging ports will only supply voltage to
devices which say what voltage they want (+5V, +12V, +40V). It won't
supply +5V by default like USB-A charging ports did.

Devices like Upright GO, which have a USB-C port on the device, still
expect to be plugged into a USB-A power supply port. They don't
implement the USB-C logic to request the +5V they need.

Why not just supply +5V by default? Because there isn't a pre-defined
source and sink to USB-C. You can plug two laptops to each other using
USB-C, and which one should be supplying the voltage? If they both tried
to do so, this would create problems.

Source: https://www.reddit.com/r/UsbCHardware/comments/c3q4s6/why_do_some_usbc_devices_not_charge_with_a_usbc/

## USB Alternate Mode

Can USB-C cables be used to deliver video? Yes. This is called alternate
mode. Lanes can be dedicated to DisplayPort communication (the most
common alternate mode).

Let's keep in mind a target goal: 4K at 60Hz. To do this, we need 18
Gbps of bandwidth. (Other sources say 12 Gbps?)

With USB 3.1 Gen 2, you can use one Tx and one Rx lane for USB, and the
other Tx/Rx lanes are given for two-lane DisplayPort. Note that
DisplayPort repurposes the Tx/Rx lanes for transmission.

DisplayPort 1.2 (from 2010) does 5.4 Gbps per lane (this is called
HBR2). So two HBR2 lanes (10.8 Gbps) is not enough for 4K@60Hz.
DisplayPort 1.3/1.4 (from 2014/2016) does 8.1 Gbps (this is called
HBR3), so we get close with two HBR3 lanes (16.2Gbps), but not enough
for 4K@60Hz.

Note that even though a USB-C lane can do 10Gbps, DisplayPort
under-utilizes the channel when it repurposes the USB-C lane as HBR2 or
HRB3.

Thus, for either DisplayPort 1.2 or 1.4, you must use **all four** USB-C
lanes for DisplayPort to get 4K@60Hz. That leaves nothing for SuperSpeed
USB, so you fall back to USB 2.0 speeds. This is how my Dell monitor is
configured.

**DisplayPort 1.4, 2.0 and DSC**

DisplayPort 2.0 (2019) has a 10 Gbps per lane mode called UHBR 10. This
could conceivably drive 4K@60Hz with just two USB-C lanes if it can do
10Gbps per lane. Any USB 3.2 Gen 2x2 20Gbps cable could do this.

The Radeon on my MacBook Pro doesn't do DisplayPort 2.0 though.

Alternatively, DisplayPort 1.4 has a feature called DSC (Display Stream
Compression). This does a 3:1 lossy compression on the image. They say
the compression is transparent to humans. This compression vastly
reduces the bandwidth needed to drive 4K@60Hz. Thus, you could use two
lanes of HBR3 (or even just HBR2) to drive 4K@60Hz if you turn on DSC.
That would leave the Tx/Rx lanes needed for USB-C SuperSpeed 10 Gbps.

It seems that maybe Apple broke/removed DSC support for MacOS
post-Catalina? Maybe just for AMD GPUs? Maybe it will come back? As of
Mar 1 2022, it is broken and has been since Nov 2020 I think!

Last, let's consider 8k@60. That needs 4x the bandwidth of 4k:
18Gbpsx4=72Gbps. Note that a USB 3.2 Gen 2x2 20Gbps cable wouldn't be
enough _even if you used all four lanes!_ You need at least a 40Gbps
cable, capable of doing 20Gbps per lane, and it would still need all
four lanes with nothing left over for SuperSpeed data peripherals. You
could also try a 80Gbps cable, but as of 2025-03-07 I've never seen
that. So you probably need to use a dedicated DisplayPort or HDMI cable
to drive a monitor like that.

Source: https://www.bigmessowires.com/2019/05/19/explaining-4k-60hz-video-through-usb-c-hub/
Source: https://www.reddit.com/r/Monitors/comments/phkcxl/4k_144hz_intel_or_just_amd_macs_macos_monterey/
Source: https://en.wikipedia.org/wiki/USB_hardware#Connectors

## Cabling Needs

- I have a few USB-A slots I use. These are for my Logitech mouse and my
  keyboard. I plug these into my monitor. They are fine.
- I have a number of micro USB devices, though they are becoming fewer I
  believe. They include my wireless speaker and my Kindle. I don't carry
  these with me often.
- I need Lightning connectors for my iPhone 13 Pro and my AirPods. I
  might upgrade my AirPods in the near future, but I will have the
  iPhone 13 Pro for many years. Thus, I must retain Lightning connectors
  for some time.
  - Upgrading AirPods will require that I carry double-ended USB-C
    cables more places, which is a bit of a nuisance. Thus, I might
    avoid upgrading as long as possible?
- I have a number of UBS-C items. My MacBook Pro. Ethernet adapter and
  SD card reader. Monitor. However, most of these are for computer
  peripherals, not for charging (the exception is the MacBook Pro).
- I feel like I should always carry a USB-C to Lightning and a USB-C to
  USB-C charging cable with my portable battery. I don't suppose I need
  a USB-C to micro-USB cable unless I am taking my Kindle with me.
