Big endian means the most significant byte is stored first. It's
basically how you would write the number in binary.

In little endian, the least significant byte is stored first.

There is also the concept of "bit endianness." This doesn't really
exist for CPUs, because there is no concept of bit addressing. All
values that you use are manipulated in terms of byte quantities. So
there's no way to tell what "order" an x86 CPU stores the bits in. It
is transparent.

But some media have bit serialization. Little endian bit ordering is
used by Ethernet and USB.
