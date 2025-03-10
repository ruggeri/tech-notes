- MicroSD is the format size that is used for cellphones or the GoPro.
  They are physically very small; about the size of a finger nail.
  - I think these are becoming seldom used; maybe just action cameras?
  - As of 2025, "real" cameras typically use full sized SD cards.
- SD cards are about double the physical size.
- They come in various _capacities_:
  - SDSC (standard capacity) can do up to 2GB.
  - SDHC (high capacity) can do up to 32GB.
  - SDXC (eXtended capacity) can do up to 2TB.
  - SDUC (ultra capacity) can do up to 128TB (I've never seen this).
  - For any of these standards, you can have Micro or regular size
    versions.
  - I believe that otherwise there is no physical distinction between
    these capacity types?
- Speed Class
  - This is weird. I guess the UHS-I (104MBps), II (312MBps), III
    (624MBps) refer to (half duplex) bus speed. That isn't necessarily
    the speed that data can be written or read, which is not symmetric
    normally anyway.
    - My Sony A7RV can only do UHS-II.
  - Thus, there are UHS speed levels. U1 requires at least 10MBps write
    speed, while U3 requires at least 30MBps write speed.
  - There are also _video speed classes_. V90 requires at least 90MBps
    write speed.
  - These classes are basically a non-numeric, simplified way to
    indicate the capabilities of a card.
- Typical markings
  - I have a Lexar card.
  - It says 128GB capacity.
  - It is marked SDXC, which corresponds to size format and capacity.
  - It says 300MB/s, which is a reference to bus speed.
  - Likewise, it is marked UHS-II. This is listed simply as roman
    numerals.
  - It is marked U3, which means it does 30MBps+ write. This is listed
    as the numeral 3 in a U (looks like in a garbage can).
  - It is further marked V90, which means it does 90MBps+ write. This is
    a V followed by the number 90.
- As of 2025-03-10 I use a SanDisk Extreme PRO 128GB card in my Sony
  a7rv.
  - It is marked SDXC (extended capacity) and 200MB/s and UHS-I (a
    simple Roman numeral).
  - It is marked C10 (10MB/s), U3 (can do 30MBps+ write) and V30 (30MBps
    video).
  - This has always been fast enough for my photography purposes.

## CFexpress

- This is a new form of media card.
- Let's start with CFexpress 1 (launched 2017). It basically connects
  two lanes of PCIe 3.0. It does full-duplex 2GBps.
- CFexpress 2 (2019) is basically the same, but it introduced three
  "types" of cards. These are physical formats. Now you connect either 1
  lane (Type A), 2 lanes (Type B), or 4 lanes (Type C) of PCIe 3.0.
  - This gives 1GBps, 2GBps, or 4GBps of full-duplex bandwidth.
- CFexpress 4 (2023) upgrades to PCIe 4. This doubles bus bandwidth.
  Physical format of cards stays the same. There was no CFexpress 3? As
  of 2025-03-XX, I don't believe any cameras do CFexpress 4 speeds yet?
- My Sony A7RV supports 1x CFexpress Type A in addition to a SD.
  Flagship camera models often offer either 1x CFexpress or 2x. And most
  flagships (besides Sony) do CFexpress Type B.
- I think if you shoot 8k video you may want to use CFexpress cards.
- Thankfully, it looks like read and write speeds are clearly written
  on cards in MBps. No more of all these crazy speed classes!
- Type B cards typically do over 1GBps in write speed (else 1 lane would
  have sufficed). Many Type A cards max out below 1GBps, but a few do
  over 1GBps (presumably meant to be used with CFexpress 4, but what
  devices even do that? Maybe USB-C card readers?).
- Many cameras don't specify CFexpress version in their spec sheets,
  because bandwidth/lane was constant until version 4 came out in 2023.
