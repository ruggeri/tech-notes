**TODO**: This is a list of CPU architecture subjects. I thought I
wrote all this stuff up long ago... I don't have time right now for
this, but these are the main subjects to writeup when I get around to
it.

## CISC and RISC

CISC combines several operations into a single instruction. In the
early days, CISC was considered an advantage because it meant that
people writing assembly could have an easier life. Also, very early
machines were slow to fetch instructions from memory, so it was a
performance advantage to ask the CPU to do multiple things.

* CISC, RISC
* VLIW, EPIC
* Flynn's Taxonomy (SIMD etc)
* Vectorized instructions
* Instruction pipeline
* Microarchitecture
* Branch Predictor
* Out-of-order execution
  * Memory dependence prediction
  * Hazard, Pipeline stall, bubble
* Superscalar processor
* Hyperthreading, SMT
* Speculative execution
* Vector processor
* NX Bit

# History of Instruction Set Architectures

- IBM System/360 (1964), PDP-11 (1970), VAX (1977)
  - These all mostly precede a personal computer era.
- Motorola 68k: 1979
  - Big users were Apple Macintosh (released 1984), Amiga (1985), Atari
    (1985).
  - Apple II (1977) used MOS 6502, which was basically a simplified
    clone of Motorola 6800 (predecessor to 68k). Apple II was the first
    "appliance" computer. Apple I was just a hobbyist circuit board,
    released 1976 (~200 units sold).
- Intel develops x86 (1978: 16bit, 1985: 32bit)
  - Earliest x86 chips were 8086 (1978) and 8088 (1979, minor variant).
    - Earlier chips like 4004, 8008 are pre-x86 architecture.
  - From the start, Intel sells license to AMD (1976) to make chips
    because big customers did not want to rely on only Intel to produce
    them. Deepens in 1982 with a second agreement.
  - Next gen was 80286 (1982). It maintains compatibility with software
    for 8086 and 8088.
  - IBM chose 8088 for IBM PC launched 1981. Successor IBM PC/AT in 1984
    ("Advanced Technology") used 80286 for backward compatibility.
    - IBM was trying to get to market quickly using off-the-shelf tech.
    - I think that 8088 was preferred over 68k because 8088 had 8bit
      external bus which worked with more peripherals, and kept cost
      down.
    - In long run, IBM created a standard around Intel x86, and also
      Microsoft for the OS. And IBM couldn't control the platform
      because it was too easily cloned because it was assembled from
      third-parties.
  - Many claim that 68k was more elegant than x86. But x86 benefited
    from IBM ecosystem.
  - A theme in computing hardware is that the platform itself wins, not
    just an individual technology on an individual merit.
- RISC Era
  - Basically, the "complex" instruction set of x86 looked difficult to
    pipeline. The simpler form of the more broadly similar "reduced"
    instruction set made it seem easier to pipeline.
  - MIPS (Stanford roots, then SGI, 1985), Sun SPARC (1986/1987), HP
    PA-RISC (1986), DEC Alpha (1992), IBM Power (1990) and PowerPC
    (1992, POWER derived, in consortium with Motorola and Apple).
    - I believe there were so many players trying to jump in because
      they wanted to become the dominant player, and displace Intel.
    - Opportunity seemed real because RISC-native would have an
      advantage even if Intel did microcode RISC.
    - They probably also hoped for vertical integration. They hoped
      their chip would be the dominant one and tie software into their
      system.
    - This was a battle in the high-end, for more valuable systems, and
      Intel wasn't necessarily going to be very successful there.
  - ARM ("Acorn RISC Machine" in Cambridge, UK, 1985/1987) also came
    from this era. We'll handle them separately.
  - Some of these RISC chips started offering 64-bit around 1992, btw.
  - PowerPC was the most widely used architecture. Used by Apple from
    1994 (starting with Power Macintosh) to 2006 when they transition to
    Intel. Obviously the Apple ecosystem helped make PowerPC a relative
    success.
    - Apple shipped a 68k emulator to run old software.
    - In theory, 68k could have done the microcode workaround that Intel
      did. But Motorola didn't have the marketshare to justify this
      expense.
    - BTW, NeXT shipped machines with 68k in 1988, 1990, 1992. Around
      1993 they withdrew from hardware manufacturing to focus on
      software. They had already ported NeXTSTEP to 80486 in 1991. Apple
      bought them in 1997.
  - The others were mostly used in higher-end workstations.
  - In the end, x86 had lots of users and software and Windows. Most
    users cared more about their software keep working.
  - And, x86 figured out how to translate CISC ISA to "microcode" of
    simpler, lower-level instructions. This was essentially CISC to RISC
    decoding/translation. You could get then perform the pipeline
    techniques to the micrcode.
    - It was no sure thing that Intel could pull this off, but it wasn't
      seen as impossible. It was a big effort justified by their market
      share.
    - You did pay a "tax" for the translation, but they had the
      transistor count and performance to do it. And lost of users
      preferred to pay this tax if it meant they could keep running
      their old software.
    - Key moment was Pentium Pro (also called 686) in 1995.
  - By the late 90s, it was clear x86 would not collapse.
    - In the 2000s DEC Alpha is killed after Compaq buys DEC.
      SPARC/MIPS/PA-RISC all are dying.
    - Apple transitions to Intel in 2005-2006.
- **TODO**: Q: why did apple not choose intel instead of POWER?

# TODO

These are disorganized notes from Notion

- Maybe a brief history of architectures?
    - Arm 64 is named aarch64
    - Around 64bit transition intel pushed Itaniym, but because it could run x86, it had huge trouble gaining market share.
    - ARM is a British risc, does a lot of licensing, targets low power devices. Eventually very good for phones. Aarch64 is ARMv8 which is a big moment when architecture became better suited for computers.
    - Recently arm is moving into data centers even.
- RISC-V is an open standard from Berkeley meaning to compete with arm but with no licensing fees .

- But it still feels like Apple may want to make their own chips and use an arch they control so they can tailor to their use case, and to maybe create loxkin
    - Can’t make x86 because Intel/AMD control a cross-licensed duopoly.
    - They can design chips with extensions, tempt vendors to build software that relies on those extensions.
    - If users want those apps, they depend on Apple.
    - Apple already started designing chips for phone, so they could leverage that synergistically to benefit their laptop business.
    - But the m1 is less about aarch64 ISA than other dimensions (control design and fab, have own cadence, target perf per watt rather than just perf, extensions for lockin).
    - Also, aarch64 could drop legacy modes because Apple can force apps to transition. So cleaner slate.
- Maybe note history of AMD:
    - In 70s-90s, it’s a second-source to Intel.
    - AMD beats Intel to 64bit and integrated memory controllers in 2003. Athlon/Opteron quite competitive with Intel.
    - But from 2006-2015, AMD declines badly, Intel dominates. AMD laden with debt after acquiring ATI. AMD spins off its foundry business.
    - Around 2017 Intel starts stumbling in processor, whereas AMD can use TMSC. AMD focuses on chiplets, which are important (what are they??). Zen/Ryzen/EPYC have success. ATI isn’t a cash cow yet, but it makes it a thread to nVIDIA.
        - Chiplets are basically assembling a chip from many smaller chips. AMD started doing it in 2017 and 2019.
        - Both yield and cost go up with more advanced nodes, so makes sense to make the most yield possible. Also, some parts of the chip don’t really *need* the most advanced nodes, so you can manufacturer those cheaper.
        - It is harder to package and connect the chiplets together. And it tends to introduce some non-uniform latencies (core-to-core, cache-to-cache).
        - This matters because it improves yield. Less likely to have an error that ruins the chip if each chip is smaller.
        - Intel does it to, they call the tiles. They started in 2023.
    - AMD crosslicensing agreement with Intel?
- Current status:
    - x86-64 is still dominant in workstations, general purpose CPUs. Cloud CPUs. Gaming computers. Still best cost-per-perf.
    - ARM: iPhones and Macs. Android phones (Qualcomm Snapdragon, Samsung Exynos, though Samsung historically used to do custom). Some interest in datacenters (AWS Graviton, Nvidia Grace). Makes more sense when perf per watt matters.
    - RISC-V is huge in microcontrollers, embedded controllers, IoT, automotive.
    - IBM Z lives on in mainframes, especially when there is legacy, and also where you want to have high reliability but you want to program it more like a single centralized machine.
    - POWER might still make sense if you value their enterprise stack. But probably doesn’t make sense for new development.
