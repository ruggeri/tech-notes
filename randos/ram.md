- RAM: random access memory. Many ways to implement.
- DRAM: dynamic RAM. "Dynamic" means that memory is stored as a charge
  in a capacitor that leaks away over time, so needs to be refreshed
  occasionally.
- SDRAM: Synchronous DRAM. Synchronous means that commands/data transfer
  are synchronized to a clock. Older DRAM wasn't synchronized.
- SDR SDRAM: single-data rate SDRAM. Means one transfer per clock.
- DDR SDRAM: double-data rate SDRAM. Transfers on leading and tailing
  edge of clock. Doubles bandwidth.
- DDR2, DDR3, DDR4, DDR5:
  - Generations of DDR SDRAM. Each is a standard by JDEC. The three RAM
    manufacturers Samsung, SK hynix, and Micron are the critical
    members.
    - There may be more than the big three manufacturing RAM, but they
      are the biggest, especially for the most advanced modules.
  - No compatibility across generations. Can't put DDR3 in DDR4 socket,
    or vice-versa.
  - Increased transfer rate (clock), increasing bandwidth.
  - Lowered voltage
  - Greater storage per module.
  - Latency tends to remain constant.
- When you see something like: DDR5-5600, this means 5600 million
  transfers per second.
  - A single channel is 64bits=8bytes.
  - So bandwidth is `2channels x 8bytes x 5600MT/s = 89.6GB/s`.
- **TODO**: GDDR.
- **TODO**: HBM High Bandwidth Memory.
