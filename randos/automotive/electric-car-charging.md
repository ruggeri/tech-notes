In the US, if you plug in your electric car to a typical outlet, you are
going to charge with 120V. Your typical circuit is going to do 20A.
That's 2.4kW of power. This is called Level 1 charging.

The next level up is 240V charging. Here you combine the two 120V phases
of the residential power. You need to install a NEMA 14-50 outlet; this
has two live wires, a ground, and a neutral. Technically, the neutral
won't be used for this application. Anyway, you want to install a 50A
circuit. 240V times 50A gives 12kW.

(Actually, it looks like Tesla sells an adapter for NEMA 6-50 which
doesn't have neutral).

Note: Level 2 charging can also happen at 208V. 208/120V is common in
commercial usage. In big box stores and industrial you can have
480/277V. I believe the reason that higher voltages are preferred is
that equipment that needs to draw a lot of power might want to do so at
high voltage and low amperage. However, in the case of 208/120V, the
line-to-line voltage is less than the 240V you get from the split-phase
240/120V.

Level 3 chargers are DC. Tesla's "Supercharger" line does
100kW/120kW/250kW for v1/v2/v3. Electrify America does 50kW/150kW/350kW.

What voltage do you use for Level 3 charging? Tesla uses 400V, but some
cars use 800V charging. The advantage is that you can do higher wattage
without higher current. But, a problem is that you are applying higher
voltages to the car. That complicates the car's design.

Also: you can only dump so much energy into a battery so fast. There is
going to be heat produced, and it is more heat the faster the charging.
This is a limitation. If the heat is due to _current_, then maybe a
larger battery pack can limit the amount of current. But, I think that
the limitation might be more about battery _chemistry_. Not exactly sure
what comes into play here.

I think that a main challenge in increasing voltages is switching so
much voltage. The connectors are part of the problem. Tesla hasn't
traditionally supported up to 1kV, but it is bringing that out
eventually.

I think that 400V batteries are just easier to work with, since 400V
needs to be supplied to the motor. I don't think you can use 800V to
charge a 400V battery? At least not with power transformation, in which
case you have big currents in the on-board battery charger? GM gets
around this by using a trick: they have two battery packs, and they wire
them in series when charging at 800V, but wire them in parallel when
driving.

## Charger Networks

- Tesla
- Electrify America
- EVgo

## Charger Connectors

- SAE J1772
  - This is single-phase AC.
  - It doesn't support DC charging.
  - Thus it can't do fast charging.
  - This was also used in Japan.
  - 1 pin live, 1 pin neutral, 1 pin protective earth. Two signalling
    pins.
  - Standardized as IEC 62196 Type 1.
    - There is a European IEC 62196 Type 2. This can connect 1 or 3
      phases. It has two extra pins, of course.
      - They are in a different arrangement than the J1772.
    - There is even a IEC 62196 Type 3 that was used in Italy or France.
      It was a different physical format, but same idea as Type 2.
  - I don't believe new battery electric vehicles really use SAE J1772
    anymore. It can't do enough power.
    - Plug-in hybrid vehicles with small wattage on-board chargers (like
      the RAV4 Prime) do often use SAE J1772.
- CCS: Combined Charging System
  - CCS1 extends SAE J1772. It adds two DC pins. This is CCS Type 1.
    - The CCS1 connector doesn't have the AC pins. But the port accepts
      them.
    - Thus J1772 connector can fit CCS1 port. But CCS1 connector can't
      fit J1772.
  - There is also CCS Type 2. That extends IEC 62196 Type 2.
    - IEC 92196 Type 2 connector can fit CCS2. But of course CCS2 won't
      fit IEC 92196 Type 2.
  - The CCS1 and CCS2 connectors/ports are shaped differently so that
    one cannot be plugged into the other. That's a little unfortunate,
    if you take your American BEV car to Europe (or vice versa).
    - This is a result of the two different IEC 6196 (types 1 and 2)
      that cannot connect to each other.
  - In both CCS types, the inlet will retain the AC pins so that AC
    charging can be performed.
    - I believe the CCS1 connector retains the AC pins, but I don't know
      why? The CCS2 connector doesn't retain the AC pins.
  - Most non-Tesla, non-Japan manufacturers are going with CCS.
- CHAdeMO
  - Some weird pun that means tea?
  - Ubiquitous in Japan.
  - Just two pins of DC. But can't do AC charging, so you need a second
    inlet for that on the car.
  - This is basically a dead end in the US.
  - Toyota and Subaru equip vehicles with CCS outside Japan. Honda uses
    CCS outside Japan.
  - Only the Nissan Leaf and Mitsubishi Outlander PHEV use CHAdeMO in
    the US.
- Tesla Charger
  - Just two pins of DC.
  - I believe that the connector for home has to do power transformation
    to DC.
- Key question: since CCS and Tesla are most common charger types in US,
  can you use one with the other?
  - Tesla sells a $250 adapter, which looks passive, to fit a CCS
    charger to Tesla.
  - Apparently Teslas manufactured before Oct 2020 need a "retrofit"; a
    firmware flash of something, I think.
  - But this won't be available until 2023.
  - First started selling this in South Korea where it was mandated.
- Can you charge CCS on Tesla network?
  - Tesla already uses CCS in Europe, where I believe it is mandated.
  - They claim that the network will soon be opened to other vehicles. I
    believe they will be able to charge at full rate.
  - That would be interesting.
  - I don't know how Tesla will accomplish this. Will they allow you to
    bring your own Tesla-to-CCS1 adapter? Will they terminate with CCS1
    at the supercharger? Wouldn't that annoy their customer base?

Source: https://www.tesla.com/support/home-charging#home-charging-options
Source: https://freewiretech.com/difference-between-ev-charging-levels/
