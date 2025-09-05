# Level 1 Charging

In the US, if you plug in your electric car to a typical outlet, you are
going to charge with 120V AC. Your typical circuit is going to do 15A.
But you are only supposed to use 80% of the sustained circuit current
capacity. You get 12A sustained, and that gives 1.44kW of power. You
will connect to a typical NEMA 5-15 receptacle.

If you have a 20A circuit, a NEMA 5-20 receptacle, and the right plug
you can charge at 80% of 20A which is 16A. That gives 1.92kW.

The J1772 connector is limited to 16A on 120V power. Most vehicles will
restrict further to 12A. I believe my Rav4 Prime XSE will allow you to
select 16A.

# Level 2 Charging

The next level up is 240V AC charging. Here you combine the two 120V
phases of the residential power.

Depending on the circuit/receptacle, you can charge at 15A (2.88kW, NEMA
6-15), 20A (3.84kW, NEMA 6-20), 30A (5.76kW, obsolete but common
ungrounded NEMA 10-30 or modern NEMA 14-30), or 50A (9.6kW, no neutral
NEMA 6-50 or NEMA 14-50, which offers neutral and thus both 120V and
240V connections). A Tesla mobile connector has adapters for all these
receptacles. These adapters are "smart" in that they tell the Tesla
mobile connector how much current is available to draw. Otherwise, the
Tesla mobile connector is just a cable to plug into the car.

Note: Level 2 charging can also happen at 208V. 208/120V is common in
commercial usage. In big box stores and industrial you can have
480/277V. I believe the reason that higher voltages are preferred is
that equipment that needs to draw a lot of power might want to do so at
high voltage and low amperage. However, in the case of 208/120V, the
line-to-line voltage is less than the 240V you get from the split-phase
240/120V.

# Level 3 Charging

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

- SAE J1772 (American name)
  - This is single-phase AC. It can do 120V or 240V charging.
  - It doesn't support DC charging.
  - Thus it can't do fast charging.
  - This was also used in Japan.
  - 1 pin live, 1 pin neutral, 1 pin protective earth. Two signalling
    pins.
  - Standardized as IEC 62196 Type 1.
    - There is a European IEC 62196 Type 2. This can connect 1 or 3
      phases. It has two extra pins, of course.
      - They are in a totally different arrangement than the J1772.
    - There is even a IEC 62196 Type 3 that was used in Italy or France.
      It was a different physical format, but same idea as Type 2.
  - I don't believe new battery electric vehicles really use SAE J1772
    anymore. It can't do enough power.
    - Plug-in hybrid vehicles with small wattage on-board chargers (like
      the RAV4 Prime) do often use SAE J1772.
    - We will see that cars with a CCS1 port will accept a J1772
      connector and charge slowly but surely.
  - SAE J1772 is commonly used at home or as a travel-charger in the US.
- CCS: Combined Charging System
  - CCS1 extends SAE J1772. It adds two DC pins. This is CCS Type 1.
    - The CCS1 connector doesn't have the AC pins. But the port accepts
      them.
    - Thus J1772 connector can fit CCS1 port. But CCS1 connector can't
      fit J1772.
    - CCS1 is commonly found in the US.
  - There is also CCS Type 2. That extends IEC 62196 Type 2.
    - IEC 92196 Type 2 connector can fit CCS2. But of course CCS2 won't
      fit IEC 92196 Type 2.
    - Agin, this is for the EU, not USA.
  - The CCS1 and CCS2 connectors/ports are shaped differently so that
    one cannot be plugged into the other. That's a little unfortunate,
    if you take your American BEV car to Europe (or vice versa).
    - This is a result of the two different IEC 6196 (types 1 and 2)
      that cannot connect to each other.
  - In both CCS types, the inlet will retain the AC pins so that AC
    charging can be performed.
    - I believe the CCS1 connector retains the AC pins, but I don't know
      why? Maybe some electronics on the car might be powered off AC?
      The CCS2 connector doesn't retain the AC pins.
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
- Tesla Charger (called NACS)
  - Just two pins of AC or DC. The same pins are used for either AC or
    DC. At home, AC is connected. At a Level 3 charger, DC is connected.

# Outlet Compatibility

- Toyota RAV4 Prime ships with an EVSE that plugs into NEMA 5-15.
- J+Booster is a modular system for J1772 that allows you to select a
  wide variety of plugs. But it costs $700.
- Tesla Mobile Connector is $300. It comes with 5-15 (120V 15A) and
  14-50 (240V 50A) connectors. You can buy Tesla branded 5-20 (120V 20A)
  and 6-20 (240V 20A) for ~$35. The connector has a 20' cable, so it
  should reach a car from quite a distance.
  - You can buy a passive Tesla-to-J1772 converter. A Lectron adapter
    (supports up to 250V 48A) costs $113.

# NACS, J1772, and CCS1 Compatibility

- J1772 to NACS: Tesla sells a $50 adapter which looks passive.
- CCS1 to NACS
  - As of 2025-09-05, Tesla sells a $300 adapter, which looks passive,
    to fit a CCS connector into a NACS port.
  - Apparently Teslas manufactured before Oct 2020 need a "retrofit"; a
    firmware flash of something, I think.
  - Lectron sells a CCS to Tesla adapter for $135.
- NACS to J1772
  - Tesla runs a network of "Destination" chargers which offer 240V and
    up to 48A.
  - Lectron sells an adapter for $120. You can then plug your J1772 or
    CCS1 vehicle into any Tesla Destination charger.
- NACS to CCS1
  - Sort of. Tesla has (1) Tesla-only superchargers, (2) All EV
    superchargers which have a magic dock adapter, (3) NACS
    superchargers which support _some_ EVs if you bring your own NACS to
    CCS1 adapter.
  - I don't know if the adapter can be third-party like from Lectron, or
    if you must buy it from the vehicle manufacturer.
  - Your vehicle manufacturer needs to sign an agreement with Tesla to
    let you plug it into the Supercharger. There is some kind of
    negotiation between your vehicle and the charger. As of 2025-09-05,
    Toyota hasn't reached an agreement with Tesla?
  - Now that NACS is open, many new BEVs will come with NACS ports.

Source: https://www.tesla.com/support/home-charging#home-charging-options
Source: https://freewiretech.com/difference-between-ev-charging-levels/
