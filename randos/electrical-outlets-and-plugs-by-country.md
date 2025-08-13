## Voltages and Frequencies

- Note these are _residential_ voltages.
  - For instance, in the US, there is 240V three-phase power, and 120V
    split-phase power to the home.
- United States and Canada
  - Electrical voltage is 120V. Frequency is 60Hz.
- Europe
  - Electrical voltage is 230V and 50Hz.
    - Britain was 240V, Europe was 220V. They harmonized at 230V in 80s.
- Asia
  - China is 220V (old Europe) and 50Hz.
  - India follows modern UK/Europe (230V 50Hz).
  - South Korea and Philippines are 220V and 60Hz.
  - Japan is 100V. Half the country works on 50Hz (including Tokyo), and
    half the country works on 60Hz (Osaka, Kyoto). I think this is
    because in 1895/1896 they bought separate equipment from German AEG
    (for Tokyo) and US General Electric (for Osaka).
  - Australia follows modern UK/Europe (230V 50Hz)
- Africa
  - Most countries remain on 220V 50Hz (old Europe).
  - Some are 230V 50Hz (new UK/Europe), and a few UK colonies like
    Nigeria and Kenya do 220V 50Hz (from UK).
- Notes
  - Most things that work at 230V will work at 220V or 240V. It's not
    normally problematic. Similarly 100V, 110V, and 120V are typically
    interchangeable.
  - Most equipment also doesn't really care 50Hz vs 60Hz. Wall clocks
    do, though.
  - Putting a US hairdryer in a European plug will consume too much
    amperage and power and blow the circuit. Putting a European
    hairdryer in a US plug will just not get very hot.
  - Transformers for computers and USB powered devices will normally
    work throughout the world.
- Source: https://en.wikipedia.org/wiki/Mains_electricity#/media/File:World_Map_of_Mains_Voltages_and_Frequencies,_Detailed.svg

## Plugs

- So, when traveling, you mostly just need to adapt _plugs_.
- We will speak in terms of socket/plug _types_, as specified by
  the International Electrotechnical Commission.
- However, every nation will have _many_ plug types for different
  applications. So the "type" is just a loose naming standard. For
  instance, US National Electrical Manufacturers Association (NEMA)
  specifies many many different kinds of plugs:
  - Grounded vs ungrounded connection
  - Plugs that specify polarity.
  - Plugs that need 120V vs 240V. Plugs for connection to three phase!
  - Plugs that will draw and sockets that can provide a specified
    current maximum.

**United States and Canada**

- Type A is the standard two flat prong, and Type B is the three prong
  (earthed).
- Let's cover some NEMA connectors:
  - Your common ungrounded, 15A plug is 1-15R and 1-15P. 1 is a class,
    the 15 is the amperage rating, and R/P is for receptacle/plug.
  - Your grounded 15A plug is 5-15R and 5-15P.
  - There are also 20A plugs/receptacles like 5-20R and 5-20P. The live
    prong is bent 90deg to prevent being plugged into a 15A socket. But
    the 20A socket is designed to receive a 1-15P plug. I've never seen
    a plug like this but I do see receptacles like this sometimes.
  - I believe I have also seen 14-50 for my oven. This has four prongs:
    it does 240V hot-to-hot, it has a neutral for 120V electronics, and
    a ground. It is rated for 50A.
  - Source: https://en.wikipedia.org/wiki/NEMA_connector

**Europe: Common Plug Types**

- C is two 4mm diameter prongs spaced ~19mm apart.
  - This is an ungrounded plug.
  - This is not a national plug, but designed to work throughout
    Continental Europe.
  - It is only supposed to pass small currents up to 2.5A. This is a lot
    lower than national plugs.
  - The Type C prongs are bent a little inward. This allows them to fit
    in a variety of hole diameters.
  - Type C is only a _plug_, there is no specified _outlet_. Type C is
    only a lowest-common denominator plug. No country uses Type C as
    their national plug/outlet (again, there is no type C outlet).
  - However, there are sometimes _nonstandard_ outlets that accept Type
    C. These can accept unearthed variants of Type L and J so should
    provide 10A. I believe I saw this on a powerstrip in Greece.
  - You can always carry the appropriate national connector. Every
    nation will have some kind of grounded connector. Type C is maybe
    most useful in Italy (Type L) and Switzerland (Type J), where type
    E/F cannot plug in. Even in Denmark you can plug E/F into type K
    outlets (though not vice versa), so long as you don't need
    grounding. So you don't really need Type C there either.
  - I try to avoid Type C because (1) it doesn't provide grounding and
    (2) it tends to fit poorly. I prefer just to take the national plugs
    I will need.
- F is an earthed connector.
  - This is the German plug often called "Schuko".
  - It has a circular recessed socket, and circular based plug.
  - It has two 4.8mm prongs set 19mm apart. Intended for loads up to
    16A.
  - Ground is provided by a metal sheath with two notches at the top and
    bottom. These provide contact area for metal clips at the socket.
  - A C plug can be plugged into the center of an F socket. The C prongs
    are smaller (4mm) than the F socket prong-holes (4.8mm). Thus the
    fit is just a little loose.
    - Also, the lozenge shape of the Type C plug base means it isn't as
      well supported as the circular Type F base.
    - Grip is provided by Type C's flexible prong base.
  - An F socket cannot be plugged into a C socket, because the prongs
    diameter is too great. Anyway, Type C is not even a standardized
    socket...
  - F is most common grounded plug type in Europe. Type F plug cannot
    fit in Type E socket because can't accept ground pin.
  - Note: Type F allows plug in either orientation and thus is not
    polarized.
- Type E is very similar to F.
  - This is the French plug type.
  - It has the same two 4.8mm prongs, set the same 19mm distance apart.
    Intended for loads up to 16A.
  - However, it doesn't have the metal sleeve/clips for ground. The plug
    has a _hole_ which a ground pin from the wall socket can mate with.
  - A C plug can be plugged into an E socket. Just as with Type F
    sockets, the Type C plug will fit just a bit loose (Type C 4mm pins
    are a little small for 4.8mm socket holes, plug base is lozenge not
    circular).
  - There is a "preferred" polarity of live/neutral connections at the
    socket, but it is not enforced by regulation in France.
  - Type E plug will fit in Type F outlet, but ground won't be
    connected.
- Type E/F (also called CEE 7/7) plugs are made to fit both Type E and
  Type F sockets.
  - There is contact for the ground both at the edges and a center hole
    for the E socket ground pin.
  - The fit is very good in Type E and Type F sockets.
  - This is a good solution and also provides grounding.
  - CEE 7/17 is an ungrounded plug, for appliances that don't need
    ground.
- C and F countries
  - Germany, Netherlands, Spain, Portugal, Austria, Greece, Baltics,
    Balkans, Nordics, ...
- C and E countries
  - France, Belgium, Poland, Czechia, Slovakia

**Europe: Other National Plugs**

- United Kingdom, Ireland (G)
  - G is for the UK. It has two parallel blades for live and neutral,
    and a third blade at a right angle for the ground.
  - All UK sockets offer grounding. Plugs that don't need ground can
    have a plastic ground pin.
  - Sockets have shutters that protect against insertion of forks or
    knives. Often sockets have a switch at the socket to easily turn off
    the power if not needed, but this is not a requirement as plug has
    insulated sheaves to prevent contact with live pins.
- Italy (C, F, and L)
  - L is used only in Italy (okay also Albania).
  - The typical plug is the 10A plug. It has 4mm prongs, set 19mm
    apart.
    - These outlets are typically not recessed.
    - Type E/F plugs cannot be plugged into a Type L 10A outlet. The
      4.8mm prongs are too big.
    - However, the Type C Europlug fits nicely. However, since the
      outlet is not recessed the plug is not physically supported by the
      outlet.
    - CEI 23-50 S 10 is the two-prong unearthed plug.
    - CEI 23-50 S 11 is the three prong earthed plug.
    - I have seen this kind. I couldn't plug my Type E/F in, naturally.
  - The 16A Italian Type L has 5mm prongs, set 26mm apart.
    - These outlets are typically not recessed.
    - This socket doesn't accept Italian Type L 10A plugs (those pins
      are spaced 19mm apart). It also can't accept Type C for the same
      reason.
    - Likewise, the 16A Type L socket can't accept Type E/F plugs
      (pin/hole diameter basically matches, but Type E/F pins are spaced
      19mm apart).
    - CEI 23-50 S 16 is the two prong unearthed plug.
    - CEI 23-50 S 17 is the three prong earthed plug.
    - In Venice I never saw this kind (to my recollection)
  - It is common to have Italian sockets that accept both 10A and 16A
    Type L (and also Europlug). This is called "Bipasso".
    - CEI 23-50 P 17/11 is the standardized name.
    - These have a central hole for ground.
    - They then have two "figure eight" holes. Each figure eight has a
      pair of (overlapping) holes. These allow 19mm and 26mm spaced
      prongs to both plug in.
    - Note that the holes set 26mm apart are bigger (5mm) than those
      set 19mm apart (4mm).
    - This can of course accept Type C.
    - This style still cannot accept Type E/F though. Those 4.8mm
      prongs are too big to fit into the 4mm Type L holes set 19mm
      apart. Also, it would not be safe, since the pins of Type E/F
      are not sheathed.
    - I saw this kind also. I again could not plug my Type E/F plug in.
  - CEI 23-50 P 40 outlet is a modification of Schuko outlet.
    - Similar to Bipasso, it has two sets of overlapping holes. These
      can accept 5mm prongs at 26mm (Type L 16A plug), and 4.8mm at 19mm
      (Type E and Type F).
    - It has a ground hole for 5mm 16A ground.
    - Type L 10A also fits because that's a 4mm diameter prong set 19mm
      distance (just like Europlug). It might be slightly loose.
    - There are side pins for contact with CEE 7/7 Type E/F plugs.
      French Type E plugs will fit but will not be grounded.
- Denmark (C and K)
  - Type K socket accepts prongs 4.8mm diameter and set at the same 19mm
    distance as Type E/F plugs.
  - Grounded sockets will also _accept_ a half-circle ground pin in a
    location similar to where French ground pin is _offered_.
  - Since 1990 new installation requires grounded sockets are mandatory,
    but lots of old sockets only accept the live/neutral pins. This
    situation is similar to how lots of older ungrounded US sockets
    cannot accept grounded US plugs.
    - Worse: in Denmark some sockets will accept a Type K plug but not
      connect the ground. Confusing! Bad!
  - Sockets (even older ones) are required to have a switch if the
    socket is not recessed. New sockets are always recessed as
    far as I know.
  - Both grounded and ungrounded Type K sockets can accept Type E and F
    plugs, since those have same diameter pins set the same distance
    apart. However, the ground will not be connected. That's not great.
  - You can use Type C Europlug. As in Type E and Type F sockets, the
    Type C plug will fit a little loosely, because the Type C prongs are
    smaller. Also, the circular recess shape doesn't support the lozenge
    that well.
  - You cannot plug a Type K plug into an E/F outlet, because the
    ground prong is not accepted by those outlets.
- Switzerland (C and J)
  - Typical plug is called Type 12 and typical socket is called Type 13.
    The plug has three prongs, each 4mm diameter. The neutral/live
    prongs are set 19mm apart. A third (ground) prong is located offset
    in a triangular pattern (similar to Danish or American grounded
    plugs). The pins are supposed to be sheathed (from 2013) and the
    plug should be recessed and lozenge shaped (from 2017). Rating is
    10A.
  - The ungrounded Swiss 10A plug is called Type 11. Also, a Type C
    Europlug will fit into a Swiss Type 13 outlet nicely. It will not
    provide grounding, of course. But it will be supported by the
    lozenge shaped outlet, and the prongs are the right size.
  - Unearthed outlets are prohibited since 1955. But older installations
    may have them.
  - There is a 16A version (Type 23 plug, Type 23 outlet) with squarish
    pins. There are also three-phase plugs/outlets.
  - Technically there is a 16A plug/socket version called Type 23. The
    plug/socket uses squarish pins (so plug cannot be connected to 10A
    outlet), but 10A T12 plugs do fit into a T23 socket.
  - Last, there are some three-phase plugs/sockets that I don't consider
    here.
  - You cannot fit Type E or Type F into Swiss outlets without an
    adapter.

**Asia**

- Japan already uses A and B plug types. No conversion is needed.
- Korea uses Type F.

**Random**

- Type D is used in India and is similar to South Africa, except prongs
  are smaller and not sheathed.
- Type H is Israel and Palestine. It has three prongs. It used to have
  have blades, but since 1989 new outlets accept both blades and round
  pins so that they can accept Type C plugs. Maybe very old outlets only
  accept the blades.
- Type M is South Africa. It does 16A, has sheathed pins, and big
  prongs.
- Type N is Brazil and is similar to Swiss Type J but with a different
  offset of ground plug.

## Converters

- I have this worldwide converter set:
  https://www.amazon.com/gp/product/B07DQDD18X/ref=ppx_yo_dt_b_asin_title_o06_s01?ie=UTF8&psc=1
- I like that it has two American grounded outlets, plus two USB
  charging outlets.
- I like that the back is interchangeable.
- I prefer using the Type E/F circular plugs, or three-prong Type L
  plugs, because they have more points of contact and fit more securely
  to the outlet.
- I suspect I would have trouble using a Type C plug in Type E/F
  outlets, because the plug tends to rotate out of the outlet. Also, I
  want grounding if possible.
- I don't like using crazy universal adapters because they seem to make
  poor electrical contact. I had trouble with this in Japan and Korea.
- Also, the Type A wall-mounted Apple charger is not good, because it
  tends to want to rotate out of the socket. I prefer using a Type B
  (American grounded 3-prong) cord, and letting the transformer body
  rest on the floor.

## Sources

- https://upload.wikimedia.org/wikipedia/commons/e/e4/World_map_of_electrical_mains_power_plug_types_used.svg
- https://en.wikipedia.org/wiki/Mains_electricity_by_country#/media/File:World_Map_of_Mains_Voltages_and_Frequencies,_Detailed.svg
- electricalsafetyfirst.org.uk
  - Pretty helpful and covers lots of countries.
  - Not always totally complete info. But generally pretty good.
- https://www.plugsocketmuseum.nl/
  - This actually gets into a lot of details about sockets from around
    the world.
  - It has in-the-field examples.
