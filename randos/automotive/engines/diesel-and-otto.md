## Otto Cycle

- Four strokes:
  - Intake stroke, in which air is pulled in through intake valves.
    Occurs at atmospheric pressure, unless a supercharger or
    turbocharger is used to pressurize air. The amount of air pulled in
    during intake limits the amount of fuel that can be burned (see air
    fuel ratio notes).
    - Carburateurs supply fuel along with air intake. Fuel injection has
      a little more control over when fuel is sprayed in.
    - Otto cycle thermodynamic analysis ignores this stroke, since its
      purpose is to breathe fluid and not for a thermodynamic purpose.
      Otto cycle analysis imagines a "closed" cycle, and this intake
      stroke is really about open, internal combustion operation.
  - Compression stroke: engine must do work to compress the air. This is
    modeled at an adiabatic compression starting at the cold
    temperature.
  - Ignition: we fire a spark to burn the fuel and release its heat.
    This occurs when the piston has reached TDC. This is modeled as an
    isochoric (constant voolume) heat addition. This spikes both
    pressure and temperature.
    - This isn't a stroke, but it is a "process" in the idealized Otto
      cycle thermodynamic analysis.
  - Power stroke: the gas mixture at the hot temperature is now allowed
    to expand. This is modeled as an adiabatic expansion starting at the
    hot temperature. Because this starts at a higher temperature than
    the compression stroke, more work is done.
    - However: please note that by the end of the power stroke, the gas
      temperature has reached neither (1) atmospheric pressure, nor (2)
      atmospheric air temperature. A hot gas needs to expand farther
      than a cold gas to reach the same pressure, and even then it will
      still be hotter than the cold gas after expansion. See: adiabatic
      index.
    - If we could expand further all the way until atmospheric pressure
      is reached, we could extract more work out of the fluid. This is
      the idea behind the **Atkinson cycle**. Even then, the gas would
      still be hotter than atmospheric air temperature. That represents
      unavoidable inefficiency; we'd have to modify the Otto cycle even
      more to get closer to the Carnot efficiency limit.
  - Exhaust stroke: exhaust gas is pushed out of the cylinder.
    - This is modeled ideally as isochoric heat rejection (leftover heat
      is rejected to atmosphere and lost), which is part of closed cycle
      Otto analysis. We then push out the spent combustion charge, which
      is ignored in the thermodynamic analysis, since it isn't for a
      thermodynamic purpose.

## Diesel Cycle

- Otto is more efficient at same compression ratio than Diesel.
  - The reason is that in Otto cycle all heat is added right at TDC
    when spark fires and fuel burns. This is modeled as isochoric heat
    addition.
  - Whereas in Otto cycle, air is compressed, then fuel is injected at
    TDC, and it burns progressively as power stroke expands. This is
    modeled as isobaric heat addition. So you get lower thermal
    efficiency because not all heat is added at max pressure.
  - Both cycles are four stroke cycles.
- But diesel has higher compression ratio, and is overall somewhat
  more efficient. Maybe 40% thermal efficiency for diesel and 30% for
  otto.
- Diesel always has higher "mpg" because diesel has more energy
  content than petrol.
- Diesel puts out more CO_2 per liter, but less per mile.
- Diesel puts out more particulates, but also more NOx, which need to
  be cleaned through catalytic converter.
- Europe gave diesel preferential tax treatment because in 1990s
  diesel was more efficient. Also, diesel was used more for freight,
  so it was thought of as an input to other costs. Also refineries
  produced an excess of diesel (many factors, including the heavy oil
  imported from Russia, North Africa, and ME).
  - But later Europe realized that particulate pollution was a
    problem.
- Diesel had a reputation of reliability because they were built
  strong for high compression, operated at low RPM so they had less
  mechanical stress, and had no spark/ignition system to fail. Today,
  emissions control systems
- Today, modern Otto cycle hybrids give better fuel economy. But
  diesel had an edge in CO2 per mile up to 2000s.
- Diesel can make very high torque, but its main problem is that it
  only can be effective at relatively small RPM ranges.
  - Also, high compression means heavy and strong parts, which means a
    lot of rotational inertia, which means slower to change revs.
  - Because burning is slow in Diesel relative to Otto, you can't rev
    high. So even though diesel can make bigger torque than Otto for
    the same displacement, the Otto can rev higher and make more power
    at the same displacement.
  - In performance auto, you really do need to make a lot of power to
    accelerate fast and maintain a high velocity against heavy drag.
  - In industrial applications, diesel does have several advantages:
    - Used to have better thermal efficiency, so less cost to operate.
    - Doesn't need complicated transmission to make big torque.
    - Otto engines aren't built for heavy continuous load; they
      normally can only continuously run at 10-20% load. Whereas
      Diesel engines can run all day. Part of this is being built to
      withstand more pressure, or from cooler peak temperatures, or
      for lower RPM operation.
    - Diesel may have "broader" torque. Certainly it seems like Diesel
      power band covers almost its entire rev range, which is not true
      of Otto engines.
    - Whereas falling out of the power band in an accelerating vehicle
      means you might simply start accelerating faster, falling out of
      the power band while trying to do some heavy work may bog the
      engine down (drop in power), lug the engine (excess vibration
      from too much load on engine producing too little torque), or
      stall it (engine stops running).
- **TODO**: I don't think I talk about how diesel doesn't need spark
  plugs, and what that means.
- Sources:
  - https://www.youtube.com/watch?v=tDoPSofACAE D4A TO REVIEW. Studies
    unique advantages of diesel in industrial context, and weaknesses of
    alternatives.
  - https://www.youtube.com/watch?v=w8r2xnITnqA D4A TO REVIEW. Discusses
    diesel policies in Europe that drove diesel adoption. Discusses that
    emissions aren't great relative to petrol (when you scale diesel to
    make more power), and that NOx is a bad problem (VW cheated in the
    United States).
  - https://www.youtube.com/watch?v=aWeqyAxlM2M D4A TO REVIEW.
    Petrol/diesel comparison.
  - https://www.youtube.com/watch?v=rXVJG9n6BAs EE TO REVIEW.
    Petrol/diesel comparison.
  - https://www.youtube.com/watch?v=D6YmAecTolQ EE TO REVIEW.
    Specifically about diesel torque.
  - https://www.youtube.com/watch?v=8Rsl8jzlgno EE TO REVIEW. Older
    petrol/diesel comparison.
