## TODO

Note source: Advanced Engine Technology.

Bore vs stroke

https://www.youtube.com/watch?v=UV3RwBPqznU
https://www.youtube.com/watch?v=j8DSBH2GG8s&t=1220s
https://www.youtube.com/watch?v=C_YNn3ZkJmU&t=100s
https://www.cycleworld.com/story/blogs/ask-kevin/how-motorcycle-cylinder-bore-stroke-affect-engine-performance/

- https://en.wikipedia.org/wiki/Internal_combustion_engine
- Have to talk about stuff like cams.
  - https://en.wikipedia.org/wiki/Valvetrain
  - https://en.wikipedia.org/wiki/Camshaft
  - https://en.wikipedia.org/wiki/Poppet_valve
  - https://en.wikipedia.org/wiki/Overhead_camshaft_engine
  - https://en.wikipedia.org/wiki/Timing_belt_(camshaft)
  - https://en.wikipedia.org/wiki/Valve_timing
  - https://en.wikipedia.org/wiki/Desmodromic_valve
  - https://en.wikipedia.org/wiki/Variable_valve_timing
- Engine Power/Torque
  - https://en.wikipedia.org/wiki/Stroke_ratio
  - https://en.wikipedia.org/wiki/Compression_ratio
    - Note that compression ratio doesn't increase power because there's
      more air and thus you can burn more fuel. Compression ratio isn't
      about shoving more air into the cylinder like turbo and
      supercharging.
    - It's that the peak temperature is higher, so more work is done on
      expansion stroke. And the stroke is effectively longer. You can
      imagine an engine with a _zero_ compression ratio; it obviously
      doesn't do any work.
    - I think that compression means that you get more work out of the
      same fuel/air drawn into the cylinder. It's more _efficient_. But
      it also makes better power because each combustion cycle makes
      more work.
    - You do have to do more negative work on the compression stroke.
      But this is offset by more positive work on combustion stroke.
    - Limits to compression include knock, but also as you compress gas
      further you start to lose heat to cylinder walls, which eventually
      means _worse_ efficiency. You also have higher pressures which
      means more stress. But in ideal Otto cycle yes higher compression
      is always better.
    - Efficiency of Otto cycle is `1 - 1/(compression_ratio ^ (gamma-1))`, where gamma is the adiabatic index.
    - The point being, effect of increasing compression ratio eventually
      decreases.
    - Since compression is good for power and good for efficiency, why
      do bikes run low compression? Because longevity of engine,
      availability/price of low octane fuel, and also because I think
      high CR means that while mpg might be better (and thus lower CO2
      emission), NOx emission might be _worse_. Which means running
      higher compression might mean bigger catalytic converters.
    - As another note, the "Atkinson" cycle is a version of the Otto
      cycle where the compression stroke is longer than the power
      stroke. That lets you get more work out of the fuel combusted.
      - In olden days, there was some complicated geometry for the
        Atkinson engine.
      - Today, you just keep the intake valve open longer past BDC to
        push air back out. So the effective intake and compression
        strokes are shorter. You'll inject and burn less fuel.
      - Since for the same displacement you are breathing less, you have
        lower power density. But you make that power more efficiently.
  - Sources:
    - https://www.youtube.com/watch?v=nveqCMNTth0 early basic
      Engineering Explained video on compression ratio.
    - To review: https://www.youtube.com/watch?v=t4yh5Pt8was
    - To review: https://www.youtube.com/watch?v=1pZqiuaZYGY
- Diesel
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
- Fuel
  - https://en.wikipedia.org/wiki/Carburetor
  - https://en.wikipedia.org/wiki/Fuel_injection
  - https://en.wikipedia.org/wiki/Fuel_pump
- Other Systems
  - Cooling system
  - Ignition system, Starting system, Alternator
  - Lubrication system
  - Super and turbo chargers
- Suspension.
