- Turbocharging is a compressor driven by exhaust gas. The turbine takes
  energy from the exhaust gas and uses it to drive a compressor to suck
  more air into the engine.
- Thus, it is harnessing waste heat. It is often used to achieve better
  efficiency.
  - Really, it is harnessing waste _pressure_, since a hot gas at
    atmospheric pressure can't run the compressor.
  - But it's kind of the same thing. The exhaust gas is hotter than
    ambient, which is why the pressure is higher.
- A supercharger is driven off a belt from the engine to run the
  compressor.
  - This is less efficient than the turbo because the turbo is using
    energy from the engine that would otherwise be wasted, while the
    supercharger is putting some of the work of the engine into
    compressing.
- By forcing more air into the cylinder, more fuel can be burnt per
  rotation, giving higher torque.
- Today, a turbo can be electric, which means you might get the energy
  for free from regenerative braking or a battery. Sometimes even the
  alternator.
- Turbochargers can improve efficiency, because they allow a
  manufacturer to produce more HP from a smaller engine. And the smaller
  engine, when not operating at high RPM or throttle can be more
  efficient than a larger engine.
  - Larger engines have more friction, more surface area to lose heat.
    When load on the engine is low on a larger engine, throttle is
    mostly closed, and so engine pumps air through a resistance.
  - Note: at peak load, bigger engines often have better performance.
    But at light load, such as in urban driving, highway cruise, partial
    throttle: small engines are more efficient.
- A turbo added to an engine of the same size should not improve
  efficiency. It should reach higher HP, but it will use more fuel as it
  sucks more air.
- The supercharger is driven by the engine, so it takes power from the
  engine. It is lower efficiency than a turbo, because it doesn't
  harness any waste energy. It can still be more efficient than an NA
  engine if you build a smaller engine.
- Compressing raises the temperature of intake air. You typically want
  to cool this using an "intercooler" (just a cooling system), otherwise
  you increase knock potential.
- The first question is: how much boost is provided _in steady state_ at
  each RPM?
  - Higher RPM means higher flow out of the engine. But also higher flow
    in. These effects mostly cancel.
  - Exhaust at higher RPM is often hotter (and higher pressure). Why?
    Basically, combustion takes about the same amount of _time_, but
    because crank is turning faster, that means the combustion is
    "slower" relative to the increase in volume in the engine. So the
    engine deviates more from ideal adiabatic expansion. That means less
    work extracted from the gas, and more energy leftover at end of
    power stroke. So turbo has somewhat more energy available to harness
    as RPM increases.
  - At high RPM, turbo starts to suffer from "choke." Basically, the
    engine needs to breathe so much air that, in order to sustain
    pressure, turbo would have to spin air so fast that it reaches speed
    of sound. But at Mach 1 flow, pressure gradient downstream no longer
    accelerates air upstream. So you cannot increase flow and maintain
    the pressure.
  - On the other hand, at low RPM, you can suffer from "surge", which is
    where you try to increase pressure too much at too low a flow rate.
  - **TODO**: I don't totally understand the physics of these phenomena.
    I think that's okay for now. I think it would take more study
    specifically of compressors and turbines.
  - In any case: these are not about _transients_.
- Note: small turbos don't make engines peakier. They increase torque
  everywhere they make boost. And they make about equal boost throughout
  lowend and midrange.
  - In steady state, turbos are generally _less_ peaky than NA engines.
  - At least, that applies to _small_ turbos. But if you have a really
    _big_ turbo, it will only efficiently make power from a high exhaust
    flow rate. And the compressor might only make good compression at a
    high flow rate.
- The last point is about turbo lag.
  - I will handwave a bit.
  - You step on accelerator, and that opens throttle. Normally that
    would let more air flow to engine.
  - But with compressor, the amount of air flowed to engine is not just
    a function of the throttle body, it also matters the compressor RPM.
  - Anyway, the engine starts ingesting more air and burning more fuel
    per rotation.
  - But as the vehicle accelerates and RPM increases, the engine needs
    more and more air, and the compressor needs to spin faster to
    provide more air at same level of boost.
  - But the compressor has a rotational inertia which prevents it from
    spinning up ("spooling" up) quickly.
  - Thus airflow rate cannot be increased quickly at constant boost.
  - I believe that as vehicle is accelerated, the engine will try to
    pull in more intake air per second (which will spin compressor) and
    push out more exhaust (which will spin turbo). But the change in
    airflow will be slower than if there were not a compressor or
    turbine.
  - This is handwavy, but more or less correct.
  - This delay is called "lag". The more rotational inertia, the slower
    the turbo will respond to changes in throttle inputs. This can make
    a very disconnected experience.
  - This is why turbos make most sense on cars that either (1) want to
    downsize to be more efficient, or (2) cars that target a HP number
    that can't be achieved otherwise (e.g., highest possible max speed).
  - A naturally aspirated engine will not have this lag, and will often
    feel better on a track where there are big changes in speed/throttle.
- Source: https://www.youtube.com/watch?v=UUFf4n0MUq8
