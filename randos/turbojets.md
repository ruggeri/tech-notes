# Turbojets

You have the following stages:

1. An axial compressor.
2. A combustion chamber.
3. A turbine that drives the compressor.
4. A nozzle that directs the exhaust out.

Propulsion is achieved via exhaust. Compressor achieves high oxygen
density in the combustion chamber, resulting in high engine efficiency.
Also: compressor ensures that expanding gas does goes through the
turbine, rather than simply out through the compressor.

## Efficiency

Thrust is force. Force is change in momentum per second. The engine is,
each second, throwing a certain volume of exhaust out at a certain
velocity.

The power (energy released per second) of the engine is fixed. So we
want to maximize

    p = mv

given a fixed energy E. Since

    KE = 1/2 mv^2

this suggests we want to move a large mass of exhaust at a low velocity.

This suggests the **turbofan** setup. Here, you use the shaft to turn a
large ducted fan.

## Turbojet Efficiency

Must we drive a ducted fan? Could we not improve the efficiency of our
regular turbojet somehow?

We need more mass to pass through the engine per second. If the
combustion chamber will stay the same size, then necessarily either (1)
the velocity of gas through the combustion chamber must be higher or (2)
the density of gas must be greater.

Compressors will tend to do three things:

1. Increase density,
2. Increase temperature,
3. Increase velocity.

Turbines will do the opposite. Anyway - if we want to increase flow
through the combustion chamber, we're going to be increasing both the
temperature and the pressure.

Since the turbine drives the compressor, it will take out all the energy
that the compressor put in. It will reduce the temperature, the
velocity, and the density of the fluid.

However, we still need the total cross-sectional flow to be constant
throughout. And we're assuming that I haven't increased the diameter
coming out of the turbine. So density times velocity must still be high.
