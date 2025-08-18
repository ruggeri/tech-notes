# The Vapor-Compression Refrigeration Cycle

- You have two sides to the machine: an _evaporator_ and a _condenser_.
- Each is a coil of metal tubing that acts as a heat exchanger to the
  environment. The idea is just high surface area with the environment
  to facilitate the transfer of heat energy.
- The coils are filled with a refrigerant. This is a substance which is
  normally a gas at normal pressure, but at high pressure condenses to a
  liquid.
- Let's focus on the **evaporator** side at first. Liquid refrigerant
  will be metered into this side. The refrigerant starts at the ambient
  temperature. But the evaporator is kept at very low pressure. At the
  ambient temperature and low pressure, the refrigerant will want to
  _evaporate_ and undergo phase transition to a gas. When the
  refrigerant evaporates, it will become **colder** than before (colder
  than ambient). The loss in heat energy is the **latent heat of
  evaporation**.
  - Later we will discuss why a liquid forced to evaporate to a gas
    under unchanged temperature but changing pressure will get
    **colder**.
  - This is in contrast to our usual expectation that liquids evaporate
    when we add heat to them and cause them to boil (without changing
    the pressure).
- Another way to think about it: at the lower pressure, the refrigerant
  will want to absorb heat and vaporize. The refrigerant can absorb
  external heat and yet not itself get hotter. The heat is stored in the
  transformed to a higher state of matter (which can be reversed, as we
  will see!).
- The evaporator coil will allow the colder-than-ambient refrigerant to
  absorb heat from the surroundings. The refrigerant may return to
  ambient temperature, but it will remain a gas.
- Because of the heat-exchanger, we assume the gaseous refrigerant on
  the evaporator is returned to ambient temperature.
- Now, a compressor pumps gaseous refrigerant to the **condenser** side
  of the device. At this pressure, the refrigerant wants to change state
  back to a liquid.
- Normally we think that a gas condenses to a liquid when we hold
  pressure constant but reduce the temperature. In this case, the
  external temperature will be held constant, but the pressure has
  increased a lot. We will see that the temperature of the condensed
  liquid will increase (by the latent heat of evaporation).
- So the refrigerant gets hotter as it condenses from gas back to
  liquid. If we do not reject this heat, then we have not returned the
  refrigerant back to its initial phase and temperature. If we feed it
  back to the evaporator like this, the refrigerant will eventually
  become too hot to vaporize in the evaporator.
- That is why we must cool the condenser coil. We can either air-cool or
  water-cool the coil; either will work. This returns the refrigerant to
  the ambient temperature, and allows us to repeat the cycle.
- Let us summarize how the system is **pumping heat**:
  - We reduce the pressure so that the refrigerant vaporizes by
    absorbing the latent heat of vaporization from the evaporator
    environment. The heat energy absorbed is "stored" in the transformed
    state of matter.
  - We then compress the refrigerant so that it condenses. By
    condensing, the "latent heat" in the gas is transformed to "sensible
    heat" (the normal kind). This is then transferred to the condenser
    environment.
- This cycle is sometimes called a **reverse Rankine cycle**.

# Gas (Air) Cycle Refrigeration

- We will now discuss a lower-efficiency version of the reverse-Rankine
  cycle. This is called **gas cycle refrigeration** or the reverse
  **Brayton cycle**.
- This cycle will _not_ use any condensation or evaporation. It will
  work on a substance which remains a gas throughout.
- Outside, let's run a compressor to fill up a tire to high PSI. Since
  we are compressing ambient air, the gas in the tire will get hotter.
  But we will let the tire cool down outside to return to ambient
  temperature and high pressure.
- Next, we will bring the tire indoors. We will now let the gas out of
  the tire. This gas is colder than ambient when we release it from the
  tire.
  - **TODO**: Explain why.
- The cold air has cooled the indoors room. We will assume that the
  indoor air is pushed out to the outside environment and replaced by
  the cooler air from the tire.
- We may now take the tire outside again and repeat the process.
- This is absolutely a **refrigeration cycle**, but it is not the
  vapor-compression cycle. It doesn't use any phase transformation, and
  it doesn't require anything special about the **working fluid** (air).
- But it is a less efficient than the vapor-compression cycle.

# What is Latent Heat?

- Why should a substance get _hotter_ when it condenses from gas to
  liquid?
- We know that being a liquid means the molecules of the substance are
  held together by "intermolecular bonds of intermediate strength."
- When we add heat to the liquid, the molecules start jiggling more. A
  lot of times, they will jiggle a bit away from the other molecules to
  which they have some attraction. But they are pulled back in until an
  opposing force grows and repels them again.
- However, if the temperature is high enough, molecules will be knocked
  away from each other and given a momentum that the intermolecular
  force is not strong enough to arrest and reverse. The molecule is then
  "freed"; the intermolecular force becomes negligible at that distance.
  This is the process of changing phase into a gas.
- However, note that the intermolecular force will have resisted the
  breakaway of the molecule. This reduces the velocity of the escaped,
  gasified molecule. This reduction in velocity is a reduction of
  translation kinetic energy which is a reduction in temperature.
- Thus we see that the breaking of bonds is **endothermic** - it takes
  energy.
- Conversely, forming bonds is **exothermic**. A bond is formed when one
  molecule is moving toward the other. As the bond forms, both molecules
  accelerate toward each other.
  - **TODO**: It would be good to develop a kinetic model intuition of
    this.
- We have previously intuited why liquids, when heated, will eventually
  evaporate into a gas. It's because the molecules are knocked away from
  each other. But what we may not have previously noticed was that some
  of the heat (translational kinetic energy) used for evaporation will
  be lost as the intermolecular bond is broken.
- Similarly, we see that a gas, when cooled, will lose kinetic energy
  and this allows for intermolecular bonds to stably form. But what we
  might not have previously noticed is that this bond formation will add
  some kinetic energy to the molecules (as they snap together) and thus
  raise the temperature.
- We can now discuss why pressure also matters. When pressure is high,
  molecules have less space to fly away before they are pushed back.
  When they are close enough to form an intermolecular bond, they find
  it difficult to get away far enough for the bond to be broken (have
  negligible force).
- Likewise, when pressure is low, a molecule that is knocked away can
  more easily break away and never return.
- As you compress a gas, it will generally get hotter. But let's assume
  that it then loses this heat to the environment. What is striking is
  that eventually the gas will begin to condense, and at this moment, it
  will get quite a bit hotter. This hotness is not due to the work
  needed to compress the gas, but because the gas has condensed to a
  liquid.
