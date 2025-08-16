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

- **TODO**: Explain why substances get hotter when they condense.
