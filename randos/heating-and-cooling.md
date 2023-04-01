# Refrigeration Cycle

- You have a closed loop.
- Evaporator interface is connected via a compressor to the condenser
  interface. The condenser is connected via an expansion valve to the
  evaporator.
  - Evaporator and condenser are basically just large surface area
    coils.
- Because a gas cools as it expands, the evaporator side will be cold.
- Conversely, upon compression, the gas will be hotter. Thus the
  condenser will be hot.
- You blow fans over evaporator/condenser to spread heat/coldness to the
  outside space. If, for instance, no air is blown over the condenser,
  then the refrigerant gas will be returned to the evaporator at the
  same temperature as it left.
- Necessarily, work needs to be performed to do the compression. There
  are necessarily energy losses.

# Implementations

- Air conditioners work on the refrigeration cycle. So do refrigerators.
- A heat pump works the same way, but is basically installed in reverse.
  It transfers heat from the outside into the house.

# Heat Pumps

- The coefficient of performance is how much heat is transferred per
  unit of energy spent running the device.
- Resistive heating by definition has a coefficient of performance of
  (practically) 1.0: all energy is being dumped as heat into the home.
- Heat pumps can increase this number, in that they are transferring
  heat from one side of the pump to the other. They don't need to
  literally create the heat.
- However, note that the heat pump efficiency will drop as the outside
  becomes colder, and it becomes harder to extract heat from the
  outside. This has been a historical problem of heat pumps.
- Additionally, note that electricity generation is not super efficient.
  If you are burning gas to generate electricity, it might be better to
  simply have a gas fired furnace in the home.
- With regard to global warming: the situation is similar to electric
  vehicle technology. But heat pumps could in principle be powered by
  clean energy.
- Some people do note that refrigerant is a super greenhouse gas. And
  any refrigeration system will inevitably have leaks.
- Another problem is that heat pumps will go off when electricity shuts
  off, whereas a gas furnace will keep running. I believe this fucked
  many people in Texas when power went out.
- Thus heat pumps make more sense in milder climates. But there are also
  _ground source_ heat pumps, which interface not with the atmosphere,
  but with the ground. The ground temperature is more stable than the
  air temperature.
- Traditional gas or boiler systems often run with just one speed
  (though not always). They turn off once a temperature is obtained, and
  then kick on again when it falls outside a band.
- Heat pumps can run at variable rates, if they use an 'inverter.' I
  believe that means that they don't have to cycle on and off, and they
  can just provide a consistent level of heating or cooling to maintain
  the ideal temperature.
- Source: https://www.nytimes.com/wirecutter/guides/heat-pump-buying-guide/

# Water Heating

- A traditional way of heating water is to burn gas, heat water, and
  store it in a large tank for later use.
  - Alternatively, you can use resistance heating or heat pumps.
- This means you have a limited quantity of hot water. But you can use
  it at a high rate until depletion.
- Tankless heaters use either gas or electric to instantly heat water
  with a heat exchanger.
  - Because they must heat rapidly, their instantaneous gas/electric
    draw can be high.
  - For electric, you may need to add additional amperage capacity,
    which can be very expensive!
- Tankless heaters can continuously provide hot water, but they have a
  limited gallons per minute flow (based on the size of the heat
  exchanger).
  - Basically, tank is better for concurrent hot water usage. Tankless
    is better for serial usage.
- Tankless can be more energy efficient because it doesn't need to
  constantly keep the water in the tank hot. Tank systems will lose
  heat and waste energy.
  - On the other hand, tankless must provide high heat instantly.
    Whereas tanks can heat the tank more slowly, or even at off-peak
    times.
  - Still, tankless has efficiency that is at least 80%, rather than at
    most 60% for tanks.
- Tankless needs (basic) maintenance to avoid fouling of the heat
  exchanger.
- Tankless can have a short delay between turning on and getting hot
  water.
  - With tank designs, the lines to the faucets are typically at least
    warm, from expansion from the hot water heater.
  - You could see this as waste, or as readily providing hot water.
  - The further the faucet is from the tank in the house, the longer it
    takes to start getting hot water from a tank.
  - So you'll get hot water faster if the tankless heater is close to
    the point of use.
- Solar water heating is used in countries with a lot of sun (like
  Greece)
  - It doesn't use any kind of photovoltaic technology.
  - You paint a collector black. You run some tubing through it.
  - You run a line from the bottom of a tank that sits higher than the
    collector. You also run a line from the collector to the top of the
    tank.
  - This is called a _thermosiphon_, and will use convection (without a
    pump), to circulate water so that the entire tank is heated.
- Conclusions
  - If you have a lot of people in your household who take serial
    showers, tankless might make sense.
  - If you have long periods of no-use, then tankless might make sense.
  - Tankless may make more sense if it can be closer to point of use.

## Recirculating Water Pump

- The idea is that the water in the hot water line is poorly insulated
  and gets cold.
- That means you have a delay between when you turn the faucet and when
  you get hot water. That wastes water. The severity of the problem
  depends on the length of the line from the hot water tank.
- The recirculating pump constantly pumps hot water out of the line and
  sends it back to the hot water heater. It might do this using the cold
  water line to return.
- Hot water is drawn from the hot water tank into the line. It's always
  there.
- There is a huge downside. The hot water in the line starts getting
  cold. That's always been the problem: the line is not necessarily well
  insulated (not as well as the tank) and it has a large surface area.
- Thus, the recirculating pump will waste heat (and also take energy to
  operate).
- Thus, lots of these pumps have timers. Others have a button you can
  push in the home to start the pump.
- If you want to save water, you can insulate your pipes. That will
  reduce the amount of energy lost in the pipes, and it will mean that
  the water in the hot water line is more likely to be hot when you turn
  it on.
- Some people talk about maybe sensors or something to kick on the pump.
  But none of these solutions seems to make more practical sense than
  timers.
  - JRN mentioned that they have plugged the pump into a "smart outlet,"
    which you can tell to give power by a web interface. It's also
    programmed to turn itself off after 1hr. So that minimizes
    recirculating pump water wastage.
- A last use of pumps: in hydronic heating. You need to circulate the
  water. You can have multiple pumps on multiple lines if you need zoned
  heating.
- Source: https://www.reddit.com/r/Plumbing/comments/fh4i3j/hot_water_recirculating_pumps_good_or_bad/
- Source: https://www.buildwithrise.com/stories/pros-and-cons-of-hot-water-recirculation-pumps
- Source: https://www.reddit.com/r/homeautomation/comments/ktzol3/looking_for_water_flow_sensor/

# Central Heating

- You can heat air with a furnace and blow it around the home.
  - This can be done with electric, a heat pump, or with gas.
  - With electric resistance heating, you might as well do it right at
    the location of your heat requirement?
  - The heat can turn on very fast.
- Steam heating distributes hot steam in the house. It runs through a
  radiator, loses heat to the room, and condenses as water. This flows
  back to the steam boiler.
  - No one installs steam heat anymore.
  - I think a major benefit was that you didn't need pumps.
  - It's apparently sophisticated to run the piping correctly so that
    the liquid water returns properly.
  - I think also you have the problem that when only moderate heating is
    desired, you still have to make steam, which requires a minimum
    heat level.
  - You also can't run different rooms at different heat levels.
- Electric baseboard heating just runs electricity through a resister. I
  think you get the same efficiency (but lower output) from anything you
  plug in the wall?
  - These systems work through convection: the spontaneous distribution
    of hotter fluid through a medium.
  - If the heat is not evenly distributed throughout the room, it will
    not be efficient in that the area close to the heater may get hot,
    but not impart much heat elsewhere.
    - This is a problem with any heating system, I suppose.
  - There are baseboards that use electricity to heat water or mineral
    oil. They are supposed to be more energy efficient.
  - I think the reason is high volume and lower temperature. By having a
    consistent temperature in a large volume closer to what you want,
    you heat the air in the room more evenly?
  - In particular, I think that oil has a higher heat capacity (amount
    of energy required to change temperature by one degree). Thus a
    larger thermal mass is heated, and cannot simply blow away like the
    air in a room.
- Other hydronic systems have a central boiler and hot water is piped to
  heaters. I think this is possible nowadays with electricity so that
  you can have pumps.
  - You can run the hydronic pipes through the floor. People set the
    thermostat lower if their feet are warm.
- I saw that 1MM BTU generation using electric baseboard was $34, versus
  $14 for an electric heat pump and $7 for gas furnace.

## Mini-Split Systems

- You can use a mini-split system with a heat pump (or regular
  air-conditioner).
- It's the in-room air-conditioner you frequently see in Europe or
  apartment buildings. Instead of running ducts for air to flow through,
  the refrigerant lines pass through the wall.
- The lines then go back to the heat pump, where they undergo
  compression or expansion, depending on what mode the system is
  operating in.
- An advantage of mini-split systems is that you can run multiple zones;
  each mini-split is basically it's own zone. That means you can run the
  heating/cooling based on need per room.
- Many heat pumps will support multiple zones on a single heat pump.
- This can matter a lot if different parts of your house have different
  natural temperatures. That can be affected by level in the house (hot
  air rises to higher floors), height of ceilings, and windows.
- You can save energy, and control the comfort in specific rooms better
  this way.

## Convection vs Radiation

- The sun heats the earth, despite the fact that the vacuum of space in
  between is exceptionally cold. That's because the heat energy is
  transferred via _radiation_.
  - Visible light carries energy.
  - But infrared also carries a lot.
  - The light needs a path to you. If you are standing in a shadow, you
    can't absorb it.
  - Likewise, a campfire on a cold night heats by radiation.
- Convection transfers heat by heating the medium.
  - Radiators are designed to have high surface area, which means a
    large surface via which to easily transfer heat from the radiator to
    the medium (normally air, sometimes liquid).
  - The heat transfer happens because hot things are shaking, and they
    impart some shake to surrounding things.
- If air keeps moving away, then your body will get cold on a cold
  night. The reason is that the temperature differential is constantly
  remaining the same.
  - Whereas, if you are in a tent, the air can't really move away. So
    you do warm the tent with your body (the ten starts out cold).
  - But eventually it gets warm; close to your body's temperature.
  - It may even start to feel hot, if your body puts out heat faster
    than the tent releases it to the outside environment.
  - Why can't the tent just start transferring heat away? It's because
    of the poor thermal conductivity of the outer layer. In fact, even
    the air has poor thermal conductivity. Probably any gas at a
    commensurate pressure will have similar thermal conductivity?
  - The main point is: it can't simply float away from us. We need to
    propagate heat _through_ the medium, and that is actually relatively
    hard.
  - This is also how wetsuits work, by the way. They aren't totally
    water tight, just as a jacket is not totally airtight. But note that
    water is much more thermally conductive (20x more than air), so we
    need a better insulator (neoprene).
