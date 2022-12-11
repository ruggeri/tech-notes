## Starting A Manual Transmission Car

In my engines document, I explain the relationship between engine speed
and wheel rotational speed via gears.

Next question: how does one engage/disengage gears?

Let's start with getting under way from a full stop. You start the
engine. You are in neutral. No power is being delivered to the wheels.
You could give gas: the engine revs, but no motion happens.

You depress the clutch pedal. What is the clutch? Hold on a moment and
I'll tell you. You shift to first gear. But the car still won't begin
moving; no power is going to the wheels.

That's because you haven't engaged the clutch. The clutch is like the
grabber in a drill: the thing that connects the motor to the drill bit.
When the clutch is disengaged, motor rotation doesn't do anything to the
drill bit. When it is fully engaged, the motor and drill bit rotate at
the same speed.

Last, if the motor is running, and the drill is stopped, then as the
clutch is engaged, there will be some grip (and angular acceleration) of
the drill bit, but also some _slipping_ of the clutch. This slipping
causes friction. It's not ideal.

So this is how you start out. After selecting first gear, you start to
engage the clutch somewhat slowly, which allows the wheels to start
turning. If you engage the clutch somewhat smoothly, you will get smooth
initial acceleration.

## Riding the clutch

You may semi-disengage the clutch to allow slipping of the clutch, thus
reducing the amount of engine power delivered to the transmission. This
is called "riding the clutch." However, it isn't very desirable. You
should prefer braking, or downshifting.

Ideally you will engage/disengage the clutch more rapidly. While the
clutch is disengaged, you want to match the engine RPM to that desired
by the next gear. You won't match it perfectly, so of course engagement
of the clutch will cause some acceleration/deceleration of the
transmission input, but you will cause less wear the closer you are to
the ideal RPM.

Riding the clutch is most common in stop-and-go traffic and in reverse,
when you want to select a speed below the minimum idle speed of the car
in the lowest gear.

## How a Clutch Works

You have the crankshaft from the engine, and you want to connect it to
the input shaft of the transmission.

The interface consists of two nested "baskets." The crankshaft is
connected to one basket, which sits inside a basket connected to the
transmission input shaft.

The baskets are splined, so that gears can mate with them. One set of
gear plates mates with the outside basket, while the other set of gear
plates mates with the inside basket. The inner and outer mating plates
alternate.

If pressure is applied to push the plates together, they will want to
rotate at the same rate, connecting the rotation rate of the crankshaft
to the transmission input shaft. If no pressure is applied, the two sets
of clutch plates can rotate at different speeds.

The clutch lever just changes the tension on the clutch plates. By
pulling the lever in, the clutch springs have their tension released,
the plates lose contact, and they can rotate separately.

Most motorcycles use a "wet clutch." Here, the transmission is bathed in
oil circulated from the engine. This helps keep the clutchplates cool,
even when you are in the friction zone and they are scraping each other.
Some high-performance bikes use a dry clutch, which can often sound
rattly. But most bikes have dropped this, because of noise regulations.

Source: https://www.youtube.com/watch?v=mxTSw3d5anE

## Gearbox: Non-synchronous

So when you move around the gear shift, what is happening? Basically, it
is a lever that engages the correct gear in the transmission.

An old way was the "crash" or "non-synchronous" gearbox. The
transmission input shaft has gears that slide along it. The gearshift
moves the gear. The selected gear is moved to mate with its
corresponding gear on the _layshaft_, which is an internal shaft with
corresponding gears. The layshaft then drives the output shaft, which
goes to the wheels.

Note that, even if the clutch were disengaged, shifting gears can
"crash" because the input shaft is not running at the proper rate
proportional to the output shaft. No power is delivered to the input
shaft, but that doesn't mean it is matching the rotation speed needed.

Thus _double-clutching_ is a thing. You clutch in, shift to neutral,
clutch out, adjust the accelerator until the right RPM is found, clutch
in, shift to the new gear, clutch out. This avoids the gears crashing.

https://en.wikipedia.org/wiki/File:Crash_gearbox_3gears_and_reverse.gif

## Synchronous transmission

Luckily, since at least the 70s they've adopted a new system: the
synchronous gearbox. This is so-called because all gears on the input
shaft are always engaged with all gears on the layshaft.

How does this work? The gears are at different ratios, so they surely
can't all be engaged? Answer: they aren't. When you select a gear, _dog
teeth_ (also called the _dog clutch_) that are part of the input shaft
engage with the gear. The gear doesn't move, but the dog teeth do. This
is like a clutch engaging the gear inside the gearbox. Thus it is called
a dog clutch.

In addition to the gear grooving that meshes with the layshaft gear, the
gear also has some conical slots for the dog clutch. Correspondingly,
grooved hub fixed to the shaft. The synchronizer sleeve (dog clutch)
sits on top of the hub. The sleeve will be shifted forward to engage
with the corresponding dog grooves; locking the input shaft with the
gear.

But here's the point: the input shaft and gear are moving at different
speeds. Thus the locking action is "not an easy task." The synchro ring
can move with the hub, but it can also be pushed axially to contact the
gear. Basically, the ring is a large surface area; it is in fact
_conical_, fitting inside the gear to an extent. So when you start
exerting pressure of the synchro ring to the gear, it will start
changing the speed of the gear to match the output shaft.

When the speed is equalized, you now can slide the sleeve forward to
lock the gear and hub.

Basically: without the synchromesh, you'd just be smashing the sleeve up
against the dog teeth in the gear and it would be a lot of force and
fragile.

Source: https://www.youtube.com/watch?v=wCu9W9xNwtI

The engagement of the dog-clutch needs to be _synchronized_, in the same
way that we use the clutch to synchronize engine RPM to desired
transmission input. The synchronization is less extreme though, and is
accomplished via _synchro rings_. These are rings with conical slots for
the dog teeth which evens out input shaft RPM with the desired layshaft
RPM.

The synchro rings can be worn if you do a shift where the transmission
input shaft RPM is inappropriate. But because the input shaft is not
connected to the engine while the clutch is out, there is less force
involved.

https://en.wikipedia.org/wiki/File:Gearbox_4gears.gif

## Alternatives

- There is _automatic transmission_, but how this works will have to be
  discussed another day. I think it works fundamentally differently.
- "Manumatic" is a common feature on many cars, allowing you to request
  a specific gear. But the computer will execute the shift for you, and
  it will not allow you to select a gear it doesn't think is suitable.
  - It's still fundamentally an automatic transmission.
- Automated manual transmission is generally built around a typical
  transmission system, but controls shifts from a computer.
- A continuous variable transmission is quite different. It's used by, I
  think scooters. Basically, there are no gears, but rather belts
  engaging with cones. The correct ratio can be continuously adjusted by
  the system. There is no shifting between discrete gears.
- The last thing to try to understand is _dual clutch transmission_.
  Basically, there are two clutches. The odd and the even gears are on
  different layshafts. There are two clutches; the even clutch connects
  to the even input shaft and layshaft, and the odd clutch connects to
  the odd input shaft and layshaft.
  - Let's say you are in gear four. Then the clutch is not engaged. You
    can select gear five (or three), then you clutch in on the even
    shaft exactly as you clutch out on the odd shaft.
  - This is a very fast change. So you don't have any dead time while
    shifting.
  - DCT can be driven just like an automatic transmission. So it is
    easy. It competes with automated manual transmission, but shift
    faster.
  - But it sounds like BMW thinks DCT won't stick around, and that
    automatic will win.
- In summary:
  - Automatic is the original easy method that uses a _torque converter_
    which is lower efficiency.
  - Manumatic is just automatic where you can specify the gear. It gives
    a little control, but isn't fundamentally different.
  - Automated manual transmission is basically just a manual
    transmission that can be actuated by computer. On high end cars, you
    can use regular manual mode yourself.
  - Dual clutch transmission is a manual, except you can't manually
    shift except via paddles. It's not like a normal manual that way.
    But it does probably have the best performance of these options.

## Cars With Manual Mode

- There are relatively few made by higher-end producers. Here is a
  basically complete list of higher end models with manual transmission.
- On the other hand, DCT does seem very popular.
- BMW M2, M3, M4
- Mazda 3, MX-5 Miata
- Porsche 911
- VW Golf GTI

## Motorcycles

- Practically all motorcycles are manual transmission.
- Only Honda sells any DCT bikes today, though possible that some other
  makers will start.
- But DCT is a premium feature and is only on bigger bikes I think.
- It would be nice to worry about one less thing while learning to ride,
  but DCT motorcycles don't seem to be a thing yet.
