Suspension has a couple points. First: you want to reduce vibration to
the passenger. Ideally, changes in surface terrain correspond to changes
in displacement of the suspension. Ideally, no displacement (i.e., no
forces) is transferred to the passenger.

Another point is to keep all wheels in contact with the ground. You want
the wheels and tires to extend as necessary to stay in contact with the
ground when they experience holes or drops. By staying in contact with
the ground, you will maintain traction and thus control.

The first thing you might add to the system is a spring. When you hit a
bump, this will transmit a force to the wheel. This force is transferred
to the spring, which compresses. The wheel will rise toward the body of
the motorcycle.

The spring will be compressed such that the energy input by the bump is
equal to the energy stored in the spring. The spring will now start to
expand again. Since the wheel has remained in contact with the ground,
the force may try to press the wheel against the ground, but the spring
cannot expand like this.

Instead, it will try to push the rider and load upward. That's pretty
much exactly what would have happened without the spring.

The other role in the system is played by the **dashpot**. This is an
oil-filled tube with a plunger. The dashpot creates a resistance force
that is linear with the velocity of the input. It does this by relying
on viscous friction of the liquid passing through a small opening.

Thus, energy of the spring is **absorbed** by the fork oil (the forks of
a motorcycle are dashpots). This heats the fork oil, but hopefully the
heat is dissipated. If not, the oil can boil which makes it a gas and
compressible. It can also leak out then. You'll have a sudden loss of
suspension.

Note that dashpots are component of door closers, to reduce the speed of
their closing/stop them from slamming. Note that door closers also need
a spring to store the input energy of opening them (which is used for
closing).

## Factors

- First, you have **travel**. This is how far the wheel can travel
  before it is fully compressed ("bottoms out"). At this point, further
  shock is transmitted directly to the rider.
- You have **spring rate**. This is pounds per inch. This is basically
  how "stiff" the spring is. It translates how much the spring will
  compress for a given force.
  - When you hit a bump, there is an upward jolt that is proportional to
    the speed.
  - The displacement will be a fixed distance: the size of the bump.
  - Thus, if the spring is infinitely stiff, then no matter your speed
    (thus no matter the force of the jolt), the spring doesn't want to
    compress.
  - Thus the displacement will be directly transmitted to the rider.
- Is it better to have an extremely springy spring? Not necessarily.
  - The same jolt will want to compress the spring _further_ than the
    displacement. It will slap the wheel off the ground, causing a loss
    of traction and control.
- **Preload** is also important. It's the amount you've compressed the
  spring from the beginning.
  - If the bike is placed on a mount (like the center stand maybe), the
    suspension will fully extend.
  - Note though that it will take a significantly non-zero force to
    compress the suspension.
  - That's because of the preload. The spring, even at the greatest
    extension of the suspension, is compressed.
  - The more preload, the more it is compressed.
- Now, let's put the bike on the ground. The amount the suspension
  compresses under the weight of the suspension is called the **sag**.
  - You can also load the bike with luggage and passenger. This will
    cause more sag.
  - Let's say there were _no_ sag. That would correspond to very high
    preload. That would be bad. when the motorcycle encountered a drop,
    there wouldn't be any more travel to extend the fork.
  - On the other hand, let's say there was too much sag. This would
    happen with insufficient preload, or just too much rider/load
    weight. Then there would be no travel left to suck up bumps.
- Think of it this way. If your bike and load is very light, then the
  bike will sit high. The wheel is already very extended. When you hit a
  drop, you will not have much room for the wheel to extend further.
  - You should have adjusted your preload so that there was already an
    intrinsic desire to extend the wheel.
- Damping is the last component.
  - First, damping will help ensure that an impact will not slap the
    suspension all the way up. As the suspension compresses, already
    energy will be lost to heating the fork oil.
  - Moreover, it will dissipate the energy stored as vibration in the
    spring.
- The amount of damping is sometimes also called "rebound damping". Ari
  didn't talk a lot in his first video about this.
  - If you had basically no damping, we know that your suspension is
    going to pogo. This will create a disconnected feeling. One aspect
    is that it may even try to jump the wheels off the ground. (Which is
    what pogoing does!).
  - If you have too much damping, then as you hit bumps, the fork may
    pack down. Each bump is compressing the spring. But then the spring
    doesn't have time to expand. Eventually the travel is consumed, and
    now it will bottom out.
  - The way to fix things is to change the size of the hole in the
    dashpot. That's what the clicker is doing on a mountain bike. This
    can typically be adjusted on the right fork tube at the top of a
    motorcycle too.

Source: https://www.youtube.com/watch?v=jOiOO7qloXQ
Source: https://www.youtube.com/watch?v=uz_p-iu9C_s
Source: https://www.youtube.com/watch?v=6b074bK7Ue8
Source: https://www.youtube.com/watch?v=jj2BRE0Unck

## Kinds of Shock Absorbers

- There are leaf springs, which are especially used on older cars and
  trucks.
- For motorcycles:
  - For the rear shock, you typically have a single shock. It's a coil
    around the dashpot.
    - It's easy to see how you would adjust the preload here.
    - I think there's often a separate reservoir.
  - For the front, you have a fork. The springs are inside there, but
    hidden.
    - You can still change the preload often! But I think it's using
      some screw on the outside that somehow compresses the spring on
      the inside of the fork.
  - You can see some designs here:
    - https://www.motorcyclistonline.com/motorcycle-suspension-tech-tips-how-to-damping-mc-garage/
    - https://bicycles.stackexchange.com/questions/80850/why-do-coil-suspension-forks-have-coils-placed-inside-the-stanchions-when-the-r
    - Explicitly shows spring in just one leg:
      https://www.motorcyclistonline.com/electronic-suspension-motorcycle-suspension-tips-mc-garage-how-to/
  - On older motorcycles, there might be gaiters which cover the
    interface of the two parts of the fork tube. But that's just to
    protect the seals.
- There are two main kinds of dashpot:
  - Orifice damping and shim damping.
  - Orifice damping just uses holes. Shim damping uses thin disks
    ("shims") that flex under pressure.
  - You've got compression damping (resistance to compression) and
    rebound damping (resistance to extension).
  - Both can be configured (on adjustable forks anyway). The rebound is
    normally set at the top of the fork, and the compression will be set
    at the bottom of the fork.
- There are fancy systems that even let you set damping at different
  levels for different velocities of shocks!
- https://www.youtube.com/watch?v=mRWmClH0bC0
  - Here Ari installs cartridge emulators.
  - These convert an orifice style damping system to a "cartridge"
    version.
  - The cartridge version has different valves (not just orifices) that
    can respond differently to compression and rebound.
  - https://racetech.com/emulators-how-they-work/

## More

- Okay, there's an important point we haven't talked about. As you push
  the piston into the damper, the rod that's pushing the piston will
  take up more space within the damper!
- Where does that oil go? We're not just "moving oil from one side to
  the other."
- If there wasn't somewhere for it to go, then the oil would be forced
  out through the seals.
- One answer is the _monotube_. Here, you have a divider that separates
  the oil part from a gas part.
  - As you push the rod in, the gas is going to be compressed.
  - But the pressure will still build on one side, and be forced through
    the orifice. It will still heat up as it is forced quickly through
    the orifice.
  - Notice the air is not in contact with the oil directly.
- Another answer is the twin tube. Here, you have a double walled
  construction. The reservoir does have an air charge.
  - However, the oil is in contact with the air, so there can be
    aeration, which will sap performance.
- Source: https://www.youtube.com/watch?v=pqKFzbclsqU
  - THANK YOU! This was the first source that explained this!
- And here is a good photo of a bicycle shock:
  - https://bikerumor.com/suspension-tech-whats-ifp/
  - This shows a monoshock which basically just has its gas chamber off
    to the side.

## Other

- I believe that some designs use _air springs_ rather than coils. I
  think this is just used for _bicycles_.
  - The air spring just compresses air.
  - Air springs are lighter. So that matters to cyclists.
  - It's easier to adjust the spring rate by changing the pressure.
    - I think this is similar to adjusting the preload on a coil system.
  - People say that coil springs give superior ride quality.
  - One reason is that you need tight seals for the air chamber, and
    that cause friction.
  - The air system will also respond differently. Springs are linear,
    whereas the air spring system is _exponential_?
  - TODO: I don't understand that.

Source: https://www.rei.com/learn/expert-advice/suspension.html
