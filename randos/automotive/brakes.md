Motorcycles typically have hydraulically actuated disc brakes. Old bikes
used to use drum brakes at the rear, but that is no longer the case.

Medium sized bikes and larger will typically have two rotors at the
front, and one in the back. Why more at the front? Because with heavy
braking, load will transfer to the front. More traction at the front
means more braking can be done there.

Hydraulic brakes are superior to cable actuation because there is less
frictional loss, and better feel.

Driving 4 Answers claims that you are limited in braking power by
traction. That is correct. There is no reason for more braking power if
you would break traction. In fact, ABS should kick in!

Then why do bigger bikes have bigger rotor diameters? It is true that a
bigger rotor should have more leverage. A more massive bike should
probably have bigger rotors because it has more traction (due to
increased mass), so you need more brake force to stop the vehicle (or
for that matter, lock the tire).

But stock brakes should have already been sized big enough to break
traction/engage ABS. For autos this is actually required by legislation
(says D4A in follow-up video). So swapping for bigger rotors won't give
more stopping power in the sense that you're always already going to
eventually be traction limited.

The larger rotors could act as better heat sinks/dissipators. As we
know, as the rotors get hot, braking performance is going to drop. One
reason is that the hydraulic fluid can vaporize, which will make it a
compressible gas. Then squeezing the brake will compress the gas, rather
than squeeze the brake. But even before that, hotter materials just have
lower coefficient of friction.

Driving 4 Answers makes clear that he thinks that upgrading to larger
brake rotors is typically useless. Unless the problem is brake fade/heat
dissipation.

As discussed in my bicycle brake document: there is an inverse
relationship between caliper piston travel distance and braking
hydraulic leverage. You want to multiply the input force at the lever.

The master piston will be small, and the caliper piston will be large.
Thus, a large motion/small force at the master piston will become a
small motion/large force at the caliper piston. This large force will
correspond to a lot of friction at the brake rotor.

Driving 4 Answers explicitly recognizes that larger brake rotors does
imply that the same frictional force corresponds to a greater braking
torque. But he says that it isn't that much of a difference (except when
comparing rim brakes to disc brakes on bicycles; rim brakes have better
leverage and thus don't need hydraulic leverage). He says the large
brakes are all about heat dissipation.

He recommends upgrading _tires_ to improve braking distance. That makes
total sense, as this increases the traction of the vehicle.

ChatGPT suggests that larger rotors (specifically with more leverage)
can lead to better brake _modulation_/feel. First, larger rotors will
require less braking power at the handle, which can make them easier to
operate (with less force). Second, hydraulic braking systems have flex
(in lines, and seals at caliper). As you add pull the lever, you'll
start to flex these. Some of your effort goes into pushing the calipers,
but other effort goes into flexing. But the lines and seals will stiffen
up. Eventually all your effort is going into pushing the calipers. That
causes a nonlinear jump in the ratio of braking to further lever pull.

The point is: if you shift the pressure needed for peak braking (by
getting more rotor leverage), then you operate in the range where the
flexing hasn't totally ramped up. Now that peak braking is at lower
pressure, we have better feel _near_ peak braking, because the system
still has relatively consistent flex.

## Where Does Braking Power Come From?

Ultimately, braking comes from friction. Friction is the coefficient of
friction times the normal force. Notice that the _surface area_ is
unimportant here. Put another way: sliding a 2x4 requires the same force
no matter what face you lie the 2x4 on.

Why?

First, let's note that "contact area" isn't what you think. The surfaces
are jagged. The faces of two surfaces are not in contact. Actually,
their peaks are in contact. So you already can see that surface area
might be somewhat deceptive.

But certainly with more surface area, there are more peaks to interface,
right? Yes.

But how much do the peaks interface with each other? This depends on the
pressure on the peaks. And _that_ depends inversely with the surface
area.

Thus the two factors cancel out, making friction force independent of
(simplified) contact area.

This is just a model, and it's not perfectly true. But it is supposedly
fairly accurate and explains the phenomenon.

- Source: https://www.youtube.com/watch?v=CyH5xOcsXxs
- Source: https://www.youtube.com/watch?v=Ql9eYh31kTw (D4A followup)
  - Argues that brakes by legislation are already enough to lock wheels
    and trigger ABS.
  - Notes that the tire define the limit.
- Source: https://physics.stackexchange.com/questions/307902/why-does-friction-does-not-depend-upon-area-of-contact

## What about tires?

But if that's true, then why does sizing up the tires seem to increase
traction?

Driving 4 Answers explains this. I won't go into detail here, but the
reason relates to the fact that the tire is rubber and can deform.

## Vacuum/Power Brakes

In a car, you want to increase the power of braking. We already talked
about how you can do this with leverage. But leverage always involves a
tradeoff with travel distance. How can we amplify the input force (foot
pressing brake), without increase travel distance of the foot?

I think one answer is larger brake rotors and applying the force at a
greater distance. But that only gets us so far (as Driving 4 Answers
emphasizes).

We can use some power from the engine. We use the engine to create a
vacuum on both sides of a diaphragm, which is connected to the brakes.
The engine naturally wants to suck away air.

When we push the brake pedal, we break a seal one side of the diaphragm.
Air floods into this side of the diaphragm. The air pressure now wants
to push on the diaphragm.

The larger the diaphragm, the more total force will be exerted on it. We
can probably even use the car to run a compressor on the other side so
that we can achieve a pressure of greater than 1 ATM, if we wanted to.

Engineering Explained notes something interesting. Turn off the car
while still rolling. Press the brakes. You'll get one last vacuum
assisted braking. Now release them. Try again. The pressure will have
equalized on both sides to ~1 ATM, and you will no longer have vacuum
assisted braking.

Of course, motorcycles don't use vacuum powered brakes, but then they
already have sufficient power to lock up the wheels (because less mass).

Source: https://www.youtube.com/watch?v=LThrL8Jjsgc

## Brake Lines

- Most hydraulic cables for brakes are made of rubber.
- But when pressure at brake lever is applied, it can bend or balloon
  the line. Especially over time the rubber gets more bendy.
- The steel braided lines don't bend or balloon as much, and thus give
  better brake feel. More of the brake lever force is transmitted to the
  pads.
- I think especially they have strong initial bite, because they won't
  bend very much at the beginning. Since even unbraided lines stiffen up
  and use up their compliance, the difference isn't really at the
  highest end, but in terms of initial bite.

## ABS

- System monitors wheel rotation rate. It doesn't wait for the wheel to
  totally stop. It detects when wheel rotation rate is insufficient to
  match (estimated) vehicle speed.
  - Best braking occurs at a given _slip ratio_. The wheel is still
    rotating but it slipping.
- ABS modulates pressure in an attempt to apply just enough pressure to
  maximize that slip rate without going over.
- A very skilled rider can hold the tire close to the maximum amount of
  braking without locking up. They might hunt less than ABS. And they
  may better understand how much grip is truly available to them in the
  road conditions. ABS needs to be conservative about the amount of
  traction available.
- Challenges of ABS:
  - ABS doesn't know actual speed perfectly, so it has a noisy estimate
    of slip rate.
  - It doesn't actually know tire grip, which I believe affects ideal
    slip rate. It doesn't know temperature, road surface, load. And
    these can change even during braking!
  - So ABS intervenes conservatively to avoid lock, backs off pressure
    more than might be minimally necessary, and only then reapplies.
  - Also: ABS doesn't know if you're in a turn (at least without IMU).
    So it is tuned to be conservative about braking too hard, because it
    doesn't want to unsettle the bike in a turn.
- Lean Sensitive ABS
  - IMU detects lean angle, and realizes that some of the friction
    budget must be used for cornering.
  - Without IMU, ABS doesn't know how much longitudinal braking to
    allow. It's conservative, so ABS doesn't give you enough braking in
    straights, but it also allows _too_ much braking in tight turns.
  - Also: when leaned over, you don't want to _change_ braking too
    quickly. It's not just that you'll use up all your traction budget
    quicker because some budget is going toward lateral
    traction/cornering. It's that changes in the amount of braking
    _also_ change chassis dynamics, suspension loading, and upsetting
    these can cause the front tire to give. So when leaned over, you
    want to adjust braking power less aggressively. Without IMU, you
    can't know how much you're leaned over, so the system is tuned to be
    conservative with change in brake application.
