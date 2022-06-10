## Gear Shifting For Maximum Acceleration

Acceleration depends on the force where the tire meets the road. This
force is equal to wheel torque divided by wheel radius. Smaller wheels
should give faster acceleration, I suppose, at expense of top speed.

To maximize acceleration, you maximize force. Which means you want to
maximize wheel torque. What goes into wheel torque?

- Engine torque
- Gear ratio
- Final drive ratio
  - This is a constant. It's just the gearing that connects the
    driveshaft to the rear axle. We may ignore it.

Great. Now, as you upshift, the gear ratio will drop. Gear ratio is
expressed in terms of input gear:output gear. So you have the highest
gear ratio in first gear. Thus, at the same engine torque, you have the
highest wheel torque in first gear.

If engine torque were non-decreasing, you would never want to upshift.
Upshifting would reduce RPMs, which reduces engine torque. Upshifting
also reduces gear ratio. Both factors work to reduce wheel torque.

Of course, even if torque were nondecreasing in RPM, you still will need
to upshift when you reach redline. Otherwise, you cannot increase your
speed further without running the engine past redline. The redline is
limiting your top speed in a given gear. This is why you _must_ upshift.
You would expect a decrease in acceleration every time you upshift.

Engine torque is typically _not_ monotonically increasing. The torque
will peak somewhere below redline, and then decrease until redline.
Normally peak torque is relatively close to redline, and the decrease in
torque is fairly small (maybe 20%) at redline.

The question now is: is there any point between peak torque and redline
where an upshift will increase torque at the wheel?

## Torque Math

```python
# Constants
current_gear_ratio
current_rpms
current_engine_torque = TORQUE_DYNO(current_rpms)

next_gear_ratio
next_rpms = (next_gear_ratio/current_gear_ratio) * current_rpms
next_engine_torque = TORQUE_DYNO(next_rpms)

# If `next_engine_torque < current_engine_torque`, then don't shift. You
# will be reducing *both* engine torque and gear ratio. Both factors
# reduce wheel torque.

current_wheel_torque = current_engine_torque * current_gear_ratio
next_wheel_torque = next_engine_torque * next_gear_ratio

# Now, let's divide next_wheel_torque by current_wheel torque. This
# is the product of two factors:

gear_ratio_change = next_gear_ratio / current_gear_ratio
engine_torque_change = next_engine_torque / current_engine_torque

# We know that gear_ratio_change < 1. We've also assumed
# engine_torque_change > 1. The question remains: is the product of
# these two > 1?
```

By looking at this from an engine-torque perspective, we see that we
cannot simply look at the torque curve to compare redline torque to the
torque we would shift to. We must further compare this (good) change in
engine torque to the (bad) change in gear ratio. That makes things a
little complicated.

## Horsepower Math

It's a little easier if we look at _horsepower_. It makes sense that we
should look at horsepower. Horsepower is instantaneous change in energy.
If engine energy is transferred to motorcycle kinetic energy (maybe with
some constant losses), then more horsepower means greater acceleration.

Let's justify this mathematically:

```
# The relevant comparison
current_wheel_torque ? next_wheel_torque
current_engine_torque * current_gear_ratio ? next_engine_torque * next_gear_ratio
# Multiply both sides by current_rpms, and divide by current_gear_ratio
current_engine_torque * current_rpms
?
next_engine_torque * (current_rpms * next_gear_ratio/current_gear_ratio)
# Note this equality
next_rpms = current_rpms * next_gear_ratio/current_gear_ratio
# So substitute
current_engine_torque * current_rpms ? next_engine_torque * next_rpms
# Last, note that torque * rpms = HP (well, with a constant factor).
# Thus we can reduce to:
current_hp ? next_hp
```

Thus we see that an upshift will increase acceleartion iff the upshift
increases wheel torque iff the upshift results in greater engine
horsepower output. It is this last formulation that we can reason about
most easily from a dynograph.

## A Practical Example

Typically we expect horsepower to monotonically increase in RPM until
peak, and then to decrease monotonically until redline. Indeed, this is
what we see on the dynograph for a BMW 1250GS.

Source: https://www.cycleworld.com/how-much-power-does-2019-bmw-r1250gs-adventure-make/

Given a horsepower curve like this, we know you should never upshift
before peak horsepower.

Once past the peak horsepower, you would like to upshift to increase
back to peak horsepower. However, if
`next_gear_ratio/current_gear_ratio` is too much less than one, then
your RPMs might drop too much and put you too low on the horsepower
curve. So let's consider the gear ratios of the BMW 1250GS:

```
1st - 2.438
2nd - 1.714
3rd - 1.296
4th - 1.059
5th - 0.943
6th - 0.848
```

More importantly, let's calculate the change in gear ratio by various
upshifts:

```
1st-to-2nd: 0.703x
2nd-to-3rd: 0.756x
3rd-to-4th: 0.817x
4th-to-5th: 0.890x
5th-to-6th: 0.900x
```

Let's consider that we upshift from 1st-to-2nd at 9k RPMs. Our new RPMs
will be about 6.3k RPM. We can see that horsepower at 9k RPMs is about
100HP and horsepower at 6.3k RPM is maybe ever-so-slightly more.

Shifting before redline would mean that pre-shift horsepower would be
greater, and post-shift horsepower would be less.

This shows that you should just-about ride the BMW 1250GS to redline in
first before shifting to second. This is typical, because the gear
changes are "taller" in lower gears (the change in gear ratio is great).

As the gear changes get "shorter", then your RPMs will drop less as you
shift. Let's say that `max_hp_rpms` is the RPMs at which horsepower is
maximized; for the BMW 1250GS this is 7.5k RPMs. Then of course we will
want to shift no later than `max_hp_rpms / (next_gear_ratio/current_gear_ratio)`. We can calculate this for some
upper-bounds on when to shift:

```
1st-to-2nd: 10.66k RPM (past redline)
2nd-to-3rd:  9.92k RPM (past redline)
3rd-to-4th:  9.18k RPM (past redline)
4th-to-5th:  8.42k RPM
5th-to-6th:  8.33k RPM
```

## Summary

- Never shift before obtaining peak horsepower.
- In lower gears, when upshifting results in a greater drop in RPMs,
  ride the engine out closer to redline.
- In higher gears, don't wait until redline.

## Non `^` Shaped Graphs

Should you ever ride past the point where HP would have increased by
upshifting?

First, when could that happen? Consider that most HP graphs are
monotonically increasing to peak HP, then are monotonically decreasing.
Thus, once it makes sense to upshift, it will never make sense to then
downshift.

I also would argue: you should upshift as soon as that increases HP. If
later you could increase HP by downshifting, then do it. But you still
are just following the HP numbers.

## Final Drive Gearing Change

On a chain-driven bicycle, you can change the front and rear sprockets
to change your top-speed/acceleration. This change the final-drive
ratio.

Say you either (1) decrease the number of teeth on the front (also
called countershaft) sprocket, or (2) increase the teeth on the rear
sprocket. Thus, each revolution of the countershaft implies less
revolution of the rear wheel. This is _increasing_ the gear ratio.

(Somewhat confusingly, we call this increased gear ratio _shorter_. It's
doubly confusing because shorter can also mean that the difference
between successive gear ratios is less.)

Anyway, your increased gear ratio will deliver more torque at the same
engine RPM. Let us say you increase gear ratio by 5%. Your torque will
increase by 5% across the rev range. On the other hand, across the rev
range, your speed will have decreased by 5%.

You'll want to shift at lower speeds, but at the same RPMs (since the
height of gear changes has not been changed). Eventually, when you're
running at maxmium RPM in the top gear, you'll find that your maximum
speed has decreased by 5%.

On most bikes, the speedometer reads the countershaft speed and assumes
it knows the final drive ratio. Thus your speedometer will read 5% too
high. Likewise, your odometer will collect miles 5% too quickly. You can
sometimes get a device that corrects for this.

In general, you can expect gas mileage to decrease when increasing the
gear ratio. You will have to cruise at 5% higher RPM to maintain the
same speed, and your engine would rather operate at a lower RPM.

On the other hand, you could _decrease_ the gear ratio (taller gearing).
This will sacrifice torque, but will give you a higher top-speed, and
better gas mileage. Since motorcycle top speed is faster than I want to
go, and because motorcycle gas mileage is already pretty decent, this is
not that interesting to me.

Source: https://www.youtube.com/watch?v=C6_FtVTjKng

## Sources

The grown-up kid from Engineering Explained has a decent explanation and
confirms redline is basically what you want:

Source: https://www.youtube.com/watch?v=zZBqb0ZJSwU
