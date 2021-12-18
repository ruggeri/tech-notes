## Gear Shifting For Maximum Acceleration

Acceleration depends on the force where the tire meets the road. This
force is equal to wheel torque times wheel radius. Smaller wheels should
give faster acceleration, I suppose, at expense of top speed.

To maximize acceleration, you maximize force. Which means you want to
maximize wheel torque. What goes into wheel torque?

- Engine torque
- Gear ratio
- Final drive ratio (a constant; it's just the gearing that connects the
  driveshaft to the rear axle).

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
limiting your top speed in a given gear. This is why you must upshift.

Engine torque is typically _not_ monotonically increasing. The torque
will peak somewhere below redline, and then decrease until redline.
Normally peak torque is relatively close to redline, and the decrease in
torque is fairly small (maybe 20%) at redline.

The question now is: is there any point between peak torque and redline
where an upshift will increase torque at the wheel?

## The Math

```python
# Constants
current_gear_ratio
current_rpms
current_engine_torque = TORQUE_DYNO(current_rpms)

next_gear_ratio
next_rpms = (next_gear_ratio/current_gear_ratio) * current_rpms
next_engine_torque = TORQUE_DYNO(next_rpms)

# If `next_torque < old_torque`, then don't shift. You will be reducing
# *both* engine torque and gear ratio. Both factors reduce wheel torque.

current_wheel_torque = current_engine_torque * current_gear_ratio
next_wheel_torque = next_engine_torque * next_gear_ratio

# Now, let's divide next_wheel_torque by current_wheel torque.

gear_ratio_change = next_gear_ratio / current_gear_ratio
engine_torque_change = next_engine_torque / current_engine_torque

# We know that gear_ratio_change < 1. We've also assumed
# engine_torque_change > 1. The question remains: is the product of
# these two > 1?
```

The problem is that the _max_ we can recover in `engine_torque` may be
approximately 20% (the difference between peak and redline torque). But
the gear ratio change will typically be greater than this. For instance,
a typical first and second gear might imply a `gear_ratio_change ~ 2`.

Also, keep in mind that such a great gear ratio change will
approximately halve the RPMs. So we'll also be thrown much farther back
past the torque peak.

Source: https://www.youtube.com/watch?v=zZBqb0ZJSwU

## Maximize Horsepower

Let me hit you with another interesting fact. Consider

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

Thus we have a simpler version of the same decision. Would decreasing
the gear ratio increase the horsepower? This makes sense: horsepower is
the rate at which we add kinetic energy to the vehicle.

## Is The Greedy Algorithm Optimal?

Should you ever ride past the point where HP would have increased by
upshifting?

First, when could that happen? Consider that most HP graphs are
monotonically increasing to peak HP, then are monotonically decreasing.
Thus, once it makes sense to upshift, it will never make sense to then
downshift.

I also would argue: you should upshift as soon as that increases HP. If
later you could increase HP by downshifting, then do it. But it still
makes the most sense to follow the HP numbers.
