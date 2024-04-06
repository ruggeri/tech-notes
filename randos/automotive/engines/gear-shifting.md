## Gear Shifting For Maximum Acceleration

Acceleration depends on the force where the tire meets the road. This
force is equal to wheel torque divided by wheel radius. Smaller wheels
should give faster acceleration, I suppose, at expense of top speed. On
the other hand, if you want larger wheels for another reason (like
handling), you should be able to adjust gearing to compensate.

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

Thus we see that an upshift will increase acceleration iff the upshift
increases wheel torque iff the upshift results in greater engine
horsepower output. It is this last formulation that we can reason about
most easily from a dynograph.

## A Practical Example

Typically we expect horsepower to monotonically increase in RPM until
peak, and then to decrease monotonically until redline. Indeed, this is
what we see on the dynograph for a BMW 1250GS.

Source: https://www.cycleworld.com/how-much-power-does-2019-bmw-r1250gs-adventure-make/

Given a horsepower curve like this, we know you should never upshift
before peak horsepower, since upshifting before peak horsepower always
throws you to lower horsepower.

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

Let's consider that we upshift from 1st-to-2nd at 9k RPMs (redline). Our
new RPMs will be about 6.3k RPM. We can see that horsepower at 9k RPMs
is about 100HP and horsepower at 6.3k RPM is maybe ever-so-slightly
more.

Shifting before redline would mean that pre-shift horsepower would be
greater, and post-shift horsepower would be less.

This shows that you should just-about ride the BMW 1250GS to redline in
first before shifting to second. This is typical, because the gear
changes are "taller" in lower gears (the change in gear ratio is great).

As the gear changes get "shorter", then your RPMs will drop less as you
shift. Let's say that `max_hp_rpms` is the RPMs at which horsepower is
maximized; for the BMW 1250GS this is 7.5k RPMs. Let's assume
momentarily that past 7.5k RPM, HP stays constant. In this ideal
scenario, there is harm in shifting at `max_hp_rpms /
(next_gear_ratio/current_gear_ratio)`. This is an upper-bound; it will
typically be better to shift before this RPM because HP will be dropping
off.

We can calculate this for some upper-bounds on when to shift:

```
1st-to-2nd: 10.66k RPM (past redline)
2nd-to-3rd:  9.92k RPM (past redline)
3rd-to-4th:  9.18k RPM (past redline)
4th-to-5th:  8.42k RPM
5th-to-6th:  8.33k RPM
```

Note that you may want to upshift earlier than these calculations, since
HP will be dropping past the peak-HP RPM number. If this post-peak drop
in HP is steeper than the pre-peak rise in HP, you might want to shift
earlier.

What we see is that we want to shift pretty close to redline.

## Comparing Torque and Horsepower

Perhaps a "perfect" engine would have constant torque throughout the
rev-range, implying a linear horsepower curve. This means that, as you
rode in a gear with throttle full-open, you would accelerate linearly. This is because:

```
KE = 1/2 m v^2
\fpartial{KE}{t} = m v a
```

That is: if you travel at twice the velocity, you need twice the power
to maintain the same acceleration. That's what a linear horsepower curve
would do.

Granted, because of drag and friction, a linear HP curve would still not
give you constant acceleration; some of the engine power would be lost
cutting through the wind at higher speeds.

Of course, as you approached redline, you would eventually have to
change gears so that you can continue to increase your speed further.
The engine torque would not change in the ideal engine (though the wheel
torque would, of course). The horsepower would change; and that is why
you would accelerate more slowly.

## Torquey and Peaky

Another note: let's compare two engines with the same rev range, but the
first has higher HP everywhere through the range. Then it follows that
it also has higher torque everywhere through the range.

So what do people mean when they say a bike is "torquey" rather than
"peaky"? What they presumably mean is that the torquey bike makes more
torque (and more horsepower!) low in the rev range, whereas the peakier
bike makes more power (and more torque!) high in the rev range (or
simply revs higher).

It's not really that one bike has "more" torque or "more" horsepower,
while it lacks the other. A bike makes _more horsepower_ at exactly
every RPM where it makes _more torque_ (and vice versa). But wherever a
bike makes more HP, it will accelerate harder at that RPM.

That said, a "torqueier" bike would be the one that makes more peak
torque, when compared to a "peakier" bike that makes more HP at a higher
RPM (with less torque). The torquier bike is going to accelerate harder
at the lower RPMs, whereas the peakier bike accelerates harder once it
is reving high.

Which engine accelerates faster from a stop? If the gearing is the same,
the engine that makes power lower in the rev range should have the
advantage for a start, until the peakier engine gets into the rev range
where it is producing more power.

If the peaky engine has a very short first gear, it should be able to
spin up quickly to peak RPM. But then you're going to have to upshift,
which loses time, and also it might push you down the peak HP curve.
And, if the upshift doesn't push you too high down the HP curve, you're
going to have to shift again very soon as you approach redline again.

## Summary

- Never shift before obtaining peak horsepower.
- In lower gears, when upshifting results in a greater drop in RPMs,
  ride the engine out closer to redline.
- In higher gears, don't necessarily wait all the way until redline.

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

## Peak Torque, Peak Horsepower

Manufacturers often list _peak_ torque and _peak_ horsepower numbers,
but those are less useful than the dyno chart.

If you had some kind of perfect CVT, then peak horsepower would really
be the ideal. You would run the engine at peak HP, and you would vary
the gear ratio as your speed increased, maintaining maximum
acceleration.

But either a peak torque or peak HP number might be misleading if the
bike doesn't have a wide plateau of torque. If it doesn't, then upshifts
are going to doubly hurt you by not just reducing RPM, but also reducing
engine torque.

An engine with high torque but low HP maxes out acceleration in a gear
at a relatively low RPM. That means you're forced to upshift earlier.
The bike might feel faster in first gear for a while, though.

An engine with high HP but a low torque might accelerate more slowly at
first, but then start to catch up as you reach higher RPM/speed.

## Sources

The grown-up kid from Engineering Explained has a decent explanation and
confirms redline is basically what you want:

Source: https://www.youtube.com/watch?v=zZBqb0ZJSwU
