## Basic Problem of Differential

- You have the driveshaft. Using a pair of bevel gears it can rotate an
  axle.
- But then both wheels would have to turn at the same rate.
- If that happened, they would have trouble going about in a circle.
- One of the wheels would have to slip forward. That wheel would
  constantly be breaking traction.

## Open Differential: Locked Differential

- To start exploring the open differential, we will first consider locking it.
- We will review this wikipedia diagram: https://en.wikipedia.org/wiki/Differential_(mechanical_device)#/media/File:Differential_free.png
- Imagine that the green gear couldn't rotate.
- Imagine the purple case rotating.
- Effectively, the green gear will act like two linear ridges to engage
  the red and yellow gears.
- So as the purple case rotates, it will drag the red and yellow gears
  forward.
- In this case, we say the differential is **locked**.
- Obviously, if we lock the green gear, there is no point in the
  differential.

## Open Differential: Unlocked, Equal Traction

- Next, imagine that the green gear is allowed to rotate freely.
- But let's also assume that there is equal traction available at each
  wheel.
- By symmetry, we know that the green rotate will not want to rotate,
  and that both red and yellow gears should spin at the same rate. But
  that doesn't necessarily explain anything.
- We can argue this way.
  - The green gear is in contact with the red gear. The force applied to
    turn the red gear forward will be equal to a force that wants to
    rotate the green gear clockwise (viewed from above).
  - Likewise, the green gear is in contact with the yellow gear. The
    force applied to turn the yellow gear forward is equal to a force
    that wants to turn the green gear **counter-clockwise**.
  - Since both forces on the red and yellow gears are the same, the
    forces that want to turn the green green gear will cancel and the
    red and yellow gears will be dragged forward as before.

## Open Differential: Locked Gear

- Next, imagine if the red wheel was locked. Ignore the yellow gear for
  a moment.
- The green gear will want to turn at a rate equal to the rate of the
  purple case.
- If nothing else, this will want to spin the yellow gear.
- But the green gear also wants to drag the yellow gear forward, as
  before.
- Then, we expect that the yellow gear will run at _double_ the rate.
- Anything that slows the rotation of the red gear will increase the
  rotation speed of the yellow gear.
- We thus see an equation: the _average_ rate of the two wheel rotations
  is equal to the rate of rotation of the purple case.
  - You can either have balance, or at an extreme one is stationary and
    the other runs at double the rotation rate.

## Open Differential: Wheel Without Traction

- Imagine if the wheel connected to the yellow gear has no traction.
- As the purple case starts turning, a force will be exerted on the red
  gear. But an equal and opposite force will be exerted to rotate the
  _green_ gear (clockwise, when viewed from above).
- The green gear turns easily, because it is connected to the yellow
  gear, which can spin freely. The red gear cannot turn easily, because
  to do so it must move the massive car forward.
- Thus the green gear will spin much faster than the red gear. The ratio
  of the red and green spin rates is the inverse of their rotational
  inertias.
- The yellow rotational speed will be the blue plus the green rotational
  speeds. It will be higher than the red rate.
- Thus, very little work is available to be performed at the red wheel.
- In general, the force exerted at the red wheel with traction is only
  ever as great the force for the gold wheel to break traction. And once
  traction is broken at the gold wheel, you lose all force at the red
  wheel.
  - Thus, you might try to slowly apply a little gas.
  - In the worst case, this won't be enough if the red wheel must move
    the car uphill. But you can't give more force, because the gold
    wheel will lose traction.
- This explanation is not entirely satisfactory, but it's good enough
  for the moment.

Source: https://www.youtube.com/watch?v=K4JhruinbWc&t=0s

## Limited Slip Differential
