- TODO: this doc is kind of random. Wanted to break out from
  "engine-configuration.md" because this didn't have anything to do with
  that...
- What limits redline?
  - Breathing: valves will eventually limit amount of air that can flow
    in, and this will hurt higher RPM torque. At that point, you should
    downshift. Can't just size bigger, because that tends to hurt low
    RPM air fuel mixing.
    - Can improve mixing with more stroke vs bore, but that is in
      opposition to sizing bigger valves.
    - Also, more stroke will increase connecting rod forces (at least
      faster than increasing bore does). See: bore-vs-stroke.
  - Valve float. Valves can't close fast enough. Can threaten a
    collision with the piston in an interference engine. Even if not, as
    you hit higher revs, valves will start to stay open when they
    shouldn't, which hurts efficiency (air might get pushed back out),
    which also hurts torque (less work done per rev), emissions...
  - Connecting rod forces. As you accelerate the piston faster through
    the stroke to cover the same distance in a shorter period, the
    tension force grows, and eventually rod would fail.
- If you scale displacement by adding more cylinders, you can keep
  running the same redline. If you scale displacement by sizing
  cylinders up, you tend to lower the redline (though you do make more
  torque).
  - At same displacement, you can raise redline typically by increasing
    bore-to-stroke, but this tends to hurt low-end torque mixing.

## Sources

- https://www.cycleworld.com/story/blogs/ask-kevin/motorcycle-redline-determines-horsepower/
  - TODO: re-review this. It has been a long time and I've learned a lot
    more.
  - This lists valve train then connecting rods.
  - He mentions that load is proportional to RPM^2 and piston stroke.
  - But why does the piston mass not matter?
  - He does note some problems with large bores: cooling the large
    pistons, and flame propagation (I guess it's harder to propagate
    flame through a small pancake).
  - Larger bore allows for larger valves.
