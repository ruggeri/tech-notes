- Say you want the average power consumed by a fixed resistance attached
  to an AC power source.
- Instantaneous power is `P=IV`, or `P=I^2 R`, or `P=V^2/R`.
- But voltage and current are constantly changing, and so is power
  consumed by the load.
- We cannot plug peak voltage into `P=V^2/R` and get the average power.
  This would give the peak instantaneous power. That's too much.
- But we also cannot take the average voltage and plug that into
  `P=V^2/R` either. First, the average voltage for a pure AC power
  source is zero, so that kind of doesn't work. But even the average
  absolute voltage is wrong.
- Imagine a constant DC voltage of 1V for 2 seconds. This gives constant
  instantaneous power of `P=1/R`.
- Now imagine a voltage of 0V for 1 second and 2V for 1 second. This
  gives `P=0` for 1sec and `P=4/R` for 1sec. Thus the average power is
  `P=2/R`! That _doubled_ power consumption.
- This brings us to root-mean-square (RMS). This will be the voltage
  which, if applied continuously, would cause the circuit to draw as
  much power over a period as the alternating voltage.
- To calculate this "equivalent" voltage, we must:
  - Average the _square_ of the alternating voltages.
  - Then take the square root.
- In our earlier example, we should square 0V (0V) and 2V (4V). We then
  take the mean over 2sec to get 2V. But now we must take the square
  root and get ~1.4V.
- Thus the 1.4V constant voltage is drives equivalent power through the
  load as the alternating 0V and 2V current.
