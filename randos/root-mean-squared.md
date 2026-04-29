- Say you want the average power consumed by a fixed resistance attached
  to an AC power source.
- Instantaneous power is $p(t) = v(t) i(t)$, or $p(t) = i(t)^2 R$, or $p
  = v(t)^2 / R$.
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
- For a purely resistive load driven by a sinusoidal alternating
  voltage, we have "real power" of $0.5 V_max^2 / R$. That's
  because the RMS of $sin(\theta) V_max$ over one cycle is $\sqrt{0.5}
  V_max$. You can make a simple pairing argument to calculate this.

## Real/Active Power, and Instantaneous Power

- The power which is used to do work is called _real power_. This is
  also called _active power_. It is denoted $P$ and averaged over one
  cycle: $P = \frac{1}{T} \int_0^T v(t) i(t) \dt$.
- You can also talk about _instantaneous_ power: $p(t) = v(t)i(t)$.
- Of course, the real power is just the average instantaneous power over
  the cycle.
- The real power is the amount of power which is actually delivered to
  the system as work.

## Apparent Power, Power Factor

- _Apparent power_ is _defined_ as $|S| = V_\text{RMS} I_\text{RMS}$.
- For purely "resistive" loads, real power is always the same as the
  apparent power.
- That happens whenever current is proportional to source voltage.
- We typically use units VA (volt-ampere) for apparent power, rather
  than W (which we use for real power).
- We can compare the ratio of real power and apparent power. This is the
  _power factor_, which runs between zero and one. This is $P/|S|$.

## Linear Circuit, Reactive Power

- Let's assume that the current profile $i(t)$ is a scaled phase-shift
  of the voltage.
- Then we can write it as $i(\omega) = v(\omega) / R + v(\omega + \pi/2)
  / X$ ($X$ is the _reactance_). This breaks the current into real and
  _reactive_ current.
  - I believe that breaking down current into real and reactive current
    is not typically done, but I believe mathematically valid.
  - In fact, if we write
- When we integrate $\int v(\omega) i(\omega) \d\omega$, we get $P =
  V_{\text{RMS}}^2 / R$: the _real power_. We measure this as Watts:
  Joules per second.
- When we integrate $\int v(\omega) i(\omega - \pi/2) \d\omega$, we get
  $Q$: the _reactive power_. This is $Q = V_{\text{RMS}}^2 / X$. We
  measure this in "var": volt-ampere reactive. Of course it is same unit
  as Watts, but var distinguishes this is a different kind of power.
- We could also write $i(\omega) = v(\omega) / R + \hat{j} v(\omega) /
  X$. $\hat{j}$ is how physicists write the imaginary unit. This would
  make current _complex valued_.
- We can now integrate, again. We can define the _complex power_ $S = P + \hat{j} Q$.
- We can take the norm of this power: $|S| = \sqrt{P^2 + Q^2} =

- I claim that a "linear circuit", when connected to an alternating
  current, will have a current profile $i(t)$ which is a scaled and
  phase-shifted version of the voltage profile $v(t)$.
- A basis for the power is thus
- The power factor can be written as $\cos\phi$ for an appropriate
  $\phi$ value.
-

## OTHER

- We can also talk about _reactive power_ (denoted $Q$).
  - We can basically break down the "apparent" power into two parts.
  - A real power which is "in-phase" with the source voltage.
  - A _reactive_ power which is 90deg out-of-phase with the source
    voltage.
  - We will say that reactive power is always imaginary, real power is
    is always real, and apparent power $S = P + Q$ is complex-valued.
  - Reactive power is sometimes denoted "var" (volt-ampere reactive).
- Power factor
  - The reactive power is of course $(1 - \cos\phi) |S|$.
  - If the voltage and current are really both sinusoids, then $\phi$
    really is the phase angle between them. This happens if the supply
    voltage is sinusoidal and the circuit is "linear". In that case,
    both voltage and current will be sinusoids of same period, but
    possibly different phase.
- A linear circuit consists of only purely resistive and purely reactive
  components. So if you're only using resistors, inductors, and
  capacitors, your circuit is going to be a linear circuit. And its
  power draw will be possibly phase-shifted, but at the same input
  frequency.
