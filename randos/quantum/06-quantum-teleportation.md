Say I want to communicate my qubit Q with you, but I don't have a
quantum channel to you. I do have a classical channel on which I can
sends ones and zeroes.

If my qubit Q were in a basis state, everything would be easy (I'd just
send a one or two). But the point is that it might not be.

If we share an EPR pair already, here's what we can do. First, let the
quantum state be notated

    (\alpha \0> + \beta |1>) \otimes (1/sqrt(2)) (|00> + |11>)

First, I control not my qubit Q with my half of the EPR pair. That gives

    (1/sqrt(2)) (\alpha |000> + \alpha |011> + \beta |110> + \beta |101>)

I *could* measure my half of the EPR pair. But if I did that (and
measured zero), I'd get something like:

    \alpha |0X0> + \beta |1X1>

It's hard to see how I could unentangle my qubit.

Let me instead apply H to the qubit I'm trying to send. We then get:

    (1/sqrt(2))
    \alpha ((1/sqrt(2)) (|000> + |100>))
    + \alpha ((1/sqrt(2)) (|011> + |111>))
    + \beta ((1/sqrt(2)) (|010> - |110>))
    + \beta ((1/sqrt(2)) (|001> - |101>))

We may rearrange:

    1/2
    (\alpha |000> + \beta |001>)
    + (\beta |010> + \alpha |011>)
    + (\alpha |100> - \beta |101>)
    + (\beta |110> - \alpha |111>)

Now, we measure both of our qubits. There are four cases:

* 00 -> They have `\alpha |0> + \beta |1>`.
* 01 -> They have `\beta |0> + \alpha |1>`. Apply Pauli X NOT.
* 10 -> They have `\alpha |0> - \beta |1>`. Apply Pauli Z phase shift.
* 11 -> They have `\beta |0> - \alpha |1>`. Apply Pauli X NOT, then
  Pauli Z phase shift.

Note: there is no faster-than-light communication, because we need to
send the bits. Second: there is no cloning because we have destroyed our
copy of the EPR pair.
