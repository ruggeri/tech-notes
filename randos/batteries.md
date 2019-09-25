## Voltaic Cell

A *voltaic cell* (sometimes called a *galvanic cell*) is the most
primitive form of *battery*. A voltaic cell consists of two *half
cells*. Each half cell has a metal *electrode* (i.e., a metal rod)
submersed in an aqueous solution of that metal. Electrons will flow from
one electrode to the other. The half cells are connected via a
"semi-permeable membrane."

You can make a voltaic cell using different metals/solutions. We'll
consider the *Daniell cell*, which is a specific version using zinc and
copper electrodes/solutions.

## Daniell Dell

**Initial setup**

You have two "half" cells. One contains a pure zinc metal rod (called
*electrode*) immersed in a zinc sulfate solution. Solid zinc sulfate
wants to dissolve in water. So you have Zn^+2 "cations" (positively
charged ions) and SO_4^-2 "anions" (negatively charged ions) in this
solution.

The other half contains a pure copper metal rod (a second electrode)
immersed in a copper sulfate solution. Solid copper sulfate wants to
dissolve in water. So you have Cu^+2 cations and SO_4^-2 anions in this
solution.

The pure Zn electrode in the zinc sulfate will not dissolve. There's no
electrical reason for it to do that. Likewise the pure Cu electrode in
the copper sulfate won't dissolve.

**Thermodynamics**

Here is what will drive the reaction:

    Cu^+2 + Zn -> Zn^+2 + Cu

Basically: if there is a deficit of two electrons, copper would prefer
to have the electrons rather than zinc.

TODO: Why? Presumably something about orbitals?

**Thought Experiment: Zinc side**

Let's imagine we *added* an SO_4^-2 anion to the zinc solution. The
solution now has net negative charge. It would like to regain a neutral
charge. Let's imagine that we could magically remove two electrons from
the zinc electrode. Then the zinc electrode could dissolve a Zn^+2
cation to match the anion. Neutral charge in the solution would be
restored.

Of course: where would those two electrons go?

**Thought Experiment: Copper side**

Let's now imagine that we *removed* an SO_4^-2 anion from the copper
sulfate half cell. Now the half cell is positively charged. It would
like to be given two electrons. If we added two electrons at the copper
electrode, it could give those to a Cu^+2 cation, turning it into normal
Cu. The Cu would not want to stay dissolved, so it would *plate out*
onto the copper electrode.

(Presumably the uncharged copper atoms plate rather than just drift away
because they want to be in a metallic lattice structure.)

**Complete Explanation**

The zinc electrode is connected to the copper electrode via a circuit.
Electrons can move between the two. A semi-permeable membrane separates
the half cells; it allows anion movement (but not cation movement).

When an anion migrates from the copper sulfate side to the zinc sulfate
side, two electrons will travel from the zinc electrode to the copper
electrode. The zinc electrode will dissolve (gaining electrons is called
*reduction*), while the copper side will plate out (removing electrons
is called *oxidization*).

The reason this is preferred is because of the thermodynamic equation
above:

    Cu^+2 + Zn -> Zn^+2 + Cu

**Terminology**

The copper electrode that attracts the copper cations and plates them
out is called the *cathode*. The zinc electrode that "attracts" the
anions (by releasing zinc cations) is called the *anode*.

## Salt bridge

**Anion movement is required to sustain the half reactions**

We've seen above that it's important for an SO_4^-2 anion to be able to
travel from the copper sulfate half cell to the zinc sulfate half cell.
If the half cells were kept totally separate, anions could not move. In
that case, as reduction and oxidization proceeded, the half cells would
not maintain electrical neutrality. The buildup of charge would
eventually resist the reduction/oxidization reactions. The energy
released by the reaction would drop (because resisted by charge buildup
in the half cells), eventually dropping to zero.

If we allow anion movement, the charge in the half cells can balance and
the half reactions can continue.

**Mass buildup via anion movement?**

If anions are always moving from the copper sulfate half cell to the
zinc sulfate half cell, then the mass in the copper sulfate half cell
will drop, while the mass in the zinc sulfate half cell will rise.

If anions can freely travel both ways, gravity will want to maintain
equivalent mass in both half cells.

I'm not sure if this is a big deal, but it might be *desirable* if Zn^+2
cations could sometimes migrate to the copper sulfate half cell.
Basically, there could be a balance in SO_4^-2 anions traveling toward
the zinc sulfate half cell with the Zn^+2 cations traveling to the
copper half cell. Either movement should make the zinc sulfate half cell
more negative (and the copper sulfate half cell more positive), thus
driving the half reactions to continue.

The ratio of transfer should be proportional to the masses of the
SO_4^-2 anions and Zn^+2 cations.

**May Cu^+2 cations travel to the zinc sulfate half cell?**

It is *not* desirable that Cu^+2 cations travel to the zinc sulfate half
cell.

If they do, the Cu^+2 cations may directly interact with the Zn
electrode. The uncharged surface Zn will dissolve into Zn^+2 cations as
the Cu^+2 cations plate as uncharged Cu onto the zinc electrode.
Basically: the zinc surface gets replaced with a copper surface.

Note: energy released from the transfer of electrons from Zn to Cu^+2 is
lost as waste heat.

When the plating is complete, there is no ability for oxidization to
happen at *either* electrode. The reason is that the interior Zn no
longer desires to dissolve into Zn^+2, because it cannot be dissolved
into the solution (it's trapped by the copper plating). If it *did*,
then the interior of the copper-plated zinc electrode would simply
become more and more negative. Since the reduction half reaction no
longer is desirable, the corresponding oxidization cannot occur at
either electrode.

Basically: SO_4^-2 anion movement between half cells is okay, and even
Zn^+2 cation movement is okay. But Cu^+2 cation movement from the copper
sulfate half cell to the zinc sulfate half cell is undesirable.

**Deionized Water Bridge?**

Let's imagine that we connected the two half cells with a bridge of
*deionized water*.

You might think: as the anode's uncharged Zn dissolves into Zn^+2, it
can push out Zn^+2 cations into the deionized bridge. H_2O could swap
with the Zn^+2. Likewise, as Cu^+2 cations are plated onto the cathode
as uncharged Cu, SO_4^-2 can be pushed out into the deionized bridge.
Again, H_2O could swap with the SO_4^-2.

This *would* keep the two half cells electrically neutral. But this
*does not* consider the *junction* of the half cells at the deionized
bridge. Basically, it is asking us to build up Zn^+2 on one side of the
deionized bridge, and SO_4^-2 on the other side of the bridge. The
charge buildup will resist the half reactions.

In order to avoid charge buildup at the ends of the deionized bridge, we
would need the Cu^+2 and SO_4^-2 to mix. Only then is there no net
charge anywhere. But to do that, they must migrate really far. That is,
they must move a lot of H_2O molecules out of the way. Those H_2O
molecules have mass, and they don't "get" anything out of moving. It
will require work to move the H_2O, and that is wasted energy.

Moreover, since we need Cu^+2 and SO_4^-2 to be equally distributed
throughout the deionized bridge, we do not truly accomplish our goal of
resisting the progress of the Cu^+2 into the zinc sulfate half cell.
Yes, it is harder for Cu^+2 to get to the zinc sulfate half cell. But it
is *equally* harder to move the Zn^+2 out of the zinc sulfate half cell.

**Salt bridge**

What we need is to add an electrolyte into our deionized bridge. This
will make it a *salt bridge*.

The Daniell cell typically uses a potassium nitrate salt: KNO_3. This
dissolves into K^+1 and NO_3^-1.

Now, when a Zn^+2 cation pushed out of the zinc sulfate half cell, it
can push out a corresponding amount of K^+1. Via a chain of pushing,
that K^+1 will get pushed into the copper sulfate half cell. Here the
K^+1 does work in pushing the oxidization reaction forward.

Likewise, when a SO_4^-2 anion is pushed out of the copper sulfate half
cell, it displaces some NO_3^-1. This pushes a whole chain, pushing
NO_3^-1 into the zinc sulfate half cell. The NO_3^-1 now does work
pushing the reduction reaction forward.

We have achieved something good. There is no buildup of charge at the
junctions. Yes mass (the K^+ and the NO_3^-1 ions) needs to be moved,
but that mass is helping drive the reaction forward. It isn't merely
dead weight, so it consumes less energy.

**Conclusion**

It was a little hard for me to understand the need for the salt bridge,
versus a deionized bridge. These chemistry Stack Exchange threads were
helpful!

Sources:

* https://chemistry.stackexchange.com/questions/93883/salt-bridges-porous-disks
* https://chemistry.stackexchange.com/questions/19540/why-the-salts-in-a-salt-bridge
* https://chemistry.stackexchange.com/questions/5477/why-is-it-important-to-use-a-salt-bridge-in-a-voltaic-cell-can-a-wire-be-used
* https://chemistry.stackexchange.com/questions/97409/what-is-the-purpose-of-the-electrolyte-in-the-half-cell-where-oxidation-is-takin
