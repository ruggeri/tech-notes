**Notes from the User’s Guide for the amsmath Package**

## Section 3: Displayed Equations

* `equation` is for a single equation.
    * `gather` just groups multiple equations. No alignment.
* `multline`: single equation of multiple lines, first is left aligned,
  last is right aligned. Middle lines (if any) are centered.
    * Not alignment operator.
* `split` is meant to be used *inside* another math environment.
    * Supports only one alignment column.
    * Else, you can try `aligned`.
* `align` lets you have multiple columns of equations.
    * Each column only has one `&`.
    * `aligned` is like `split`. It can be used inside. (But more
      flexible. `split` expects to be the *only* thing in a block.)
    * `aligned` could be used for piecewise functions, but the `cases`
      environment is a little easier and preferred. (it'll put the curly
      brace in for you).

Sometimes you want just one line of text in a series of equations. When
you want this, you can use `\intertext` or `\shortinterext`. It adds
left aligned text.

## Section 4: Miscellaneous

* `\pmatrix`, but also `\psmallmatrix` which is smaller and good for
  inside text.
* `\overset` and `\underset` let you put symbols below/above a symbol.
  `\stackrel` is the Latex command that does this, but only for binary
  relations.
* They recommend `\bigl, \Bigl, \biggl, \Biggl` to specify sizing of
  nested parentheses.
* Again, they mention `\DeclareMathOperator` for defining things like
  `\sin`.
* They mention that `\text` should be used for text in a display, but
  `\mathrm` should be used for Roman stuff like the `d` in `dx`. That's
  semantic.
* There's stuff about `\sideset` and `\limits, \nolimits,
  \displaylimits`, but I don't exactly understand...

## Conclusion

Not a ton of new stuff here. Good resource though. Generally well
covered from Not So Short Introduction.
