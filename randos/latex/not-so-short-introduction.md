## Chapter 2: Typesetting Text

* `\include` command.
* `\label` (which names a place), `\ref`, `pageref` all allow you to
  connect between parts of a PDF.
* Use `\emph` vs `\textit`, since then you can nest.

```
\begin{enumerate}
  \item This gives.
  \item You numbered bullets.
\end{enumerate}

\begin{itemize}
  \item This is.
  \item An unordered.
  \item List.
\end{itemize}
```

```
\begin{quote}
  This is a quotation.
\end{quote}

\begin{quotation}
  This is better for a multiparagraph quotation.

  It will indent the start of paragraphs per normal.
\end{quotation}

\begin{verbatim}
  10 THIS IS GOOD
  20 FOR CODE.
\end{verbatim}
```

They show the `tabular` environment which builds tables:

```
% r, l stand for right-aligned, left-aligned, and | means vertical line
\begin{tabular}{|r|l|}
  % Top horizontal line.
  \hline

  top-left & top-right \\

  % Next row
  \hline

  mid-left1 & mid-right \\
  \cline{2-2} % puts a line just under col #2.
  mid-left2 & lower-right \\

  \hline\hline
  This % is a second table \\
  \hline
\end{tabular}
```

You can do all sorts of craziness:

```
% This uses a trick. First, @{.} lets you choose a separator of your
% choice. In this case, the period.
%
% Here we typeset a table of decimals, each with the . aligned.
\begin{tabular}{r @{.} l}
  % You can use \multicolumn to span two columns.
  \multicolumn{2}{c}{Value} \\
  \hline
  123&456 \\
   12&12 \\
    1&9 \\
\end{tabular}
```

They mention that you can look at the `booktabs` library, which makes
designing tables easier.

They mention the `graphicx` library, which is used for inserting photos.

They mention the `figure` environment, which lets you float a figure
(like a photo, or table). You'll probably want to make a label/reference
to it so that the reader knows what you're referring to. You can also
provide a caption.

## Ch3: Typesetting Mathematical Formulae

"Text style" math is delimited by "$". "Display style" goes in a
`\begin{equation}` block.

Equations will be numbered unless you either (1) use `\begin{equation*}`
or (2) use the convenience synonym `\[ xyz \]`.

You can specify the label, if desired, by using the `\tag` command.
Otherwise it is just a number. You can "capture" and name the equation
tag with the `\label` command (that is usual). You can reference it
later with `\eqref`.

```
\begin{equation}
  x = 1 + 2 + 3 \label{mycoollabel}
\end{equation}

This refers to equation \eqref{mycoolllabel}.
```

Text style math can sometimes cause you to have more vertical spacing
than desired in lines. You can use the `\smash{$x^y^z^a$}` command if
desired. That will ignore the height of what is inside the smash.

In math, spaces are generally ignored: you provide them with `\ , \quad,
\qquad`. There are no empty lines. Letters are assumed to be variables;
you need to use `\text{description}` if you want normal text in a
formula.

They list common symbols:

* Greek letters.
* Superscripts and subscripts.
* `\cdot`, `\cdots`.
* `\hat{x}, \widehat{XY}, \bar{x}` all put various kinds of hats.
* Common functions:
  * Trigonometric ones.
  * `\exp, \log`
  * `\inf, \sup, \lim, \liminf, \limsup`, `\max, \min`
  * `\det, \dim`
  * `\Pr`
  * You can write your own with `\DeclareMathOperator`.
* `\frac`. And of course they teach you `\partial` the normal way. Sigh.
* `\sum, \prod, \int`.

```
\begin{equation*}
  \sum_{
    \substack{i,j \\ i\ne j}
  } i^2 j^2
\end{equation*}
```

Delimiters of course have `\left` and `\right` versions. But of course
you can specify size manually, too. You can go all the way to `\bigg,
\Bigg`.

### 3.4 Single Equations That Are Too Long

The simplest way is to use `multline`:

```
\begin{multline}
  x + y + z
  \\
  = a + b + c
\end{multline}
```

But this doesn't give the possibility of alignment. (Comes from amsmath
I think).

You can use `split` for a *single* equation with alignment. It will only
get one equation number. (BTW, you use `split` inside an `equation`
environment.)

### 3.5 Multiple Equations

The `array` environment corresponds to the `tabular` environment. It
just has a default of math mode (and maybe a little formatting). You
have to specify columns and alignment. I don't believe anyone gets an
equation number. I think it's actually really meant for use *inside* an
equation... I think it's like `split` but more powerful.

(Source: https://tex.stackexchange.com/questions/204838/difference-between-tabular-and-array-environment)

You can use amsmath's `gather` environment for a series of *unaligned*,
centered equations.

Okay, but what about aligned series of equations?

The `eqnarray` mode is *not* suggested. Everyone says don't use it (for
instance, has weird spacing around binary operators). Even the Latex
manual says not to! Basically, `eqnarray` just does a three-column
`array` setup. You flank the `=` sign with ampersands. But the spacing
is inconsistent.

The next option is to use `align`, from `amsmath`.

```
\begin{align}
  x &= b + c \\
    &= d + e
\end{align}
```

I believe that `align` will let you do equations side-by-side (though
each line gets only one equation number). Like so:

```
\begin{align}
  x &= b + c  &  y &= abc
    &= d + e  &  z &= 123

  right &= left & right &= left
\end{align}
```

But I think `align` really assumes (a) one equal sign to align per
column, (b) every even-indexed ampersand means a new column for
equations. I think you are required to give an odd number of ampersands?

Note: there are versions `gathered` and `aligned` that don't by default
take up the whole line. They are only as wide as the content, and are
meant to be used *inside* an equation environment.

### 3.5.2: IEEEeqnarray

They really like `IEEEeqnarray`. This lets you explicitly specify the
number of columns. You can also use `\IEEEeqnarraymulticol` to span
multiple columns, if desired.

I think that `IEEEeqnarray` may be preferred to `array` because you get
equation numbering.

### 3.6: Arrays/Matrices

The `array` environment can be used to write piecewise functions:

```
\begin{equation*}
  |x| = \left\{
    \begin{array}{rl}
      -x & \text{if } x < 0,\\
      0 & \text{if } x = 0,\\
      x & \text{if } x > 0.
    \end{array} \right.
\end{equation*}
```

But `amsmath` will do the work for you with the `\cases` command.

```
\begin{equation*}
  |x| =
  \begin{cases}
    -x & \text{if } x < 0,\\
    0 & \text{if } x = 0,\\
    x & \text{if } x > 0.
  \end{cases}
\end{equation*}
```

You *can* use `array` for matrices:

```
\begin{equation*}
  \mathbf{X} = \left(
    \begin{array}{ccc}
      x_1 & x_2 & \ldots \\
      x_3 & x_4 & \ldots \\
      \vdots & \vdots & \ddots
    \end{array} \right)
\end{equation*}
```

But again, `amsmath` has you with `\bmatrix`:

```
\begin{equation*}
  \begin{bmatrix}
    p_{11} & p_{12} & \ldots
    & p_{1n} \\
    p_{21} & p_{22} & \ldots
    & p_{2n} \\
    \vdots & \vdots & \ddots
    & \vdots \\
    p_{m1} & p_{m2} & \ldots
    & p_{mn}
  \end{bmatrix}
\end{equation*}
```

### Equation Environments Conclusions

* For a single equation `\begin{align}` is fine.
  * `\begin{equation}` is the most basic environment.
* For multiple equations, prefer `IEEEeqnarray`.
  * I think it's just more flexible than `array`?
  * And gives you multiple equation numbers.
* Use either `aligned` or `array` for complex stuff inside an equation.

## 3.7 Spacing in Math Mode

* In order of increasing space:
  * `\,`, `\:`, `\;`, `\ `, `\quad`, `\qquad`.
* Negative space: `\!`.
* They mention an example for `\,`: typesetting the dx for an integral:
  * `\int x^2 \,\mathrm{d}x`. `mathrm` means "math Roman."
* If you do a double integral, `\int\int` has an awkward space.
  * You could use `\int \!\!\! \int` to move it together.
  * Or more simply, use `\iint` from amsmath.

## 3.8 Fonts

* `\mathcal \mathfrak \mathbb` are your basic math fonts.
* In (math) text style, sums and limits are written small-style. This
  also happens in `\frac`. You can force by using `\displaymode` to get
  the big sigmas.
* For bold symbols, there is `\mathbf`, but it (a) doesn't work on greek
  letters and (b) makes them roman (vs italic).
  * For that reason, you probably want `\boldsymbol` from `amsbsy`
    (included by `amsmath`).

## 3.9 Theorems

```
% PREAMBLE

% `theoremstyle` is from amsthm. plain means roman naming, italic body.
% `newtheorem` defines an *environment*. Here we define an environment
% called `thm`. It will print the title `Theorem` at each use.

\theoremstyle{plain} \newtheorem{thm}{Theorem}

% This defines a lemma environment, but gives it the same counter as
% theorems.

\theoremstyle{plain} \newtheorem{lemma}[thm]{Lemma}

% Another example. `remark` theorem style means italic title, roman
% body.
%
% Environment is named `remark`, and text label is "Remark"

\theoremstyle{remark} \newtheorem{remark}{Remark}

% BODY

\begin{thm}
  This is my cool theorem.
\end{thm}

\begin{remark}
  I can make a remark here.
\end{remark}

\begin{thm}{Ned's Theorem}
  I can even name a theorem!
\end{thm}
```

## Ch4: Specialties

TODO

## Ch5: Producing Mathematical Graphics

TODO

## Ch6: Customizing Latex

Of course, you can define new commands:
`\newcommand{cmdname}[argcount]{definition}`.

But you can also define new environments:

```
\newenvironment{nthm}[1]{
  The name of this this theorem is #1.
}{
  The end of the theorem is here.
}

\begin{nthm}{Cool Theorem}
  Here is the theorem body.
\end{nthm}
```

There is a command called `\ifthenelse`. How helpful!

You should extract your macros to a `.sty` file, which can be loaded by
`usepackage`. In the `.sty` file just have the macro definitions, but
first write `\ProvidesPackage`. I believe if the `.sty` is in the same
folder as the `.tex` file, you can load by name. Else you can give a
path.

Fonts:

* `\textrm` (roman) `\texttt` (typewriter font)
* `\textmd` (medium weight) `\textbf` (bold)
* `\textup` (upright) `\textit` (italic) `\textsc` (small caps)
* `\emph`
* There are named font sizes (like `\small, \normalsize, \large`).

## 6.3: Spacing

* Use `\hspace{5cm}` to give an explicit blank horizontal space.
* Use `\hspace{\stretch}` to stretch to fill available space.
  * Use `x\hspace{\stretch{1}}x\hspace{3}x\stretch{5}x` to stretch in
    proportions of `1:3:5`.

## TODO

* Done! Except glance through chapters 4 and 5.
