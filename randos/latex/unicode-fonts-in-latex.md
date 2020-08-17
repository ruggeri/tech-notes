You need to do something like this to have unicode fonts in LaTeX:

```
\usepackage{fontspec}
\usepackage{unicode-math}
\setmathfont[
  Path = /Library/TeX/Root/texmf-dist/fonts/opentype/public/firamath/,
  Extension = .otf,
  ]{FiraMath-Regular}
```

I think `fontspec` gives you the `setmainfont` and `setmathfont`
macros. You then have to give it a path to the font directory. You
give the extension type, and the name of the font.

I think `unicode-math` in theory allows you to use unicode characters
in math mode. But I think that `FiraMath` doesn't actually support
unicode characters? Not sure.

I think you need to build with `xelatex` else `latex` will freak out
when it sees unicode characters.

That's all I know for now.
