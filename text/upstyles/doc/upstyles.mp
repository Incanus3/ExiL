input boxes;

verbatimtex
\def\obsah#1{\bgroup%
  \font\rm csr10 \rm%
  \vbox{\halign{\hfil#1\cr}}%
  \egroup}%
etex

beginfig (1)
  # := 1cm;
  vardef _q (suffix s)(expr p, t) =
    boxit.s (t);
    s.dx = s.dy = 2mm;
    s.c = p;
  enddef;
  
  vardef _c (suffix s)(expr p, t) =
    circleit.s (t);
    s.dx = s.dy = 2mm;
    s.c = p;
  enddef;

  vardef _sag (suffix a, b)(expr s) =
    a.c ... (.5 [a.c, b.c] shifted s) ... b.c
  enddef;

  vardef _d (suffix a, b)(expr p) =
    path pp;
    pp := p cutbefore bpath a cutafter bpath b;
    drawarrow pp;
  enddef;

  _q (tex, (2#, 0), btex {\font\rm csr10 \rm \TeX} etex);
  _q (dri, (6.5#, 0), btex \obsah{#\hfil\cr výstupní\cr ovladač} etex);
  _q (mf, (4.25#, 3#), btex {\font\logo logo10 \logo METAFONT} etex);

  _c (in, (0, .75#), btex \obsah{#\hfil\cr vstupní\cr soubor} etex);
  _c (out, (9#, 0), btex \obsah{#\hfil\cr výstupní\cr soubor} etex);
  _c (fmt, (0, -.75#), btex {\font\rm csr10 \rm formát} etex);
  _c (cls, (-#, -2#), btex {\font\rm csr10 \rm styly} etex);
  _c (sty, (#, -2#), btex \obsah{#\hfil\cr dodatečné\cr styly} etex);
  _c (tfm, (2#, 2#), btex \obsah{#\hfil\cr metrika\cr fontů} etex);
  _c (pk, (6.5#, 2#), btex \obsah{#\hfil\cr tvary\cr znaků} etex);
  _c (dvi, (4#, 0), btex \obsah{#\hfil\cr soubor\cr {\tt dvi}} etex);
  _c (pi, (6.5#, -2#), btex \obsah{#\hfil\cr dodatečná\cr data} etex);

  drawboxed (tex, dri, mf);
  drawboxed (in, out, fmt, cls, sty, tfm, pk, dvi, pi);
  _d (in, tex, _sag (in, tex, (0, 0)));
  _d (fmt, tex, _sag (fmt, tex, (0, 0)));
  _d (tex, dvi, _sag (tex, dvi, (0, 0)));
  _d (cls, fmt, _sag (cls, fmt, (0, 0)));
  _d (sty, fmt, _sag (sty, fmt, (0, 0)));
  _d (dvi, dri, _sag (dvi, dri, (0, 0)));
  _d (dri, out, _sag (dri, out, (0, 0)));
  _d (mf, tfm, _sag (mf, tfm, (0, 0)));
  _d (mf, pk, _sag (mf, pk, (0, 0)));
  _d (tfm, tex, _sag (tfm, tex, (0, 0)));
  _d (pk, dri, _sag (pk, dri, (0, 0)));
  _d (pi, dri, _sag (pi, dri, (0, 0)));
endfig;

beginfig (2)
  \input upfigure.mp
  dx := 4cm; dy := 3cm;
  ax := 0; bx := 2;
  ay := 0; by := 2;

  transform t;
  t := _mk_transform;

  _draw_arrow_axis (ax, bx, -0.2by, by, t);

  path p, q;
  p := (0.2, -0.5){dir 70} ... {dir 20}(1.8, 1.8);
  q := (0.2, 1.8){dir -70} ... {dir -10}(1.8, 0.2);

  draw p transformed t;
  draw q transformed t;

  z1 = p intersectionpoint q;
  draw (z1 -- (x1, 0)) transformed t dashed dotted_line;

  pair pl, ql;
  pl := point length (p) of p;
  ql := point length (q) of q;
  label.rt (btex $y=f_1(x)$ etex, pl transformed t);
  label.rt (btex $y=f_2(x)$ etex, ql transformed t);
  _x_label (btex $\xi$ etex, x1, t);
endfig;
bye
