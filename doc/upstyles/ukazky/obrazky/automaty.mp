input boxes;

%% pomocna makra
vardef _q (suffix s)(expr t, p) =
  circleit.s (t);
  s.dx = s.dy = 1.8mm;
  s.c = p;
enddef;

vardef _draw_q (text all) =
  forsuffixes s = all:
    drawboxed (s);
  endfor
enddef;

vardef _draw_f (text all) =
  forsuffixes s = all:
    transform tt;
    s.c transformed tt = s.c;
    s.w transformed tt = s.w + (.5mm, 0);
    s.n transformed tt = s.n + (0, -.5mm);
    draw bpath.s transformed tt;
    drawboxed (s);
  endfor
enddef;

vardef _sag (suffix a, b)(expr s) =
  a.c ... (.5 [a.c, b.c] shifted s) ... b.c
enddef;

vardef _draw_d (suffix a, b)(expr p)(suffix pos)(expr t) =
  path pp;
  pp := p cutbefore bpath a cutafter bpath b;
  drawarrow pp;
  label.pos (t, point .5 * length pp of pp);
enddef;

vardef _draw_c (suffix s)(expr angle)(suffix pos)(expr t) =
  transform tt;
  path pp;
  s.c transformed tt = s.c;
  s.w transformed tt = s.w + (-1.75mm, 0);
  s.n transformed tt = s.n + (0, 1.75mm);
  pp := (fullcircle xscaled 5mm yscaled 5mm xscaled -1)
      rotated angle shifted
      ((bpath s transformed tt) intersectionpoint
       (s.c -- (s.c shifted ((20cm, 0) rotated angle))))
      cutbefore bpath s cutafter bpath s;
  drawarrow pp;
  label.pos (t, point .5 * length pp of pp);
enddef;

%% prevod KNA na KDA
beginfig (1)
  u := 0.25mm;
  # := 2.5cm;
  % definice vzajemne polohy stavu (uzlu)
  _q (q0, btex $q_0$ etex, (0, 0));
  _q (q1, btex $q_1$ etex, (0, -#));
  _q (q2, btex $q_2$ etex, (#, -#));
  _q (q3, btex $q_3$ etex, (#, 0));
  % vykresli nekoncove stavy
  _draw_q (q0, q1, q3);
  % vykresli koncove stavy
  _draw_f (q2);
  % definice prechodu (na sebe sama)
  _draw_c (q0, 180, lft, btex $a$ etex);
  _draw_c (q2, 0, rt, btex $a$ etex);
  % definice prechodu (na jine stavu)
  _draw_d (q0, q1, _sag (q0, q1, (-10u, -10u)), lft, btex $a$ etex);
  _draw_d (q0, q3, _sag (q0, q3, (0, 0)), bot, btex $a|b$ etex);
  _draw_d (q1, q0, _sag (q1, q0, (10u, 10u)), lrt, btex $b$ etex);
  _draw_d (q1, q2, _sag (q1, q2, (0, 0)), bot, btex $b$ etex);
  _draw_d (q1, q3, _sag (q1, q3, (0, 0)), lrt, btex $b$ etex);
  _draw_d (q2, q3, _sag (q2, q3, (-10u, -10u)), lft, btex $a$ etex);
  _draw_d (q3, q2, _sag (q3, q2, (10u, 10u)), lrt, btex $a$ etex);
endfig;

bye
