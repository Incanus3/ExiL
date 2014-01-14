\beginfig (1)
input upfigure.mp

dx:= 5cm ;dy:= 5cm;
ax:= -0.2;bx:= 4;
ay:= -0.2;by:= 4;

transform t;
t:= _mk_transform;
_draw_arrow_axis (ax,bx,ay,.9by,t);

path p;p:=(0.2,0.5){dir65}..(3.8,3.0);
draw p transformed t;

path pc;pc:=(0.2,1.0)--(3.8,3.2);
draw pc transformed t;
pair prusecikp;prusecikp:= p intersectionpoint pc;
path pd;pd:= pc rotatedaround(prusecikp,19);
path pdd;pdd=subpath(0,0.75) of pd;
draw pdd transformed t;

path cp;cp:=(-0.7,0)--(3,0);
path cpp;cpp:= cp shifted(prusecikp);
draw cpp transformed t dashed dotted_line;

path dp;dp:=(0,-1.3)--(0,0);
path dpp;dpp:= dp shifted(prusecikp);
draw dpp transformed t dashed dotted_line;

path q;q:=subpath(0.5,1) of p;
pair prusecikd;prusecikd:= q intersectionpoint pc;
path ep;ep:=(0,-2.9)--(0,0);
path epp;epp:= ep shifted(prusecikd);
draw epp transformed t dashed dotted_line;

path kr;kr=fullcircle scaled 1.8;
kr:= kr shifted(prusecikp);
path krv;krv:= subpath(0,0.7) of kr;
draw krv transformed t;

label.rt (btex $y=f(x)$ etex,(point length (p) of p) transformed t);
label.rt (btex $\rho$ etex,(point length (pc) of pc) transformed t);
label.rt (btex $\theta$ etex,(point length (pdd) of pdd) transformed t);
label.rt (btex $\alpha$ etex,(1.1,1.45)  transformed t);
label.rt (btex $\longleftarrow$ etex,(1.8,-0.2)  transformed t);
label.rt (btex $\underbrace{\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ }$ etex,(0.55,1.2) transformed t);
label.rt (btex $x-x_0$ etex,(1.6,1)  transformed t);
label.rt (btex $\underbrace{\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ }$ etex rotated(90),(3.275,2.1) transformed t);
label.rt (btex $f(x)-f(x_0)$ etex,(3.6,2.1)  transformed t);

_x_label(btex $x_0$ etex,0.685,t);
_x_label(btex $x$ etex,3.385,t);
_y_label(btex $f(x_0)$ etex,1.275,t);

\endfig;
bye
