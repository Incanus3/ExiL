%%%  UP Figure Style For METAPOST
%%%  Copyright (C) 2001 Vil\'em Vychodil, <vilem.vychodil@upol.cz>
%%%
%%%  This style is free software; you can redistribute it and/or modify
%%%  it under the terms of the GNU General Public License as published by
%%%  the Free Software Foundation; either version 2 of the License, or
%%%  (at your option) any later version.
%%%
%%%  This style is distributed in the hope that it will be useful,
%%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%  GNU General Public License for more details.
%%%
%%%  You should have received a copy of the GNU General Public License
%%%  along with this file; if not, write to the Free Software
%%%  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

%% line dashes
picture dotted_line, dashed_line, solid_line;
dotted_line := dashpattern (on 0.5mm off 0.5mm);
dashed_line := dashpattern (on 4mm off 1mm);
solid_line := dashpattern (on 1mm);

%% pen thickness
pen bold_pen, hair_pen;
bold_pen := pencircle scaled 0.5mm;
hair_pen := currentpen;

%% colors
color gray_color;
gray_color := .9white;

%% create a transformation
def _mk_transform =
  identity xscaled (1 / (bx - ax)) yscaled (1 / (by - ay))
           shifted (- ax, - ay)
           xscaled (dx) yscaled (dy)
enddef;

%% draw axis
def _draw_axis (expr fx, lx, fy, ly, t) =
  numeric lap;
  lap := 2mm;
  
  draw (((fx, 0) transformed t) shifted (- lap, 0)) ---
       (((lx, 0) transformed t) shifted (lap, 0));
  draw (((0, fy) transformed t) shifted (0, - lap)) ---
       (((0, ly) transformed t) shifted (0, lap));
enddef;

%% draw arrow axis
def _draw_arrow_axis (expr fx, lx, fy, ly, t) =
  numeric lap;
  lap := 2mm;
  
  drawarrow (((fx, 0) transformed t) shifted (- lap, 0)) ---
            (((lx, 0) transformed t) shifted (lap, 0));
  drawarrow (((0, fy) transformed t) shifted (0, - lap)) ---
            (((0, ly) transformed t) shifted (0, lap));
enddef;

%% draw label for x-axis
def _x_label (expr txt, w, t) =
  numeric lap;
  lap := 1mm;
  
  draw ((w, 0) transformed t shifted (0, -lap)) ---
       ((w, 0) transformed t shifted (0, lap));
  label.bot (txt, (w, 0) transformed t shifted (0, - lap));
enddef;

%% draw label for y-axis
def _y_label (expr txt, w, t) =
  numeric lap;
  lap := 1mm;
  
  draw ((0, w) transformed t shifted (-lap, 0)) ---
       ((0, w) transformed t shifted (lap, 0));
  label.lft (txt, (0, w) transformed t shifted (- lap, 0));
enddef;
