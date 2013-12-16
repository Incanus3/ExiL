%%%  UP Logo
%%%  Copyright (C) 2001 Vil\'em Vychodil, <vilem.vychodil@upol.cz>
%%%
%%%  This file is free software; you can redistribute it and/or modify
%%%  it under the terms of the GNU General Public License as published by
%%%  the Free Software Foundation; either version 2 of the License, or
%%%  (at your option) any later version.
%%%
%%%  This file is distributed in the hope that it will be useful,
%%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%  GNU General Public License for more details.
%%%
%%%  You should have received a copy of the GNU General Public License
%%%  along with this file; if not, write to the Free Software
%%%  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

prologues := 1;
beginfig (1)
  path p[];
  picture pic;
  
  p1 = (142.06, 728.14)
       .. controls (147.17, 706.25) and (156.17, 689.83) .. (170.64, 671.76)
       .. controls (185.33, 687.67) and (196.85, 714.24) .. (202.68, 740.16)
       .. controls (177.62, 733.61) and (165.24, 733.75) .. (151.20, 737.50)
       .. controls (150.05, 732.53) and (147.60, 729.72) .. cycle;

  p2 = (144.79, 726.34)
       .. controls (149.40, 706.82) and (157.54, 692.21) .. (170.64, 676.08)
       .. controls (183.96, 690.26) and (194.33, 713.95) .. (199.58, 737.06)
       .. controls (176.98, 731.23) and (165.74, 731.38) .. (153.07, 734.69)
       .. controls (151.99, 730.30) and (149.83, 727.78) .. cycle;

  p3 = (181.01, 714.38)
       --- (187.13, 714.38)
       .. controls (185.26, 713.38) and (184.97, 714.38) .. (183.60, 710.64)
       --- (181.15, 696.74)
       .. controls (178.70, 685.44) and (162.43, 685.44) .. (160.20, 696.74)
       --- (157.75, 710.06)
       .. controls (156.96, 712.94) and (156.02, 713.52) .. (154.51, 714.38)
       --- (160.13, 714.38)
       --- (163.37, 696.96)
       .. controls (164.88, 689.18) and (176.40, 689.18) .. (178.06, 696.96)
       --- cycle;

  p4 = (166.46, 727.42)
       --- (154.58, 726.98)
       --- (155.74, 729.58)
       --- (171.72, 729.58)
       .. controls (175.68, 729.58) and (178.20, 726.84) .. (179.06, 725.47)
       .. controls (181.15, 721.22) and (180.22, 714.31) .. (178.70, 711.50)
       .. controls (177.05, 707.83) and (172.94, 706.90) .. (169.78, 707.33)
       --- (169.70, 690.70)
       --- (166.46, 690.91)
       --- cycle;

  p5 = (169.78, 727.42)
       --- (169.78, 710.06)
       .. controls (173.66, 709.70) and (176.83, 710.64) .. (177.05, 718.92)
       .. controls (177.34, 727.34) and (173.52, 727.63) .. cycle;

  linejoin := mitered;       
       
  fill p1 withcolor black;
  draw p2 withpen pencircle scaled .25mm withcolor white;
  fill p3 withcolor white;
  fill p4 withcolor white;
  fill p5 withcolor black;

  pic := currentpicture;
  currentpicture := nullpicture;
  addto currentpicture also pic;
endfig;

beginfig (2)
  path p[];
  picture pic;
  
  p1 = (142.06, 728.14)
       .. controls (147.17, 706.25) and (156.17, 689.83) .. (170.64, 671.76)
       .. controls (185.33, 687.67) and (196.85, 714.24) .. (202.68, 740.16)
       .. controls (177.62, 733.61) and (165.24, 733.75) .. (151.20, 737.50)
       .. controls (150.05, 732.53) and (147.60, 729.72) .. cycle;

  p2 = (144.79, 726.34)
       .. controls (149.40, 706.82) and (157.54, 692.21) .. (170.64, 676.08)
       .. controls (183.96, 690.26) and (194.33, 713.95) .. (199.58, 737.06)
       .. controls (176.98, 731.23) and (165.74, 731.38) .. (153.07, 734.69)
       .. controls (151.99, 730.30) and (149.83, 727.78) .. cycle;

  p3 = (181.01, 714.38)
       --- (187.13, 714.38)
       .. controls (185.26, 713.38) and (184.97, 714.38) .. (183.60, 710.64)
       --- (181.15, 696.74)
       .. controls (178.70, 685.44) and (162.43, 685.44) .. (160.20, 696.74)
       --- (157.75, 710.06)
       .. controls (156.96, 712.94) and (156.02, 713.52) .. (154.51, 714.38)
       --- (160.13, 714.38)
       --- (163.37, 696.96)
       .. controls (164.88, 689.18) and (176.40, 689.18) .. (178.06, 696.96)
       --- cycle;

  p4 = (166.46, 727.42)
       --- (154.58, 726.98)
       --- (155.74, 729.58)
       --- (171.72, 729.58)
       .. controls (175.68, 729.58) and (178.20, 726.84) .. (179.06, 725.47)
       .. controls (181.15, 721.22) and (180.22, 714.31) .. (178.70, 711.50)
       .. controls (177.05, 707.83) and (172.94, 706.90) .. (169.78, 707.33)
       --- (169.70, 690.70)
       --- (166.46, 690.91)
       --- cycle;

  p5 = (169.78, 727.42)
       --- (169.78, 710.06)
       .. controls (173.66, 709.70) and (176.83, 710.64) .. (177.05, 718.92)
       .. controls (177.34, 727.34) and (173.52, 727.63) .. cycle;

  linejoin := mitered;       
       
  draw p2 withpen pencircle scaled .25mm withcolor black;
  fill p3 withcolor black;
  fill p4 withcolor black;
  fill p5 withcolor white;

  pic := currentpicture;
  currentpicture := nullpicture;
  addto currentpicture also pic;
endfig;

bye
