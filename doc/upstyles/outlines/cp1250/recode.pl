#!/usr/bin/perl

open (F, $ARGV [0]);
open (G, $ARGV [1]);
while (<F>) {
    chomp;
    $s = $_;
    $d = <G>;
    chomp $d;
    $p [ord ($s)] = ord ($d);
}
close (G);
close (F);

print <<'EOF';

/*  Magic Outlines --- Recoding Header File
 *  Copyright (C) 2001  Vilem Vychodil, <vilem.vychodil@upol.cz>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

#ifndef __MOL_ENC__
#define __MOL_ENC__

EOF
print "unsigned char recode [] = {\n     ";
$n = 0;
for (0 .. 255) {
    $p [$_] = (! $p [$_]) ? sprintf ('%i', $_) : $p [$_];
    printf ('0x%.2x', $p [$_]);
    $n = ($n + 1) % 12;
    print ((!$n) ?
	   (($_ == 255) ? "" : ",\n     ") : 
	   (($_ == 255) ? "" : ", "));
}
print "};\n\n#endif\n";
