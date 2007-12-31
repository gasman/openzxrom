# Makefile: Build script
# From the OpenZXRom project
# Copyright (c) 2005-2007 Matthew Westcott
#
# $Id: Makefile 16 2005-12-18 16:45:24Z gasman $
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 2 as
# published by the Free Software Foundation.
#
# This program is distributed in the hope that it will be useful
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
#
# Author contact information:
#
# E-mail: matthew@west.co.tt
# Postal address: 14 Daisy Hill Drive, Adlington, Chorley,
#                 Lancs, PR6 9NE, United Kingdom

openzxrom.bin: rom.asm restarts.asm startup.asm printing.asm keyboard.asm interpreter.asm cassette.asm clear_screen.asm errors.asm memory.asm expressions.asm functions.asm commands.asm print_string.asm graphics.asm calculator.asm diagnostics.asm splash_screen.asm clairsys.bin
	pasmo rom.asm openzxrom.bin openzxrom.sym

clean:
	rm -f openzxrom.bin openzxrom.sym