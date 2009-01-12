# Makefile: Build script
# From the Open82 project
# Copyright (c) 2000 Matthew Westcott
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

# =====================
# Configuration options - override these in a file named config.mk if required

# Path to the Fuse <http://fuse-emulator.sourceforge.net/> executable, used by
# 'make fuse' to start up the emulator with the ROM image installed
FUSE=fuse

# Font to incorporate into the ROM. Valid options are ZX81ACE and CLAIRSYS
FONT=ZX81ACE

# End configuration options
# =========================

-include config.mk

.PHONY: fuse clean

ifeq ($(FONT),CLAIRSYS)
  FONT_FILE=fonts/clairsys.bin
else
  FONT_FILE=fonts/zx81_ace.asm
endif

open82.bin: rom.asm restarts.asm startup.asm printing.asm keyboard.asm interpreter.asm cassette.asm clear_screen.asm errors.asm memory.asm expressions.asm functions.asm commands.asm print_string.asm graphics.asm calculator.asm diagnostics.asm splash_screen.asm $(FONT_FILE)
	pasmo --equ FONT_$(FONT) rom.asm open82.bin open82.sym

# build the ROM and run it in Fuse
fuse: open82.bin
	$(FUSE) --rom-48 open82.bin --machine 48 --no-traps

clean:
	rm -f open82.bin open82.sym

