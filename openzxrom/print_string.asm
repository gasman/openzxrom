; print_string.asm: print a string to the screen
; From the OpenZXRom project
; Copyright (c) 2005-2007 Matthew Westcott
;
; $Id$
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License version 2 as
; published by the Free Software Foundation.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
;
; Author contact information:
;
; E-mail: matthew@west.co.tt
; Postal address: 14 Daisy Hill Drive, Adlington, Chorley,
;                 Lancs, PR6 9NE, United Kingdom

			fillto 0x2030
print_string_permanent
; print bc characters starting from address de, using permanent attributes
			ld a,(perm_attribute)	; copy permanent attribute variables to temporary ones
			ld (temp_attribute),a
			ld a,(perm_mask)
			ld (temp_mask),a
; 0x203c: print bc characters starting from address de
print_string
			ld a,b
			or c
			dec bc
			ret z
			ld a,(de)
			inc de
			rst putchar
			jr print_string
