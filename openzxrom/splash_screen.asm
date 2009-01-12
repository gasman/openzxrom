; splash_screen.asm: Initial splash screen text
; From the Open82 project
; Copyright (c) 2009 Matthew Westcott
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

	db 0x11, 0x06	; paper yellow
	db "+------------------------------+"
	db "| Open82 v2009-xx-xx           |"
	db "| ", 0x7f, " 2009 Open82 Project        |"
	db "+------------------------------+"
	db 0x11, 0x07, 0x10, 0x01	; paper white, ink blue
	db 0x0d	; new line
	db "Open a snapshot, or press enter "
	db "to load from a tape image"
