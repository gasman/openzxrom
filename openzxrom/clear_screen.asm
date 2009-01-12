; clear_screen.asm: clear the screen
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

	fillto 0x0d6b
	jr clear_screen	; Another entry point to clear_screen;
		; this one would normally perform additional
		; housekeeping tasks which we don't bother
		; with here yet

cmd_cls
	call assert_eos	; assert that an end-of-statement follows the CLS token; die if not
	jr clear_screen

	fillto 0x0daf
clear_screen
	ld hl,screen	; clear screen
	ld (cursor_addr),hl	; set cursor to start of screen
	ld de,screen + 1
	ld bc,attributes - screen
	ld (hl),l
	ldir
	ld a,(perm_attribute)	; get permanent attribute value
	ld (hl),a	; and clear screen to those colours
	ld bc,attributes_end - attributes - 1
	ldir
	ret
