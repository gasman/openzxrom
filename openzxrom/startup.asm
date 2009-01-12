; startup.asm: actions to perform on system startup
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

cmd_new
; handle NEW command
	call assert_eos	; assert that an end-of-statement follows the NEW token; die if not
	jr warm_start	; if OK, perform a warm start

cold_start
; startup operations
		; TODO: detecting 16K models
		; copy bitmaps A-U to UDG memory
	ld hl,font-0x0100+('A'*8)	; hl = start of 'A' bitmap
	ld de,udg_mem-1
	ld (ramtop),de	; set ramtop to base of UDG memory - 1
	inc de
	ld (udg_ptr),de	; set UDG pointer
	ld bc,0x00a8	; copy 0x00a8 bytes of bitmaps
	ldir	; perform the copy
warm_start
	ld iy,sysvars_base	; point IY here to allow indirect access to system vars
	ld hl,(ramtop)
	ld sp,hl
	ld hl,0
	xor a
	ld (rand_seed),hl	; reset random seed to 0
	ld (frames),hl	; reset frames counter to 0
	ld (frames+2),a
	ei	; enable interrupts, as SP is somewhere safe now
	ld (next_char_type),a	; next character is not a parameter to a control code
	ld (perm_mask),a	; attributes are fully opaque
	ld hl,font - 0x0100	; point to font bitmap
	ld (font_ptr),hl
	ld hl,membot	; set pointer to calculator memory area
	ld (mem),hl
	ld hl,prog_mem	; designate all memory from prog_mem
	ld (vars_addr),hl	; as spare (i.e. set BASIC length to zero)
	ld (workspace),hl
	ld (calc_stack),hl
	ld (stkend),hl
	ld a,0x38	; set attributes to black on white
	ld (perm_attribute),a
		; TODO: establish exactly where we should copy perm_attribute to
		; temp_attribute - currently doing this in clear_screen
	ld a,7
	call set_border	; border white
	call clear_screen
	ld de,splash_text	; draw splash screen
	ld bc,splash_text_end - splash_text
	call print_string_permanent

		; wait for enter to be pressed
	ld bc,0xbffe
wait_enter
	in a,(c)
	and 1
	jr nz,wait_enter

		; display 'loading' message
	call clear_screen
	ld de,loading_text
	ld bc,loading_text_end - loading_text
	call print_string

		; set up 'expected tape header' buffer for LOAD ""
	ld hl,(calc_stack)	; allocate 0x0022 bytes for actual / expected headers
	ex de,hl
	ld bc,0x0022
	call alloc_space
	push de	; store buffer address to be popped at start of load_basic routine
	push de	; transfer buffer address to ix
	pop ix
	ld (ix+0x12),0xff	; set first byte of expected filename to 0xff, to indicate 'any filename'
	jp load_basic	; jump into load_basic

loading_text
	db "Loading..."
loading_text_end
