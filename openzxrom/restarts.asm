; restarts.asm: Routines to be called by the Z80's restart and interrupt features
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

			org 0x0000
; RST 0x0000: here's where it all begins
			di
			im 1
			ei
			jr cold_start
			
			fillto 0x0008
; RST 0x0008: output error message
error
			pop hl							; Fetch code from the address immediately
			ld a,(hl)						; following the call to this routine
			jp print_err					; NB We throw away the return address
			
			fillto 0x0010
; RST 0x0010: Output the character specified in A
putchar		jp putchar_main

			fillto 0x0018
; RST 0x0018: return next char to be interpreted
nextchar	ld hl,(interp_ptr)
			ld a,(hl)
			ret
			
			fillto 0x0020
; RST 0x0020: consume next character of program
; (i.e. advance to next non-ignorable character; ignorable = space and control codes)
consume		jp consume_main
			
			fillto 0x0028
; RST 0x0028: enter calculator mode
calc			pop hl
			jp calc_main

			fillto 0x0030
; Arrive here if we execute any undefined area of the ROM
fatal_error
			di
			jp fatal_error_main
			
			fillto 0x0038
; RST 0x0038: IM 1 interrupt service routine
			push af
			push hl
			ld hl,frames
			inc (hl)						; increment frames system variable,
			jr nz,done_frames				; overflowing into second and third
			inc l							; byte as necessary
			inc (hl)
			jr nz,done_frames
			inc l
			inc (hl)
done_frames
			pop hl
			pop af
			ei
			ret

fatal_error_main
fatal_error_lp
			ld a,6
			out (254),a
			ld a,3
			out (254),a
			jr fatal_error_lp
