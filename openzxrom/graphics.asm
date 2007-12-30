; graphics.asm: High-resolution graphics routines
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

			fillto 0x2298
cmd_border
; process BORDER command
			call get_colour_arg
			; entry point set_border at 0x229b - used by various Jonathan Cauldwell games
			; (Egghead 4, Lunaris, Fantastic Mr Fruity)
set_border
			out (0xfe),a			; change the border colour
			add a,a						; shift left to make it the PAPER component of an attribute
			add a,a
			add a,a
				; TODO: fill INK with a contrasting colour (could be tricky in the two remaining bytes...)
			ld (border_colour),a
			ret
			
			fillto 0x22a5
pixel_addr
; Find screen address and byte position for the given pixel coordinates.
; Enter with B = y coord (in BASIC coordinate system, i.e. 175 is top row), C = x coord.
; Return with HL = address, A = position within byte (a number 0..7, where 0 = leftmost (highest) bit).
; Throws 'out of range' if B > 175.
			ld a,175
			sub b	; flip coordinate system so that 0,0 is 175 pixels down
			jp c,err_out_of_range	; trigger error if b > 175
			; now c = x coord, a = y coord
			; Want to end up with HL = 0 1 0 y7 y6 y2 y1 y0  y5 y4 y3 x7 x6 x5 x4 x3
			rra
			rr d
			rra
			rr d
			rra
			rr d	; now a = ? ? ? y7 y6 y5 y4 y3 , d = y2 y1 y0 ? ? ? ? ?
			ld l,c
			rrca
			rr l
			rrca
			rr l
			rrca
			rr l	; now a = y5 y4 y3 ? ? ? y7 y6, d = y2 y1 y0 ? ? ? ? ?, l = y5 y4 y3 x7 x6 x5 x4 x3
			sla d
			rla
			sla d
			rla
			sla d
			rla		; now a = ? ? ? y7 y6 y2 y1 y0, l = y5 y4 y3 x7 x6 x5 x4 x3
			and 0x1f
			or 0x40
			ld h,a	; now h = 0 1 0 y7 y6 y2 y1 y0, l = y5 y4 y3 x7 x6 x5 x4 x3
			ld a,c
			and 0x07
			ret

			fillto 0x22e5
plot_bc
; plot pixel, given coordinates
; TODO: apply temporary attributes and display modifiers to the plotted point -
; INK / PAPER / BRIGHT / FLASH / INVERSE / OVER
			call pixel_addr
			ld b,0x80	; bitmap for setting leftmost pixel of byte; shift this right
								; the number of times given in A
			or a			; initial test to see if a is zero
			jr shift_pixel_start
shift_pixel_lp
			rrc b			; shift pixel right
			dec a
shift_pixel_start
			jr nz,shift_pixel_lp	; continue shifting while not zero

			ld a,(hl)
			or b
			ld (hl),a
			ret
			
cmd_plot
			; TODO: recognise modifiers - INK / PAPER / BRIGHT / FLASH / INVERSE / OVER
			call consume_a		; fetch X coordinate
			push af
			rst nextchar
			cp ','						; ensure that a comma follows
			jp nz,fatal_error
			rst consume
			call consume_a		; fetch Y coordinate
			push af
			call assert_eos			; ensure that an end-of-statement follows
			pop bc						; recall Y and X coordinates into B and C
			pop af
			ld c,a
			; TODO: store coords in system variable for use in DRAW
			jr plot_bc				; now plot point at coords BC
