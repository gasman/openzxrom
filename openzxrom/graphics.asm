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
			call nz,syntax_error
			rst consume
			call consume_a		; fetch Y coordinate
			push af
			call assert_eos			; ensure that an end-of-statement follows
			pop bc						; recall Y and X coordinates into B and C
			pop af
			ld c,a
			; TODO: store coords in system variable for use in DRAW
			jr plot_bc				; now plot point at coords BC

cmd_circle
			; TODO: recognise modifiers - INK / PAPER / BRIGHT / FLASH / INVERSE / OVER
			call consume_a	; fetch X coordinate
			push af
			rst nextchar
			cp ','						; ensure that a comma follows
			call nz,syntax_error
			rst consume

			call consume_a		; fetch Y coordinate
			pop de			; now X coord is in D
			ld e,a			; Y coord in E
			push de
			rst nextchar
			cp ','						; ensure that a comma follows
			call nz,syntax_error
			rst consume

			call consume_a		; fetch radius
			push af
			call assert_eos			; ensure that an end-of-statement follows
			pop af

			pop de						; recall X and Y coordinates into D and E
			; continue directly into circle plotter routine

; Circle routine
; x^2+y^2=r^2 represents the real variable equation of a circle which is to be plotted using a
;grid of discrete pixels where each pixel has integer coordinates.
;   Note. original ROM circles are  slightly displaced to the right as noticed

; enter with DE = x/y coordinates of centre, A = radius

circle:	
;Setup of parameters
	ld h,0  ;H is x ; init to 0
	ld l,a  ;L is y ; init to radius
	
	exx
	cpl
	ld c,a
	ld b,$ff
	inc bc     ;bc' is -radius
	ld hl,1
	add hl,bc
	ex de,hl   ;de' is f=1-radius ; f error control
	;and a
	rl c
	rl b       ;-2*radius
	ld hl,5
	add hl,bc  ;hl'  is ddfy =5-2*r
	ld bc,3    ;bc' is  ddfx =3
	exx

;*************************************
;*******Main circle procedure*********
;*************************************

circle_loop:	

;*******Set 8 pixels, one for each circle's octant*********
	ld A,D ;Point #1
	add A,H
	ld c,A
	ld A,E
	add A,L
	ld b,A
	call plot_noregs
	
	ld A,E ;Point #2
	sub L
	ld b,A
	call plot_noregs
	
	ld A,D ;Point #4
	sub H
	ld c,A
	call plot_noregs
	
	ld A,E ;Point #3
	add A,L
	ld b,A
	call plot_noregs
	
	ld A,D ;Point #5
	add A,L
	ld c,A
	ld A,E
	add A,H
	ld b,A
	call plot_noregs
	
	ld A,E ;Point #6
	sub H
	ld b,A
	call plot_noregs
	
	ld A,D ;Point #8
	sub L
	ld c,A
	call plot_noregs
	
	ld A,E ;Point #7
	add A,H
	ld b,A
	call plot_noregs

;********Main logic***********	
	ld a,h  ;H is x
	cp l    ;L is y
	ret nc   ;While  (y > x)

		exx
		bit 7,d  ;if f>0
		;exx
		jr nz,circle_fneg 

			;exx

		    ex de,hl
			add hl,de
			ex de,hl	

			inc hl  ;hl' is ddfy 
			inc hl
	    	exx

			dec l
		    jr circle_fneg2	

				;end if
circle_fneg:
		;exx

		ex de,hl
		add hl,bc
		ex de,hl	
		exx

circle_fneg2:
	             exx
		inc bc   ;bc' is ddfx
		inc bc 

		inc hl
	             inc hl
		exx

		inc h		;H is x

    	jr circle_loop     ;end While

; plot_bc with registers preserved
plot_noregs:
	push hl
	push bc
	push de
	call plot_bc
	pop de
	pop bc
	pop hl
	ret
