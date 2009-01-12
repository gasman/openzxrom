; speaker.asm: tone generation routines for the built-in speaker
; From the Open82 project
; Copyright (c) 2008 Jan Bobrowski
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

	fillto 0x03B5
beeper:
	add hl,hl
	add hl,hl
	ld bc,118
	add hl,bc

		; hl = half period

	ld bc,-89
	add hl,bc
	ld b,h
	ld c,l

	ld hl,0
	scf
	sbc hl,de
	push hl
	pop ix

	ld a,(border_colour)
	rrca
	rrca
	rrca
	and 7
	or 8
	ex af,af'

	ld de,0
	di
beeper_loop
	ex af,af'
	xor 10h
	out (0xfe),a
	ex af,af'
	push bc
	call delay
	pop bc
	ld a,e
	xor 1
	ld e,a
	add ix,de
	jr nc,beeper_loop

	ei
	ret

delay		; wait bc T (including call; bc>=141)
		; call with bc=0 in order to wait 65536 T
		; destroys: af, bc, hl

	ld hl, -141
	add hl, bc
	ld bc, -23
delay_loop	add hl, bc
	jr c, delay_loop
	ld a, l
	add a,15
	jr nc, delay_g0
	cp 8
	jr c, delay_g1
	or 0
delay_g0	inc hl
delay_g1	rra
	jr c, delay_b0
	nop
delay_b0	rra
	jr nc, delay_b1
	or 0
delay_b1	rra
	ret nc
	ret
