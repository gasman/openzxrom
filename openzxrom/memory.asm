; memory.asm: Memory management routines
; From the OpenZXRom project
; Copyright (c) 2007-2008 Matthew Westcott
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

; Open up BC bytes of space starting at DE, and adjust system variables accordingly.
; (NB: does not update pointers to strings within the calculator stack)
; Returns with BC and DE intact
alloc_space
	push bc	; store length of space
	ld hl,(stkend)	; calculate number of bytes to be moved;
	or a	; = stkend - address_to_open_at
	sbc hl,de
	push hl

	ld hl,(stkend)	; calculate last byte to move;
	dec hl	; = stkend - 1
	push hl
	add hl,bc	; determine where to move it to
		; FIXME: throw out-of-memory if this exceeds 0xffff / RAMTOP
		; (or the gosub stack, or whatever lives above the spare memory)
	ex de,hl	; and put that address in de
	pop hl	; recall 'from' address
	pop bc	; recall length
		; skip the move if there are zero bytes being moved
	ld a,b
	or c
	jr z,alloc_space_no_move
	lddr	; do the move
alloc_space_no_move
	inc hl	; correct hl/de to compensate for
	inc de	; LDDR post-decrementing them
		; now hl = base of free space, de = end address of free space
		; NB: As we end up with a copy of the old data in the 'free space',
		; in some cases we can use DE onward as the free space instead.
		; For the purposes of adjusting system variables, though, we'll
		; take them as moving to the higher area of memory.
	pop bc	; recall length of space
	ex de,hl	; any system variables now pointing >= DE
		; should be incremented by BC
		; at the moment, the only system variables we expect to change are calc_stack
		; and stkend.
		; TODO: update this (probably looping over all affected variables) as and
		; when new system variables are introduced
		; NOTE: When 'workspace' becomes movable (i.e. when we implement the VARS store
		; or inserting program lines) we'll need to be selective about whether it's moved
		; or not; if variable store is empty, then allocating space in variable store
		; should move WORKSPACE but allocating space in the workspace shouldn't,
		; despite them being both at the same address.
	ld hl,(calc_stack)
	or a
	sbc hl,de	; test whether calc_stack is >= DE
	jr c,no_inc_calc_stack	; skip increasing calc_stack if it's less than DE
	add hl,de	; re-add DE to restore value of calc_stack
	add hl,bc	; increment by BC
	ld (calc_stack),hl
no_inc_calc_stack
	ld hl,(stkend)
	or a
	sbc hl,de
	jr c,no_inc_stkend	; skip increasing calc_stack if it's less than DE
	add hl,de
	add hl,bc
	ld (stkend),hl
no_inc_stkend
	ret

; TODO: free_space routine
; (the original ROM doesn't bother, from the looks of things... but admittedly it's
; probably more conservative about sticking things into workspace in the first place)
