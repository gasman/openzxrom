; keyboard.asm: Keyboard scanning
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

			fillto 0x028e
key_scan
			; scan keyboard, returning key code(s) in DE.
			; Numbering rows B-Sp=0, H-En=1, Y-P=2, 6-0=3, 1-5=4, Q-T=5, A-G=6, Cs-V=7,
			; key code is rownum + ((5 - bit number) << 3).
			; The first key encountered, ordering by descending row and ascending bit
			; within row, is put in E. The second key is put in D.
			; If one of the keys is caps shift, this will be placed in D.
			; Otherwise, if one of the keys is symbol shift, this will be placed in D.
			; The zero flag is returned reset if >2 keys are pressed, or 2 keys are pressed
			; and neither is a shift.
			ld de,0xffff
			ld b,7							; scan each of 8 rows
			ld a,0xfe						; with each bit in turn held low
key_scan_row
			push af
			in a,(0xfe)
			ld c,a							; pick apart result of IN in register C
			ld a,0x20						; count down bit number in A, premultiplied by 8
key_scan_bit
			push af
			rr c
			jr c,key_scan_bit_done			; if bit is nonzero (-> carry set), key not pressed; move on
			add a,b							; assemble key code from bit number and row number
			inc e							; check if e register is vacant
			jr nz,key_scan_e_not_vacant
			ld e,a
			jr key_scan_bit_done
key_scan_e_not_vacant
			dec e							; e is already occupied; restore value
			inc d							; check if d register is vacant
			jr z,key_scan_d_vacant
			pop hl							; if not, there are too many keys;
			pop hl							; restore stack and exit with Z reset
			ret
key_scan_d_vacant
			ld d,a
key_scan_bit_done
			pop af
			sub 8							; if counter in A does not roll over,
			jr nc,key_scan_bit				; there are bits remaining to check
			pop af							; go to next row once we've checked 5 bits
			dec b
			rlca							; keep scanning rows for as long as the zero bit
			jr c,key_scan_row				; doesn't fall off the end of A
			; keys collected; now handle shifts
			ld a,d
			inc a							; see if d is still 0xff (i.e. only one key)
			ret z							; if so, exit with Z set
			ld a,e
			cp 0x27							; check E for caps shift
											; (it's the first key we check, so it'll always
											; be in E if at all)
			jr nz,key_scan_no_cs
			ld e,d							; if E is caps shift, switch D and E
			ld d,a							; and exit with Z set
			ret
key_scan_no_cs
			cp 0x18							; check E for symbol shift
			jr nz,key_scan_no_ss
			ld e,d							; if E is sym shift, switch D and E
			ld d,a							; and exit with Z set
			ret
key_scan_no_ss
			ld a,d							; only remaining valid condition is if D is
			cp 0x18							; symbol shift; check for this condition and
			ret								; return with Z flag indicating the result
