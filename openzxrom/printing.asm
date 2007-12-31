; printing.asm: Character printing routines
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

putchar_main
; Output the character specified in A
			exx								; preserve registers
			ld l,a
			ld a,(next_char_type)			; are we expecting a parameter to a control code?
			or a
			jp z,putchar_no_param			; if not, skip to putchar_no_param
			cp 0x10							; is this an INK c colour?
			jr z,putchar_ink
			cp 0x11							; is this a PAPER c colour?
			jr z,putchar_paper
			cp 0x12							; is this a FLASH c setting?
			jr z,putchar_flash
			cp 0x13							; is this a BRIGHT c setting?
			jr z,putchar_bright
			cp 0x16							; is this an AT y coordinate?
			jr z,putchar_at
			cp 0x17							; is this a TAB x coordinate?
			jr z,putchar_tab
			; TODO: handle other control codes
			jp error_unprintable	; other control codes not supported yet

putchar_ink
			; set temporary ink colour as specified in l
			; TODO: check for overflow and handle INK 8 / INK 9
			ld a,(temp_attribute)
			and 0xf8						; strip out previous ink colour
			jr write_temp_attribute

putchar_paper
			; set temporary paper colour as specified in l
			; TODO: check for overflow and handle PAPER 8 / PAPER 9
			sla l
			sla l
			sla l							; shift l to bits 3-5
			ld a,(temp_attribute)
			and 0xc7						; strip out previous paper colour
			jr write_temp_attribute

putchar_flash
			; set temporary FLASH attribute as specified in l
			; TODO: check for overflow and handle FLASH 8
			rrca
			ld a,(temp_attribute)
			and 0x7f						; strip out previous FLASH bit
			jr write_temp_attribute

putchar_bright
			; set temporary BRIGHT attribute as specified in l
			; TODO: check for overflow and handle BRIGHT 8
			rrca
			rrca
			ld a,(temp_attribute)
			and 0xbf						; strip out previous BRIGHT bit
write_temp_attribute
			or l								; merge bits from l into new attribute
			ld (temp_attribute),a	; write back to temp_attribute
			xor a							; expect ordinary char in next call
			ld (next_char_type),a			; to putchar
			exx
			ret

putchar_at
			; move cursor to specified y line (given in l)
			; TODO: check for overflow
			ld h,0x02						; shifted left five times to give 0x40,
											; start of screen memory
			sla l
			sla l
			sla l
			add hl,hl
			add hl,hl						; top two bits of y position go into high byte
			sla h
			sla h
			sla h
			ld (cursor_addr),hl
			ld a,0x17						; next value passed to putchar will provide
			ld (next_char_type),a			; x coordinate
			exx
			ret

putchar_tab
			; move cursor to specified x column (given in l)
			; TODO: check for overflow
			ld e,l
			ld hl,(cursor_addr)
			ld a,l							; get low byte of cursor address
			and 0x1f						; extract current x coordinate
			cp e							; are we already past the destination column?
			jr c,putchar_tab_no_wrap		; if not, don't advance to next line
			rrc h
			rrc h
			rrc h
			ld bc,0x0020
			add hl,bc
			rlc h
			rlc h
			rlc h
putchar_tab_no_wrap
			ld a,l
			and 0xe0						; strip out existing x coordinate
			or e							; merge in new x coordinate
			ld l,a
			ld (cursor_addr),hl
			xor a
			ld (next_char_type),a			; next value passed to putchar is
											; an ordinary character
			exx
			ret

putchar_no_param
			ld a,l
			cp 0xa5							; test whether character is a keyword token (ASCII >= 0xa5)
			jp nc,putchar_keyword
			cp 0x90							; test whether character is a UDG (ASCII >= 0x90)
			jr nc,putchar_udg
			cp 0x80							; test whether character is a block graphic (ASCII >= 0x80)
			jp nc,putchar_block_graphic
			cp 0x20							; test whether character is plaintext (ASCII >= 0x20)
			jr nc,putchar_plain
			cp 0x06							; is character a comma control?
			jr z,putchar_comma
			cp 0x0d							; is character a newline?
			jr z,putchar_newline
			ld (next_char_type),a			; if not, store control code in
											; next_char_type awaiting parameters
			exx
			ret
			
putchar_comma
			; comma control: output whitespace until cursor address mod 16 is 0
			ld a,' '
			rst putchar						; output a space
			ld a,(cursor_addr)		; test cursor_addr
			and 0x0f							; loop unless cursor_addr & 0x0f is zero
			jr nz,putchar_comma
			exx
			ret
			
putchar_newline
			exx								; restore registers (must only use A from here on)
			ld a,(cursor_addr)				; get low byte of cursor
			and 0xe0						; set x coord to 0
			add a,0x20						; increment y coord
			ld (cursor_addr),a
			ret nc							; we're done, unless we moved to next screen third
			ld a,(cursor_addr + 1)
			add a,0x08
			; TODO: handle bottom-of-screen overflow
			ld (cursor_addr + 1),a
			ret
			
putchar_udg
			sub 0x90						; convert char to address within UDG table
			ld l,a
			ld h,0
			add hl,hl
			add hl,hl
			add hl,hl
			ld de,(udg_ptr)
			add hl,de
			jr copy_char_bitmap
			
putchar_plain
			ld h,0							; convert char to address within font
			add hl,hl
			add hl,hl
			add hl,hl
			ld de,(font_ptr)
			add hl,de
; copy bitmap address pointed to by HL to current cursor position
copy_char_bitmap
			ld de,(cursor_addr)
			push de
			ld b,8
copy_char_bitmap_lp
			ld a,(hl)
			ld (de),a
			inc d
			inc l
			djnz copy_char_bitmap_lp
			pop hl
			push hl
			; write attribute cell
			ld a,h							; translate cursor to attribute position
			rrca
			rrca
			rrca
			or 0x50
			ld h,a
			ld a,(temp_attribute)			; write current temporary attribute value
			ld (hl),a
			pop hl
			; advance cursor
			rr h
			rr h
			rr h
			inc hl
			rl h
			rl h
			rl h
			; TODO: scroll screen and move cursor to start of line 23
			; when we reach the bottom of the screen (H = 0x58)
			ld (cursor_addr),hl
			exx
			ret

putchar_keyword
			jp error_unprintable		; TODO: implement keyword printing
putchar_block_graphic
			jp error_unprintable		; TODO: implement block graphic printing