; errors.asm: Display error messages
; From the OpenZXRom project
; Copyright (c) 2005-2008 Matthew Westcott
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

print_err	
; Print the error message specified by the code in A
			inc a							; why oh why did they make "OK" 0xff?!?
			ld l,a
			ld h,0
			add hl,hl
			ld de,err_table					; convert code to offset into err_table
			add hl,de
			ld e,(hl)
			inc hl
			ld d,(hl)
			xor a									; ensure we don't try to print mid-control-code
			ld (next_char_type),a
			ld a,0x0d
			rst putchar						; output leading newline
print_err_lp
			ld a,(de)
			or a
			jr z,print_err_done				; end loop if byte is zero
			rst putchar
			inc de
			jr print_err_lp
print_err_done
			ld a,2							; border red to signal error
			out (254),a
print_err_done_lp
			jr print_err_done_lp			; halt permanently

; table of error offsets
err_table
			dw report_0, 0, 0, 0, report_4, 0, 0, 0
			dw 0, 0, 0, report_b, 0, 0, 0, 0
			dw 0, 0, 0, 0, 0, 0, 0, 0
			dw 0, 0, 0, report_r

report_0	db "Program finished", 0	; also used for report 9 (STOP statement)
report_4	db "Out of memory", 0
report_b	db "Out of range", 0	; also used for report K (Invalid colour)
									; and M (Ramtop no good)
report_r	db "Loading error! :-(", 0
