; diagnostics.asm: Error diagnostics
; From the OpenZXRom project
; Copyright (c) 2007 Matthew Westcott
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

; throw an 'unsupported BASIC command' error
error_command
			ld de,err_msg_command
			ld bc,err_msg_command_len
			add a,0xce							; we previously subtracted 0xce from the command code
															; to give an index into the table of commands
			jr print_error_with_a

; throw an 'unsupported BASIC function' error
error_function
			ld de,err_msg_function
			ld bc,err_msg_function_len
			add a,0xa5							; we previously subtracted 0xa5 from the command code
															; to give an index into the table of functions
			jr print_error_with_a

; throw an 'unprintable character' error
error_unprintable
			ld de,err_msg_unprintable
			ld bc,err_msg_unprintable_len
			jr print_error_with_a

; throw an 'unsupported RST 0x0028 calculator operation' error
error_calc_op
			srl a ; divide by two, because by the time we get here A has been multiplied
				; by two since we fetched the opcode
			ld de,err_msg_calc_op
			ld bc,err_msg_calc_op_len
; print error message followed by the A register output in hex
print_error_with_a
			push af
			call print_diagnostic_error
			pop af
			jr print_error_resume_a

; throw a 'Here be dragons' (call to an empty area of ROM) error
error_here_be_dragons
			ld de,err_msg_here_be_dragons
			ld bc,err_msg_here_be_dragons_len
; print error message followed by HL output in hex
print_error_with_hl
			push hl
			call print_diagnostic_error
			pop hl
			ld a,h
			call print_hex_byte
			ld a,l
print_error_resume_a
			call print_hex_byte
			ld a,6						; comma control to fill rest of line
			rst putchar
; do the general crash-and-burn border effect
fatal_error_lp
			ld a,6
			out (254),a
			ld a,2
			out (254),a
			jr fatal_error_lp

; throw a syntax error
syntax_error
			pop hl						; retrieve return address
			dec hl						; rewind to the address of the CALL
			dec hl
			dec hl
			ld de,err_msg_syntax
			ld bc,err_msg_syntax_len
			jr print_error_with_hl

; print error message, ensuring that font/screen settings are back to standard ones
print_diagnostic_error
			ld hl,font-0x0100	; revert font to standard one
			ld (font_ptr),hl
			ld a,0x57					; set attributes to bright white on red
			ld (temp_attribute),a
			ld hl,screen			; set cursor to top of screen
			ld (cursor_addr),hl
			jp print_string

; print the contents of A in hex
print_hex_byte
			push af
			srl a				; get high digit into lower 4 bits
			srl a
			srl a
			srl a
			call print_hex_digit
			pop af
			and 0x0f		; now take the low digit
print_hex_digit
			add a,'0'		; convert to an ASCII digit
			cp '9'+1		; if it's in the range 0-9, skip ahead and print it
			jr c,hex_digit_0to9
			add a,'A'-'9'-1		; otherwise, add a bit more so it's in range A-F
hex_digit_0to9
			rst putchar
			ret

err_msg_here_be_dragons
			db "Here be dragons: 0x"
err_msg_here_be_dragons_len	equ $ - err_msg_here_be_dragons
err_msg_calc_op
			db "Unsupported calculator op: 0x"
err_msg_calc_op_len	equ $ - err_msg_calc_op
err_msg_command
			db "Unsupported BASIC command: 0x"
err_msg_command_len	equ $ - err_msg_command
err_msg_syntax
			db "Syntax error at: 0x"
err_msg_syntax_len	equ $ - err_msg_syntax
err_msg_function
			db "Unsupported function: 0x"
err_msg_function_len	equ $ - err_msg_function
err_msg_unprintable
			db "Unprintable character: 0x"
err_msg_unprintable_len	equ $ - err_msg_unprintable
