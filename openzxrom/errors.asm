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
			dw err_msg_program_finished
err_code_program_finished	equ 0xff

			dw 0
err_code_next_without_for equ 0x00

			dw 0
err_code_variable_not_found equ 0x01

			dw 0
err_code_subscript_wrong equ 0x02

			dw err_msg_out_of_memory
err_code_out_of_memory		equ 0x03

			dw 0
err_code_out_of_screen equ 0x04

			dw err_msg_number_too_big
err_code_number_too_big equ 0x05

			dw 0
err_code_return_without_gosub equ 0x06

			dw 0
err_code_end_of_file equ 0x07

			dw 0
err_code_stop_statement equ 0x08

			dw err_msg_invalid_argument
err_code_invalid_argument equ 0x09

			dw err_msg_out_of_range
err_code_out_of_range		equ 0x0a

			dw 0
err_code_nonsense_in_basic equ 0x0b

			dw 0
err_code_break_cont_repeats equ 0x0c

			dw 0
err_code_out_of_data equ 0x0d

			dw 0
err_code_invalid_file_name equ 0x0e

			dw 0
err_code_no_room_for_line equ 0x0f

			dw 0
err_code_stop_in_input equ 0x10

			dw 0
err_code_for_without_next equ 0x11

			dw 0
err_code_invalid_io_device equ 0x12

			dw 0
err_code_invalid_colour equ 0x13

			dw 0
err_code_break_into_program equ 0x14

			dw 0
err_code_ramtop_no_good equ 0x15

			dw 0
err_code_statement_lost equ 0x16

			dw 0
err_code_invalid_stream equ 0x17

			dw 0
err_code_fn_without_def equ 0x18

			dw 0
err_code_parameter_error equ 0x19

			dw err_msg_loading_error
err_code_loading_error		equ 0x1a


err_msg_program_finished	db "Program finished", 0	; also used for report 9 (STOP statement)
err_msg_out_of_memory		db "Out of memory", 0
err_msg_number_too_big		db "Number too big", 0
err_msg_invalid_argument	db "Invalid argument", 0
err_msg_out_of_range		db "Out of range", 0	; also used for report K (Invalid colour)
									; and M (Ramtop no good)
err_msg_loading_error		db "Loading error! :-(", 0
