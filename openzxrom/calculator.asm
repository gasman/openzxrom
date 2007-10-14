; calculator.asm: Floating-point calculator routines
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

			fillto 0x2ab6
calc_push_aedcb
; PUSH the contents of AEDCB onto the calculator stack
			; TODO: check for out-of-memory condition
			ld hl,(calc_stack_end)
			ld (hl),a
			inc hl
			ld (hl),e
			inc hl
			ld (hl),d
			inc hl
			ld (hl),c
			inc hl
			ld (hl),b
			inc hl
			ld (calc_stack_end),hl
			ret
			
; ---------------
			fillto 0x2bf1
calc_pop_aedcb
; POP a value from the top of the calculator stack into AEDCB
			ld hl,(calc_stack_end)
			dec hl
			ld b,(hl)
			dec hl
			ld c,(hl)
			dec hl
			ld d,(hl)
			dec hl
			ld e,(hl)
			dec hl
			ld a,(hl)
			ld (calc_stack_end),hl
			ret

			fillto 0x2d28
calc_push_a
; PUSH the A register onto the calculator stack
			ld c,a	; move contents of A into BC and continue into calc_push_bc
			ld b,0
calc_push_bc
; PUSH the BC register pair onto the calculator stack
			xor a		; move contents of BC into DC portion of AEDCB, and continue into calc_push_aedcb
			ld e,a
			ld d,c
			ld c,b
			ld b,a
			jp calc_push_aedcb
			
			fillto 0x2da2
calc_pop_bc
; POP a value from the top of the calculator stack into BC
; Returns with carry set on overflow, zero set (and modulus in BC) if negative
			call calc_pop_aedcb		; grab the whole 5 bytes
			or a					; if exponent byte is zero, it's an integer
			jr nz,calc_pop_bc_fp	; otherwise, handle it as a floating point value
			ld b,c					; juggle registers to leave 16-bit result in BC
			ld c,d
			cp e					; check sign byte - 0x00 = positive, 0xff = negative
			jr nz,calc_pop_bc_int_neg
			inc a					; reset zero flag (and leave carry reset)
			ret
calc_pop_bc_int_neg
			or a					; reset carry
			ld hl,0					; negate BC by subtracting it from 0; this will
			sbc hl,bc				; set carry unless BC=0, which is an overflow
									; (equal to -65536)
			ccf
			ret c					; return with carry set upon overflow
			ld b,h					; get result back into BC
			ld c,l
			inc a					; A was 0xff; set zero flag without setting carry
			ret

			fillto 0x2dd5
calc_pop_a
; POP a value from the top of the calculator stack into A
; Returns with carry set on overflow, zero set (and modulus in A) if negative
			call calc_pop_aedcb		; grab the whole 5 bytes
			or a					; if exponent byte is zero, it's an integer
			jr nz,calc_pop_a_fp		; otherwise, handle it as a floating point value
			cp e					; check sign byte - 0x00 = positive, 0xff = negative
			jr nz,calc_pop_a_int_neg
			sub c					; indicate overflow (but leave zero unset) if c is non-zero
			ret c
			or 1					; clear carry and zero flags
			ld a,d
			ret
			
calc_pop_a_int_neg
			ld a,c					; c must be 0xff, or else there's an overflow
			cp 255
			ret c
			ld a,d
			neg						; get modulus; 0x00 means overflow (-256),
									; and carry will be reset in this case
			ccf						; - but we want to return with carry set on overflow
			ret c
			cp a					; set zero (to indicate negative result), keep carry reset
			ret
			
calc_pop_a_fp
			call calc_pop_bc_fp	; just use calc_pop_bc and take low byte of return value
			ld a,c
				; ... but need to return carry set if we overflow that byte (i.e. b is not 0)
			push af	; store flags before testing b
			xor a
			cp b	; will trigger a carry if b is not 0
			jr c,calc_pop_a_fp_overflow
			pop af	; recall return value and flags
			ret
calc_pop_a_fp_overflow
			pop bc	; remove stored af from stack
			ret		; return with carry still set

calc_pop_bc_fp
			add a,0x6f  ; cause a carry if exp > 0x90
			ret c ; return with carry set if overflow
			; TODO: maybe short-circuit the loop and return 0 if exponent is much too small
			sla c
			rl d
			rl e
			; sign flag will now be in carry - remember it for later
			push af
			
			; now shift right and increment a until a becomes 0
			scf	; first shift right needs to reinstate the high 1 bit
calc_pop_bc_fp_loop
			rr e
			rr d
			inc a
			jr z,calc_pop_bc_fp_exit
			or a  ; reset carry flag so that next shifts right will fill with zeros
			jp calc_pop_bc_fp_loop
  
calc_pop_bc_fp_exit
			adc a,d ; carry will still be in place from shifting d right;
				; use this to round up while copying result into bc
			ld c,a
			ld a,0
			adc a,e
			ld b,a
			pop af
				; carry flag will be set iff number was negative;
				; we want to set the zero flag in this case
			ccf	; now carry flag reset iff number is negative
			ld a,0
			adc a,0	; reset carry; set zero flag if carry flag was reset
			ret

; ---------------
calc_resume
			pop hl	; calculator ops return here to read next instruction
calc_main
; Calculator mode; enter with HL pointing to the sequence of calculator instructions. Continue executing
; until we encounter byte 0x38, at which we return control to the code following the instruction stream.
			ld a,(hl)
			inc hl
			cp 0x38
			jr z,calc_op_end_calc
			jp nc,fatal_error	; we don't handle anything above 0x38 for now
				; TODO: handle calc opcodes >0x38
			push hl
			ld l,a		; translate instruction code into address in jump table
			ld h,0
			add hl,hl
			ld de,calc_op_table
			add hl,de
			ld e,(hl)
			inc hl
			ld d,(hl)
			ld hl,calc_resume	; return address after performing operation
			push hl
			push de						; jump to looked-up address
			ret
calc_op_end_calc
			jp (hl)						; return to next address after instruction stream
			
; jump table for calculator opcodes
; jumps to fatal_error where opcode is unimplemented / undefined
calc_op_table
			dw fatal_error
			dw fatal_error	; 01 = exchange
			dw fatal_error	; 02 = delete
			dw fatal_error	; 03 = subtract
			dw fatal_error	; 04 = multiply
			dw fatal_error	; 05 = divide
			dw fatal_error	; 06 = power
			dw fatal_error	;
			dw fatal_error	;
			dw fatal_error	;
			dw fatal_error	;
			dw fatal_error	;
			dw fatal_error	;
			dw fatal_error	;
			dw fatal_error	;
			dw fatal_error	; 0f = add
			dw fatal_error	;
			dw fatal_error	;
			dw fatal_error	;
			dw fatal_error	;
			dw fatal_error	;
			dw fatal_error	;
			dw fatal_error	;
			dw fatal_error	; 17 = s_add
			dw fatal_error	; 18 = val$
cc_usr_s	equ 0x19
			dw fatal_error	; 19 = usr_s
			dw fatal_error	; 
			dw fatal_error	; 1b = negate
			dw fatal_error	; 1c = code
			dw fatal_error	; 1d = val
			dw fatal_error	; 1e = len
			dw fatal_error	; 1f = sin
			dw fatal_error	; 20 = cos
			dw fatal_error	; 21 = tan
			dw fatal_error	; 22 = asn
			dw fatal_error	; 23 = acs
			dw fatal_error	; 24 = atn
			dw fatal_error	; 25 = ln
			dw fatal_error	; 26 = exp
			dw fatal_error	; 27 = int
			dw fatal_error	; 28 = sqr
			dw fatal_error	; 29 = sgn
			dw fatal_error	; 2a = abs
cc_peek		equ 0x2b
			dw calcop_peek	; 2b = peek
cc_in			equ 0x2c
			dw calcop_in		; 2c = in
cc_usr_n	equ 0x2d
			dw calcop_usr_n	; 2d = usr_n
			dw fatal_error	; 2e = str$
			dw fatal_error	; 2f = chr$
			dw fatal_error	;
			dw fatal_error	; 31 = duplicate
			dw fatal_error	;
			dw fatal_error	;
			dw fatal_error	;
			dw fatal_error	;
			dw fatal_error	;
			dw fatal_error	;
cc_endcalc	equ 0x38

; ---------------			
calcop_peek
; PEEK calculator operation
			call calc_pop_bc_validate		; recall address and ensure it's in 0-ffff
			ld a,(bc)										; get address contents
			jp calc_push_a							; store the result and return
calcop_in
; IN calculator operation
			call calc_pop_bc_validate		; recall address and ensure it's in 0-ffff
			in a,(c)										; get port input value
			jp calc_push_a							; store the result and return
calcop_usr_n
; USR <numeric> calculator operation
			ld hl,calc_push_bc					; place calc_push_bc on the stack as the return address
																	; from the user routine, so that bc will be pushed on the
																	; calc stack before returning to the expression evaluator
			push hl
			call calc_pop_bc_validate		; recall address and ensure it's in 0-ffff
			push bc
			ret													; jump to address bc

; ---------------
consume_bc
; consume a numeric expression from interp_ptr and return it in bc,
; triggering the appropriate error if expression is missing/invalid/string
; or result is negative or >0xffff
			call get_num_expr
calc_pop_bc_validate
; pop value from calculator stack into bc,
; triggering the appropriate error if expression is missing/invalid
; or result is negative or >0xffff
			call calc_pop_bc
			jp c,err_out_of_range				; must be within 16 bits
			jp z,err_out_of_range				; must be positive
			ret

consume_a
; consume a numeric expression from interp_ptr and return it in a,
; triggering the appropriate error if expression is missing/invalid/string
; or magnitude is >255. (Note: we allow negative values here, although
; calling functions invariably ignore the sign bit...)
			call get_num_expr
calc_pop_a_validate
; pop value from calculator stack into a,
; triggering appropriate error if expression is missing/invalid
; or magnitude is >255
			call calc_pop_a
			jp c,err_out_of_range
			ret
			
