; functions.asm: Function evaluation
; From the Open82 project
; Copyright (c) 2009 Matthew Westcott
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

num_func_table
; vector table of numeric function handlers.
; When a keyword in this character set range is encountered in the course of
; expression evaluation, the corresponding function will be called, with the
; index into the table (= character set code - 0xa5) retained in A.

; dw error_function = not implemented yet, or invalid (e.g. AT - not a function)
	dw func_rnd	; 0x00 - RND
	dw error_function	; 0x01 - INKEY$
	dw func_pi	; 0x02 - PI
	dw error_function	; 0x03 - FN
	dw error_function	; 0x04 - POINT
	dw error_function	; 0x05 - SCREEN$
	dw error_function	; 0x06 - ATTR
	dw error_function	; 0x07 - AT
	dw error_function	; 0x08 - TAB
	dw error_function	; 0x09 - VAL$
	dw error_function	; 0x0a - CODE
	dw error_function	; 0x0b - VAL
	dw error_function	; 0x0c - LEN
	dw num_to_num_fn	; 0x0d - SIN
	dw num_to_num_fn	; 0x0e - COS
	dw num_to_num_fn	; 0x0f - TAN
	dw num_to_num_fn	; 0x10 - ASN
	dw num_to_num_fn	; 0x11 - ACS
	dw num_to_num_fn	; 0x12 - ATN
	dw num_to_num_fn	; 0x13 - LN
	dw num_to_num_fn	; 0x14 - EXP
	dw num_to_num_fn	; 0x15 - INT
	dw num_to_num_fn	; 0x16 - SQR
	dw num_to_num_fn	; 0x17 - SGN
	dw num_to_num_fn	; 0x18 - ABS
	dw num_to_num_fn	; 0x19 - PEEK
	dw num_to_num_fn	; 0x1a - IN
	dw func_usr	; 0x1b - USR
	dw error_function	; 0x1c - STR$
	dw error_function	; 0x1d - CHR$
	dw func_not	; 0x1e - NOT
	dw get_num_literal	; 0x1f - BIN (not really a function -
		; signifies that a literal is coming)

; Evaluate a numeric-to-numeric function:
; Consume a precedence-8 numeric expression (which excludes binary operations,
; so COS 2+2 only consumes the first 2) and apply the function to it (by
; selecting and applying the appropriate calculator operation)
num_to_num_fn:
	add a,cc_sin - 0x0d	; translate index (which is in the
		; range 0x0d = SIN to 0x1a = IN) to calculator opcode (which
		; begins at cc_sin)
	push af	; store the calculator code
	call get_num_expr_8	; fetch numeric operand
	pop bc	; retrieve calculator code into B
	rst fp_calc
	db cc_calc_2	; apply the function in B
	db cc_end_calc
	xor a	; signal successful fetch of numeric expression (carry reset, zero set)
	ret

func_not:
; NOT function. This is a special case because the numeric expression it consumes is much lower
; precedence, all the way down to comparison operators (precedence-4) - so that
; NOT 1=1 means NOT (1=1), whereas COS 1=1 doesn't mean COS (1=1).
	call get_expr_4	; consume (low-precedence) expression
	jp c,syntax_error	; trigger error if expression missing
	jp nz,syntax_error	; trigger error if expression non-numeric
	rst fp_calc	; perform NOT operation
	db cc_fn_not
	db cc_end_calc
	xor a	; indicate numeric result (carry reset, zero set)
	ret

func_usr
; USR function
	call get_expr_8	; retrieve expression, which may be string or numeric
	call c,syntax_error	; die if expression is missing
	jr nz,func_usr_str	; jump to func_usr_str if expression is a string
	rst fp_calc
	db cc_usr_no
	db cc_end_calc
	xor a	; indicate numeric result (carry reset, zero set)
	ret
func_usr_str
	rst fp_calc
	db cc_usr_str
	db cc_end_calc
	xor a	; indicate numeric result (carry reset, zero set)
	ret

func_pi
; PI keyword, treated as a function with no arguments
	rst fp_calc
	db cc_stk_pi_div_2	; stack half-PI
	db cc_end_calc
	inc (hl)	; increment the exponent, to multiply by two
	xor a	; indicate numeric result (carry reset, zero set)
	ret

; ------------------
; THE 'RND' FUNCTION
; ------------------
; taken from ZX81 ROM
func_rnd
	LD BC,(rand_seed)	; sv SEED_lo
	CALL stack_bc	; routine STACK-BC

	RST fp_calc	;; FP-CALC
	DEFB cc_stk_one	;;stk-one
	DEFB cc_addition	;;addition
	DEFB cc_stk_data	;;stk-data
	DEFB $37	;;Exponent: $87, Bytes: 1
	DEFB $16	;;(+00,+00,+00)
	DEFB cc_multiply	;;multiply
	DEFB cc_stk_data	;;stk-data
	DEFB $80	;;Bytes: 3
	DEFB $41	;;Exponent $91
	DEFB $00,$00,$80	;;(+00)
	DEFB cc_n_mod_m	;;n-mod-m
	DEFB cc_delete	;;delete
	DEFB cc_stk_one	;;stk-one
	DEFB cc_subtract	;;subtract
	DEFB cc_duplicate	;;duplicate
	DEFB cc_end_calc	;;end-calc

	CALL fp_to_bc	; routine FP-TO-BC
	LD (rand_seed),BC	; update the SEED system variable.
	CALL conv_to_fp	; need to work in floating-point form to arrive
		; at a random value 0 <= x < 1
	LD A,(HL)	; HL addresses the exponent of the last value.
	AND A	; test for zero
	JR Z,rnd_done	; forward, if so, to rnd_done

	SUB $10	; else reduce exponent by sixteen
	LD (HL),A	; thus dividing by 65536 for last value.

rnd_done
	xor a	; indicate numeric result (carry reset, zero set)
	ret

