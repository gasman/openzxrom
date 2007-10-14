; functions.asm: Function evaluation
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

num_func_table
; vector table of numeric function handlers
; dw fatal_error = not implemented yet, or invalid (e.g. AT - not a function)
			dw fatal_error			; RND
			dw fatal_error			; INKEY$
			dw fatal_error			; PI
			dw fatal_error			; FN
			dw fatal_error			; POINT
			dw fatal_error			; SCREEN$
			dw fatal_error			; ATTR
			dw fatal_error			; AT
			dw fatal_error			; TAB
			dw fatal_error			; VAL$
			dw fatal_error			; CODE
			dw fatal_error			; VAL
			dw fatal_error			; LEN
			dw fatal_error			; SIN
			dw fatal_error			; COS
			dw fatal_error			; TAN
			dw fatal_error			; ASN
			dw fatal_error			; ACS
			dw fatal_error			; ATN
			dw fatal_error			; LN
			dw fatal_error			; EXP
			dw fatal_error			; INT
			dw fatal_error			; SQR
			dw fatal_error			; SGN
			dw fatal_error			; ABS
			dw func_peek				; PEEK
			dw func_in					; IN
			dw func_usr					; USR
			dw fatal_error			; STR$
			dw fatal_error			; CHR$
			dw fatal_error			; NOT
			dw get_num_literal		; BIN (not really a function -
										; signifies that a literal is coming)

; ---------------
func_peek
; PEEK function
; TODO: size-optimise by combining all functions that just call a single calculator op corresponding
; to their index number
			call get_num_expr_8	; fetch numeric operand 
			rst calc					; perform PEEK operation
			db cc_peek
			db cc_endcalc
			xor a								; signal successful fetch of numeric expression (carry reset, zero set)
			ret
func_in
; IN function
			call get_num_expr_8	; as for func_peek
			rst calc
			db cc_in
			db cc_endcalc
			xor a								; signal successful fetch of numeric expression (carry reset, zero set)
			ret
func_usr
; USR function
			call get_expr_8		; retrieve expression, which may be string or numeric
			jp c,fatal_error	; die if expression is missing
			jr nz,func_usr_str	; jump to func_usr_str if expression is a string
			rst calc
			db cc_usr_n
			db cc_endcalc
			xor a							; indicate numeric result (carry reset, zero set)
			ret
func_usr_str
			rst calc
			db cc_usr_s
			db cc_endcalc
			xor a							; indicate numeric result (carry reset, zero set)
			ret
