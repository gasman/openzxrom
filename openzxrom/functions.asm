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
; dw error_function = not implemented yet, or invalid (e.g. AT - not a function)
			dw error_function			; RND
			dw error_function			; INKEY$
			dw error_function			; PI
			dw error_function			; FN
			dw error_function			; POINT
			dw error_function			; SCREEN$
			dw error_function			; ATTR
			dw error_function			; AT
			dw error_function			; TAB
			dw error_function			; VAL$
			dw error_function			; CODE
			dw error_function			; VAL
			dw error_function			; LEN
			dw error_function			; SIN
			dw error_function			; COS
			dw error_function			; TAN
			dw error_function			; ASN
			dw error_function			; ACS
			dw error_function			; ATN
			dw error_function			; LN
			dw error_function			; EXP
			dw error_function			; INT
			dw error_function			; SQR
			dw error_function			; SGN
			dw error_function			; ABS
			dw func_peek				; PEEK
			dw func_in					; IN
			dw func_usr					; USR
			dw error_function			; STR$
			dw error_function			; CHR$
			dw error_function			; NOT
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
			call c,syntax_error	; die if expression is missing
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
