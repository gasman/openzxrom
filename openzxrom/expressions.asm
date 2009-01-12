; expressions.asm: parsing and evaluation of numeric and string expressions
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

get_num_expr
; read a number expression from interp_ptr and leave it pushed on the calculator stack.
; Triggers a syntax error if expression is missing or not numeric
	call get_expr	; just wrap get_expr with error checking
	jp c,syntax_error	; trigger error if expression missing
	jp nz,syntax_error	; trigger error if expression non-numeric
	ret

get_num_expr_8
; read a level 8 number expression (i.e. that which is picked up as an argument to
; numeric functions except for NOT) from interp_ptr and leave it pushed on the calculator stack.
; Triggers a fatal error if expression is missing or not numeric
	call get_expr_8	; just wrap get_expr with error checking
	jp c,syntax_error	; trigger error if expression missing
	jp nz,syntax_error	; trigger error if expression non-numeric
	ret

get_string_expr
; read a number expression from interp_ptr and leave it pushed on the calculator stack.
; Triggers a fatal error if expression is missing or not string
	call get_expr	; just wrap get_expr with error checking
	jp c,syntax_error	; trigger error if expression missing
	jp z,syntax_error	; trigger error if expression non-string
	ret

get_expr
; read a number or string expression from interp_ptr and leave it pushed on the calculator stack
; Return carry set if the expression is absent. Return zero flag set if numeric, reset if string
	call get_expr_2
	ret c	; return if no level 2 expr found
	ret nz	; return if level 2 expr is a string
search_num_or
	rst nextchar
	cp 0xc5	; look for an OR op
	jr z,num_or
	xor a	; if none found, return success (carry reset, zero set)
	ret

num_or
; handle the rest of the OR expression
	rst consume	; consume the OR token
	call get_expr_2	; get right operand
	call c,syntax_error	; syntax error if no expr found
	call nz,syntax_error	; syntax error if operand is a string
	rst fp_calc	; perform OR operation
	db cc_fn_or
	db cc_end_calc
	jr search_num_or	; return to look for further ORs

get_expr_2
; read a level 2 numexpr - one containing anything above OR in the order of precedence
	call get_expr_4
	ret c	; return if no level 4 expr found
	jr nz,search_str_and	; if expr is a string, look for operands of a string-based AND
		; otherwise look for a numeric AND
search_num_and
	rst nextchar
	cp 0xc6	; AND
	jr z,num_and	; if there's no AND, we're done
	xor a	; signal successful fetch of numeric expression (carry reset, zero set)
	ret

search_str_and
	rst nextchar
	cp 0xc6	; AND
	jr z,str_and	; if there's no AND, we're done
	or 1	; signal successful fetch of string expression (carry reset, zero reset)
	ret

num_and
	rst consume	; consume the AND token
	call get_expr_4	; get right operand
	call c,syntax_error	; syntax error if no expr found
	call nz,syntax_error	; syntax error if right operand is a string

	rst fp_calc	; perform AND operation
	db cc_no_and_no
	db cc_end_calc

	jr search_num_and	; look for additional ANDs

str_and
	rst consume	; consume the AND token
	call get_expr_4	; get right operand
	call c,syntax_error	; syntax error if no num_expr found
	call nz,syntax_error	; syntax error if right operand is a string
		; FIXME: perform string AND op here
	jr search_str_and	; look for additional ANDs

; get_expr_3
; - this space unintentionally left blank
; (I thought NOT went here, but in fact that's at the top of the order of precedence
; so that we can have COS NOT 0 = COS (NOT 0) without COS 2^2 becoming COS (2^2).
; Confusingly though, NOT's argument can contain operations of lower precedence -
; e.g. COS NOT 1+1 = COS NOT (1+1) )



get_expr_4
; read a level 4 expr - one containing anything above AND in the order of precedence
	call get_expr_5
	ret c
	jr nz,search_str_comparator

search_num_comparator
	rst nextchar
	cp '='
	jr z,num_equals
	cp '<'
	jr z,num_less_than
	cp '>'
	jr z,num_greater_than
	cp 0xc7	; <=
	jr z,num_less_equals
	cp 0xc8	; >=
	jr z,num_greater_equals
	cp 0xc9	; <>
	jr z,num_not_equal
	xor a	; signal successful fetch of numeric expression (carry reset, zero set)
	ret

search_str_comparator
	rst nextchar
	cp '='
	jr z,str_equals
	cp '<'
	jr z,str_less_than
	cp '>'
	jr z,str_greater_than
	cp 0xc7	; <=
	jr z,str_less_equals
	cp 0xc8	; >=
	jr z,str_greater_equals
	cp 0xc9	; <>
	jr z,str_not_equal
	or 1	; signal successful fetch of string expression (carry reset, zero reset)
	ret

num_equals
	ld b,cc_nos_eql	; code for = operation
	jr expr_num_comparison
num_less_than
	ld b,cc_no_less	; code for < operation
	jr expr_num_comparison
num_greater_than
	ld b,cc_no_grtr	; code for > operation
	jr expr_num_comparison
num_less_equals
	ld b,cc_no_l_eql	; code for <= operation
	jr expr_num_comparison
num_greater_equals
	ld b,cc_no_gr_eql	; code for >= operation
	jr expr_num_comparison
num_not_equal
	ld b,cc_nos_neql	; code for <> operation
	jr expr_num_comparison

; perform a numeric comparison:
; enter with the comparison calculator opcode in B.
; Consumes the operator, fetches and validates right-hand expression,
; performs the comparison and returns to search_num_comparator
expr_num_comparison:
	push bc
	rst consume	; consume the operator token
	call get_expr_5	; get right-hand expression
	pop bc
	call c,syntax_error	; die if it's missing
	call nz,syntax_error	; die if it isn't numeric

	rst fp_calc	; perform the comparison
	db cc_calc_2
	db cc_end_calc
	jr search_num_comparator

str_equals
	call get_str_comparator_operand
		; FIXME: perform = op here
	jr search_str_comparator
str_less_than
	call get_str_comparator_operand
		; FIXME: perform < op here
	jr search_str_comparator
str_greater_than
	call get_str_comparator_operand
		; FIXME: perform > op here
	jr search_str_comparator
str_less_equals
	call get_str_comparator_operand
		; FIXME: perform <= op here
	jr search_str_comparator
str_greater_equals
	call get_str_comparator_operand
		; FIXME: perform >= op here
	jr search_str_comparator
str_not_equal
	call get_str_comparator_operand
		; FIXME: perform <> op here
	jr search_str_comparator

get_num_comparator_operand
	rst consume	; consume the operator token
	call get_expr_5	; get right-hand expression
	call c,syntax_error	; die if it's missing
	call nz,syntax_error	; die if it isn't numeric
	ret

get_str_comparator_operand
	rst consume	; consume the operator token
	call get_expr_5	; get right-hand expression
	call c,syntax_error	; die if it's missing
	call z,syntax_error	; die if it isn't string
	ret

get_expr_5
; read a level 5 expr - one containing anything above comparators in the order of precedence
	call get_expr_6
	ret c
	jr nz,search_str_sum_op	; if left expression is a string, search for + (for concatenation)
		; but not -
search_num_sum_op
	rst nextchar
	cp '+'
	jr z,num_plus
	cp '-'
	jr z,num_minus
	xor a	; signal successful fetch of numeric expression (carry reset, zero set)
	ret
search_str_sum_op
	rst nextchar
	cp '+'
	jr z,str_concat
	or 1	; signal successful fetch of string expression (carry reset, zero reset)
	ret

num_plus
	rst consume	; consume + token
	call get_expr_6	; fetch right-hand expression
	call c,syntax_error	; die if right-hand expression is missing
	call nz,syntax_error	; die if right-hand expression is a string
	rst fp_calc	; perform + operation
	db cc_addition
	db cc_end_calc
	jr search_num_sum_op

num_minus
	rst consume	; consume - token
	call get_expr_6	; fetch right-hand expression
	call c,syntax_error	; die if right-hand expression is missing
	call nz,syntax_error	; die if right-hand expression is a string
	rst fp_calc	; perform - operation
	db cc_subtract
	db cc_end_calc
	jr search_num_sum_op

str_concat
	rst consume	; consume + token
	call get_expr_6	; fetch right-hand expression
	call c,syntax_error	; die if right-hand expression is missing
	call z,syntax_error	; die if right-hand expression is numeric
		; FIXME: perform + (concat) op here
	jr search_str_sum_op

get_expr_6
; read a level 6 numexpr - one containing anything above binary +/- in the order of precedence
	call get_expr_7
	ret c
	ret nz	; return if it's a string (no currently-recognised operators apply to
		; strings at level 6)
		; TODO: detect string slicing operations at this point;
		; look for a following '(' and if found, read string slicing parameters
search_num_product
	rst nextchar
	cp '*'
	jr z,num_multiply
	cp '/'
	jr z,num_divide
	xor a	; signal successful fetch of numeric expression (carry reset, zero set)
	ret
num_multiply
	rst consume
	call get_expr_7
	call c,syntax_error
	call nz,syntax_error
	rst fp_calc	; perform * operation
	db cc_multiply
	db cc_end_calc
	jr search_num_product
num_divide
	rst consume
	call get_expr_7
	call c,syntax_error
	call nz,syntax_error
	rst fp_calc	; perform / operation
	db cc_division
	db cc_end_calc
	jr search_num_product

get_expr_7
; read a level 7 numexpr - one containing anything above * and / in the order of precedence
	call get_expr_8
	ret c
	ret nz	; return if it's a string (no operators apply to strings at level 7)
search_num_power
	rst nextchar
	cp '^'
	jr z,num_power
	xor a	; signal successful fetch of numeric expression (carry reset, zero set)
	ret
num_power
	rst consume
	call get_expr_8
	call c,syntax_error
	call nz,syntax_error
	rst fp_calc	; perform ^ operation
	db cc_to_power
	db cc_end_calc
	jr search_num_power

get_unary_plus
	rst consume	; cunning tail recursion, since unary plus is a no-op.
		; Doing it this way means that there's no place to validate that the
		; expression following the + is numeric, but in fact the original ROM
		; doesn't do that either: PRINT +"hello" is valid. That's a good sign
		; that we're doing something right :-)
get_expr_8
; read a level 8 expr - one above ^ in the order of precedence
	rst nextchar
	cp '"'	; Look for quotes, which begin a string literal
	jr z,get_str_literal
	cp '('	; Look for bracketed expressions
	jr z,get_bracketed
	cp '-'
	jr z,get_num_unary_minus
	cp '+'
	jr z,get_unary_plus
	cp '.'	; Look for characters '.' or 0-9
	jr z,get_num_literal	; which signal an upcoming number literal
		; (in fact so does BIN, but we catch that later on through
		; the function vector table)
	cp '0'
	ret c	; anything else below '0' is invalid / marks a missing expression
	cp '9'+1
	jr c,get_num_literal
		; TODO: grok variable names
		; For now treat everything else under 0xa5 as an end-of-expression
	sub 0xa5
	ret c
	cp 0x20	; anything >= 0xc5 is invalid (marks a missing expression)
	jr nc,return_missing_expr
	ld l,a	; look up code in function vector table
	ld h,0
	add hl,hl
	ld de,num_func_table
	add hl,de
	ld e,(hl)
	inc hl
	ld d,(hl)
	rst consume	; advance past function token
	push de
	ret
return_missing_expr
	scf
	ret

get_bracketed
	rst consume	; consume left bracket
	call get_expr	; evaluate everything within brackets
	call c,syntax_error	; die if no expression found
	push af	; remember zero flag (which indicates whether bracketed expression
		; is numeric or string
	rst nextchar	; Next char must be ')', or else
	cp ')'	; it will make baby Jesus cry
	call nz,syntax_error
		; NOTE: this leaves AF on stack; we'll want to pop this before jumping
		; to syntax_error, if and when we ever implement recovery from 'fatal' errors
	rst consume	; consume right bracket
	pop af	; recall zero/carry flags for return code
	ret

get_num_unary_minus
	rst consume
	call get_expr_8
	call c, syntax_error
	call nz,syntax_error
	rst fp_calc	; perform unary minus op
	db cc_negate
	db cc_end_calc
	xor a	; signal successful fetch of numeric expression (carry reset, zero set)
	ret

get_num_literal
; enter with HL = interp_ptr
; find the next embedded 0x0e sequence, pushes it onto the calculator stack
; and advances interp_ptr past it
	ld a,0x0e
get_num_find_0e
	cpi
	jr nz,get_num_find_0e
	ld de,(stkend)	; copy five bytes after the 0x0e
	ld bc,5	; to the end of the calculator stack
		; TODO: check for out-of-memory condition
	ldir
	ld (stkend),de
	ld (interp_ptr),hl	; write interp_ptr back, now pointing to byte after numeric literal
	call skip_whitespace	; advance interp_ptr past any trailing whitespace
	xor a	; signal successful fetch of numeric expression (carry reset, zero set)
	ret

get_str_literal
; enter with HL pointing to the opening " character;
; push the string onto the calculator stack
	inc hl	; advance to the first character inside the "
	ld (interp_ptr),hl	; store starting position, so we can return here for second pass

; first pass over string; get length of string, accounting for embedded " characters			
	ld bc,0	; bc contains string length
	ld a,'"'
string_lit_len_lp
	cp (hl)	; check whether next char is "
	inc hl	; advance pointer
	jr z,found_quote
	inc bc	; if not ", count it as one character
	jr string_lit_len_lp
found_quote
	cp (hl)	; is there a second " character?
	jr nz,string_lit_len_done	; if not, we've reached the end of the string
	inc bc	; if it is, count that as one more character
	inc hl	; advance past it
	jr string_lit_len_lp	; and continue counting chars

string_lit_len_done
	ld de,(calc_stack)	; allocate the required amount of space
	call alloc_space	; below the calculator stack

	push bc	; store string length and address
	push de

	ld hl,(interp_ptr)
; copy string from interp_ptr to heap, accounting for embedded " characters
	ld a,'"'
string_lit_to_heap_lp
	cp (hl)	; check if next char is "
	jr z,string_lit_to_heap_quote	; handle quoting if it is
	ldi	; otherwise, copy to heap and continue to next character
	jr string_lit_to_heap_lp

string_lit_to_heap_quote
	inc hl	; skip past " character
	cp (hl)	; is the following char a quote too?
	jr nz,string_lit_to_heap_done	; if not, it's the end of the string
	ldi	; if it is, copy the " and continue to next character
	jr string_lit_to_heap_lp
string_lit_to_heap_done
	ld (interp_ptr),hl	; write back interp_ptr, now pointing to the byte after end of string
	pop de	; recall string address and length
	pop bc
	call stk_store	; write string parameters onto calculator stack
	call skip_whitespace	; advance interp_ptr past any trailing whitespace
	or 1	; signal successful fetch of string expression (carry reset, zero reset)
	ret

; ---------------
consume_bc
; consume a numeric expression from interp_ptr and return it in bc,
; triggering the appropriate error if expression is missing/invalid/string
; or result is negative or >0xffff
	call get_num_expr
find_int
; pop value from calculator stack into bc,
; triggering the appropriate error if expression is missing/invalid
; or result is negative or >0xffff
	call fp_to_bc
	jp c,err_out_of_range	; must be within 16 bits
	jp nz,err_out_of_range	; must be positive
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
	call fp_to_a
	jp c,err_out_of_range
	ret
