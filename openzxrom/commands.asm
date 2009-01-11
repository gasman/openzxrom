; commands.asm: Implementation of BASIC commands
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

command_table
; Where commands are unimplemented, they are set as a pointer to error_command
			dw error_command	; DEF FN
			dw error_command	; CAT
			dw error_command	; FORMAT
			dw error_command	; MOVE
			dw error_command	; ERASE
			dw error_command	; OPEN #
			dw error_command	; CLOSE #
			dw error_command	; MERGE
			dw error_command	; VERIFY
			dw error_command	; BEEP
			dw cmd_circle	; CIRCLE
			dw cmd_ink		; INK
			dw cmd_paper	; PAPER
			dw cmd_flash	; FLASH
			dw cmd_bright	; BRIGHT
			dw error_command	; INVERSE
			dw error_command	; OVER
			dw cmd_out		; OUT
			dw error_command	; LPRINT
			dw error_command	; LLIST
			dw cmd_stop		; STOP
			dw error_command	; READ
			dw error_command	; DATA
			dw error_command	; RESTORE
			dw cmd_new		; NEW
			dw cmd_border	; BORDER
			dw error_command	; CONTINUE
			dw error_command	; DIM
			dw cmd_rem		; REM
			dw error_command	; FOR
			dw cmd_goto		; GO TO
			dw error_command	; GO SUB
			dw error_command	; INPUT
			dw cmd_load	; LOAD
			dw error_command	; LIST
			dw error_command	; LET
			dw cmd_pause	; PAUSE
			dw error_command	; NEXT
			dw cmd_poke		; POKE
			dw cmd_print	; PRINT
			dw cmd_plot		; PLOT
			dw cmd_run		; RUN
			dw error_command	; SAVE
			dw cmd_randomize	; RANDOMIZE
			dw error_command	; IF
			dw cmd_cls		; CLS
			dw error_command	; DRAW
			dw cmd_clear	; CLEAR
			dw error_command	; RETURN
			dw error_command	; COPY

cmd_rem
; process REM command
			pop hl					; just override the usual return to interp
			jp interp_new_line		; and advance to the next line instead

cmd_goto
; process GO TO command
			pop hl							; Discard the return address (overridden here)
			call get_num_expr		; read line number
			call assert_eos			; ensure that the end of statement follows
			call find_int		; fetch the line number into bc, ensuring that
													; it's positive and <=0xffff
				; (actually the original ROM gives "B Integer out of range, 0:1" for >=61440,
				; and "N Statement lost, 0:255" for >=32768. How terribly random)
goto_bc
; set next_line_ptr to the line number specified in BC
			ld ix,prog_mem
goto_lp
			push ix
			pop hl
			ld de,(vars_addr)				; check whether we've scanned to
			or a							; the end of program memory (indicated by vars_addr)
			sbc hl,de
			jr nc,goto_found				; if so, consider this our destination
											; so that the main interpreter loop will exit
											; with 'program finished'
			ld h,(ix+0x00)					; read line number (back-to-front...)
			ld l,(ix+0x01)
			or a
			sbc hl,bc						; if this line number >= bc, carry will be unset
			jr nc,goto_found
				; otherwise advance ix to next line
			ld e,(ix+0x02)					; read line length
			ld d,(ix+0x03)
			inc de							; increase by 4 to skip past this line's header too
			inc de
			inc de
			inc de
			add ix,de						
			jr goto_lp
goto_found
			ld (next_line_ptr),ix
			jp interp_new_line

cmd_bright
; process BRIGHT command
			call consume_a				; fetch parameter (expected to be 0, 1 or 8)
			ld ix,perm_attribute	; tell set_bright to work with permanent_attributes
			call set_bright
			jp assert_eos					; return, while verifying that this is indeed the end of the statement

; enter with af = BRIGHT parameter and validation flags as received from consume_a,
; ix = pointer to perm_attribute or temp_attribute to indicate type of attribute to set
set_bright
			jr nz,err_out_of_range	; reject immediately if consume_a returned a negative result
set_bright_safe			; alternative entry point if we know argument isn't negative (i.e. from RST putchar)
			cp 8
			jr z,set_bright_transparent	; if bright 8, go to set transparent bright
			srl a					; otherwise, it must be 1 or 0
			jr nz,err_out_of_range
			rra						; shift bright bit into bit 6
			rra
			ld b,a				; and store in b
			ld a,(ix)			; get old attribute byte
			and 0xbf			; strip out old bright bit
			or b					; and merge in new one
			ld (ix),a			; write attribute back
			res 6,(ix+1)	; and set it as opaque in the attribute mask
			ret
set_bright_transparent
			set 6,(ix+1)		; set bright bit of mask to transparent
			res 6,(ix)			; reset bright bit of attribute
			ret							; (so it'll be unchanged when we OR the attribute onto the screen)			

cmd_flash
; process FLASH command
			call consume_a				; fetch parameter (expected to be 0, 1 or 8)
			ld ix,perm_attribute	; tell set_flash to work with permanent_attributes
			call set_flash
			jp assert_eos					; return, while verifying that this is indeed the end of the statement

; enter with af = FLASH parameter and validation flags as received from consume_a,
; ix = pointer to perm_attribute or temp_attribute to indicate type of attribute to set
set_flash
			jr nz,err_out_of_range	; reject immediately if consume_a returned a negative result
set_flash_safe			; alternative entry point if we know argument isn't negative (i.e. from RST putchar)
			cp 8
			jr z,set_flash_transparent	; if flash 8, go to set transparent flash
			srl a					; otherwise, it must be 1 or 0
			jr nz,err_out_of_range
			rra						; shift flash bit into bit 7
			ld b,a				; and store in b
			ld a,(ix)			; get old attribute byte
			and 0x7f			; strip out old flash bit
			or b					; and merge in new one
			ld (ix),a			; write attribute back
			res 7,(ix+1)	; and set it as opaque in the attribute mask
			ret
set_flash_transparent
			set 7,(ix+1)		; set flash bit of mask to transparent
			res 7,(ix)			; reset flash bit of attribute
			ret							; (so it'll be unchanged when we OR the attribute onto the screen)			

get_colour_arg
; fetch a numeric argument and ensure that it's a valid colour (0-7); return it in A
			call get_num_expr		; read colour argument
			call assert_eos
			call calc_pop_a_validate			; fetch colour argument into a
			jr nz,err_out_of_range	; explicitly forbid negative values, because calc_pop_a_validate doesn't
			cp 8								; ensure it's <8
			jr nc,err_out_of_range
			ret
err_out_of_range
			rst error
			db err_code_out_of_range

cmd_ink
; process INK command
			call consume_a				; fetch ink colour
			ld ix,perm_attribute	; tell set_ink to work with permanent attributes
			call set_ink
			jp assert_eos					; return, while verifying that this is indeed the end of the statement

; enter with af = ink colour and validation flags as received from consume_a,
; ix = pointer to perm_attribute or temp_attribute to indicate type of attribute to set
set_ink
			jr nz,err_out_of_range	; reject immediately if consume_a returned a negative result
set_ink_safe			; alternative entry point if we know argument isn't negative (i.e. from RST putchar)
			cp 9
			jr z,set_ink_contrast	; if ink 9, go to set contrasting ink colour
			jr nc,err_out_of_range	; if ink >9, reject
			cp 8
			jr z,set_ink_transparent	; if ink 8, go to set transparent ink colour
set_ink_opaque
			ld b,a					; store ink colour in B
			ld a,(ix+1)			; get mask byte
			and 0xf8				; set ink bits to non-transparent
			ld (ix+1),a			; and write back
set_ink_attr_only
			ld a,(ix)				; get attribute byte
			and 0xf8				; strip out ink bits
			or b						; merge new ink bits in
			ld (ix),a				; and write back
			ret
set_ink_contrast
			xor a						; set a=0 (black ink)
			bit 5,(ix)			; test high bit of current paper colour
			jr nz,set_ink_opaque	; if it's set (= light paper), jump back to set black ink
			ld a,7					; otherwise, set white ink
			jr set_ink_opaque
set_ink_transparent
			ld a,(ix+1)			; get mask byte
			or 0x07					; set ink bits to transparent
			ld (ix+1),a			; and write back
			ld b,0					; now jump back to set ink attribute bits to 0
			jr set_ink_attr_only	; so that they'll be unchanged when we OR the attribute onto the screen
			
cmd_paper
; process PAPER command
			call consume_a			; fetch paper colour
			ld ix,perm_attribute	; tell set_paper to work with permanent attributes
			call set_paper
			jp assert_eos				; return, while verifying that this is indeed the end of the statement

; enter with af = paper colour and validation flags as received from consume_a,
; ix = pointer to perm_attribute or temp_attribute to indicate type of attribute to set
set_paper
			jr nz,err_out_of_range	; reject immediately if consume_a returned a negative result
set_paper_safe		; alternative entry point if we know argument isn't negative (i.e. from RST putchar)
			cp 9
			jr z,set_paper_contrast	; if paper 9, go to set contrasting paper colour
			jr nc,err_out_of_range	; if paper >9, reject
			cp 8
			jr z,set_paper_transparent	; if paper 8, go to set transparent paper colour
			sla a						; shift paper colour into bits 3-5
			sla a
			sla a
set_paper_opaque
			ld b,a					; store paper bits in B
			ld a,(ix+1)			; get mask byte
			and 0xc7				; set paper bits to non-transparent
			ld (ix+1),a			; and write back
set_paper_attr_only
			ld a,(ix)				; get attribute byte
			and 0xc7				; strip out paper bits
			or b						; merge new paper bits in
			ld (ix),a				; and write back
			ret
set_paper_contrast
			xor a						; set a=0 (black paper)
			bit 2,(ix)			; test high bit of current ink colour
			jr nz,set_paper_opaque	; if it's set (= light ink), jump back to set black paper
			ld a,0x38				; otherwise, set white paper
			jr set_paper_opaque
set_paper_transparent
			ld a,(ix+1)			; get mask byte
			or 0x38					; set paper bits to transparent
			ld (ix+1),a			; and write back
			ld b,0					; now jump back to set paper attribute bits to 0
			jr set_paper_attr_only	; so that they'll be unchanged when we OR the attribute onto the screen
			
cmd_clear
; process CLEAR command
			call get_expr								; look for optional argument
			jr c,cmd_clear_no_arg			; if not present, just perform stock CLEAR actions (clear screen and vars)
			call nz,syntax_error				; die if argument is a string
			call assert_eos						; die if expression is not followed by end of statement
			call find_int		; pop argument into bc, ensuring that it's positive and within 16 bits
			push bc
			call vanilla_clear					; do stock CLEAR actions (clear screen and vars)
			pop bc
			sbc hl,bc					; check that requested address (BC) is above bottom of free memory (HL)
				; TODO: consider making the limit slightly above stkend, for some
				; breathing room (could do INC H here, but 256 bytes breathing room
				; is probably excessive)
			jp nc,err_out_of_range				; RAMTOP no good...
			ld h,b
			ld l,c
			ld (ramtop),hl						; save new RAMTOP
			ld sp,hl							; relocate stack
			jp interp							; and return to interpreter
												; (without doing a RET)

cmd_clear_no_arg
			call assert_eos					; ensure that end-of-statement follows CLEAR
vanilla_clear
; perform the actions of a parameterless CLEAR: clear the screen and delete variables.
; Return with stkend (address of bottom of free memory) in HL.
; Also performed on RUN.
			call clear_screen
			ld hl,(vars_addr)					; collapse all memory areas
			ld (calc_stack),hl					; from end of program onwards
			ld (stkend),hl
			ret

cmd_run
; process RUN command
			pop hl								; override return address
			call vanilla_clear					; do stock CLEAR actions
			call get_expr							; fetch optional argument
			jr c,run_no_arg						; if none found, go to line 0
			call nz,syntax_error			; die if it's a string rather than a number
			call assert_eos						; ensure end-of-statement follows argument
			call find_int	; retrieve argument into bc, ensuring it's
			jp goto_bc								; within 0<=bc<=0xffff
run_no_arg
			call assert_eos						; ensure end-of-statement follows RUN
			ld bc,0
			jp goto_bc

cmd_randomize
; process RANDOMIZE command
			call get_expr						; look for optional argument
			jr c,randomize_frames			; if none supplied, use FRAMES
			call nz,syntax_error			; die if it's a string rather than numeric
			call assert_eos						; ensure end-of-statement follows argument
			call find_int		; if one supplied, check it's within 0<=bc<=0xffff
			ld a,b								; check if it's 0
			or c
			jr nz,randomize_bc					; if not, use that as our seed
randomize_frames
			call assert_eos						; ensure end-of-statement follows RANDOMIZE
			ld bc,(frames)
randomize_bc
			ld (rand_seed),bc
			ret
			
cmd_poke
; process POKE command
			call consume_bc						; fetch address into bc
			push bc
			rst nextchar							; confirm that next char is a comma
			cp ','
			call nz,syntax_error
			rst consume								; consume the comma
			call consume_a						; fetch byte argument
			push af
			call assert_eos
			pop af
			pop hl
			ld (hl),a									; perform the poke
			ret

cmd_out
; process OUT command
			call consume_bc						; fetch address into bc
			push bc
			rst nextchar							; confirm that next char is a comma
			cp ','
			call nz,syntax_error
			rst consume								; consume the comma
			call consume_a						; fetch byte argument
			push af
			call assert_eos
			pop af
			pop bc
			out (c),a									; perform the output
			ret

cmd_load
; process LOAD command
			; allocate 17*2 bytes of workspace, for 'actual' and 'expected' tape headers:
			; 00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F 10 11 12 13 14 15 16 17 18 19 1A 1B 1C 1D 1E 1F 20 21
			; |--------------------actual----------------------| |--------------------expected--------------------|
			; typ|-------filename------------| |len| |-params--| typ|-------filename------------| |len| |--params-|
			ld hl,(calc_stack)
			ex de,hl
			ld bc,0x0022
			call alloc_space
			push de										; store workspace address
			ld h,d										; pad buffer with spaces
			ld l,e										; (only actually necessary for filename bytes of
			ld (hl),' '								; 'expected' header, but who's counting...)
			inc de
			dec bc
			ldir
			
			call get_string_expr			; fetch filename expression
			call stk_fetch				; get address and length of string
																; into DE and BC respectively

			; get filename length in a; = BC or 10, whichever is less
			ld a,b
			or a
			jr nz,filename_len_10
			ld a,c
			cp 0x0a
			jr c,filename_len_lt_10
filename_len_10
			ld a,0x0a
filename_len_lt_10

			pop hl										; recall workspace address
			push hl										; and store it again
			ld bc,0x0012							; advance to first filename byte of header
			add hl,bc

			or a											; if a = 0, filename is empty (LOAD "")
			jr nz,nonempty_filename
			ld (hl),0xff							; mark empty filename with byte 0xff at the start
			jr filename_copied

nonempty_filename
			ex de,hl									; hl = string buffer containing filename, de = header buffer
			ld c,a
			ld b,0
			ldir											; copy filename 
			
filename_copied
			; TODO: free the original string expression. (Would be a good idea to not actually
			; pop AEDCB above so we still have its address and length on the stack, then...)

			; continue parsing the instruction
			call nextchar_is_eos			; are we at end of statement?
			jp z,load_basic						; if so, we're loading basic
			cp 0xaf										; otherwise, check if it's CODE
			jr z,load_code
			cp 0xaa										; check if it's SCREEN$
			jr z,load_screen
			; TODO: grok DATA
			call syntax_error						; reject anything else as invalid

load_code
			pop ix										; retrieve buffer address
			ld (ix+0x11),0x03					; set expected filetype to 3 = Bytes
			rst consume								; consume the CODE token

			call nextchar_is_eos			; is this the end of the statement?
			jr z,load_code_no_args		; treat as no-argument LOAD "filename"CODE if so
			push ix
			call consume_bc						; fetch CODE start address into bc
			pop ix
			ld (ix+0x1e),c						; store start address in 'expected' header
			ld (ix+0x1f),b

			call nextchar_is_eos			; end of statement now?
			jr z,load_code_one_arg		; if so, treat as LOAD "filename"CODE start
			cp ','										; otherwise, it must be ','
			call nz,syntax_error			; or else it's invalid
			rst consume								; consume the comma
			push ix
			call consume_bc						; fetch CODE length into bc
			pop ix
			ld (ix+0x1c),c						; store code length in 'expected' header
			ld (ix+0x1d),b
			
			call assert_eos						; end of statement must happen now
				; now handle full LOAD "filename"CODE start,length form

load_code_two_args
			call search_tape_header		; read tape headers until we get one that matches
			ld e,(ix+0x1c)						; recall length from 'expected' buffer
			ld d,(ix+0x1d)						; (which came from LOAD parameter)
			ld c,(ix+0x1e)						; recall start address from 'expected' buffer
			ld b,(ix+0x1f)						; (which came from LOAD parameter)
			push bc										; set ix to start address
			pop ix
			jr load_code_datablock

load_screen
			pop ix										; retrieve buffer address
			ld (ix+0x11),0x03					; set expected filetype to 3 = Bytes
			rst consume								; consume the SCREEN$ token
			call assert_eos						; this must be the end of the statement
			ld (ix+0x1c),0x00					; set expected length to 0x1b00
			ld (ix+0x1d),0x1b
			ld (ix+0x1e),0x00					; set expected address to 0x4000
			ld (ix+0x1f),0x40
			jr load_code_two_args			; now continue as if we'd invoked
																; LOAD "filename"CODE 16384,6912
load_code_no_args
			call search_tape_header		; read tape headers until we get one that matches
			ld e,(ix+0x0b)						; read length from actual tape header
			ld d,(ix+0x0c)
			ld c,(ix+0x0d)						; read start address from actual tape header
			ld b,(ix+0x0e)
			push bc										; set ix to start address
			pop ix
			jr load_code_datablock

load_code_one_arg
			call search_tape_header		; read tape headers until we get one that matches
			ld e,(ix+0x0b)						; read length from actual tape header
			ld d,(ix+0x0c)
			ld c,(ix+0x1e)						; recall start address from 'expected' buffer
			ld b,(ix+0x1f)						; (which came from LOAD parameter)
			push bc										; set ix to start address
			pop ix
load_code_datablock
			ld a,0xff									; 0xff = load datablock
			scf												; carry set = load (not verify)
			call load_bytes						; load data block
			jr nc,report_loading_error
			ret

report_loading_error
			rst error
			db err_code_loading_error

load_basic
			pop ix										; retrieve buffer address
			ld (ix+0x11),0x00					; set expected filetype to 0 = Program
			call search_tape_header		; read tape headers until we get one that matches
			; Read the BASIC header and make room for the incoming program
			ld e,(ix+0x0d)					; get LINE number
			ld d,(ix+0x0e)
			push de
			ld hl,prog_mem
			push hl
			ld e,(ix+0x0f)					; get BASIC length
			ld d,(ix+0x10)
			add hl,de
			ld (vars_addr),hl				; store position of variables table
			pop hl							; recall prog_mem
			push hl
			ld e,(ix+0x0b)					; get full data length
			ld d,(ix+0x0c)
			add hl,de						; find new location of spare memory
			; TODO: compare with RAMTOP and complain if RAMTOP too low
			ld (workspace),hl				; set new spare mem location
			ld (calc_stack),hl
			ld (stkend),hl
			pop ix							; recall prog_mem again
			ld a,0xff						; a=0xff for data block
			scf								; carry set for load
			call load_bytes
			jr nc,report_loading_error
			pop bc
			jp goto_bc						; jump to LINE number
				; - this will happen even if a LINE number wasn't set, since in
				; that situation the bytes will come in as something >= 32768,
				; and for any non-diabolically-hacked program GO TO 32768 will
				; result in an immediate 'program finished', which is what we want.
			; From goto_bc we launch straight into the main interpreter loop.

cmd_print
; process PRINT command
			ld a,(perm_attribute)	; copy permanent attribute variables to temporary ones
			ld (temp_attribute),a
			ld a,(perm_mask)
			ld (temp_mask),a
do_print_item
; parse and process the sequence of PRINT items, each of which may be one of the following:
; * a display modifier: INK, PAPER, BRIGHT, FLASH, AT, TAB, INVERSE, OVER
; * a stream redirection: #n
; * a string or numeric expression
; * a separator: ; , '
; * the end of statement
			rst nextchar					; get next character
			cp 0xd9								; check if it's a colour display modifier
			jr c,print_item_not_colour_modifier	; - i.e. INK/PAPER/FLASH/BRIGHT/INVERSE/OVER,
			cp 0xdf								; code 0xd9-0xdf
			jr c,print_item_colour_modifier
print_item_not_colour_modifier
			cp 0xac								; check for AT
			jr z,print_item_at
			cp 0xad								; check for TAB
			jr z,print_item_tab
			; TODO: recognise stream redirection (#n) at this point			
			call get_expr					; look for a string or numeric expression
			jr c,print_item_expect_separator	; jump ahead if not found
			call z,print_item_numeric		; if it's numeric, convert to a string
			; we now have a string on top of the calculator stack
			call stk_fetch		; fetch string parameters into AEDCB
			call print_string			; and print the string

			; next character must be a separator or end of statement
print_item_expect_separator
			rst nextchar					; look at next character
			cp ';'								; match it against separator characters ; ' ,
			jr z,print_item_semicolon
			cp "'"
			jr z,print_item_apostrophe
			cp ','
			jr z,print_item_comma
			; having exhausted all other possibilities, assert that we're at the end of the statement
			call assert_eos
			ld a,0x0d							; if so, output a newline character and return
			rst putchar
			ret	

print_item_at
			rst consume						; consume the AT token
			ld a,0x16
			rst putchar						; output an AT control code
			call consume_a				; fetch y coordinate
			rst putchar						; output it
			rst nextchar					; next character must be a comma
			cp ','
			call nz,syntax_error
			jr print_item_one_arg	; now fetch the remaining parameter and output it as for colour items

print_item_tab
			ld a,0x17							; TAB control code
			jr print_item_control_code_one_arg	; output control code then handle parameter as for colour items
			
print_item_colour_modifier
; handle INK, PAPER, FLASH, BRIGHT, INVERSE, OVER
			sub 0xc9							; control codes (0x10-0x15) are 0xc9 less than the token codes (0xd9-0xde)
print_item_control_code_one_arg
			rst putchar						; output the control code
print_item_one_arg
			rst consume						; consume the token
			call consume_a				; fetch the parameter
			rst putchar						; output it
			jr print_item_expect_separator	; this must now be followed by a separator or end of statement

print_item_comma
			ld a,0x06							; output a comma control
			jr print_item_comma_next
print_item_apostrophe
			ld a,0x0d							; output a newline
print_item_comma_next
			rst putchar
print_item_semicolon
			rst consume						; consume the separator character
			call nextchar_is_eos	; check if we're at the end of the statement
			ret z									; just return (with no trailing newline) if so
			jr do_print_item			; otherwise, go back for more print items

print_item_numeric
			rst fp_calc								; invoke calculator
			db cc_str_str, cc_end_calc	; to perform STR$ on the number on top of the stack
			ret											; then return to deal with it as a string

cmd_pause
; process PAUSE command
			call consume_bc				; fetch argument
			call assert_eos				; and confirm that this is the end of the statement
			ld hl,flags
			res 5,(hl)						; reset 'key is pressed' flag
pause_lp
			halt
			bit 5,(hl)						; check for key
			ret nz								; return if key pressed
			ld a,b								; is bc zero? (Will only be the case for PAUSE 0)
			or c
			jr z,pause_lp					; if so, loop indefinitely until key pressed
			dec bc								; otherwise, decrement and return if it's zero now
			ld a,b
			or c
			jr nz,pause_lp
			ret
			