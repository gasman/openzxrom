; commands.asm: Implementation of BASIC commands
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

command_table
; Where commands are unimplemented, they are set as a pointer to fatal_error
			dw fatal_error	; DEF FN
			dw fatal_error	; CAT
			dw fatal_error	; FORMAT
			dw fatal_error	; MOVE
			dw fatal_error	; ERASE
			dw fatal_error	; OPEN #
			dw fatal_error	; CLOSE #
			dw fatal_error	; MERGE
			dw fatal_error	; VERIFY
			dw fatal_error	; BEEP
			dw fatal_error	; CIRCLE
			dw cmd_ink		; INK
			dw cmd_paper	; PAPER
			dw fatal_error	; FLASH
			dw fatal_error	; BRIGHT
			dw fatal_error	; INVERSE
			dw fatal_error	; OVER
			dw cmd_out		; OUT
			dw fatal_error	; LPRINT
			dw fatal_error	; LLIST
			dw cmd_stop		; STOP
			dw fatal_error	; READ
			dw fatal_error	; DATA
			dw fatal_error	; RESTORE
			dw cmd_new		; NEW
			dw cmd_border	; BORDER
			dw fatal_error	; CONTINUE
			dw fatal_error	; DIM
			dw cmd_rem		; REM
			dw fatal_error	; FOR
			dw cmd_goto		; GO TO
			dw fatal_error	; GO SUB
			dw fatal_error	; INPUT
			dw fatal_error	; LOAD
			dw fatal_error	; LIST
			dw fatal_error	; LET
			dw fatal_error	; PAUSE
			dw fatal_error	; NEXT
			dw cmd_poke		; POKE
			dw fatal_error	; PRINT
			dw cmd_plot		; PLOT
			dw cmd_run		; RUN
			dw fatal_error	; SAVE
			dw cmd_randomize	; RANDOMIZE
			dw fatal_error	; IF
			dw cmd_cls		; CLS
			dw fatal_error	; DRAW
			dw cmd_clear	; CLEAR
			dw fatal_error	; RETURN
			dw fatal_error	; COPY

cmd_rem
; process REM command
			pop hl					; just override the usual return to interp
			jp interp_new_line		; and advance to the next line instead

cmd_goto
; process GO TO command
			pop hl							; Discard the return address (overridden here)
			call get_num_expr		; read line number
			call assert_eos			; ensure that the end of statement follows
			call calc_pop_bc_validate		; fetch the line number into bc, ensuring that
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

cmd_border
; process BORDER command
			call get_colour_arg
			out (0xfe),a			; change the border colour
				; TODO: save in a system variable to be preserved during BEEP / after LOAD etc
			ret
			
get_colour_arg
; fetch a numeric argument and ensure that it's a valid colour; return it in A
; TODO: handle pseudo-colours 8 and 9 (but not for BORDER)
			call get_num_expr		; read colour argument
			call assert_eos
			call calc_pop_a_validate			; fetch colour argument into a
			jr z,err_out_of_range	; explicitly forbid negative values, because calc_pop_a_validate doesn't
			cp 8								; ensure it's <8
			jr nc,err_out_of_range
			ret
err_out_of_range
			rst error
			db 0x0a					; code for "out of range"

cmd_ink
; process INK command
			ld a,(perm_attribute)	; get current attribute
			and 0xf8				; strip out old INK colour
			push af
			call get_colour_arg
			pop bc
			or b					; merge with remaining attributes
			ld (perm_attribute),a	; and write back
			ret
			
cmd_paper
; process PAPER command
			ld a,(perm_attribute)	; get current attribute
			and 0xc7				; strip out old PAPER colour
			push af
			call get_colour_arg
			sla a					; shift into PAPER bits
			sla a
			sla a
			pop bc
			or b					; merge with remaining attributes
			ld (perm_attribute),a	; and write back
			ret

cmd_clear
; process CLEAR command
			call get_expr								; look for optional argument
			jr c,cmd_clear_no_arg			; if not present, just perform stock CLEAR actions (clear screen and vars)
			jp nz,fatal_error					; die if argument is a string
			call assert_eos						; die if expression is not followed by end of statement
			call calc_pop_bc_validate		; pop argument into bc, ensuring that it's positive and within 16 bits

			call vanilla_clear					; do stock CLEAR actions (clear screen and vars)
			sbc hl,bc					; check that requested address (BC) is above bottom of free memory (HL)
				; TODO: consider making the limit slightly above calc_stack_end, for some
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
; Return with calc_stack_end (address of bottom of free memory) in HL.
; Also performed on RUN.
			call clear_screen
			ld hl,(vars_addr)					; collapse all memory areas
			ld (calc_stack),hl					; from end of program onwards
			ld (calc_stack_end),hl
			ret

cmd_run
; process RUN command
			pop hl								; override return address
			call vanilla_clear					; do stock CLEAR actions
			call get_expr							; fetch optional argument
			jr c,run_no_arg						; if none found, go to line 0
			jp nz,fatal_error					; die if it's a string rather than a number
			call assert_eos						; ensure end-of-statement follows argument
			call calc_pop_bc_validate	; retrieve argument into bc, ensuring it's
			jp goto_bc								; within 0<=bc<=0xffff
run_no_arg
			call assert_eos						; ensure end-of-statement follows RUN
			ld bc,0
			jp goto_bc

cmd_randomize
; process RANDOMIZE command
			call get_expr						; look for optional argument
			jr c,randomize_frames				; if none supplied, use FRAMES
			jp nz,fatal_error				; die if it's a string rather than numeric
			call assert_eos						; ensure end-of-statement follows argument
			call calc_pop_bc_validate		; if one supplied, check it's within 0<=bc<=0xffff
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
			jp nz,fatal_error
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
			jp nz,fatal_error
			rst consume								; consume the comma
			call consume_a						; fetch byte argument
			push af
			call assert_eos
			pop af
			pop bc
			out (c),a									; perform the output
			ret
