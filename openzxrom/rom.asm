; rom.asm: A freely-licenced replacement firmware for the ZX Spectrum
; From the OpenZXRom project
; Copyright (c) 2005-2006 Matthew Westcott
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

; macro to mark addresses up to (and not including) addr as unused memory
fillto		macro addr
			ds addr - $, 0xF7				; 0xF7 = RST 0x0030
			endm
			
; ---------------
			org 0x0000
; page 0: restarts and interrupts
; RST 0x0000: here's where it all begins
			di
			im 1
			ei
			jr cold_start
			
			fillto 0x0008
; RST 0x0008: output error message
error
			pop hl							; Fetch code from the address immediately
			ld a,(hl)						; following the call to this routine
			jp print_err					; NB We throw away the return address
			
			fillto 0x0010
; RST 0x0010: Output the character specified in A
putchar		jp putchar_main

			fillto 0x0018
; RST 0x0018: return next char to be interpreted
nextchar	ld hl,(interp_ptr)
			ld a,(hl)
			ret
			
			fillto 0x0020
; RST 0x0020: consume next character of program
; (i.e. advance to next non-ignorable character; ignorable = space and control codes)
consume		ld hl,interp_ptr
			inc (hl)
			jp skip_whitespace
			
			fillto 0x0028
; RST 0x0028: enter calculator mode
calc			pop hl
			jp calc_main

			fillto 0x0030
; Arrive here if we execute any undefined area of the ROM
fatal_error
			di
			jp fatal_error_main
			
			fillto 0x0038
; RST 0x0038: IM 1 interrupt service routine
			push hl
			ld hl,frames
			inc (hl)						; increment frames system variable,
			jr nz,done_frames				; overflowing into second and third
			inc l							; byte as necessary
			inc (hl)
			jr nz,done_frames
			inc l
			inc (hl)
done_frames
			pop hl
			ei
			ret

fatal_error_main
fatal_error_lp
			ld a,6
			out (254),a
			ld a,3
			out (254),a
			jr fatal_error_lp

; ---------------
cmd_new
; handle NEW command
			call nextchar_is_eos			; check syntax first; ensure next char is
			jp nz,fatal_error				; end-of-statement
			jr warm_start					; if OK, perform a warm start
			
cold_start
; startup operations
			ld hl,udg_mem					; set RAMTOP to traditional location
			ld (ramtop),hl
				; TODO: detecting 16K models
warm_start
			ld hl,(ramtop)
			ld sp,hl
			ld hl,0
			xor a
			ld (rand_seed),hl				; reset random seed to 0
			ld (frames),hl					; reset frames counter to 0
			ld (frames+2),a
			ld hl,font - 0x0100				; point to font bitmap
			ld (font_ptr),hl
			ld hl,prog_mem					; designate all memory from prog_mem
			ld (vars_addr),hl				; as spare (i.e. set BASIC length to zero)
			ld (calc_stack),hl
			ld (calc_stack_end),hl
			ld a,0x38						; set attributes to black on white
			ld (perm_attribute),a
			; TODO: establish exactly where we should copy perm_attribute to
			; temp_attribute - currently doing this in clear_screen
			ld a,7
			out (254),a						; border white
			call clear_screen
			ld de,splash_text				; draw splash screen
			ld bc,splash_text_end - splash_text
			call print_string

			; wait for enter to be pressed
			ld bc,0xbffe
wait_enter
			in a,(c)
			and 1
			jr nz,wait_enter

			; display 'loading' message
			call clear_screen
			ld de,loading_text
			ld bc,loading_text_end - loading_text
			call print_string

			; load blocks from tape until we find a BASIC header
search_basic_header
			ld ix,(calc_stack_end)			; load tape header at start of spare memory
			ld de,0x0011					; 17 bytes long
			xor a							; a=0 for header
			scf								; carry=set for load
			push ix
			call load_bytes
			pop ix
			jr nc, search_basic_header
			call describe_tape_file			; print filename
			ld a,(ix+0)
			or a
			jr nz,search_basic_header
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
			ld (calc_stack),hl				; set new spare mem location
			ld (calc_stack_end),hl
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
			
report_loading_error
			rst error
			db 0x1a			; code for loading error

; ---------------
cmd_goto
; handle GO TO command
			pop hl							; Discard the return address (overridden here)
			call consume_bc			; fetch the line number into bc, ensuring that
													; it's present, positive and <=0xffff
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
						
; ---------------			
putchar_main
; Output the character specified in A
			exx								; preserve registers
			ld l,a
			ld a,(next_char_type)			; are we expecting a parameter to a control code?
			or a
			jr z,putchar_no_param			; if not, skip to putchar_no_param
			cp 0x10							; is this an INK c colour?
			jr z,putchar_ink
			cp 0x11							; is this a PAPER c colour?
			jr z,putchar_paper
			cp 0x16							; is this an AT y coordinate?
			jr z,putchar_at
			cp 0x17							; is this a TAB x coordinate?
			jr z,putchar_tab
			; TODO: handle other control codes
			rst fatal_error	; other control codes not supported yet

putchar_ink
			; set temporary ink colour as specified in l
			; TODO: check for overflow and handle INK 8 / INK 9
			ld a,(temp_attribute)
			and 0xf8						; strip out previous ink colour
			or l
			ld (temp_attribute),a
			xor a							; expect ordinary char in next call
			ld (next_char_type),a			; to putchar
			exx
			ret

putchar_paper
			; set temporary paper colour as specified in l
			; TODO: check for overflow and handle PAPER 8 / PAPER 9
			sla l
			sla l
			sla l							; shift l to bits 3-5
			ld a,(temp_attribute)
			and 0xc7						; strip out previous paper colour
			or l
			ld (temp_attribute),a
			xor a							; expect ordinary char in next call
			ld (next_char_type),a			; to putchar
			exx
			ret

putchar_at
			; move cursor to specified y line (given in l)
			; TODO: check for overflow
			ld h,0x02						; shifted left five times to give 0x40,
											; start of screen memory
			sla l
			sla l
			sla l
			add hl,hl
			add hl,hl						; top two bits of y position go into high byte
			sla h
			sla h
			sla h
			ld (cursor_addr),hl
			ld a,0x17						; next value passed to putchar will provide
			ld (next_char_type),a			; x coordinate
			exx
			ret

putchar_tab
			; move cursor to specified x column (given in l)
			; TODO: check for overflow
			ld e,l
			ld hl,(cursor_addr)
			ld a,l							; get low byte of cursor address
			and 0x1f						; extract current x coordinate
			cp e							; are we already past the destination column?
			jr c,putchar_tab_no_wrap		; if not, don't advance to next line
			rrc h
			rrc h
			rrc h
			ld bc,0x0020
			add hl,bc
			rlc h
			rlc h
			rlc h
putchar_tab_no_wrap
			ld a,l
			and 0xe0						; strip out existing x coordinate
			or e							; merge in new x coordinate
			ld l,a
			ld (cursor_addr),hl
			xor a
			ld (next_char_type),a			; next value passed to putchar is
											; an ordinary character
			exx
			ret

putchar_no_param
			ld a,l							; test whether character is plaintext (ASCII >= 0x20)
			; TODO: handling of more characters outside 0x20 - 0x7F
			cp 0x20
			jr nc,putchar_plain
			cp 0x0d							; is character a newline?
			jr z,putchar_newline
			ld (next_char_type),a			; if not, store control code in
											; next_char_type awaiting parameters
			exx
			ret
			
putchar_newline
			exx								; restore registers (must only use A from here on)
			ld a,(cursor_addr)				; get low byte of cursor
			and 0xe0						; set x coord to 0
			add a,0x20						; increment y coord
			ld (cursor_addr),a
			ret nc							; we're done, unless we moved to next screen third
			ld a,(cursor_addr + 1)
			add a,0x08
			; TODO: handle bottom-of-screen overflow
			ld (cursor_addr + 1),a
			ret
			
putchar_plain
			ld h,0							; convert char to address within font
			add hl,hl
			add hl,hl
			add hl,hl
			ld de,(font_ptr)
			add hl,de
			ld de,(cursor_addr)
			push de
			ld b,8
copy_char_bitmap
			ld a,(hl)
			ld (de),a
			inc d
			inc l
			djnz copy_char_bitmap
			pop hl
			push hl
			; write attribute cell
			ld a,h							; translate cursor to attribute position
			rrca
			rrca
			rrca
			or 0x50
			ld h,a
			ld a,(temp_attribute)			; write current temporary attribute value
			ld (hl),a
			pop hl
			; advance cursor
			rr h
			rr h
			rr h
			inc hl
			rl h
			rl h
			rl h
			; TODO: scroll screen and move cursor to start of line 23
			; when we reach the bottom of the screen (H = 0x58)
			ld (cursor_addr),hl
			exx
			ret
			
; ---------------			
			fillto 0x028e
key_scan
			; scan keyboard, returning key code(s) in DE.
			; Numbering rows B-Sp=0, H-En=1, Y-P=2, 6-0=3, 1-5=4, Q-T=5, A-G=6, Cs-V=7,
			; key code is rownum + ((5 - bit number) << 3).
			; The first key encountered, ordering by descending row and ascending bit
			; within row, is put in E. The second key is put in D.
			; If one of the keys is caps shift, this will be placed in D.
			; Otherwise, if one of the keys is symbol shift, this will be placed in D.
			; The zero flag is returned reset if >2 keys are pressed, or 2 keys are pressed
			; and neither is a shift.
			ld de,0xffff
			ld b,7							; scan each of 8 rows
			ld a,0xfe						; with each bit in turn held low
key_scan_row
			push af
			in a,(0xfe)
			ld c,a							; pick apart result of IN in register C
			ld a,0x20						; count down bit number in A, premultiplied by 8
key_scan_bit
			push af
			rr c
			jr c,key_scan_bit_done			; if bit is nonzero (-> carry set), key not pressed; move on
			add a,b							; assemble key code from bit number and row number
			inc e							; check if e register is vacant
			jr nz,key_scan_e_not_vacant
			ld e,a
			jr key_scan_bit_done
key_scan_e_not_vacant
			dec e							; e is already occupied; restore value
			inc d							; check if d register is vacant
			jr z,key_scan_d_vacant
			pop hl							; if not, there are too many keys;
			pop hl							; restore stack and exit with Z reset
			ret
key_scan_d_vacant
			ld d,a
key_scan_bit_done
			pop af
			sub 8							; if counter in A does not roll over,
			jr nc,key_scan_bit				; there are bits remaining to check
			pop af							; go to next row once we've checked 5 bits
			dec b
			rlca							; keep scanning rows for as long as the zero bit
			jr c,key_scan_row				; doesn't fall off the end of A
			; keys collected; now handle shifts
			ld a,d
			inc a							; see if d is still 0xff (i.e. only one key)
			ret z							; if so, exit with Z set
			ld a,e
			cp 0x27							; check E for caps shift
											; (it's the first key we check, so it'll always
											; be in E if at all)
			jr nz,key_scan_no_cs
			ld e,d							; if E is caps shift, switch D and E
			ld d,a							; and exit with Z set
			ret
key_scan_no_cs
			cp 0x18							; check E for symbol shift
			jr nz,key_scan_no_ss
			ld e,d							; if E is sym shift, switch D and E
			ld d,a							; and exit with Z set
			ret
key_scan_no_ss
			ld a,d							; only remaining valid condition is if D is
			cp 0x18							; symbol shift; check for this condition and
			ret								; return with Z flag indicating the result

; ---------------
; The main Basic interpreter loop
interp_new_line
; This entry point will start interpreting from the line indicated by next_line_ptr,
; advancing next_line_ptr to the new next line in the process.
			ld hl,(next_line_ptr)
			push hl
			ld de,(vars_addr)				; check whether we've hit
			or a							; the end of program memory (indicated by vars_addr)
			sbc hl,de
			pop hl
			jr nc,interp_finished
			; read new line number (back-to-front)
			ld d,(hl)
			inc hl
			ld e,(hl)
			inc hl
			ld (curr_line_num),de
			; read line length
			ld e,(hl)
			inc hl
			ld d,(hl)
			inc hl
			ld (interp_ptr),hl				; this is the next byte to be interpreted
			add hl,de
			ld (next_line_ptr),hl

interp
; This entry point will start interpreting from interp_ptr
			rst nextchar
			cp 0x0d							; if next char a new line, jump to next
			jr z,interp_new_line			; via interp_new_line
			rst consume						; otherwise we consume it
			cp ':'
			jr z,interp						; if it was a colon, do another fetch
			; we expect a statement keyword (>= 0xce) at this point
			sub 0xce
			jr c,interp_invalid				; signal invalid tokens
			ld l,a							; look up keyword in the command vector table
			ld h,0
			add hl,hl
			ld de,command_table
			add hl,de
			ld e,(hl)
			inc hl
			ld d,(hl)
			ld hl,interp					; jump to looked-up command routine
			push hl							; leaving interp on the stack as the return
			push de							; address; individual command routines (e.g. GO TO)
			ret								; will override this as and when required

cmd_stop
; handle STOP statement									
			call nextchar_is_eos			; check syntax first; ensure next char is
			jp nz,fatal_error				; end-of-statement
interp_finished
			rst 0x0008						; report "program finished"
			db 0xff
interp_invalid
			; equivalent to 'nonsense in basic', but we'll do our standard
			; crash-and-burn thing instead to prompt us to do a post-mortem
			; and see if it's actually something valid that I forgot to handle...
			rst fatal_error
			
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
			dw fatal_error	; OUT
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
			dw fatal_error	; PLOT
			dw cmd_run		; RUN
			dw fatal_error	; SAVE
			dw cmd_randomize	; RANDOMIZE
			dw fatal_error	; IF
			dw cmd_cls		; CLS
			dw fatal_error	; DRAW
			dw cmd_clear	; CLEAR
			dw fatal_error	; RETURN
			dw fatal_error	; COPY
			
; ---------------
nextchar_is_eos
			rst nextchar
is_eos
; return z set if A is an end-of-statement character
			cp 0x0d
			ret z
			cp ':'
			ret
; ---------------
consume_bc
; consume a numeric expression from interp_ptr and return it in bc,
; triggering the appropriate error if expression is missing/invalid
; or result is negative or >0xffff
			call get_num_expr
			jp c,fatal_error					; die if expression is missing entirely
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
; triggering the appropriate error if expression is missing/invalid
; or magnitude is >255. (Note: we allow negative values here, although
; calling functions invariably ignore the sign bit...)
			call get_num_expr
			jp c,fatal_error				; die if expression is missing entirely
calc_pop_a_validate
; pop value from calculator stack into a,
; triggering appropriate error if expression is missing/invalid
; or magnitude is >255
			call calc_pop_a
			jp c,err_out_of_range
			ret
			
; ---------------
			fillto 0x0556
load_bytes
			; load bytes from tape
			; DE = length of block
			; A = 0x00 for header, 0xff for data block
			; IX = start address
			; carry = reset for VERIFY, set for LOAD (TODO: implement VERIFY) 
			; Returns carry set on success, reset on failure

			ex af,af'						; when we reach load_bytes_fuse_trap,
											; flag byte should be in af'
			cp a							; set zero flag to ensure the RET NZ is not taken
			jr load_bytes_fuse_trap

			fillto 0x0565
load_bytes_fuse_trap
			ret nz							; FUSE traps a RET NZ instruction
											; at this address for flashloading
			di
											
load_bytes_retry
			ld l,0x00						; initial state of MIC bit
			call load_find_leader
			ld h,0x01						; border = blue / yellow
			call load_get_byte
			jr nc,load_bytes_retry
			ex af,af'						; compare flag byte
			cp c
			jr z,load_flag_ok				; good, it's what we were looking for
			ex af,af'
			jr load_bytes_retry
			; now load real bytes
load_flag_ok
			ex af,af'						; keep flag byte as the initial value of
											; the checksum
			ld a,d
			or e
			jr z,load_bytes_loaded			; test if we've loaded all bytes
			call load_get_byte
			ret nc
			ld (ix),c						; store byte in memory
			inc ix
			ex af,af'
			xor c							; update checksum
			dec de
			jr load_flag_ok					; go round for next byte
load_bytes_loaded
			; get checksum byte
			call load_get_byte
			ret nc
			ex af,af'
			xor c							; should xor to 0 if checksum is correct
			ret nz							; if checksum doesn't match, return failure
			scf								; otherwise set carry to indicate success
			ret

			fillto 0x05e2
			ret								; FUSE returns from tape loading trap
											; at this address

; if return value from load_get_edge is less than load_zero_threshold, consider it a 0 bit;
; otherwise consider it a 1 bit (unless it's waaay longer)
load_zero_threshold equ 0x1e
load_one_threshold equ load_zero_threshold * 2

; Read one edge from tape
; enter with h = border colour, l = previous level in bit 6
; returns with b = loops counted before edge found (counting down from load_one_threshold),
; c flag reset if timed out
load_get_edge
			ld b,load_one_threshold
load_get_edge_lp
			dec b
			jr z,load_fail
			in a,(0xfe)
			and 0x40
			cp l							; keep sampling until signal differs from
			jr z,load_get_edge_lp			; previous level, kept in e
			ld l,a
			sbc a,a							; set a to either 0xff or 0x00 depending on carry
			and 0x0f
			xor h							; create loading bar colour
			out (0xfe),a
			scf
			ret

load_fail
			or a							; reset carry flag
			ret

; Keep polling for a leader tone and only return when we've successfully reached
; the end of one and are ready to start reading data
load_find_leader
			ld l,0x00
load_leader_retry
			ld c,0							; counter of successful leader pulses
			ld h,0x02						; red/cyan border
load_find_leader_lp2
			call load_get_edge
			jr nc,load_leader_retry			; timeout; reset pulse counter
			ld a,b
			cp load_zero_threshold
			jr nc,load_leader_found_zero	; found a zero bit; skip ahead to test
											; for end of leader
			inc c							; increment pulse counter
			jr nz,load_find_leader_lp2		; but don't let it get past 255
			dec c
			jr load_find_leader_lp2
load_leader_found_zero
			bit 7,c							; did we count at least 0x80 good pulses?
			jr z, load_leader_retry			; retry if not
			call load_get_edge				; expect a second zero pulse
			ld a,b
			cp load_zero_threshold
			jr c,load_leader_retry			; fail if pulse was too long for a zero
			ret
			
; fetch one byte from tape, and return in c. Return carry flag set if fetch succeeded
load_get_byte
			ld c,0x01						; set to 1 so that we can stop when
											; shifting causes a carry
load_get_byte_lp
			call load_get_edge
			jr nc,load_fail
			; throw away the result because we only need to work with one pulse,
			; rather than the pair. TODO: Find out whether this is a really evil
			; thing to do, and do something nicer if so
			call load_get_edge
			jr nc,load_fail
			ld a,b
			cp load_zero_threshold			; carry set for a one pulse
			rl c
			ret c
			jr load_get_byte_lp
			
; enter with ix=start of tape header structure; print filetype and filename
describe_tape_file
			ld a,(ix+0)
			ld de,program_text
			ld bc,program_text_end - program_text
			or a
			jr z,put_tape_message
			ld de,num_array_text
			ld bc,num_array_text_end - num_array_text			
			dec a
			jr z,put_tape_message
			ld de,char_array_text
			ld bc,char_array_text_end - char_array_text			
			dec a
			jr z,put_tape_message
			ld de,bytes_text
			ld bc,bytes_text_end - bytes_text			
			dec a
			ret nz							; return if file type is unrecognised
put_tape_message
			call print_string
			push ix							; now print filename - 10 bytes from (ix+1)
			pop de
			inc de
			ld bc,10
			jp print_string

program_text
			db 0x0d,"Program: " ; always starts on a new line
program_text_end
num_array_text
			db 0x0d,"Number array: "
num_array_text_end
char_array_text
			db 0x0d,"Character array: "
char_array_text_end
bytes_text
			db 0x0d,"Bytes: "
bytes_text_end
			
; ---------------			
			fillto 0x0d6b
			jr clear_screen					; Another entry point to clear_screen;
											; this one would normally perform additional
											; housekeeping tasks which we don't bother
											; with here yet
			
cmd_cls
			call nextchar_is_eos			; check syntax first; ensure next char is
			jp nz,fatal_error				; end-of-statement
			jr clear_screen
			
			fillto 0x0daf
clear_screen
			ld hl,screen					; clear screen
			ld (cursor_addr),hl				; set cursor to start of screen
			ld de,screen + 1
			ld bc,attributes - screen
			ld (hl),l
			ldir
			ld a,(perm_attribute)			; get permanent attribute value
			ld (temp_attribute),a			; copy to temporary value for putchar
			ld (hl),a						; and clear screen to those colours
			ld bc,attributes_end - attributes - 1
			ldir
			ret
			
; ---------------			
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
			ld a,0x0d
			rst 0x0010						; output leading newline
print_err_lp
			ld a,(de)
			or a
			jr z,print_err_done				; end loop if byte is zero
			rst 0x0010
			inc de
			jr print_err_lp
print_err_done
			ld a,2							; border red to signal error
			out (254),a
print_err_done_lp
			jr print_err_done_lp			; halt permanently

; table of error offsets
err_table
			dw report_0, 0, 0, 0, 0, 0, 0, 0
			dw 0, 0, 0, report_b, 0, 0, 0, 0
			dw 0, 0, 0, 0, 0, 0, 0, 0
			dw 0, 0, 0, report_r

report_0	db "Program finished", 0	; also used for report 9 (STOP statement)
report_b	db "Out of range", 0	; also used for report K (Invalid colour)
									; and M (Ramtop no good)
report_r	db "Loading error! :-(", 0

; ---------------
skip_whitespace
; Advance interp_ptr past any ignorable characters: space and control codes other than 0x0d

			push af
			ld hl,(interp_ptr)
skip_whitespace_lp
			ld a,(hl)
			cp 0x0d
			jr z,skip_whitespace_done
			cp 0x21
			jr nc,skip_whitespace_done	; code >= 0x21, so OK to return

			cp 0x10
			jr c,skip_whitespace_1	; codes <0x10 have no params, so just skip one byte
			cp 0x18
			jr nc,skip_whitespace_1	; codes >0x18 have no params, so just skip one byte
			cp 0x16
			jr nz,skip_whitespace_2	; all but 0x16 have 1 param, so skip 2 bytes for those
			inc hl					; 0x16 (AT) has 2 params; 3 bytes in total
skip_whitespace_1
			inc hl
skip_whitespace_2
			inc hl
			jr skip_whitespace_lp
			
skip_whitespace_done
			ld (interp_ptr),hl
			pop af
			ret

; ---------------
get_num_expr
; read a number expression from interp_ptr and leave it pushed on the calculator stack
; Return carry set if the numexpr is absent
			call get_num_expr_2
			ret c
search_num_or
			rst nextchar
			cp 0xc5						; look for an OR op
			jr z,num_or
			or a						; if none found, return success (carry reset)
			ret

num_or
; handle the rest of the OR expression
			rst consume					; consume the OR token
			call get_num_expr_2			; get right operand
			jp c,fatal_error			; syntax error if no num_expr found
			; FIXME: perform OR op here
			jr search_num_or			; return to look for further ORs
			
get_num_expr_2
; read a level 2 numexpr - one containing anything above OR in the order of precedence
			call get_num_expr_4
			ret c
search_num_and
			rst nextchar
			cp 0xc6		; AND
			jr z,num_and				; if there's no AND, we're done
			or a
			ret
			
num_and
			rst consume					; consume the AND token
			call get_num_expr_4			; get right operand
			jp c,fatal_error			; syntax error if no num_expr found
			; FIXME: perform AND op here
			jr search_num_and			; look for additional ANDs

; get_num_expr_3
; - this space unintentionally left blank
; (I thought NOT went here, but in fact that's at the top of the order of precedence
; so that we can have COS NOT 0 = COS (NOT 0) without COS 2^2 becoming COS (2^2).
; Confusingly though, NOT's argument can contain operations of lower precedence -
; e.g. COS NOT 1+1 = COS NOT (1+1) )


			
get_num_expr_4
; read a level 4 numexpr - one containing anything above AND in the order of precedence
			call get_num_expr_5
			ret c
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
			or a
			ret
			
num_equals
			call get_comparator_operand
			; FIXME: perform = op here
			jr search_num_comparator
num_less_than
			call get_comparator_operand
			; FIXME: perform < op here
			jr search_num_comparator
num_greater_than
			call get_comparator_operand
			; FIXME: perform > op here
			jr search_num_comparator
num_less_equals
			call get_comparator_operand
			; FIXME: perform <= op here
			jr search_num_comparator
num_greater_equals
			call get_comparator_operand
			; FIXME: perform >= op here
			jr search_num_comparator
num_not_equal
			call get_comparator_operand
			; FIXME: perform <> op here
			jr search_num_comparator

get_comparator_operand
			rst consume
			call get_num_expr_5
			ret nc
			rst fatal_error

get_num_expr_5
; read a level 5 numexpr - one containing anything above comparators in the order of precedence
			call get_num_expr_6
			ret c
search_num_sum_op
			rst nextchar
			cp '+'
			jr z,num_plus
			cp '-'
			jr z,num_minus
			or a
			ret
num_plus
			rst consume
			call get_num_expr_6
			jp c,fatal_error
			; FIXME: perform + op here
			jr search_num_sum_op
			
num_minus
			rst consume
			call get_num_expr_6
			jp c,fatal_error
			; FIXME: perform - op here
			jr search_num_sum_op

get_num_expr_6
; read a level 6 numexpr - one containing anything above binary +/- in the order of precedence
			call get_num_expr_7
			ret c
search_num_product
			rst nextchar
			cp '*'
			jr z,num_multiply
			cp '/'
			jr z,num_divide
			or a
			ret
num_multiply
			rst consume
			call get_num_expr_7
			jp c,fatal_error
			; FIXME: perform * op here
			jr search_num_product
num_divide
			rst consume
			call get_num_expr_7
			jp c,fatal_error
			; FIXME: perform / op here
			jr search_num_product
			
get_num_expr_7
; read a level 7 numexpr - one containing anything above * and / in the order of precedence
			call get_num_expr_8
			ret c
search_num_power
			rst nextchar
			cp '^'
			jr z,num_power
			or a
			ret
num_power
			rst consume
			call get_num_expr_8
			jp c,fatal_error
			; FIXME: perform ^ op here
			jr search_num_power
			
get_num_unary_plus
			rst consume				; cunning tail recursion, since unary plus is a no-op
get_num_expr_8
; read a level 8 expr - one above ^ in the order of precedence
			rst nextchar
			cp '('					; Look for bracketed expressions
			jr z,get_num_bracketed
			cp '-'
			jr z,get_num_unary_minus
			cp '+'
			jr z,get_num_unary_plus
			cp '.'					; Look for characters '.' or 0-9
			jr z,get_num_literal	; which signal an upcoming number literal
				; (in fact so does BIN, but we catch that later on through
				; the function vector table)
			cp '0'
			jp c,fatal_error		; anything else below '0' is invalid
			cp '9'+1
			jr c,get_num_literal
				; TODO: grok variable names
				; For now treat everything else under 0xa5 as a syntax error
			sub 0xa5
			jp c,fatal_error
			cp 0x20					; anything >= 0xc5 is invalid
			jp nc,fatal_error
			ld l,a					; look up code in function vector table
			ld h,0
			add hl,hl
			ld de,num_func_table
			add hl,de
			ld e,(hl)
			inc hl
			ld d,(hl)
			rst consume			; advance past function token
			push de
			ret

get_num_bracketed
			rst consume				; consume left bracket
			call get_num_expr		; evaluate everything within brackets
			rst nextchar			; Next char must be ')', or else
			cp ')'					; it will make baby Jesus cry
			jp nz,fatal_error
			rst consume			; consume right bracket
			or a
			ret
			
get_num_unary_minus
			rst consume
			call get_num_expr_8
			jp c, fatal_error
			; FIXME: perform unary minus op here
			or a
			ret

get_num_literal
; enter with HL = interp_ptr
; find the next embedded 0x0e sequence, pushes it onto the calculator stack
; and advances interp_ptr past it
			ld a,0x0e
get_num_find_0e
			cpi
			jr nz,get_num_find_0e
			ld de,(calc_stack_end)	; copy five bytes after the 0x0e
			ld bc,5					; to the end of the calculator stack
				; TODO: check for out-of-memory condition
			ldir
			ld (calc_stack_end),de
			dec hl				; move back to final byte read, as 'consume' always advances by at least one byte
			ld (interp_ptr),hl
			rst consume		; move to next character, skipping any whitespace
			or a					; signal success
			ret

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
			dw fatal_error			; USR
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
			jp c,fatal_error	; die if no valid numeric expression was found
			rst calc					; perform PEEK operation
			db cc_peek
			db cc_endcalc
			ret
func_in
; IN function
			call get_num_expr_8	; as for func_peek
			jp c,fatal_error
			rst calc
			db cc_in
			db cc_endcalc
			ret

; ---------------
cmd_rem
; process REM command
			pop hl					; just override the usual return to interp
			jp interp_new_line		; and advance to the next line instead

; ---------------
cmd_border
; process BORDER command
			call get_colour_arg
			out (0xfe),a			; change the border colour
				; TODO: save in a system variable to be preserved during BEEP / after LOAD etc
			ret
			
get_colour_arg
; fetch a numeric argument and ensure that it's a valid colour; return it in A
; TODO: handle pseudo-colours 8 and 9 (but not for BORDER)
			call consume_a			; fetch colour argument into a
			jr z,err_out_of_range	; explicitly forbid negative values, because consume_a doesn't
			cp 8								; ensure it's <8
			jr nc,err_out_of_range
			ret
err_out_of_range
			rst error
			db 0x0a					; code for "out of range"

; ---------------
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

; ---------------
cmd_clear
; process CLEAR command
			call vanilla_clear					; do stock CLEAR actions (clear screen and vars)
			call get_num_expr					; look for a numeric argument
			ret c								; return if there isn't one
			call calc_pop_bc_validate		; pop argument into bc, ensuring that it's positive and within 16 bits
			ld hl,(calc_stack_end)				; check that it's somewhere in spare memory
				; TODO: consider making the limit slightly above calc_stack_end, for some
				; breathing room (could do INC H here, but 256 bytes breathing room
				; is probably excessive)
			sbc hl,bc
			jp nc,err_out_of_range				; RAMTOP no good...
			ld h,b
			ld l,c
			ld (ramtop),hl						; save new RAMTOP
			ld sp,hl							; relocate stack
			jp interp							; and return to interpreter
												; (without doing a RET)

vanilla_clear
; perform the actions of a parameterless CLEAR: clear the screen and delete variables.
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
			call get_num_expr
			jr c,run_no_arg						; if none found, go to line 0
			call calc_pop_bc_validate	; if found, retrieve it into bc, ensuring it's
			jp goto_bc								; within 0<=bc<=0xffff
run_no_arg
			ld bc,0
			jp goto_bc

; ---------------
cmd_randomize
; process RANDOMIZE command
			call get_num_expr					; look for numeric argument
			jr c,randomize_frames				; if none supplied, use FRAMES
			call calc_pop_bc_validate		; if one supplied, check it's within 0<=bc<=0xffff
			ld a,b								; check if it's 0
			or c
			jr nz,randomize_bc					; if not, use that as our seed
randomize_frames
			ld bc,(frames)
randomize_bc
			ld (rand_seed),bc
			ret
			
; ---------------
cmd_poke
; process POKE command
			call consume_bc						; fetch address into bc
			push bc
			rst nextchar							; confirm that next char is a comma
			cp ','
			jp nz,fatal_error
			rst consume								; consume the comma
			call consume_a						; fetch byte argument
			pop hl
			ld (hl),a									; perform the poke
			ret
			
; ---------------
			fillto 0x203c
print_string
; print bc characters starting from address de
			ld a,b
			or c
			dec bc
			ret z
			ld a,(de)
			inc de
			rst 0x0010
			jr print_string

; ---------------
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
			dw fatal_error	; 2d = usr_n
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
			
; ---------------
splash_text
			include "splash_screen.asm"
splash_text_end
loading_text
			db "Loading..."
loading_text_end

; ---------------			
			fillto 0x3d00
font
			incbin "clairsys.bin"
			
screen		equ 0x4000
attributes	equ 0x5800
attributes_end	equ 0x5b00

cursor_addr	equ 0x5b00		; Screen address at which next character will be displayed
next_char_type	equ 0x5b02	; how to interpret next character received by RST 0x0010:
							; 00 = ordinary character
							; 10, 11, 12, 13 = ink, paper, flash, bright value
							; 16 = y coordinate (PRINT AT)
							; 17 = x coordinate (PRINT TAB)
font_ptr		equ 0x5c36	; pointer to font bitmap (minus 0x0100 bytes)
curr_line_num	equ 0x5c45	; current line number
vars_addr		equ 0x5c4b	; pointer to start of variables table
next_line_ptr	equ 0x5c55	; pointer to first (header) byte of next program line
interp_ptr		equ 0x5c5d	; pointer to bit of program currently being executed
calc_stack		equ 0x5c63	; pointer to start of calculator stack
calc_stack_end	equ 0x5c65	; pointer to first unused byte after the calculator stack
								; (and start of spare memory)
rand_seed		equ 0x5c76	; seed for random number generator
frames			equ 0x5c78	; 3-byte frame counter (lowest byte first)
perm_attribute	equ 0x5c8d	; attribute value for global use (e.g. in CLS)
temp_attribute	equ 0x5c8f	; attribute value for temporary use in putchar
ramtop			equ 0x5cb2	; where to reset stack pointer to on NEW
prog_mem		equ 0x5ccb	; traditional start of BASIC program memory
udg_mem			equ 0xff58	; traditional start of user-defined graphic bitmaps