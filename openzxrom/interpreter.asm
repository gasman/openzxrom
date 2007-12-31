; interpreter.asm: BASIC interpreter loop
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
			call skip_whitespace
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
			call assert_eos			; assert that an end-of-statement follows the STOP token; die if not
interp_finished
			rst error						; report "program finished"
			db 0xff
interp_invalid
			; equivalent to 'nonsense in basic', but we'll do our standard
			; crash-and-burn thing instead to prompt us to do a post-mortem
			; and see if it's actually something valid that I forgot to handle...
			call syntax_error
			
			
; ---------------
assert_eos
; assert that the next character at interp_ptr is an end-of-statement character; die if not
			call nextchar_is_eos
			ret z
			jp nz,syntax_error	; JP rather than CALL so that diagnostics will report the address
													; assert_eos was called from

nextchar_is_eos
			rst nextchar
is_eos
; return z set if A is an end-of-statement character
			cp 0x0d
			ret z
			cp ':'
			ret

; ---------------
; consume next character of program (called from RST 0x0020)
; (i.e. advance to next non-ignorable character; ignorable = space and control codes)
consume_main
			ld hl,(interp_ptr)
			inc hl
			ld (interp_ptr),hl
skip_whitespace
; Advance interp_ptr past any ignorable characters: space and control codes other than 0x0d and 0x0e
			push af
			ld hl,(interp_ptr)
skip_whitespace_lp
			ld a,(hl)
			cp 0x0d
			jr z,skip_whitespace_done
			cp 0x0e
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
skip_whitespace_2
			inc hl
skip_whitespace_1
			inc hl
			jr skip_whitespace_lp
			
skip_whitespace_done
			ld (interp_ptr),hl
			pop af
			ret
