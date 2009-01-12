; restarts.asm: Routines to be called by the Z80's restart and interrupt features
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

	org 0x0000
; RST 0x0000: here's where it all begins
	di
	im 1
		; don't re-enable interrupts until we're sure that SP is somewhere sensible
	jr cold_start

	fillto 0x0008
; RST 0x0008: output error message
error
	pop hl	; Fetch code from the address immediately
	ld a,(hl)	; following the call to this routine
	jp print_err	; NB We throw away the return address

	fillto 0x0010
; RST 0x0010: Output the character specified in A
putchar	jp putchar_main

	fillto 0x0018
; RST 0x0018: return next char to be interpreted
nextchar	ld hl,(interp_ptr)
	ld a,(hl)
	ret

	fillto 0x0020
; RST 0x0020: consume next character of program
; (i.e. advance to next non-ignorable character; ignorable = space and control codes)
consume	jp consume_main


; === begin ZX81 ROM excerpt ===

; ---------------------------------------
; THE 'FLOATING POINT CALCULATOR' RESTART
; ---------------------------------------
; this restart jumps to the recursive floating-point calculator.
; the ZX81's internal, FORTH-like, stack-based language.
;
; In the five remaining bytes there is, appropriately, enough room for the
; end-calc literal - the instruction which exits the calculator.

	fillto 0x0028
;; FP-CALC
fp_calc
	jp calculate	; jump immediately to the CALCULATE routine.

; ---

;; end-calc
end_calc
	pop af	; drop the calculator return address RE-ENTRY
	exx	; switch to the other set.

	ex (sp),hl	; transfer H'L' to machine stack for the
		; return address.
		; when exiting recursion then the previous
		; pointer is transferred to H'L'.

	exx	; back to main set.
	ret	; return.

; === end ZX81 ROM excerpt ===

	fillto 0x0030
; Arrive here if we execute any undefined area of the ROM
fatal_error
	di
	pop hl	; recover return address
	dec hl	; decrement to give the location of the fatal RST 0x0030 call
	jp error_here_be_dragons	; jump to error reporting routine

	fillto 0x0038
; RST 0x0038: IM 1 interrupt service routine
	push af
	push bc
	push de
	push hl
	ld hl,frames
	inc (hl)	; increment frames system variable,
	jr nz,done_frames	; overflowing into second and third
	inc l	; byte as necessary
	inc (hl)
	jr nz,done_frames
	inc l
	inc (hl)
done_frames
		; perform key scan
		; TODO: handle key debouncing / repeat
	call key_scan
	jr nz,int_key_scan_fail	; reject scan if zero flag reset (= invalid multiple-key combination)
	call key_test
	jr nc,int_key_scan_fail	; reject scan if carry flag reset (= no meaningful key, just a shift or nothing)
	ld e,a
	call key_code	; convert to ASCII code
		; NB when we implement keyboard modes besides C/L, we'll need to call key_code
		; with appropriate mode flags filled in C and D registers (see key_code in keyboard.asm)
	ld (last_key),a	; store result in last_key system variable
	ld hl,flags	; and indicate that a new key is ready in bit 5 of FLAGS
	set 5,(hl)
int_key_scan_fail
	pop hl
	pop de
	pop bc
	pop af
	ei
	ret
