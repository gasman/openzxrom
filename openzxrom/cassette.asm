; cassette.asm: Cassette handling routines
; From the Open82 project
; Copyright (c) 2008 Matthew Westcott
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

	fillto 0x053f
	jp load_bytes_exit	; entry point to a routine equivalent to load_bytes_exit,
		; used by Behind Closed Doors and Juggernaut

	fillto 0x0556
load_bytes
		; load bytes from tape
		; DE = length of block
		; A = 0x00 for header, 0xff for data block
		; IX = start address
		; carry = reset for VERIFY, set for LOAD (TODO: implement VERIFY)
		; Returns carry set on success, reset on failure

	ex af,af'	; when we reach load_bytes_fuse_trap,
		; flag byte should be in af'
	cp a	; set zero flag to ensure the RET NZ is not taken
	jr load_bytes_fuse_trap

	fillto 0x0565
load_bytes_fuse_trap
	ret nz	; FUSE traps a RET NZ instruction
		; at this address for flashloading
	di

load_bytes_retry
	ld l,0x00	; initial state of MIC bit
	call load_find_leader
	ld h,0x01	; border = blue / yellow
	call load_get_byte
	jr nc,load_bytes_retry
	ex af,af'	; compare flag byte
	cp c
	jr z,load_flag_ok	; good, it's what we were looking for
	ex af,af'
	jr load_bytes_retry
		; now load real bytes
load_flag_ok
	ex af,af'	; keep flag byte as the initial value of
		; the checksum
	ld a,d
	or e
	jr z,load_bytes_loaded	; test if we've loaded all bytes
	call load_get_byte
	jr nc,load_bytes_exit
	ld (ix),c	; store byte in memory
	inc ix
	ex af,af'
	xor c	; update checksum
	dec de
	jr load_flag_ok	; go round for next byte
load_bytes_loaded
		; get checksum byte
	call load_get_byte
	jr nc,load_bytes_exit
	ex af,af'
	xor c	; should xor to 0 if checksum is correct
	jr nz,load_bytes_exit	; if checksum doesn't match, return failure
	scf	; otherwise set carry to indicate success
; common exit route, regardless of success or failure
load_bytes_exit
; restore border colour and re-enable interrupts
	ex af,af'	; preserve flags
	ld a,(border_colour)	; restore border colour
	srl a
	srl a
	srl a
	out (0xfe),a
	ex af,af'	; restore flags
	ei
	ret

	fillto 0x05e2
	ret	; FUSE returns from tape loading trap
		; at this address

; if return value from load_get_edge is less than load_zero_threshold, consider it a 0 bit;
; otherwise consider it a 1 bit (unless it's waaay longer)
load_zero_threshold	equ 0x1e
load_one_threshold	equ load_zero_threshold * 2

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
	cp l	; keep sampling until signal differs from
	jr z,load_get_edge_lp	; previous level, kept in e
	ld l,a
	sbc a,a	; set a to either 0xff or 0x00 depending on carry
	and 0x0f
	xor h	; create loading bar colour
	out (0xfe),a
	scf
	ret

load_fail
	or a	; reset carry flag
	ret

; Keep polling for a leader tone and only return when we've successfully reached
; the end of one and are ready to start reading data
load_find_leader
	ld l,0x00
load_leader_retry
	ld c,0	; counter of successful leader pulses
	ld h,0x02	; red/cyan border
load_find_leader_lp2
	call load_get_edge
	jr nc,load_leader_retry	; timeout; reset pulse counter
	ld a,b
	cp load_zero_threshold
	jr nc,load_leader_found_zero	; found a zero bit; skip ahead to test
		; for end of leader
	inc c	; increment pulse counter
	jr nz,load_find_leader_lp2	; but don't let it get past 255
	dec c
	jr load_find_leader_lp2
load_leader_found_zero
	bit 7,c	; did we count at least 0x80 good pulses?
	jr z, load_leader_retry	; retry if not
	call load_get_edge	; expect a second zero pulse
	ld a,b
	cp load_zero_threshold
	jr c,load_leader_retry	; fail if pulse was too long for a zero
	ret

; fetch one byte from tape, and return in c. Return carry flag set if fetch succeeded
load_get_byte
	ld c,0x01	; set to 1 so that we can stop when
		; shifting causes a carry
load_get_byte_lp
	call load_get_edge
	jr nc,load_fail
		; throw away the result because we only need to work with one pulse,
		; rather than the pair. (As I understand it, pulses are only paired up
		; to ensure that the signal is electrically centred at 0V, rather than
		; for any data integrity reason - so this is an okay thing to do)
	call load_get_edge
	jr nc,load_fail
	ld a,b
	cp load_zero_threshold	; carry set for a one pulse
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
	ret nz	; return if file type is unrecognised
put_tape_message
	call print_string_permanent
	push ix	; now print filename - 10 bytes from (ix+1)
	pop de
	inc de
	ld bc,10
	jp print_string

; enter with ix = pointer to a 2*17 byte buffer containing space to load
; an 'actual' tape header, followed by a prefilled 'expected' tape header.
; Return (with IX unchanged) after we have encountered a tape header which matches
; the 'expected' header up to the first 11 bytes, or up to the first 0xff, whichever
; is sooner. (11 bytes = file type identifier + 10-char filename)
search_tape_header
	push ix	; store start address of buffer
	ld de,0x0011	; load 0x11 bytes
	xor a	; a = 0x00 to load header
	scf	; carry set = load (not verify)
	call load_bytes	; load the header
	pop ix
	jr nc,search_tape_header	; resume search if loading error occurred
	call describe_tape_file	; print header descriptor

	push ix	; point HL to actual header, DE to expected one
	pop de
	ld hl,0x0011
	add hl,de
	ex de,hl

	ld b,0x0b	; compare buffers for 0x0b bytes
compare_tape_header
	ld a,(de)
	cp 0xff	; byte 0xff = stop comparing, consider it a match
	jr z,tape_header_matched	; found match, so jump to 'success' routine
	cp (hl)
	jr nz,search_tape_header	; if match fails, jump back to search for next
	inc hl	; continue to next byte
	inc de
	djnz compare_tape_header
		; if we get here, full header has matched
tape_header_matched
	ld a,0x0d	; output a newline
	rst putchar
	ret	; return successful match

program_text
	db 0x0d,"Program: "	; always starts on a new line
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
