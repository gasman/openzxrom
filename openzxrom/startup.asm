; startup.asm: actions to perform on system startup
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

cmd_new
; handle NEW command
			call assert_eos			; assert that an end-of-statement follows the NEW token; die if not
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

loading_text
			db "Loading..."
loading_text_end
