; calculator.asm: Floating-point calculator routines (from the ZX81 ROM)
; From the OpenZXRom project
; Copyright (c) 1981 Nine Tiles Networks Ltd
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
; E-mail: webenq [at] ninetiles.com
; Postal address: The Hall, 9 Station Road, Waterbeach,
;                 Cambridge, CB25 9HT, England

	fillto 0x2ab6
;; STK-STO-$
stk_store:
	PUSH BC	;
	CALL test_5_sp	; routine TEST-5-SP
	POP BC	;
	LD HL,(stkend)	; sv STKEND
	LD (HL),A	;
	INC HL	;
	LD (HL),E	;
	INC HL	;
	LD (HL),D	;
	INC HL	;
	LD (HL),C	;
	INC HL	;
	LD (HL),B	;
	INC HL	;
	LD (stkend),HL	; sv STKEND
	RES 6,(IY+iy_flags)	; update FLAGS - signal string result
	RET	; return.

; --------------------------
; THE 'TEST ROOM' SUBROUTINE
; --------------------------
; Tests that there are at least 'BC' bytes of spare memory; this is considered
; to be the case if we do not hit address 0x0000, or 0x0024 bytes below (above?)
; SP. (MW)

;; TEST-ROOM
test_room:
	LD HL,(stkend)	; sv STKEND_lo
	ADD HL,BC	;
	JR C,err_out_of_memory	; to REPORT-4

	EX DE,HL	;
	LD HL,$0024	;
	ADD HL,DE	;
	SBC HL,SP	;
	RET C	;

;; REPORT-4
err_out_of_memory
	RST error
	DB err_code_out_of_memory


; --------------------------
; THE 'STK-FETCH' SUBROUTINE
; --------------------------
; This routine fetches a five-byte value from the calculator stack
; reducing the pointer to the end of the stack by five.
; For a floating-point number the exponent is in A and the mantissa
; is the thirty-two bits EDCB.
; For strings, the start of the string is in DE and the length in BC.
; A is unused.

	fillto 0x2bf1
;; STK-FETCH
stk_fetch:
	LD HL,(stkend)	; load HL from system variable STKEND

	DEC HL	;
	LD B,(HL)	;
	DEC HL	;
	LD C,(HL)	;
	DEC HL	;
	LD D,(HL)	;
	DEC HL	;
	LD E,(HL)	;
	DEC HL	;
	LD A,(HL)	;

	LD (stkend),HL	; set system variable STKEND to lower value.
	RET	; return.

; ------------------------
; THE 'STACK-A' SUBROUTINE
; ------------------------
;
	fillto 0x2d28
;; STACK-A
stack_a:
	LD C,A	;
	LD B,$00	;

; -------------------------
; THE 'STACK-BC' SUBROUTINE
; -------------------------
; - rewritten for OpenZXRom
; Here we take advantage of the Spectrum's special-case notation for integers
; under 65536 and simply push BC in integer form.

; Calculator operation routines frequently exit via stack_bc, so
; we need to ensure that it satisfies the postconditions of a calculator
; operation: DE points to the new stkend and HL points to the result.


	fillto 0x2d2b
;; STACK-BC
stack_bc:
	LD IY,sysvars_base	; re-initialize the system variables pointer.
	PUSH BC	; save the integer value.

; now stack zero, five zero bytes as a starting point.
; This also serves to place DE and HL into their desired final positions.
	RST 28H	;; FP-CALC
	DEFB cc_stk_zero	;;stk-zero                      0.
	DEFB cc_end_calc	;;end-calc

	POP BC	; restore integer value.
	INC HL
	INC HL
	LD (HL),C	; write BC to 3rd and 4th bytes of the five
	INC HL
	LD (HL),B
	DEC HL	; return HL to point to the exponent byte
	DEC HL
	DEC HL
	RET

; Convert two five-byte values (at DE and HL) to floating point
conv_two_to_fp:
	PUSH HL
	PUSH DE
	CALL conv_to_fp	; convert value at HL
	POP HL	; now HL = old DE
	CALL conv_to_fp	; convert value at old DE
	POP DE	; now DE = old HL
	EX DE,HL	; re-switch DE and HL
	RET

; ---------------------------------------
; THE 'CONVERT TO FLOATING POINT' ROUTINE
; ---------------------------------------
; - new for OpenZXRom (not required on ZX81)
; Examines the five-byte value at address HL, and if it is in integer form,
; converts it to floating point.

conv_to_fp:
	LD A,(HL)
	OR A	; exit if first byte is nonzero -
	RET NZ	; i.e. already floating point

	INC HL	; read sign byte into E and value into BC,
	LD E,(HL)	; resetting to zero as we go
	LD (HL),A
	INC HL
	LD C,(HL)
	LD (HL),A
	INC HL
	LD B,(HL)
	LD (HL),A
	DEC HL	; boldly assume that the last byte of the
	DEC HL	; five is already zero
	DEC HL
	CALL get_bc_magnitude	; test sign byte and negate BC to obtain
		; absolute value if necessary
	EX AF,AF'	; store state of Z flag (reset if negative)

; Following code is repurposed from the ZX81's STACK-BC routine, which had to do
; the floating-point conversion because it didn't implement the special case for
; integers. Continuing with HL pointing to the first byte of the five, BC
; containing the integer magnitude:

	LD (HL),$91	; place $91 in exponent         65536.
		; this is the maximum possible value

	LD A,B	; fetch hi-byte.
	AND A	; test for zero.
	JR NZ,stk_bc_2	; forward if not zero to STK-BC-2

	LD (HL),A	; else make exponent zero again
	OR C	; test lo-byte
	RET Z	; return if BC was zero - done.

; else  there has to be a set bit if only the value one.

	LD B,C	; save C in B.
	LD C,(HL)	; fetch zero to C
	LD (HL),$89	; make exponent $89             256.

;; STK-BC-2
stk_bc_2:
	DEC (HL)	; decrement exponent - halving number
	SLA C	;  C<-76543210<-0
	RL B	;  C<-76543210<-C
	JR NC,stk_bc_2	; loop back if no carry to STK-BC-2

	SRL B	;  0->76543210->C
	RR C	;  C->76543210->C

	INC HL	; address first byte of mantissa
	EX AF,AF'	; recall Z flag for original sign bit
	JR Z,conv_fp_positive
	SET 7,B	; set top bit of B if negative
conv_fp_positive:
	LD (HL),B	; insert B
	INC HL	; address second byte of mantissa
	LD (HL),C	; insert C

	DEC HL	; point to the
	DEC HL	; exponent again
	RET	; return.


; -------------------------------------
; THE 'FLOATING-POINT TO BC' SUBROUTINE
; -------------------------------------
; The floating-point form on the calculator stack is compressed directly into
; the BC register rounding up if necessary.
; Valid range is 0 to 65535.4999

	fillto 0x2da2
;; FP-TO-BC
fp_to_bc:
	CALL stk_fetch	; routine STK-FETCH - exponent to A
		; mantissa to EDCB.
	AND A	; test for value zero.
	JR NZ,fpbc_nzro	; forward if not to FPBC-NZRO

; else A is zero => number is an integer, stored in EDC
; where E = 0x00 for positive, 0xff for negative.
; (This case was not handled in the ZX81 ROM; additional code begins here)

	ld b,c	; juggle registers to leave 16-bit result in BC
	ld c,d
	call get_bc_magnitude	; Negate BC (and reset zero flag) if E is 0xff,
		; indicating a negative result
	jr fpbc_int_end

; Secondary entry point used by conv_to_fp.
; Enter with A=0. Negates BC and returns with Z flag reset iff E is nonzero.
; Preserves HL
get_bc_magnitude
	cp e	; check sign byte - 0x00 = positive, 0xff = negative
	ret z	; if it's positive, exit with 16-bit value in bc,
		; carry reset (no overflow), zero set (positive)

	xor a	; clear carry
	push hl
	ld hl,0x0000	; negate bc by subtracting from 0x0000
	sbc hl,bc
	ld b,h
	ld c,l
	pop hl
	ccf	; this will always cause a carry unless bc was 0x0000,
		; which equals -65536, which is the one case where we
		; want to return a carry.
	inc a	; reset zero flag to indicate negative result
	ret

; (end integer-handling code)

; ------------------------------------
; THE 'FLOATING-POINT TO A' SUBROUTINE
; ------------------------------------
;
;
	fillto 0x2dd5
;; FP-TO-A
fp_to_a:
	CALL fp_to_bc	; routine FP-TO-BC
	RET C	;

	PUSH AF	;
	DEC B	;
	INC B	;
	JR Z,fp_a_end	; forward if in range to FP-A-END

	POP AF	; fetch result
	SCF	; set carry flag signaling overflow
	RET	; return

;; FP-A-END
fp_a_end:
	POP AF	;
	RET	;

; ---

; EDCB  =>  BCE

;; FPBC-NZRO
fpbc_nzro:
	LD B,E	; transfer the mantissa from EDCB
	LD E,C	; to BCE. Bit 7 of E is the 17th bit which
	LD C,D	; will be significant for rounding if the
		; number is already normalized.

	SUB $91	; subtract 65536
	CCF	; complement carry flag
	BIT 7,B	; test sign bit
	PUSH AF	; push the result

	SET 7,B	; set the implied bit
	JR C,fpbc_end	; forward with carry from SUB/CCF to FPBC-END
		; number is too big.

	INC A	; increment the exponent and
	NEG	; negate to make range $00 - $0F

	CP $08	; test if one or two bytes
	JR C,big_int	; forward with two to BIG-INT

	LD E,C	; shift mantissa
	LD C,B	; 8 places right
	LD B,$00	; insert a zero in B
	SUB $08	; reduce exponent by eight

;; BIG-INT
big_int:
	AND A	; test the exponent
	LD D,A	; save exponent in D.

	LD A,E	; fractional bits to A
	RLCA	; rotate most significant bit to carry for
		; rounding of an already normal number.

	JR Z,exp_zero	; forward if exponent zero to EXP-ZERO
		; the number is normalized

;; FPBC-NORM
fpbc_norm:
	SRL B	;   0->76543210->C
	RR C	;   C->76543210->C

	DEC D	; decrement exponent

	JR NZ,fpbc_norm	; loop back till zero to FPBC-NORM

;; EXP-ZERO
exp_zero:
	JR NC,fpbc_end	; forward without carry to NO-ROUND

	INC BC	; round up.
	LD A,B	; test result
	OR C	; for zero
	JR NZ,fpbc_end	; forward if not to GRE-ZERO

	POP AF	; restore sign flag
	SCF	; set carry flag to indicate overflow
fpbc_int_end:
	PUSH AF	; save combined flags again

;; FPBC-END
fpbc_end:
	PUSH BC	; save BC value

; set HL and DE to calculator stack pointers.

	RST fp_calc	;; FP-CALC
	DEFB cc_end_calc	;;end-calc


	POP BC	; restore BC value
	POP AF	; restore flags
	LD A,C	; copy low byte to A also.
	RET	; return

; -------------------------------
; THE 'PREPARE TO ADD' SUBROUTINE
; -------------------------------
; This routine is called twice to prepare each floating point number for
; addition, in situ, on the calculator stack.
; The exponent is picked up from the first byte which is then cleared to act
; as a sign byte and accept any overflow.
; If the exponent is zero then the number is zero and an early return is made.
; The now redundant sign bit of the mantissa is set and if the number is 
; negative then all five bytes of the number are twos-complemented to prepare 
; the number for addition.
; On the second invocation the exponent of the first number is in B.


;; PREP-ADD
prep_add:
	LD A,(HL)	; fetch exponent.
	LD (HL),$00	; make this byte zero to take any overflow and
		; default to positive.
	AND A	; test stored exponent for zero.
	RET Z	; return with zero flag set if number is zero.

	INC HL	; point to first byte of mantissa.
	BIT 7,(HL)	; test the sign bit.
	SET 7,(HL)	; set it to its implied state.
	DEC HL	; set pointer to first byte again.
	RET Z	; return if bit indicated number is positive.>>

; if negative then all five bytes are twos complemented starting at LSB.

	PUSH BC	; save B register contents.
	LD BC,$0005	; set BC to five.
	ADD HL,BC	; point to location after 5th byte.
	LD B,C	; set the B counter to five.
	LD C,A	; store original exponent in C.
	SCF	; set carry flag so that one is added.

; now enter a loop to twos-complement the number.
; The first of the five bytes becomes $FF to denote a negative number.

;; NEG-BYTE
neg_byte:
	DEC HL	; point to first or more significant byte.
	LD A,(HL)	; fetch to accumulator.
	CPL	; complement.
	ADC A,$00	; add in initial carry or any subsequent carry.
	LD (HL),A	; place number back.
	DJNZ neg_byte	; loop back five times to NEG-BYTE

	LD A,C	; restore the exponent to accumulator.
	POP BC	; restore B register contents.

	RET	; return.

; ----------------------------------
; THE 'FETCH TWO NUMBERS' SUBROUTINE
; ----------------------------------
; This routine is used by addition, multiplication and division to fetch
; the two five-byte numbers addressed by HL and DE from the calculator stack
; into the Z80 registers.
; The HL register may no longer point to the first of the two numbers.
; Since the 32-bit addition operation is accomplished using two Z80 16-bit
; instructions, it is important that the lower two bytes of each mantissa are
; in one set of registers and the other bytes all in the alternate set.
;
; In: HL = highest number, DE= lowest number
;
;         : alt':   :
; Out:    :H,B-C:C,B: num1
;         :L,D-E:D-E: num2

;; FETCH-TWO
fetch_two:
	PUSH HL	; save HL
	PUSH AF	; save A - result sign when used from division.

	LD C,(HL)	;
	INC HL	;
	LD B,(HL)	;
	LD (HL),A	; insert sign when used from multiplication.
	INC HL	;
	LD A,C	; m1
	LD C,(HL)	;
	PUSH BC	; PUSH m2 m3

	INC HL	;
	LD C,(HL)	; m4
	INC HL	;
	LD B,(HL)	; m5  BC holds m5 m4

	EX DE,HL	; make HL point to start of second number.

	LD D,A	; m1
	LD E,(HL)	;
	PUSH DE	; PUSH m1 n1

	INC HL	;
	LD D,(HL)	;
	INC HL	;
	LD E,(HL)	;
	PUSH DE	; PUSH n2 n3

	EXX	; - - - - - - -

	POP DE	; POP n2 n3
	POP HL	; POP m1 n1
	POP BC	; POP m2 m3

	EXX	; - - - - - - -

	INC HL	;
	LD D,(HL)	;
	INC HL	;
	LD E,(HL)	; DE holds n4 n5

	POP AF	; restore saved
	POP HL	; registers.
	RET	; return.

; -----------------------------
; THE 'SHIFT ADDEND' SUBROUTINE
; -----------------------------
; The accumulator A contains the difference between the two exponents.
; This is the lowest of the two numbers to be added 

;; SHIFT-FP
shift_fp:
	AND A	; test difference between exponents.
	RET Z	; return if zero. both normal.

	CP $21	; compare with 33 bits.
	JR NC,addend_0	; forward if greater than 32 to ADDEND-0

	PUSH BC	; preserve BC - part
	LD B,A	; shift counter to B.

; Now perform B right shifts on the addend  L'D'E'D E
; to bring it into line with the augend     H'B'C'C B

;; ONE-SHIFT
one_shift:
	EXX	; - - -
	SRA L	;    76543210->C    bit 7 unchanged.
	RR D	; C->76543210->C
	RR E	; C->76543210->C
	EXX	; - - -
	RR D	; C->76543210->C
	RR E	; C->76543210->C
	DJNZ one_shift	; loop back B times to ONE-SHIFT

	POP BC	; restore BC
	RET NC	; return if last shift produced no carry.   >>

; if carry flag was set then accuracy is being lost so round up the addend.

	CALL add_back	; routine ADD-BACK
	RET NZ	; return if not FF 00 00 00 00

; this branch makes all five bytes of the addend zero and is made during
; addition when the exponents are too far apart for the addend bits to 
; affect the result.

;; ADDEND-0
addend_0:
	EXX	; select alternate set for more significant
		; bytes.
	XOR A	; clear accumulator.


; this entry point (from multiplication) sets four of the bytes to zero or if 
; continuing from above, during addition, then all five bytes are set to zero.

;; ZEROS-4/5
zeros_4_5:
	LD L,$00	; set byte 1 to zero.
	LD D,A	; set byte 2 to A.
	LD E,L	; set byte 3 to zero.
	EXX	; select main set
	LD DE,$0000	; set lower bytes 4 and 5 to zero.
	RET	; return.

; -------------------------
; THE 'ADD-BACK' SUBROUTINE
; -------------------------
; Called from SHIFT-FP above during addition and after normalization from
; multiplication.
; This is really a 32-bit increment routine which sets the zero flag according
; to the 32-bit result.
; During addition, only negative numbers like FF FF FF FF FF,
; the twos-complement version of xx 80 00 00 01 say 
; will result in a full ripple FF 00 00 00 00.
; FF FF FF FF FF when shifted right is unchanged by SHIFT-FP but sets the 
; carry invoking this routine.

;; ADD-BACK
add_back:
	INC E	;
	RET NZ	;

	INC D	;
	RET NZ	;

	EXX	;
	INC E	;
	JR NZ,all_added	; forward if no overflow to ALL-ADDED

	INC D	;

;; ALL-ADDED
all_added:
	EXX	;
	RET	; return with zero flag set for zero mantissa.

; ---------------------------
; THE 'SUBTRACTION' OPERATION
; ---------------------------
; just switch the sign of subtrahend and do an add.

;; subtract
subtract:
	CALL conv_two_to_fp	; ensure both values are floating-point.
	LD A,(DE)	; fetch exponent byte of second number the
		; subtrahend.
	AND A	; test for zero
	RET Z	; return if zero - first number is result.

	INC DE	; address the first mantissa byte.
	LD A,(DE)	; fetch to accumulator.
	XOR $80	; toggle the sign bit.
	LD (DE),A	; place back on calculator stack.
	DEC DE	; point to exponent byte.
	JR addition_fp	; continue into addition routine.

; ------------------------
; THE 'ADDITION' OPERATION
; ------------------------
; The addition operation pulls out all the stops and uses most of the Z80's
; registers to add two floating-point numbers.
; This is a binary operation and on entry, HL points to the first number
; and DE to the second.

;; addition
addition:
	CALL conv_two_to_fp	; ensure both values are floating-point.
addition_fp:
	EXX	; - - -
	PUSH HL	; save the pointer to the next literal.
	EXX	; - - -

	PUSH DE	; save pointer to second number
	PUSH HL	; save pointer to first number - will be the
		; result pointer on calculator stack.

	CALL prep_add	; routine PREP-ADD
	LD B,A	; save first exponent byte in B.
	EX DE,HL	; switch number pointers.
	CALL prep_add	; routine PREP-ADD
	LD C,A	; save second exponent byte in C.
	CP B	; compare the exponent bytes.
	JR NC,shift_len	; forward if second higher to SHIFT-LEN

	LD A,B	; else higher exponent to A
	LD B,C	; lower exponent to B
	EX DE,HL	; switch the number pointers.

;; SHIFT-LEN
shift_len:
	PUSH AF	; save higher exponent
	SUB B	; subtract lower exponent

	CALL fetch_two	; routine FETCH-TWO
	CALL shift_fp	; routine SHIFT-FP

	POP AF	; restore higher exponent.
	POP HL	; restore result pointer.
	LD (HL),A	; insert exponent byte.
	PUSH HL	; save result pointer again.

; now perform the 32-bit addition using two 16-bit Z80 add instructions.

	LD L,B	; transfer low bytes of mantissa individually
	LD H,C	; to HL register

	ADD HL,DE	; the actual binary addition of lower bytes

; now the two higher byte pairs that are in the alternate register sets.

	EXX	; switch in set
	EX DE,HL	; transfer high mantissa bytes to HL register.

	ADC HL,BC	; the actual addition of higher bytes with
		; any carry from first stage.

	EX DE,HL	; result in DE, sign bytes ($FF or $00) to HL

; now consider the two sign bytes

	LD A,H	; fetch sign byte of num1

	ADC A,L	; add including any carry from mantissa
		; addition. 00 or 01 or FE or FF

	LD L,A	; result in L.

; possible outcomes of signs and overflow from mantissa are
;
;  H +  L + carry =  L    RRA  XOR L  RRA
; ------------------------------------------------------------
; 00 + 00         = 00    00   00
; 00 + 00 + carry = 01    00   01     carry
; FF + FF         = FE C  FF   01     carry
; FF + FF + carry = FF C  FF   00
; FF + 00         = FF    FF   00
; FF + 00 + carry = 00 C  80   80

	RRA	; C->76543210->C
	XOR L	; set bit 0 if shifting required.

	EXX	; switch back to main set
	EX DE,HL	; full mantissa result now in D'E'D E registers.
	POP HL	; restore pointer to result exponent on
		; the calculator stack.

	RRA	; has overflow occurred ?
	JR NC,test_neg	; skip forward if not to TEST-NEG

; if the addition of two positive mantissas produced overflow or if the
; addition of two negative mantissas did not then the result exponent has to
; be incremented and the mantissa shifted one place to the right.

	LD A,$01	; one shift required.
	CALL shift_fp	; routine SHIFT-FP performs a single shift
		; rounding any lost bit
	INC (HL)	; increment the exponent.
	JR Z,add_rep_6	; forward to ADD-REP-6 if the exponent
		; wraps round from FF to zero as number is too
		; big for the system.

; at this stage the exponent on the calculator stack is correct.

;; TEST-NEG
test_neg:
	EXX	; switch in the alternate set.
	LD A,L	; load result sign to accumulator.
	AND $80	; isolate bit 7 from sign byte setting zero
		; flag if positive.
	EXX	; back to main set.

	INC HL	; point to first byte of mantissa
	LD (HL),A	; insert $00 positive or $80 negative at
		; position on calculator stack.

	DEC HL	; point to exponent again.
	JR Z,go_nc_mlt	; forward if positive to GO-NC-MLT

; a negative number has to be twos-complemented before being placed on stack.

	LD A,E	; fetch lowest (rightmost) mantissa byte.
	NEG	; Negate
	CCF	; Complement Carry Flag
	LD E,A	; place back in register

	LD A,D	; ditto
	CPL	;
	ADC A,$00	;
	LD D,A	;

	EXX	; switch to higher (leftmost) 16 bits.

	LD A,E	; ditto
	CPL	;
	ADC A,$00	;
	LD E,A	;

	LD A,D	; ditto
	CPL	;
	ADC A,$00	;
	JR NC,end_compl	; forward without overflow to END-COMPL

; else entire mantissa is now zero.  00 00 00 00

	RRA	; set mantissa to 80 00 00 00
	EXX	; switch.
	INC (HL)	; increment the exponent.

;; ADD-REP-6
add_rep_6:
	JP Z,err_number_too_big	; jump forward if exponent now zero to REPORT-6
		; 'Number too big'

	EXX	; switch back to alternate set.

;; END-COMPL
end_compl:
	LD D,A	; put first byte of mantissa back in DE.
	EXX	; switch to main set.

;; GO-NC-MLT
go_nc_mlt:
	XOR A	; clear carry flag and
		; clear accumulator so no extra bits carried
		; forward as occurs in multiplication.

	JR test_norm	; forward to common code at TEST-NORM
		; but should go straight to NORMALIZE.

; ----------------------------------------------
; THE 'PREPARE TO MULTIPLY OR DIVIDE' SUBROUTINE
; ----------------------------------------------
; this routine is called twice from multiplication and twice from division
; to prepare each of the two numbers for the operation.
; Initially the accumulator holds zero and after the second invocation bit 7
; of the accumulator will be the sign bit of the result.

;; PREP-M/D
prep_m_d:
	SCF	; set carry flag to signal number is zero.
	DEC (HL)	; test exponent
	INC (HL)	; for zero.
	RET Z	; return if zero with carry flag set.

	INC HL	; address first mantissa byte.
	XOR (HL)	; exclusive or the running sign bit.
	SET 7,(HL)	; set the implied bit.
	DEC HL	; point to exponent byte.
	RET	; return.

; ------------------------------
; THE 'MULTIPLICATION' OPERATION
; ------------------------------
;
;

;; multiply
multiply:
	CALL conv_two_to_fp	; ensure both values are floating-point.
	XOR A	; reset bit 7 of running sign flag.
	CALL prep_m_d	; routine PREP-M/D
	RET C	; return if number is zero.
		; zero * anything = zero.

	EXX	; - - -
	PUSH HL	; save pointer to 'next literal'
	EXX	; - - -

	PUSH DE	; save pointer to second number

	EX DE,HL	; make HL address second number.

	CALL prep_m_d	; routine PREP-M/D

	EX DE,HL	; HL first number, DE - second number
	JR C,zero_rslt	; forward with carry to ZERO-RSLT
		; anything * zero = zero.

	PUSH HL	; save pointer to first number.

	CALL fetch_two	; routine FETCH-TWO fetches two mantissas from
		; calc stack to B'C'C,B  D'E'D E
		; (HL will be overwritten but the result sign
		; in A is inserted on the calculator stack)

	LD A,B	; transfer low mantissa byte of first number
	AND A	; clear carry.
	SBC HL,HL	; a short form of LD HL,$0000 to take lower
		; two bytes of result. (2 program bytes)
	EXX	; switch in alternate set
	PUSH HL	; preserve HL
	SBC HL,HL	; set HL to zero also to take higher two bytes
		; of the result and clear carry.
	EXX	; switch back.

	LD B,$21	; register B can now be used to count thirty
		; three shifts.
	JR strt_mlt	; forward to loop entry point STRT-MLT

; ---

; The multiplication loop is entered at  STRT-LOOP.

;; MLT-LOOP
mlt_loop:
	JR NC,no_add	; forward if no carry to NO-ADD

		; else add in the multiplicand.

	ADD HL,DE	; add the two low bytes to result
	EXX	; switch to more significant bytes.
	ADC HL,DE	; add high bytes of multiplicand and any carry.
	EXX	; switch to main set.

; in either case shift result right into B'C'C A

;; NO-ADD
no_add:
	EXX	; switch to alternate set
	RR H	; C > 76543210 > C
	RR L	; C > 76543210 > C
	EXX	;
	RR H	; C > 76543210 > C
	RR L	; C > 76543210 > C

;; STRT-MLT
strt_mlt:
	EXX	; switch in alternate set.
	RR B	; C > 76543210 > C
	RR C	; C > 76543210 > C
	EXX	; now main set
	RR C	; C > 76543210 > C
	RRA	; C > 76543210 > C
	DJNZ mlt_loop	; loop back 33 times to MLT-LOOP

;

	EX DE,HL	;
	EXX	;
	EX DE,HL	;
	EXX	;
	POP BC	;
	POP HL	;
	LD A,B	;
	ADD A,C	;
	JR NZ,make_expt	; forward to MAKE-EXPT

	AND A	;

;; MAKE-EXPT
make_expt:
	DEC A	;
	CCF	; Complement Carry Flag

;; DIVN-EXPT
divn_expt:
	RLA	;
	CCF	; Complement Carry Flag
	RRA	;
	JP P,oflw1_clr	; forward to OFLW1-CLR

	JR NC,err_number_too_big	; forward to REPORT-6

	AND A	;

;; OFLW1-CLR
oflw1_clr:
	INC A	;
	JR NZ,oflw2_clr	; forward to OFLW2-CLR

	JR C,oflw2_clr	; forward to OFLW2-CLR

	EXX	;
	BIT 7,D	;
	EXX	;
	JR NZ,err_number_too_big	; forward to REPORT-6

;; OFLW2-CLR
oflw2_clr:
	LD (HL),A	;
	EXX	;
	LD A,B	;
	EXX	;

; addition joins here with carry flag clear.

;; TEST-NORM
test_norm:
	JR NC,normalize	; forward to NORMALIZE

	LD A,(HL)	;
	AND A	;

;; NEAR-ZERO
near_zero:
	LD A,$80	; prepare to rescue the most significant bit
		; of the mantissa if it is set.
	JR Z,skip_zero	; skip forward to SKIP-ZERO

;; ZERO-RSLT
zero_rslt:
	XOR A	; make mask byte zero signaling set five
		; bytes to zero.

;; SKIP-ZERO
skip_zero:
	EXX	; switch in alternate set
	AND D	; isolate most significant bit (if A is $80).

	CALL zeros_4_5	; routine ZEROS-4/5 sets mantissa without
		; affecting any flags.

	RLCA	; test if MSB set. bit 7 goes to bit 0.
		; either $00 -> $00 or $80 -> $01
	LD (HL),A	; make exponent $01 (lowest) or $00 zero
	JR C,oflow_clr	; forward if first case to OFLOW-CLR

	INC HL	; address first mantissa byte on the
		; calculator stack.
	LD (HL),A	; insert a zero for the sign bit.
	DEC HL	; point to zero exponent
	JR oflow_clr	; forward to OFLOW-CLR

; ---

; this branch is common to addition and multiplication with the mantissa
; result still in registers D'E'D E .

;; NORMALIZE
normalize:
	LD B,$20	; a maximum of thirty-two left shifts will be
		; needed.

;; SHIFT-ONE
shift_one:
	EXX	; address higher 16 bits.
	BIT 7,D	; test the leftmost bit
	EXX	; address lower 16 bits.

	JR NZ,norml_now	; forward if leftmost bit was set to NORML-NOW

	RLCA	; this holds zero from addition, 33rd bit
		; from multiplication.

	RL E	; C < 76543210 < C
	RL D	; C < 76543210 < C

	EXX	; address higher 16 bits.

	RL E	; C < 76543210 < C
	RL D	; C < 76543210 < C

	EXX	; switch to main set.

	DEC (HL)	; decrement the exponent byte on the calculator
		; stack.

	JR Z,near_zero	; back if exponent becomes zero to NEAR-ZERO
		; it's just possible that the last rotation
		; set bit 7 of D. We shall see.

	DJNZ shift_one	; loop back to SHIFT-ONE

; if thirty-two left shifts were performed without setting the most significant 
; bit then the result is zero.

	JR zero_rslt	; back to ZERO-RSLT

; ---

;; NORML-NOW
norml_now:
	RLA	; for the addition path, A is always zero.
		; for the mult path, ...

	JR NC,oflow_clr	; forward to OFLOW-CLR

; this branch is taken only with multiplication.

	CALL add_back	; routine ADD-BACK

	JR NZ,oflow_clr	; forward to OFLOW-CLR

	EXX	;
	LD D,$80	;
	EXX	;
	INC (HL)	;
	JR Z,err_number_too_big	; forward to REPORT-6

; now transfer the mantissa from the register sets to the calculator stack
; incorporating the sign bit already there.

;; OFLOW-CLR
oflow_clr:
	PUSH HL	; save pointer to exponent on stack.
	INC HL	; address first byte of mantissa which was
		; previously loaded with sign bit $00 or $80.

	EXX	; - - -
	PUSH DE	; push the most significant two bytes.
	EXX	; - - -

	POP BC	; pop - true mantissa is now BCDE.

; now pick up the sign bit.

	LD A,B	; first mantissa byte to A
	RLA	; rotate out bit 7 which is set
	RL (HL)	; rotate sign bit on stack into carry.
	RRA	; rotate sign bit into bit 7 of mantissa.

; and transfer mantissa from main registers to calculator stack.

	LD (HL),A	;
	INC HL	;
	LD (HL),C	;
	INC HL	;
	LD (HL),D	;
	INC HL	;
	LD (HL),E	;

	POP HL	; restore pointer to num1 now result.
	POP DE	; restore pointer to num2 now STKEND.

	EXX	; - - -
	POP HL	; restore pointer to next calculator literal.
	EXX	; - - -

	RET	; return.

; ---

err_number_too_big
	rst error
	db err_code_number_too_big

; ------------------------
; THE 'DIVISION' OPERATION
; ------------------------
;   "Of all the arithmetic subroutines, division is the most complicated and
;   the least understood.  It is particularly interesting to note that the 
;   Sinclair programmer himself has made a mistake in his programming ( or has
;   copied over someone else's mistake!) for
;   PRINT PEEK 6352 [ $18D0 ] ('unimproved' ROM, 6351 [ $18CF ] )
;   should give 218 not 225."
;   - Dr. Ian Logan, Syntax magazine Jul/Aug 1982.
;   [  i.e. the jump should be made to div-34th ]

;   First check for division by zero.

;; division
division:
	CALL conv_two_to_fp	; ensure both values are floating-point.
	EX DE,HL	; consider the second number first.
	XOR A	; set the running sign flag.
	CALL prep_m_d	; routine PREP-M/D
	JR C,err_number_too_big	; back if zero to REPORT-6
		; 'Arithmetic overflow'

	EX DE,HL	; now prepare first number and check for zero.
	CALL prep_m_d	; routine PREP-M/D
	RET C	; return if zero, 0/anything is zero.

	EXX	; - - -
	PUSH HL	; save pointer to the next calculator literal.
	EXX	; - - -

	PUSH DE	; save pointer to divisor - will be STKEND.
	PUSH HL	; save pointer to dividend - will be result.

	CALL fetch_two	; routine FETCH-TWO fetches the two numbers
		; into the registers H'B'C'C B
		;                    L'D'E'D E
	EXX	; - - -
	PUSH HL	; save the two exponents.

	LD H,B	; transfer the dividend to H'L'H L
	LD L,C	;
	EXX	;
	LD H,C	;
	LD L,B	;

	XOR A	; clear carry bit and accumulator.
	LD B,$DF	; count upwards from -33 decimal
	JR div_start	; forward to mid-loop entry point DIV-START

; ---

;; DIV-LOOP
div_loop:
	RLA	; multiply partial quotient by two
	RL C	; setting result bit from carry.
	EXX	;
	RL C	;
	RL B	;
	EXX	;

;; div-34th
div_34th:
	ADD HL,HL	;
	EXX	;
	ADC HL,HL	;
	EXX	;
	JR C,subn_only	; forward to SUBN-ONLY

;; DIV-START
div_start:
	SBC HL,DE	; subtract divisor part.
	EXX	;
	SBC HL,DE	;
	EXX	;
	JR NC,no_rstore	; forward if subtraction goes to NO-RSTORE

	ADD HL,DE	; else restore
	EXX	;
	ADC HL,DE	;
	EXX	;
	AND A	; clear carry
	JR count_one	; forward to COUNT-ONE

; ---

;; SUBN-ONLY
subn_only:
	AND A	;
	SBC HL,DE	;
	EXX	;
	SBC HL,DE	;
	EXX	;

;; NO-RSTORE
no_rstore:
	SCF	; set carry flag

;; COUNT-ONE
count_one:
	INC B	; increment the counter
	JP M,div_loop	; back while still minus to DIV-LOOP

	PUSH AF	;
	JR Z,div_start	; back to DIV-START

; "This jump is made to the wrong place. No 34th bit will ever be obtained
; without first shifting the dividend. Hence important results like 1/10 and
; 1/1000 are not rounded up as they should be. Rounding up never occurs when
; it depends on the 34th bit. The jump should be made to div-34th above."
; - Dr. Frank O'Hara, "The Complete Spectrum ROM Disassembly", 1983,
; published by Melbourne House.
; (Note. on the ZX81 this would be JR Z,div_34th)
;
; However if you make this change, then while (1/2=.5) will now evaluate as
; true, (.25=1/4), which did evaluate as true, no longer does.

	LD E,A	;
	LD D,C	;
	EXX	;
	LD E,C	;
	LD D,B	;

	POP AF	;
	RR B	;
	POP AF	;
	RR B	;

	EXX	;
	POP BC	;
	POP HL	;
	LD A,B	;
	SUB C	;
	JP divn_expt	; jump back to DIVN-EXPT

; ------------------------------------------------
; THE 'INTEGER TRUNCATION TOWARDS ZERO' SUBROUTINE
; ------------------------------------------------
;
; TODO: translate to integer form, where possible, as machine-code
; programs might expect the result to be in that format

;; truncate
truncate:
	LD A,(HL)	; fetch exponent
	OR A	; if it's 0, this is in integer form
	RET Z	; and therefore there's nothing to do.

	CP $81	; compare to +1
	JR NC,t_gr_zero	; forward, if 1 or more, to T-GR-ZERO

; else the number is smaller than plus or minus 1 and can be made zero.

	LD (HL),$00	; make exponent zero.
	LD A,$20	; prepare to set 32 bits of mantissa to zero.
	JR nil_bytes	; forward to NIL-BYTES

; ---

;; T-GR-ZERO
t_gr_zero:
	SUB $A0	; subtract +32 from exponent
	RET P	; return if result is positive as all 32 bits
		; of the mantissa relate to the integer part.
		; The floating point is somewhere to the right
		; of the mantissa

	NEG	; else negate to form number of rightmost bits
		; to be blanked.

; for instance, disregarding the sign bit, the number 3.5 is held as 
; exponent $82 mantissa .11100000 00000000 00000000 00000000
; we need to set $82 - $A0 = $E2 NEG = $1E (thirty) bits to zero to form the 
; integer.
; The sign of the number is never considered as the first bit of the mantissa
; must be part of the integer.

;; NIL-BYTES
nil_bytes:
	PUSH DE	; save pointer to STKEND
	EX DE,HL	; HL points at STKEND
	DEC HL	; now at last byte of mantissa.
	LD B,A	; Transfer bit count to B register.
	SRL B	; divide by
	SRL B	; eight
	SRL B	;
	JR Z,bits_zero	; forward if zero to BITS-ZERO

; else the original count was eight or more and whole bytes can be blanked.

;; BYTE-ZERO
byte_zero:
	LD (HL),$00	; set eight bits to zero.
	DEC HL	; point to more significant byte of mantissa.
	DJNZ byte_zero	; loop back to BYTE-ZERO

; now consider any residual bits.

;; BITS-ZERO
bits_zero:
	AND $07	; isolate the remaining bits
	JR Z,ix_end	; forward if none to IX-END

	LD B,A	; transfer bit count to B counter.
	LD A,$FF	; form a mask 11111111

;; LESS-MASK
less_mask:
	SLA A	; 1 <- 76543210 <- o     slide mask leftwards.
	DJNZ less_mask	; loop back for bit count to LESS-MASK

	AND (HL)	; lose the unwanted rightmost bits
	LD (HL),A	; and place in mantissa byte.

;; IX-END
ix_end:
	EX DE,HL	; restore result pointer from DE.
	POP DE	; restore STKEND from stack.
	RET	; return.

; ------------------------
; THE 'TABLE OF CONSTANTS'
; ------------------------
; The ZX81 has only floating-point number representation.
; Both the ZX80 and the ZX Spectrum have integer numbers in some form.

;; stk-zero                                                 00 00 00 00 00
stk_zero:
	DEFB $00	;;Bytes: 1
	DEFB $B0	;;Exponent $00
	DEFB $00	;;(+00,+00,+00)

;; stk-one                                                  81 00 00 00 00
stk_one:
	DEFB $31	;;Exponent $81, Bytes: 1
	DEFB $00	;;(+00,+00,+00)


;; stk-half                                                 80 00 00 00 00
stk_half:
	DEFB $30	;;Exponent: $80, Bytes: 1
	DEFB $00	;;(+00,+00,+00)


;; stk-pi/2                                                 81 49 0F DA A2
stk_pi_div_2:
	DEFB $F1	;;Exponent: $81, Bytes: 4
	DEFB $49,$0F,$DA,$A2	;;

;; stk-ten                                                  84 20 00 00 00
stk_ten:
	DEFB $34	;;Exponent: $84, Bytes: 1
	DEFB $20	;;(+00,+00,+00)


; ------------------------
; THE 'TABLE OF ADDRESSES'
; ------------------------
;
; starts with binary operations which have two operands and one result.
; three pseudo binary operations first.

; (MW - OpenZXRom:) Remapped to Spectrum opcodes where appropriate.
; Where opcodes are unimplemented, jump to error_calc_op instead

;; tbl-addrs
tbl_addrs:

cc_jump_true	equ 0x00
	dw jump_true
cc_exchange	equ 0x01
	dw exchange
cc_delete	equ 0x02
	dw delete

; true binary operations.

cc_subtract	equ 0x03
	dw subtract
cc_multiply	equ 0x04
	dw multiply
cc_division	equ 0x05
	dw division
cc_to_power	equ 0x06
	dw to_power
cc_fn_or	equ 0x07
	dw fn_or
cc_no_and_no	equ 0x08
	dw no_and_no
cc_no_l_eql	equ 0x09
	dw comparison
cc_no_gr_eql	equ 0x0a
	dw comparison
cc_nos_neql	equ 0x0b
	dw comparison
cc_no_grtr	equ 0x0c
	dw comparison
cc_no_less	equ 0x0d
	dw comparison
cc_nos_eql	equ 0x0e
	dw comparison
cc_addition	equ 0x0f
	dw addition
cc_str_and_no	equ 0x10
	dw error_calc_op
cc_str_l_eql	equ 0x11
	dw comparison
cc_str_gr_eql	equ 0x12
	dw comparison
cc_strs_neql	equ 0x13
	dw comparison
cc_str_grtr	equ 0x14
	dw comparison
cc_str_less	equ 0x15
	dw comparison
cc_strs_eql	equ 0x16
	dw comparison
cc_strs_add	equ 0x17
	dw error_calc_op

; unary follow

cc_val_str	equ 0x18
	dw error_calc_op
cc_usr_str	equ 0x19
	dw error_calc_op
cc_read_in	equ 0x1a
	dw error_calc_op
cc_negate	equ 0x1b
	dw negate
cc_code	equ 0x1c
	dw error_calc_op
cc_val	equ 0x1d
	dw error_calc_op
cc_len	equ 0x1e
	dw error_calc_op
cc_sin	equ 0x1f
	dw sin
cc_cos	equ 0x20
	dw cos
cc_tan	equ 0x21
	dw tan
cc_asn	equ 0x22
	dw asn
cc_acs	equ 0x23
	dw acs
cc_atn	equ 0x24
	dw atn
cc_ln	equ 0x25
	dw ln
cc_exp	equ 0x26
	dw exp
cc_int	equ 0x27
	dw int
cc_sqr	equ 0x28
	dw sqr
cc_sgn	equ 0x29
	dw sgn
cc_abs	equ 0x2a
	dw abs
cc_peek	equ 0x2b
	dw peek
cc_fn_in	equ 0x2c
	dw fn_in
cc_usr_no	equ 0x2d
	dw usr_no
cc_str_str	equ 0x2e
	dw error_calc_op
cc_chr_str	equ 0x2f
	dw error_calc_op
cc_fn_not	equ 0x30
	dw fn_not

; end of true unary

cc_duplicate	equ 0x31
	dw duplicate
cc_n_mod_m	equ 0x32
	dw n_mod_m
cc_jump	equ 0x33
	dw jump
cc_stk_data	equ 0x34
	dw stk_data
cc_dec_jr_nz	equ 0x35
	dw dec_jr_nz
cc_less_0	equ 0x36
	dw less_0
cc_greater_0	equ 0x37
	dw greater_0
cc_end_calc	equ 0x38
	dw end_calc
cc_get_argt	equ 0x39
	dw get_argt
cc_truncate	equ 0x3a
	dw truncate
cc_calc_2	equ 0x3b
	dw calc_2
cc_e_to_fp	equ 0x3c
	dw error_calc_op
cc_re_stack	equ 0x3d
	dw error_calc_op

; the following are just the next available slots for the 128 compound literals
; which are in range $80 - $FF.

compound_literal_offset	equ $ - tbl_addrs

	dw series_xx	; codes 0x80-0x9f
	dw stk_const_xx	; codes 0xa0-0xbf
	dw st_mem_xx	; codes 0xc0-0xdf
	dw get_mem_xx	; codes 0xe0-0xff

cc_series_06	equ 0x86
cc_series_08	equ 0x88
cc_series_0c	equ 0x8c

cc_stk_zero	equ 0xa0
cc_stk_one	equ 0xa1
cc_stk_half	equ 0xa2
cc_stk_pi_div_2	equ 0xa3
cc_stk_ten	equ 0xa4

cc_st_mem_0	equ 0xc0
cc_st_mem_1	equ 0xc1
cc_st_mem_2	equ 0xc2
cc_st_mem_3	equ 0xc3
cc_st_mem_4	equ 0xc4
cc_st_mem_5	equ 0xc5

cc_get_mem_0	equ 0xe0
cc_get_mem_1	equ 0xe1
cc_get_mem_2	equ 0xe2
cc_get_mem_3	equ 0xe3
cc_get_mem_4	equ 0xe4
cc_get_mem_5	equ 0xe5


; -------------------------------
; THE 'FLOATING POINT CALCULATOR'
; -------------------------------
;
;

;; CALCULATE
calculate:
	CALL stk_pntrs	; routine STK-PNTRS is called to set up the
		; calculator stack pointers for a default
		; unary operation. HL = last value on stack.
		; DE = STKEND first location after stack.

; the calculate routine is called at this point by the series generator...

;; GEN-ENT-1
gen_ent_1:
	LD A,B	; fetch the Z80 B register to A
	LD (breg),A	; and store value in system variable BREG.
		; this will be the counter for dec-jr-nz
		; or if used from fp-calc2 the calculator
		; instruction.

; ... and again later at this point

;; GEN-ENT-2
gen_ent_2:
	EXX	; switch sets
	EX (SP),HL	; and store the address of next instruction,
		; the return address, in H'L'.
		; If this is a recursive call then the H'L'
		; of the previous invocation goes on stack.
		; c.f. end-calc.
	EXX	; switch back to main set.

; this is the re-entry looping point when handling a string of literals.

;; RE-ENTRY
re_entry:
	LD (stkend),DE	; save end of stack in system variable STKEND
	EXX	; switch to alt
	LD A,(HL)	; get next literal
	INC HL	; increase pointer'

; single operation jumps back to here

;; SCAN-ENT
scan_ent:
	PUSH HL	; save pointer on stack   *
	AND A	; now test the literal
	JP P,first_3d	; forward to FIRST-3D if in range $00 - $3D
		; anything with bit 7 set will be one of
		; 128 compound literals.

; compound literals have the following format.
; bit 7 set indicates compound.
; bits 6-5 the subgroup 0-3.
; bits 4-0 the embedded parameter $00 - $1F.
; The subgroup 0-3 needs to be manipulated to form the next available four
; address places after the simple literals in the address table.

	LD D,A	; save literal in D
	AND $60	; and with 01100000 to isolate subgroup
	RRCA	; rotate bits
	RRCA	; 4 places to right
	RRCA	; not five as we need offset * 2
	RRCA	; 00000xx0
	ADD A,compound_literal_offset
		; add length of non-compound table to give
		; correct offset.
	LD L,A	; store in L for later indexing.
	LD A,D	; bring back compound literal
	AND $1F	; use mask to isolate parameter bits
	JR ent_table	; forward to ENT-TABLE

; ---

; the branch was here with simple literals.

;; FIRST-3D
first_3d:
	CP $18	; compare with first unary operations.
	JR NC,double_a	; to DOUBLE-A with unary operations

; it is binary so adjust pointers.

	EXX	;
	LD BC,$FFFB	; the value -5
	LD D,H	; transfer HL, the last value, to DE.
	LD E,L	;
	ADD HL,BC	; subtract 5 making HL point to second
		; value.
	EXX	;

;; DOUBLE-A
double_a:
	RLCA	; double the literal
	LD L,A	; and store in L for indexing

;; ENT-TABLE
ent_table:
	LD DE,tbl_addrs	; Address: tbl-addrs
	LD H,$00	; prepare to index
	ADD HL,DE	; add to get address of routine
	LD E,(HL)	; low byte to E
	INC HL	;
	LD D,(HL)	; high byte to D

	LD HL,re_entry	; Address: RE-ENTRY
	EX (SP),HL	; goes on machine stack
		; address of next literal goes to HL. *


	PUSH DE	; now the address of routine is stacked.
	EXX	; back to main set
		; avoid using IY register.
	LD BC,(stkend_hi)	; STKEND_hi
		; nothing much goes to C but BREG to B
		; and continue into next ret instruction
		; which has a dual identity

; -----------------------
; THE 'DELETE' SUBROUTINE
; -----------------------
; offset $02: 'delete'
; A simple return but when used as a calculator literal this
; deletes the last value from the calculator stack.
; On entry, as always with binary operations,
; HL=first number, DE=second number
; On exit, HL=result, DE=stkend.
; So nothing to do

;; delete
delete:
	RET	; return - indirect jump if from above.

; ---------------------------------
; THE 'SINGLE OPERATION' SUBROUTINE
; ---------------------------------
; offset $37: 'fp-calc-2'
; this single operation is used, in the first instance, to evaluate most
; of the mathematical and string functions found in BASIC expressions.

;; fp-calc-2
calc_2:
	POP AF	; drop return address.
	LD A,(breg)	; load accumulator from system variable BREG
		; value will be literal eg. 'tan'
	EXX	; switch to alt
	JR scan_ent	; back to SCAN-ENT
		; next literal will be end-calc in scanning

; ------------------------------
; THE 'TEST 5 SPACES' SUBROUTINE
; ------------------------------
; This routine is called from MOVE-FP, STK-CONST and STK-STORE to
; test that there is enough space between the calculator stack and the
; machine stack for another five-byte value. It returns with BC holding
; the value 5 ready for any subsequent LDIR.

;; TEST-5-SP
test_5_sp:
	PUSH DE	; save
	PUSH HL	; registers
	LD BC,$0005	; an overhead of five bytes
	CALL test_room	; routine TEST-ROOM tests free RAM raising
		; an error if not.
	POP HL	; else restore
	POP DE	; registers.
	RET	; return with BC set at 5.

; ---------------------------------------------
; THE 'MOVE A FLOATING POINT NUMBER' SUBROUTINE
; ---------------------------------------------
; offset $2D: 'duplicate'
; This simple routine is a 5-byte LDIR instruction
; that incorporates a memory check.
; When used as a calculator literal it duplicates the last value on the
; calculator stack.
; Unary so on entry HL points to last value, DE to stkend

;; duplicate
;; MOVE-FP
duplicate:
move_fp:
	CALL test_5_sp	; routine TEST-5-SP test free memory
		; and sets BC to 5.
	LDIR	; copy the five bytes.
	RET	; return with DE addressing new STKEND
		; and HL addressing new last value.
; -------------------------------
; THE 'STACK LITERALS' SUBROUTINE
; -------------------------------
; offset $30: 'stk-data'
; When a calculator subroutine needs to put a value on the calculator
; stack that is not a regular constant this routine is called with a
; variable number of following data bytes that convey to the routine
; the floating point form as succinctly as is possible.

;; stk-data
stk_data:
	LD H,D	; transfer STKEND
	LD L,E	; to HL for result.

;; STK-CONST
stk_const:
	CALL test_5_sp	; routine TEST-5-SP tests that room exists
		; and sets BC to $05.

	EXX	; switch to alternate set
	PUSH HL	; save the pointer to next literal on stack
	EXX	; switch back to main set

	EX (SP),HL	; pointer to HL, destination to stack.

	PUSH BC	; save BC - value 5 from test room ??.

	LD A,(HL)	; fetch the byte following 'stk-data'
	AND $C0	; isolate bits 7 and 6
	RLCA	; rotate
	RLCA	; to bits 1 and 0  range $00 - $03.
	LD C,A	; transfer to C
	INC C	; and increment to give number of bytes
		; to read. $01 - $04
	LD A,(HL)	; reload the first byte
	AND $3F	; mask off to give possible exponent.
	JR NZ,form_exp	; forward to FORM-EXP if it was possible to
		; include the exponent.

; else byte is just a byte count and exponent comes next.

	INC HL	; address next byte and
	LD A,(HL)	; pick up the exponent ( - $50).

;; FORM-EXP
form_exp:
	ADD A,$50	; now add $50 to form actual exponent
	LD (DE),A	; and load into first destination byte.
	LD A,$05	; load accumulator with $05 and
	SUB C	; subtract C to give count of trailing
		; zeros plus one.
	INC HL	; increment source
	INC DE	; increment destination
	LD B,$00	; prepare to copy
	LDIR	; copy C bytes

	POP BC	; restore 5 counter to BC ??.

	EX (SP),HL	; put HL on stack as next literal pointer
		; and the stack value - result pointer -
		; to HL.

	EXX	; switch to alternate set.
	POP HL	; restore next literal pointer from stack
		; to H'L'.
	EXX	; switch back to main set.

	LD B,A	; zero count to B
	XOR A	; clear accumulator

;; STK-ZEROS
stk_zeros:
	DEC B	; decrement B counter
	RET Z	; return if zero.          >>
		; DE points to new STKEND
		; HL to new number.

	LD (DE),A	; else load zero to destination
	INC DE	; increase destination
	JR stk_zeros	; loop back to STK-ZEROS until done.


; -------------------------------
; THE 'SKIP CONSTANTS' SUBROUTINE
; -------------------------------
; This routine traverses variable-length entries in the table of constants,
; stacking intermediate, unwanted constants onto a dummy calculator stack,
; in the first five bytes of the ZX81 ROM.

;; SKIP-CONS
skip_cons:
	AND A	; test if initially zero.

;; SKIP-NEXT
skip_next:
	RET Z	; return if zero.          >>

	PUSH AF	; save count.
	PUSH DE	; and normal STKEND

	LD DE,$0000	; dummy value for STKEND at start of ROM
		; Note. not a fault but this has to be
		; moved elsewhere when running in RAM.
		;
	CALL stk_const	; routine STK-CONST works through variable
		; length records.

	POP DE	; restore real STKEND
	POP AF	; restore count
	DEC A	; decrease
	JR skip_next	; loop back to SKIP-NEXT

; --------------------------------
; THE 'MEMORY LOCATION' SUBROUTINE
; --------------------------------
; This routine, when supplied with a base address in HL and an index in A,
; will calculate the address of the A'th entry, where each entry occupies
; five bytes. It is used for addressing floating-point numbers in the
; calculator's memory area.

;; LOC-MEM
loc_mem:
	LD C,A	; store the original number $00-$1F.
	RLCA	; double.
	RLCA	; quadruple.
	ADD A,C	; now add original value to multiply by five.

	LD C,A	; place the result in C.
	LD B,$00	; set B to 0.
	ADD HL,BC	; add to form address of start of number in HL.

	RET	; return.

; -------------------------------------
; THE 'GET FROM MEMORY AREA' SUBROUTINE
; -------------------------------------
; offsets $E0 to $FF: 'get-mem-0', 'get-mem-1' etc.
; A holds $00-$1F offset.
; The calculator stack increases by 5 bytes.

;; get-mem-xx
get_mem_xx:
	PUSH DE	; save STKEND
	LD HL,(mem)	; MEM is base address of the memory cells.
	CALL loc_mem	; routine LOC-MEM so that HL = first byte
	CALL move_fp	; routine MOVE-FP moves 5 bytes with memory
		; check.
		; DE now points to new STKEND.
	POP HL	; the original STKEND is now RESULT pointer.
	RET	; return.

; ---------------------------------
; THE 'STACK A CONSTANT' SUBROUTINE
; ---------------------------------
; offset $A0: 'stk-zero'
; offset $A1: 'stk-one'
; offset $A2: 'stk-half'
; offset $A3: 'stk-pi/2'
; offset $A4: 'stk-ten'
; This routine allows a one-byte instruction to stack up to 32 constants
; held in short form in a table of constants. In fact only 5 constants are
; required. On entry the A register holds the literal ANDed with $1F.
; It isn't very efficient and it would have been better to hold the
; numbers in full, five byte form and stack them in a similar manner
; to that which would be used later for semi-tone table values.

;; stk-const-xx
stk_const_xx:
	LD H,D	; save STKEND - required for result
	LD L,E	;
	EXX	; swap
	PUSH HL	; save pointer to next literal
	LD HL,stk_zero	; Address: stk-zero - start of table of
		; constants
	EXX	;
	CALL skip_cons	; routine SKIP-CONS
	CALL stk_const	; routine STK-CONST
	EXX	;
	POP HL	; restore pointer to next literal.
	EXX	;
	RET	; return.

; ---------------------------------------
; THE 'STORE IN A MEMORY AREA' SUBROUTINE
; ---------------------------------------
; Offsets $C0 to $DF: 'st-mem-0', 'st-mem-1' etc.
; Although 32 memory storage locations can be addressed, only six
; $C0 to $C5 are required by the ROM and only the thirty bytes (6*5)
; required for these are allocated. ZX81 programmers who wish to
; use the floating point routines from assembly language may wish to
; alter the system variable MEM to point to 160 bytes of RAM to have
; use the full range available.
; A holds derived offset $00-$1F.
; Unary so on entry HL points to last value, DE to STKEND.

;; st-mem-xx
st_mem_xx:
	PUSH HL	; save the result pointer.
	EX DE,HL	; transfer to DE.
	LD HL,(mem)	; fetch MEM the base of memory area.
	CALL loc_mem	; routine LOC-MEM sets HL to the destination.
	EX DE,HL	; swap - HL is start, DE is destination.
	CALL move_fp	; routine MOVE-FP.
		; note. a short ld bc,5; ldir
		; the embedded memory check is not required
		; so these instructions would be faster!
	EX DE,HL	; DE = STKEND
	POP HL	; restore original result pointer
	RET	; return.

; -------------------------
; THE 'EXCHANGE' SUBROUTINE
; -------------------------
; offset $01: 'exchange'
; This routine exchanges the last two values on the calculator stack
; On entry, as always with binary operations,
; HL=first number, DE=second number
; On exit, HL=result, DE=stkend.

;; exchange
exchange:
	LD B,$05	; there are five bytes to be swapped

; start of loop.

;; SWAP-BYTE
swap_byte:
	LD A,(DE)	; each byte of second
	LD C,(HL)	; each byte of first
	EX DE,HL	; swap pointers
	LD (DE),A	; store each byte of first
	LD (HL),C	; store each byte of second
	INC HL	; advance both
	INC DE	; pointers.
	DJNZ swap_byte	; loop back to SWAP-BYTE until all 5 done.

	EX DE,HL	; even up the exchanges
		; so that DE addresses STKEND.
	RET	; return.

; ---------------------------------
; THE 'SERIES GENERATOR' SUBROUTINE
; ---------------------------------
; offset $86: 'series-06'
; offset $88: 'series-08'
; offset $8C: 'series-0C'
; The ZX81 uses Chebyshev polynomials to generate approximations for
; SIN, ATN, LN and EXP. These are named after the Russian mathematician
; Pafnuty Chebyshev, born in 1821, who did much pioneering work on numerical
; series. As far as calculators are concerned, Chebyshev polynomials have an
; advantage over other series, for example the Taylor series, as they can
; reach an approximation in just six iterations for SIN, eight for EXP and
; twelve for LN and ATN. The mechanics of the routine are interesting but
; for full treatment of how these are generated with demonstrations in
; Sinclair BASIC see "The Complete Spectrum ROM Disassembly" by Dr Ian Logan
; and Dr Frank O'Hara, published 1983 by Melbourne House.

;; series-xx
series_xx:
	LD B,A	; parameter $00 - $1F to B counter
	CALL gen_ent_1	; routine GEN-ENT-1 is called.
		; A recursive call to a special entry point
		; in the calculator that puts the B register
		; in the system variable BREG. The return
		; address is the next location and where
		; the calculator will expect its first
		; instruction - now pointed to by HL'.
		; The previous pointer to the series of
		; five-byte numbers goes on the machine stack.

; The initialization phase.

	DEFB cc_duplicate	;;duplicate       x,x
	DEFB cc_addition	;;addition        x+x
	DEFB cc_st_mem_0	;;st-mem-0        x+x
	DEFB cc_delete	;;delete          .
	DEFB cc_stk_zero	;;stk-zero        0
	DEFB cc_st_mem_2	;;st-mem-2        0

; a loop is now entered to perform the algebraic calculation for each of
; the numbers in the series

;; G-LOOP
g_loop:
	DEFB cc_duplicate	;;duplicate       v,v.
	DEFB cc_get_mem_0	;;get-mem-0       v,v,x+2
	DEFB cc_multiply	;;multiply        v,v*x+2
	DEFB cc_get_mem_2	;;get-mem-2       v,v*x+2,v
	DEFB cc_st_mem_1	;;st-mem-1
	DEFB cc_subtract	;;subtract
	DEFB cc_end_calc	;;end-calc

; the previous pointer is fetched from the machine stack to H'L' where it
; addresses one of the numbers of the series following the series literal.

	CALL stk_data	; routine STK-DATA is called directly to
		; push a value and advance H'L'.
	CALL gen_ent_2	; routine GEN-ENT-2 recursively re-enters
		; the calculator without disturbing
		; system variable BREG
		; H'L' value goes on the machine stack and is
		; then loaded as usual with the next address.

	DEFB cc_addition	;;addition
	DEFB cc_exchange	;;exchange
	DEFB cc_st_mem_2	;;st-mem-2
	DEFB cc_delete	;;delete

	DEFB cc_dec_jr_nz	;;dec-jr-nz
	DEFB g_loop - $	;;back to L1A89, G-LOOP

; when the counted loop is complete the final subtraction yields the result
; for example SIN X.

	DEFB cc_get_mem_1	;;get-mem-1
	DEFB cc_subtract	;;subtract
	DEFB cc_end_calc	;;end-calc

	RET	; return with H'L' pointing to location
		; after last number in series.

; -----------------------
; Handle unary minus (18)
; -----------------------
; Unary so on entry HL points to last value, DE to STKEND.

;; NEGATE
;; negate
negate:
	PUSH DE
	CALL conv_to_fp	; Original ZX81 routine only handles
		; floating point format.
		; TODO: handle integers as an optimised
		; special case, rather than converting them
	POP DE

	LD A,(HL)	; fetch exponent of last value on the
		; calculator stack.
	AND A	; test it.
	RET Z	; return if zero.

	INC HL	; address the byte with the sign bit.
	LD A,(HL)	; fetch to accumulator.
	XOR $80	; toggle the sign bit.
	LD (HL),A	; put it back.
	DEC HL	; point to last value again.
	RET	; return.

; -----------------------
; Absolute magnitude (27)
; -----------------------
; This calculator literal finds the absolute value of the last value,
; floating point, on calculator stack.

;; abs
abs:
	XOR A	; test whether this is in integer form
	OR (HL)

	INC HL	; point to byte with sign bit.
	JR Z,abs_int	; number is integer, so treat as a special case

	RES 7,(HL)	; make the sign positive.
	DEC HL	; point to last value again.
	RET	; return.

abs_int:
	INC HL	; test sign byte
	OR (HL)
	JR Z,abs_int_done	; leave number alone if positive (or zero)
	LD (HL),0	; clear sign byte
	INC HL
	LD A,(HL)	; read 3rd and 4th bytes into BC,
	CPL	; complementing as we go
	LD C,A
	INC HL
	LD A,(HL)
	CPL
	LD B,A
	INC BC	; incrementing the complemented value gives us
	LD (HL),B	; the negated value. Write it back
	DEC HL
	LD (HL),C
	DEC HL

abs_int_done:
	DEC HL
	RET

; -----------
; Signum (26)
; -----------
; This routine replaces the last value on the calculator stack,
; which is in floating point form, with one if positive and with -minus one
; if negative. If it is zero then it is left as such.
; MW: Reimplemented in OpenZXRom because ZX81 routine is inherently
; floating-point-form based

;; sgn
sgn:
	CALL test_zero	; test if value is zero
	RET Z	; and return immediately if so
	LD (HL),$81	; exponent of 1 and - 1 is $81
	INC HL
	LD A,(HL)
	AND $80	; clear all except sign bit
	LD (HL),A
	INC HL
	XOR A
	LD (HL),A	; clear all other bytes
	INC HL
	LD (HL),A
	INC HL
	LD (HL),A
	DEC HL	; return HL to point to exponent byte
	DEC HL
	DEC HL
	DEC HL
	RET

; -------------------------
; Handle PEEK function
; -------------------------
; This function returns the contents of a memory address.
; The entire address space can be peeked including the ROM.

;; peek
peek:
	CALL find_int	; routine FIND-INT puts address in BC.
	LD A,(BC)	; load contents into A register.
	JP stack_a	; exit via STACK-A to put value on the
		; calculator stack.

; -------------------------
; Handle IN function
; -------------------------
; (not in the ZX81, but trivial enough :-) )

;; fn_in
fn_in:
	CALL find_int	; routine FIND-INT puts address in BC.
	IN A,(C)	; read port value into A register.
	JP stack_a	; exit via STACK-A to put value on the
		; calculator stack.

; ---------------
; USR number
; ---------------
; The USR function followed by a number 0-65535 is the method by which
; the ZX81 invokes machine code programs. This function returns the
; contents of the BC register pair.
; Note. that STACK-BC re-initializes the IY register to sysvars_base if a
; user-written program has altered it.

;; usr-no
usr_no:
	CALL find_int	; routine FIND-INT to fetch the
		; supplied address into BC.

	LD HL,stack_bc	; address: STACK-BC is
	PUSH HL	; pushed onto the machine stack.
	PUSH BC	; then the address of the machine code
		; routine.

	RET	; make an indirect jump to the routine
		; and, hopefully, to STACK-BC also.

; -------------
; Test for zero
; -------------
; (new to OpenZXRom)
; Returns with A=0 and zero flag set if the five-byte value at HL is zero.
; On the ZX81, only the exponent byte needed to be examined, but here an
; exponent of zero just means it's in integer form, so we need to look at
; additional bytes to be sure.

test_zero:
	LD A,(HL)	; collect exponent byte
	INC HL
	OR (HL)	; overall number is 0 if these bytes
	INC HL	; OR together as zero
	OR (HL)
	INC HL
	OR (HL)
	DEC HL	; return HL to the first byte
	DEC HL
	DEC HL
	RET

; -----------------------
; Greater than zero ($33)
; -----------------------
; Test if the last value on the calculator stack is greater than zero.
; This routine is also called directly from the end-tests of the comparison
; routine.

;; GREATER-0
;; greater-0
greater_0:
	CALL test_zero	; test for zero
	RET Z	; return if so.

	LD A,$FF	; prepare XOR mask for sign bit
	JR sign_to_c	; forward to SIGN-TO-C
		; to put sign in carry
		; (carry will become set if sign is positive)
		; and then overwrite location with 1 or 0
		; as appropriate.

; ------------------------
; Handle NOT operator ($2C)
; ------------------------
; This overwrites the last value with 1 if it was zero else with zero
; if it was any other value.
;
; e.g. NOT 0 returns 1, NOT 1 returns 0, NOT -3 returns 0.
;
; The subroutine is also called directly from the end-tests of the comparison
; operator.

;; NOT
;; not
fn_not:
	CALL test_zero	; returns with A=0 if overall value is zero
	NEG	; negate - sets carry if non-zero.
	CCF	; complement so carry set if zero, else reset.
	JR fp_0_1	; forward to FP-0/1.

; -------------------
; Less than zero (32)
; -------------------
; Destructively test if last value on calculator stack is less than zero.
; Bit 7 of second byte will be set if so.
; MW: Handily, this is true of the integer form as well.

;; less-0
less_0:
	XOR A	; set xor mask to zero
		; (carry will become set if sign is negative).

; transfer sign of mantissa to Carry Flag.

;; SIGN-TO-C
sign_to_c:
	INC HL	; address 2nd byte.
	XOR (HL)	; bit 7 of HL will be set if number is negative.
	DEC HL	; address 1st byte again.
	RLCA	; rotate bit 7 of A to carry.

; -----------
; Zero or one
; -----------
; This routine places an integer value zero or one at the addressed location
; of calculator stack or MEM area. The value one is written if carry is set on
; entry else zero.

;; FP-0/1
fp_0_1:
	PUSH HL	; save pointer to the first byte
	LD B,$05	; five bytes to do.

;; FP-loop
fp_loop:
	LD (HL),$00	; insert a zero.
	INC HL	;
	DJNZ fp_loop	; repeat.

	POP HL	;
	RET NC	;

	LD (HL),$81	; make value 1. Doesn't really matter that we're
		; not using the proper integer form...
	RET	; return.

; -----------------------
; Handle OR operator (07)
; -----------------------
; The Boolean OR operator. eg. X OR Y
; The result is zero if both values are zero else a non-zero value.
;
; e.g.    0 OR 0  returns 0.
;        -3 OR 0  returns -3.
;         0 OR -3 returns 1.
;        -3 OR 2  returns 1.
;
; A binary operation.
; On entry HL points to first operand (X) and DE to second operand (Y).

;; or
fn_or:
	EX DE,HL	; test second number:
	CALL test_zero	; check whether value at DE is zero
	EX DE,HL
	RET Z	; return if zero.

	SCF	; set carry flag
	JR fp_0_1	; back to FP-0/1 to overwrite the first operand
		; with the value 1.

; -----------------------------
; Handle number AND number (08)
; -----------------------------
; The Boolean AND operator.
;
; e.g.    -3 AND 2  returns -3.
;         -3 AND 0  returns 0.
;          0 and -2 returns 0.
;          0 and 0  returns 0.
;
; Compare with OR routine above.

;; no-&-no
no_and_no:
	EX DE,HL	; test second number:
	CALL test_zero	; check whether value at DE is zero
	EX DE,HL
	RET NZ	; return if not zero.

	JR fp_0_1	; back to FP-0/1 to overwrite the first operand
		; with zero for return value.

; -----------------------------------
; Perform comparison ($09-$0E, $11-$16)
; -----------------------------------
; True binary operations.
;
; A single entry point is used to evaluate six numeric and six string
; comparisons. On entry, the calculator literal is in the B register and
; the two numeric values, or the two string parameters, are on the
; calculator stack.
; The individual bits of the literal are manipulated to group similar
; operations although the SUB 8 instruction does nothing useful and merely
; alters the string test bit.
; Numbers are compared by subtracting one from the other, strings are
; compared by comparing every character until a mismatch, or the end of one
; or both, is reached.
;
; Numeric Comparisons.
; --------------------
; The 'x>y' example is the easiest as it employs straight-thru logic.
; Number y is subtracted from x and the result tested for greater-0 yielding
; a final value 1 (true) or 0 (false).
; For 'x<y' the same logic is used but the two values are first swapped on the
; calculator stack.
; For 'x=y' NOT is applied to the subtraction result yielding true if the
; difference was zero and false with anything else.
; The first three numeric comparisons are just the opposite of the last three
; so the same processing steps are used and then a final NOT is applied.
;
; literal    Test   No  sub 8       ExOrNot  1st RRCA  exch sub  ?   End-Tests
; =========  ====   == ======== === ======== ========  ==== ===  =  === === ===
; no-l-eql   x<=y   09 00000001 dec 00000000 00000000  ---- x-y  ?  --- >0? NOT
; no-gr-eql  x>=y   0A 00000010 dec 00000001 10000000c swap y-x  ?  --- >0? NOT
; nos-neql   x<>y   0B 00000011 dec 00000010 00000001  ---- x-y  ?  NOT --- NOT
; no-grtr    x>y    0C 00000100  -  00000100 00000010  ---- x-y  ?  --- >0? ---
; no-less    x<y    0D 00000101  -  00000101 10000010c swap y-x  ?  --- >0? ---
; nos-eql    x=y    0E 00000110  -  00000110 00000011  ---- x-y  ?  NOT --- ---
;
;                                                           comp -> C/F
;                                                           ====    ===
; str-l-eql  x$<=y$ 11 00001001 dec 00001000 00000100  ---- x$y$ 0  !or >0? NOT
; str-gr-eql x$>=y$ 12 00001010 dec 00001001 10000100c swap y$x$ 0  !or >0? NOT
; strs-neql  x$<>y$ 13 00001011 dec 00001010 00000101  ---- x$y$ 0  !or >0? NOT
; str-grtr   x$>y$  14 00001100  -  00001100 00000110  ---- x$y$ 0  !or >0? ---
; str-less   x$<y$  15 00001101  -  00001101 10000110c swap y$x$ 0  !or >0? ---
; strs-eql   x$=y$  16 00001110  -  00001110 00000111  ---- x$y$ 0  !or >0? ---
;
; String comparisons are a little different in that the eql/neql carry flag
; from the 2nd RRCA is, as before, fed into the first of the end tests but
; along the way it gets modified by the comparison process. The result on the
; stack always starts off as zero and the carry fed in determines if NOT is
; applied to it. So the only time the greater-0 test is applied is if the
; stack holds zero which is not very efficient as the test will always yield
; zero. The most likely explanation is that there were once separate end tests
; for numbers and strings.

;; no-l-eql,etc.
comparison:
	LD A,B	; transfer literal to accumulator.
	SUB $08	; subtract eight - which is not useful.

	BIT 2,A	; isolate '>', '<', '='.

	JR NZ,ex_or_not	; skip to EX-OR-NOT with these.

	DEC A	; else make $00-$02, $08-$0A to match bits 0-2.

;; EX-OR-NOT
ex_or_not:
	RRCA	; the first RRCA sets carry for a swap.
	JR NC,nu_or_str	; forward to NU-OR-STR with other 8 cases

; for the other 4 cases the two values on the calculator stack are exchanged.

	PUSH AF	; save A and carry.
	PUSH HL	; save HL - pointer to first operand.
		; (DE points to second operand).

	CALL exchange	; routine exchange swaps the two values.
		; (HL = second operand, DE = STKEND)

	POP DE	; DE = first operand
	EX DE,HL	; as we were.
	POP AF	; restore A and carry.

; Note. it would be better if the 2nd RRCA preceded the string test.
; It would save two duplicate bytes and if we also got rid of that sub 8
; at the beginning we wouldn't have to alter which bit we test.

;; NU-OR-STR
nu_or_str:
	BIT 2,A	; test if a string comparison.
	JR NZ,strings	; forward to STRINGS if so.

; continue with numeric comparisons.

	RRCA	; 2nd RRCA causes eql/neql to set carry.
	PUSH AF	; save A and carry

	CALL subtract	; routine subtract leaves result on stack.
	JR end_tests	; forward to END-TESTS

; ---
strings:
	RST fatal_error	; TODO: string comparisons not implemented yet

;   both numeric and string paths converge here.

;; END-TESTS
end_tests:
	POP AF	; pop carry  - will be set if eql/neql
	PUSH AF	; save it again.

	CALL C,fn_not	; routine NOT sets true(1) if equal(0)
		; or, for strings, applies true result.
	CALL greater_0	; greater-0  ??????????


	POP AF	; pop A
	RRCA	; the third RRCA - test for '<=', '>=' or '<>'.
	CALL NC,fn_not	; apply a terminal NOT if so.
	RET	; return.

; --------------------
; Check stack pointers
; --------------------
;   Register DE is set to STKEND and HL, the result pointer, is set to five
;   locations below this.
;   This routine is used when it is inconvenient to save these values at the
;   time the calculator stack is manipulated due to other activity on the
;   machine stack.
;   This routine is also used to terminate the VAL routine for
;   the same reason and to initialize the calculator stack at the start of
;   the CALCULATE routine.

;; STK-PNTRS
stk_pntrs:
	LD HL,(stkend)	; fetch STKEND value from system variable.
	LD DE,$FFFB	; the value -5
	PUSH HL	; push STKEND value.

	ADD HL,DE	; subtract 5 from HL.

	POP DE	; pop STKEND to DE.
	RET	; return.

; -------------------------------------
; THE 'DECREASE THE COUNTER' SUBROUTINE
; -------------------------------------
; (offset $31: 'dec-jr-nz')
;   The calculator has an instruction that decrements a single-byte
;   pseudo-register and makes consequential relative jumps just like
;   the Z80's DJNZ instruction.

;; dec-jr-nz
dec_jr_nz:
	EXX	; switch in set that addresses code

	PUSH HL	; save pointer to offset byte
	LD HL,breg	; address BREG in system variables
	DEC (HL)	; decrement it
	POP HL	; restore pointer

	JR NZ,jump_2	; to JUMP-2 if not zero

	INC HL	; step past the jump length.
	EXX	; switch in the main set.
	RET	; return.

;   Note. as a general rule the calculator avoids using the IY register
;   otherwise the cumbersome 4 instructions in the middle could be replaced by
;   dec (iy+$xx) - using three instruction bytes instead of six.


; ---------------------
; THE 'JUMP' SUBROUTINE
; ---------------------
; (Offset $2F; 'jump')
;   This enables the calculator to perform relative jumps just like
;   the Z80 chip's JR instruction.
;   This is one of the few routines to be polished for the ZX Spectrum.
;   See, without looking at the ZX Spectrum ROM, if you can get rid of the
;   relative jump.

; (MW: I guess it involves getting the result of the test in the carry flag
; instead - using CP 0x80? - and then doing a SBC A,A to get 0x00 or 0xff.
; But I'm not going to fix what ain't broken... :-) )

;; jump
;; JUMP
jump:
	EXX	;switch in pointer set

;; JUMP-2
jump_2:
	LD E,(HL)	; the jump byte 0-127 forward, 128-255 back.
	XOR A	; clear accumulator.
	BIT 7,E	; test if negative jump
	JR Z,jump_3	; skip, if positive, to JUMP-3.

	CPL	; else change to $FF.

;; JUMP-3
jump_3:
	LD D,A	; transfer to high byte.
	ADD HL,DE	; advance calculator pointer forward or back.

	EXX	; switch out pointer set.
	RET	; return.

; -----------------------------
; THE 'JUMP ON TRUE' SUBROUTINE
; -----------------------------
; (Offset $00; 'jump-true')
;   This enables the calculator to perform conditional relative jumps
;   dependent on whether the last test gave a true result
;   On the ZX81, the exponent will be zero for zero or else $81 for one.

;; jump-true
jump_true:
	EX DE,HL
	CALL test_zero	; check whether value at DE is zero
	EX DE,HL

	JR NZ,jump	; back to JUMP if true (1).

	EXX	; else switch in the pointer set.
	INC HL	; step past the jump length.
	EXX	; switch in the main set.
	RET	; return.

; ------------------------
; THE 'MODULUS' SUBROUTINE
; ------------------------
; ( Offset $2E: 'n-mod-m' )
; ( i1, i2 -- i3, i4 )
;   The subroutine calculate N mod M where M is the positive integer, the
;   'last value' on the calculator stack and N is the integer beneath.
;   The subroutine returns the integer quotient as the last value and the
;   remainder as the value beneath.
;   e.g.    17 MOD 3 = 5 remainder 2
;   It is invoked during the calculation of a random number and also by
;   the PRINT-FP routine.

;; n-mod-m
n_mod_m:
	RST fp_calc	;; FP-CALC          17, 3.
	DEFB cc_st_mem_0	;;st-mem-0          17, 3.
	DEFB cc_delete	;;delete            17.
	DEFB cc_duplicate	;;duplicate         17, 17.
	DEFB cc_get_mem_0	;;get-mem-0         17, 17, 3.
	DEFB cc_division	;;division          17, 17/3.
	DEFB cc_int	;;int               17, 5.
	DEFB cc_get_mem_0	;;get-mem-0         17, 5, 3.
	DEFB cc_exchange	;;exchange          17, 3, 5.
	DEFB cc_st_mem_0	;;st-mem-0          17, 3, 5.
	DEFB cc_multiply	;;multiply          17, 15.
	DEFB cc_subtract	;;subtract          2.
	DEFB cc_get_mem_0	;;get-mem-0         2, 5.
	DEFB cc_end_calc	;;end-calc          2, 5.

	RET	; return.

; ----------------------
; THE 'INTEGER' FUNCTION
; ----------------------
; (offset $24: 'int')
;   This function returns the integer of x, which is just the same as truncate
;   for positive numbers. The truncate literal truncates negative numbers
;   upwards so that -3.4 gives -3 whereas the BASIC INT function has to
;   truncate negative numbers down so that INT -3.4 is 4.
;   It is best to work through using, say, plus or minus 3.4 as examples.

;; int
int:
	RST fp_calc	;; FP-CALC              x.    (= 3.4 or -3.4).
	DEFB cc_duplicate	;;duplicate             x, x.
	DEFB cc_less_0	;;less-0                x, (1/0)
	DEFB cc_jump_true	;;jump-true             x, (1/0)
	DEFB x_neg - $	;;to X-NEG

	DEFB cc_truncate	;;truncate              trunc 3.4 = 3.
	DEFB cc_end_calc	;;end-calc              3.

	RET	; return with + int x on stack.


;; X-NEG
x_neg:
	DEFB cc_duplicate	;;duplicate             -3.4, -3.4.
	DEFB cc_truncate	;;truncate              -3.4, -3.
	DEFB cc_st_mem_0	;;st-mem-0              -3.4, -3.
	DEFB cc_subtract	;;subtract              -.4
	DEFB cc_get_mem_0	;;get-mem-0             -.4, -3.
	DEFB cc_exchange	;;exchange              -3, -.4.
	DEFB cc_fn_not	;;not                   -3, (0).
	DEFB cc_jump_true	;;jump-true             -3.
	DEFB exit - $	;;to L1C59, EXIT        -3.

	DEFB cc_stk_one	;;stk-one               -3, 1.
	DEFB cc_subtract	;;subtract              -4.

;; EXIT
exit:
	DEFB cc_end_calc	;;end-calc              -4.

	RET	; return.

; ----------------
; Exponential (23)
; ----------------
;
;

;; EXP
;; exp
exp:
	RST fp_calc	;; FP-CALC
	DEFB cc_stk_data	;;stk-data
	DEFB $F1	;;Exponent: $81, Bytes: 4
	DEFB $38,$AA,$3B,$29	;;
	DEFB cc_multiply	;;multiply
	DEFB cc_duplicate	;;duplicate
	DEFB cc_int	;;int
	DEFB cc_st_mem_3	;;st-mem-3
	DEFB cc_subtract	;;subtract
	DEFB cc_duplicate	;;duplicate
	DEFB cc_addition	;;addition
	DEFB cc_stk_one	;;stk-one
	DEFB cc_subtract	;;subtract
	DEFB cc_series_08	;;series-08
	DEFB $13	;;Exponent: $63, Bytes: 1
	DEFB $36	;;(+00,+00,+00)
	DEFB $58	;;Exponent: $68, Bytes: 2
	DEFB $65,$66	;;(+00,+00)
	DEFB $9D	;;Exponent: $6D, Bytes: 3
	DEFB $78,$65,$40	;;(+00)
	DEFB $A2	;;Exponent: $72, Bytes: 3
	DEFB $60,$32,$C9	;;(+00)
	DEFB $E7	;;Exponent: $77, Bytes: 4
	DEFB $21,$F7,$AF,$24	;;
	DEFB $EB	;;Exponent: $7B, Bytes: 4
	DEFB $2F,$B0,$B0,$14	;;
	DEFB $EE	;;Exponent: $7E, Bytes: 4
	DEFB $7E,$BB,$94,$58	;;
	DEFB $F1	;;Exponent: $81, Bytes: 4
	DEFB $3A,$7E,$F8,$CF	;;
	DEFB cc_get_mem_3	;;get-mem-3
	DEFB cc_end_calc	;;end-calc

	CALL fp_to_a	; routine FP-TO-A
	JR NZ,n_negtv	; to N-NEGTV

	JR C,report_6b	; to REPORT-6b

	ADD A,(HL)	;
	JR NC,result_ok	; to RESULT-OK


;; REPORT-6b
report_6b:
	rst error
	db err_code_number_too_big

;; N-NEGTV
n_negtv:
	JR C,rslt_zero	; to RSLT-ZERO

	SUB (HL)	;
	JR NC,rslt_zero	; to RSLT-ZERO

	NEG	; Negate

;; RESULT-OK
result_ok:
	LD (HL),A	;
	RET	; return.


;; RSLT-ZERO
rslt_zero:
	RST fp_calc	;; FP-CALC
	DEFB cc_delete	;;delete
	DEFB cc_stk_zero	;;stk-zero
	DEFB cc_end_calc	;;end-calc

	RET	; return.

; --------------------------------
; THE 'NATURAL LOGARITHM' FUNCTION
; --------------------------------
; (offset $22: 'ln')
;   Like the ZX81 itself, 'natural' logarithms came from Scotland.
;   They were devised in 1614 by well-traveled Scotsman John Napier who noted
;   "Nothing doth more molest and hinder calculators than the multiplications,
;    divisions, square and cubical extractions of great numbers".
;
;   Napier's logarithms enabled the above operations to be accomplished by 
;   simple addition and subtraction simplifying the navigational and 
;   astronomical calculations which beset his age.
;   Napier's logarithms were quickly overtaken by logarithms to the base 10
;   devised, in conjunction with Napier, by Henry Briggs a Cambridge-educated 
;   professor of Geometry at Oxford University. These simplified the layout
;   of the tables enabling humans to easily scale calculations.
;
;   It is only recently with the introduction of pocket calculators and
;   computers like the ZX81 that natural logarithms are once more at the fore,
;   although some computers retain logarithms to the base ten.
;   'Natural' logarithms are powers to the base 'e', which like 'pi' is a 
;   naturally occurring number in branches of mathematics.
;   Like 'pi' also, 'e' is an irrational number and starts 2.718281828...
;
;   The tabular use of logarithms was that to multiply two numbers one looked
;   up their two logarithms in the tables, added them together and then looked 
;   for the result in a table of antilogarithms to give the desired product.
;
;   The EXP function is the BASIC equivalent of a calculator's 'antiln' function 
;   and by picking any two numbers, 1.72 and 6.89 say,
;     10 PRINT EXP ( LN 1.72 + LN 6.89 ) 
;   will give just the same result as
;     20 PRINT 1.72 * 6.89.
;   Division is accomplished by subtracting the two logs.
;
;   Napier also mentioned "square and cubicle extractions". 
;   To raise a number to the power 3, find its 'ln', multiply by 3 and find the 
;   'antiln'.  e.g. PRINT EXP( LN 4 * 3 )  gives 64.
;   Similarly to find the n'th root divide the logarithm by 'n'.
;   The ZX81 ROM used PRINT EXP ( LN 9 / 2 ) to find the square root of the 
;   number 9. The Napieran square root function is just a special case of 
;   the 'to_power' function. A cube root or indeed any root/power would be just
;   as simple.

;   First test that the argument to LN is a positive, non-zero number.

;; ln
ln:
	CALL conv_to_fp	;; Routine assumes floating point form.
		;; (MW: TODO: Figure out where... :-) )
	RST fp_calc	;; FP-CALC
	DEFB cc_duplicate	;;duplicate
	DEFB cc_greater_0	;;greater-0
	DEFB cc_jump_true	;;jump-true
	DEFB valid - $	;;to VALID

	DEFB cc_end_calc	;;end-calc


;; REPORT-Ab
report_ab:
	RST error	; ERROR-1
	DEFB err_code_invalid_argument	; Error Report: Invalid argument

;; VALID
valid:
	DEFB cc_stk_zero	;;stk-zero              Note. not
	DEFB cc_delete	;;delete                necessary.
	DEFB cc_end_calc	;;end-calc
	LD A,(HL)	;

	LD (HL),$80	;
	CALL stack_a	; routine STACK-A

	RST fp_calc	;; FP-CALC
	DEFB cc_stk_data	;;stk-data
	DEFB $38	;;Exponent: $88, Bytes: 1
	DEFB $00	;;(+00,+00,+00)
	DEFB cc_subtract	;;subtract
	DEFB cc_exchange	;;exchange
	DEFB cc_duplicate	;;duplicate
	DEFB cc_stk_data	;;stk-data
	DEFB $F0	;;Exponent: $80, Bytes: 4
	DEFB $4C,$CC,$CC,$CD	;;
	DEFB cc_subtract	;;subtract
	DEFB cc_greater_0	;;greater-0
	DEFB cc_jump_true	;;jump-true
	DEFB gre_8 - $	;;to GRE.8

	DEFB cc_exchange	;;exchange
	DEFB cc_stk_one	;;stk-one
	DEFB cc_subtract	;;subtract
	DEFB cc_exchange	;;exchange
	DEFB cc_end_calc	;;end-calc

	INC (HL)	;

	RST fp_calc	;; FP-CALC

;; GRE.8
gre_8:
	DEFB cc_exchange	;;exchange
	DEFB cc_stk_data	;;stk-data
	DEFB $F0	;;Exponent: $80, Bytes: 4
	DEFB $31,$72,$17,$F8	;;
	DEFB cc_multiply	;;multiply
	DEFB cc_exchange	;;exchange
	DEFB cc_stk_half	;;stk-half
	DEFB cc_subtract	;;subtract
	DEFB cc_stk_half	;;stk-half
	DEFB cc_subtract	;;subtract
	DEFB cc_duplicate	;;duplicate
	DEFB cc_stk_data	;;stk-data
	DEFB $32	;;Exponent: $82, Bytes: 1
	DEFB $20	;;(+00,+00,+00)
	DEFB cc_multiply	;;multiply
	DEFB cc_stk_half	;;stk-half
	DEFB cc_subtract	;;subtract
	DEFB cc_series_0c	;;series-0C
	DEFB $11	;;Exponent: $61, Bytes: 1
	DEFB $AC	;;(+00,+00,+00)
	DEFB $14	;;Exponent: $64, Bytes: 1
	DEFB $09	;;(+00,+00,+00)
	DEFB $56	;;Exponent: $66, Bytes: 2
	DEFB $DA,$A5	;;(+00,+00)
	DEFB $59	;;Exponent: $69, Bytes: 2
	DEFB $30,$C5	;;(+00,+00)
	DEFB $5C	;;Exponent: $6C, Bytes: 2
	DEFB $90,$AA	;;(+00,+00)
	DEFB $9E	;;Exponent: $6E, Bytes: 3
	DEFB $70,$6F,$61	;;(+00)
	DEFB $A1	;;Exponent: $71, Bytes: 3
	DEFB $CB,$DA,$96	;;(+00)
	DEFB $A4	;;Exponent: $74, Bytes: 3
	DEFB $31,$9F,$B4	;;(+00)
	DEFB $E7	;;Exponent: $77, Bytes: 4
	DEFB $A0,$FE,$5C,$FC	;;
	DEFB $EA	;;Exponent: $7A, Bytes: 4
	DEFB $1B,$43,$CA,$36	;;
	DEFB $ED	;;Exponent: $7D, Bytes: 4
	DEFB $A7,$9C,$7E,$5E	;;
	DEFB $F0	;;Exponent: $80, Bytes: 4
	DEFB $6E,$23,$80,$93	;;
	DEFB cc_multiply	;;multiply
	DEFB cc_addition	;;addition
	DEFB cc_end_calc	;;end-calc

	RET	; return.

; -----------------------------
; THE 'TRIGONOMETRIC' FUNCTIONS
; -----------------------------
;   Trigonometry is rocket science. It is also used by carpenters and pyramid
;   builders. 
;   Some uses can be quite abstract but the principles can be seen in simple
;   right-angled triangles. Triangles have some special properties -
;
;   1) The sum of the three angles is always PI radians (180 degrees).
;      Very helpful if you know two angles and wish to find the third.
;   2) In any right-angled triangle the sum of the squares of the two shorter
;      sides is equal to the square of the longest side opposite the right-angle.
;      Very useful if you know the length of two sides and wish to know the
;      length of the third side.
;   3) Functions sine, cosine and tangent enable one to calculate the length 
;      of an unknown side when the length of one other side and an angle is 
;      known.
;   4) Functions arcsin, arccosine and arctan enable one to calculate an unknown
;      angle when the length of two of the sides is known.

; --------------------------------
; THE 'REDUCE ARGUMENT' SUBROUTINE
; --------------------------------
; (offset $35: 'get-argt')
;
;   This routine performs two functions on the angle, in radians, that forms
;   the argument to the sine and cosine functions.
;   First it ensures that the angle 'wraps round'. That if a ship turns through 
;   an angle of, say, 3*PI radians (540 degrees) then the net effect is to turn 
;   through an angle of PI radians (180 degrees).
;   Secondly it converts the angle in radians to a fraction of a right angle,
;   depending within which quadrant the angle lies, with the periodicity 
;   resembling that of the desired sine value.
;   The result lies in the range -1 to +1.              
;
;                       90 deg.
; 
;                       (pi/2)
;                II       +1        I
;                         |
;          sin+      |\   |   /|    sin+
;          cos-      | \  |  / |    cos+
;          tan-      |  \ | /  |    tan+
;                    |   \|/)  |           
;   180 deg. (pi) 0 -|----+----|-- 0  (0)   0 degrees
;                    |   /|\   |
;          sin-      |  / | \  |    sin-
;          cos-      | /  |  \ |    cos+
;          tan+      |/   |   \|    tan-
;                         |
;                III      -1       IV
;                       (3pi/2)
;
;                       270 deg.


;; get-argt
get_argt:
	RST fp_calc	;; FP-CALC         X.
	DEFB cc_stk_data	;;stk-data
	DEFB $EE	;;Exponent: $7E,
		;;Bytes: 4
	DEFB $22,$F9,$83,$6E	;;                 X, 1/(2*PI)
	DEFB cc_multiply	;;multiply         X/(2*PI) = fraction

	DEFB cc_duplicate	;;duplicate
	DEFB cc_stk_half	;;stk-half
	DEFB cc_addition	;;addition
	DEFB cc_int	;;int

	DEFB cc_subtract	;;subtract         now range -.5 to .5

	DEFB cc_duplicate	;;duplicate
	DEFB cc_addition	;;addition         now range -1 to 1.
	DEFB cc_duplicate	;;duplicate
	DEFB cc_addition	;;addition         now range -2 to 2.

;   quadrant I (0 to +1) and quadrant IV (-1 to 0) are now correct.
;   quadrant II ranges +1 to +2.
;   quadrant III ranges -2 to -1.

	DEFB cc_duplicate	;;duplicate        Y, Y.
	DEFB cc_abs	;;abs              Y, abs(Y).    range 1 to 2
	DEFB cc_stk_one	;;stk-one          Y, abs(Y), 1.
	DEFB cc_subtract	;;subtract         Y, abs(Y)-1.  range 0 to 1
	DEFB cc_duplicate	;;duplicate        Y, Z, Z.
	DEFB cc_greater_0	;;greater-0        Y, Z, (1/0).

	DEFB cc_st_mem_0	;;st-mem-0         store as possible sign
		;;                 for cosine function.

	DEFB cc_jump_true	;;jump-true
	DEFB zplus - $	;;to ZPLUS  with quadrants II and III

;   else the angle lies in quadrant I or IV and value Y is already correct.

	DEFB cc_delete	;;delete          Y    delete test value.
	DEFB cc_end_calc	;;end-calc        Y.

	RET	; return.         with Q1 and Q4 >>>

;   The branch was here with quadrants II (0 to 1) and III (1 to 0).
;   Y will hold -2 to -1 if this is quadrant III.

;; ZPLUS
zplus:
	DEFB cc_stk_one	;;stk-one         Y, Z, 1
	DEFB cc_subtract	;;subtract        Y, Z-1.       Q3 = 0 to -1
	DEFB cc_exchange	;;exchange        Z-1, Y.
	DEFB cc_less_0	;;less-0          Z-1, (1/0).
	DEFB cc_jump_true	;;jump-true       Z-1.
	DEFB yneg - $	;;to YNEG
		;;if angle in quadrant III

;   else angle is within quadrant II (-1 to 0)

	DEFB cc_negate	;;negate          range +1 to 0


;; YNEG

yneg:
	DEFB cc_end_calc	;;end-calc        quadrants II and III correct.

	RET	; return.

; ---------------------
; THE 'COSINE' FUNCTION
; ---------------------
; (offset $1D: 'cos')
;   Cosines are calculated as the sine of the opposite angle rectifying the 
;   sign depending on the quadrant rules. 
;
;
;             /|
;          h /y|
;           /  |o
;          /x  |
;         /----|    
;           a
;
;   The cosine of angle x is the adjacent side (a) divided by the hypotenuse 1.
;   However if we examine angle y then a/h is the sine of that angle.
;   Since angle x plus angle y equals a right-angle, we can find angle y by 
;   subtracting angle x from pi/2.
;   However it's just as easy to reduce the argument first and subtract the
;   reduced argument from the value 1 (a reduced right-angle).
;   It's even easier to subtract 1 from the angle and rectify the sign.
;   In fact, after reducing the argument, the absolute value of the argument
;   is used and rectified using the test result stored in mem-0 by 'get-argt'
;   for that purpose.

;; cos
cos:
	RST fp_calc	;; FP-CALC              angle in radians.
	DEFB cc_get_argt	;;get-argt              X       reduce -1 to +1

	DEFB cc_abs	;;abs                   ABS X   0 to 1
	DEFB cc_stk_one	;;stk-one               ABS X, 1.
	DEFB cc_subtract	;;subtract              now opposite angle
		;;                      though negative sign.
	DEFB cc_get_mem_0	;;get-mem-0             fetch sign indicator.
	DEFB cc_jump_true	;;jump-true
	DEFB c_ent - $	;;fwd to C-ENT
		;;forward to common code if in QII or QIII


	DEFB cc_negate	;;negate                else make positive.
	DEFB cc_jump	;;jump
	DEFB c_ent - $	;;fwd to C-ENT
		;;with quadrants QI and QIV

; -------------------
; THE 'SINE' FUNCTION
; -------------------
; (offset $1C: 'sin')
;   This is a fundamental transcendental function from which others such as cos
;   and tan are directly, or indirectly, derived.
;   It uses the series generator to produce Chebyshev polynomials.
;
;
;             /|
;          1 / |
;           /  |x
;          /a  |
;         /----|    
;           y
;
;   The 'get-argt' function is designed to modify the angle and its sign 
;   in line with the desired sine value and afterwards it can launch straight
;   into common code.

;; sin
sin:
	RST fp_calc	;; FP-CALC      angle in radians
	DEFB cc_get_argt	;;get-argt      reduce - sign now correct.

;; C-ENT
c_ent:
	DEFB cc_duplicate	;;duplicate
	DEFB cc_duplicate	;;duplicate
	DEFB cc_multiply	;;multiply
	DEFB cc_duplicate	;;duplicate
	DEFB cc_addition	;;addition
	DEFB cc_stk_one	;;stk-one
	DEFB cc_subtract	;;subtract

	DEFB cc_series_06	;;series-06
	DEFB $14	;;Exponent: $64, Bytes: 1
	DEFB $E6	;;(+00,+00,+00)
	DEFB $5C	;;Exponent: $6C, Bytes: 2
	DEFB $1F,$0B	;;(+00,+00)
	DEFB $A3	;;Exponent: $73, Bytes: 3
	DEFB $8F,$38,$EE	;;(+00)
	DEFB $E9	;;Exponent: $79, Bytes: 4
	DEFB $15,$63,$BB,$23	;;
	DEFB $EE	;;Exponent: $7E, Bytes: 4
	DEFB $92,$0D,$CD,$ED	;;
	DEFB $F1	;;Exponent: $81, Bytes: 4
	DEFB $23,$5D,$1B,$EA	;;

	DEFB cc_multiply	;;multiply
	DEFB cc_end_calc	;;end-calc

	RET	; return.

; ----------------------
; THE 'TANGENT' FUNCTION
; ----------------------
; (offset $1E: 'tan')
;
;   Evaluates tangent x as    sin(x) / cos(x).
;
;
;             /|
;          h / |
;           /  |o
;          /x  |
;         /----|    
;           a
;
;   The tangent of angle x is the ratio of the length of the opposite side 
;   divided by the length of the adjacent side. As the opposite length can 
;   be calculates using sin(x) and the adjacent length using cos(x) then 
;   the tangent can be defined in terms of the previous two functions.

;   Error 6 if the argument, in radians, is too close to one like pi/2
;   which has an infinite tangent. e.g. PRINT TAN (PI/2)  evaluates as 1/0.
;   Similarly PRINT TAN (3*PI/2), TAN (5*PI/2) etc.

;; tan
tan:
	RST fp_calc	;; FP-CALC          x.
	DEFB cc_duplicate	;;duplicate         x, x.
	DEFB cc_sin	;;sin               x, sin x.
	DEFB cc_exchange	;;exchange          sin x, x.
	DEFB cc_cos	;;cos               sin x, cos x.
	DEFB cc_division	;;division          sin x/cos x (= tan x).
	DEFB cc_end_calc	;;end-calc          tan x.

	RET	; return.

; ---------------------
; THE 'ARCTAN' FUNCTION
; ---------------------
; (Offset $21: 'atn')
;   The inverse tangent function with the result in radians.
;   This is a fundamental transcendental function from which others such as
;   asn and acs are directly, or indirectly, derived.
;   It uses the series generator to produce Chebyshev polynomials.

;; atn
atn:
	CALL conv_to_fp	; convert integer form to floating point
	LD A,(HL)	; fetch exponent
	CP $81	; compare to that for 'one'
	JR C,small	; forward, if less, to SMALL

	RST fp_calc	;; FP-CALC      X.
	DEFB cc_stk_one	;;stk-one
	DEFB cc_negate	;;negate
	DEFB cc_exchange	;;exchange
	DEFB cc_division	;;division
	DEFB cc_duplicate	;;duplicate
	DEFB cc_less_0	;;less-0
	DEFB cc_stk_pi_div_2	;;stk-pi/2
	DEFB cc_exchange	;;exchange
	DEFB cc_jump_true	;;jump-true
	DEFB cases - $	;;to CASES

	DEFB cc_negate	;;negate
	DEFB cc_jump	;;jump
	DEFB cases - $	;;to CASES

; ---

;; SMALL
small:	RST fp_calc	;; FP-CALC
	DEFB cc_stk_zero	;;stk-zero

;; CASES
cases:
	DEFB cc_exchange	;;exchange
	DEFB cc_duplicate	;;duplicate
	DEFB cc_duplicate	;;duplicate
	DEFB cc_multiply	;;multiply
	DEFB cc_duplicate	;;duplicate
	DEFB cc_addition	;;addition
	DEFB cc_stk_one	;;stk-one
	DEFB cc_subtract	;;subtract

	DEFB cc_series_0c	;;series-0C
	DEFB $10	;;Exponent: $60, Bytes: 1
	DEFB $B2	;;(+00,+00,+00)
	DEFB $13	;;Exponent: $63, Bytes: 1
	DEFB $0E	;;(+00,+00,+00)
	DEFB $55	;;Exponent: $65, Bytes: 2
	DEFB $E4,$8D	;;(+00,+00)
	DEFB $58	;;Exponent: $68, Bytes: 2
	DEFB $39,$BC	;;(+00,+00)
	DEFB $5B	;;Exponent: $6B, Bytes: 2
	DEFB $98,$FD	;;(+00,+00)
	DEFB $9E	;;Exponent: $6E, Bytes: 3
	DEFB $00,$36,$75	;;(+00)
	DEFB $A0	;;Exponent: $70, Bytes: 3
	DEFB $DB,$E8,$B4	;;(+00)
	DEFB $63	;;Exponent: $73, Bytes: 2
	DEFB $42,$C4	;;(+00,+00)
	DEFB $E6	;;Exponent: $76, Bytes: 4
	DEFB $B5,$09,$36,$BE	;;
	DEFB $E9	;;Exponent: $79, Bytes: 4
	DEFB $36,$73,$1B,$5D	;;
	DEFB $EC	;;Exponent: $7C, Bytes: 4
	DEFB $D8,$DE,$63,$BE	;;
	DEFB $F0	;;Exponent: $80, Bytes: 4
	DEFB $61,$A1,$B3,$0C	;;

	DEFB cc_multiply	;;multiply
	DEFB cc_addition	;;addition
	DEFB cc_end_calc	;;end-calc

	RET	; return.

; ---------------------
; THE 'ARCSIN' FUNCTION
; ---------------------
; (Offset $1F: 'asn')
;   The inverse sine function with result in radians.
;   Derived from arctan function above.
;   Error A unless the argument is between -1 and +1 inclusive.
;   Uses an adaptation of the formula asn(x) = atn(x/sqr(1-x*x))
;
;
;                 /|
;                / |
;              1/  |x
;              /a  |
;             /----|    
;               y
;
;   e.g. We know the opposite side (x) and hypotenuse (1) 
;   and we wish to find angle a in radians.
;   We can derive length y by Pythagoras and then use ATN instead. 
;   Since y*y + x*x = 1*1 (Pythagoras Theorem) then
;   y=sqr(1-x*x)                         - no need to multiply 1 by itself.
;   So, asn(a) = atn(x/y)
;   or more fully,
;   asn(a) = atn(x/sqr(1-x*x))

;   Close but no cigar.

;   While PRINT ATN (x/SQR (1-x*x)) gives the same results as PRINT ASN x,
;   it leads to division by zero when x is 1 or -1.
;   To overcome this, 1 is added to y giving half the required angle and the 
;   result is then doubled. 
;   That is, PRINT ATN (x/(SQR (1-x*x) +1)) *2
;
;
;               . /|
;            .  c/ |
;         .     /1 |x
;      . c   b /a  |
;    ---------/----|    
;      1      y
;
;   By creating an isosceles triangle with two equal sides of 1, angles c and 
;   c are also equal. If b+c+d = 180 degrees and b+a = 180 degrees then c=a/2.
;
;   A value higher than 1 gives the required error as attempting to find  the
;   square root of a negative number generates an error in Sinclair BASIC.

;; asn
asn:
	RST fp_calc	;; FP-CALC      x.
	DEFB cc_duplicate	;;duplicate     x, x.
	DEFB cc_duplicate	;;duplicate     x, x, x.
	DEFB cc_multiply	;;multiply      x, x*x.
	DEFB cc_stk_one	;;stk-one       x, x*x, 1.
	DEFB cc_subtract	;;subtract      x, x*x-1.
	DEFB cc_negate	;;negate        x, 1-x*x.
	DEFB cc_sqr	;;sqr           x, sqr(1-x*x) = y.
	DEFB cc_stk_one	;;stk-one       x, y, 1.
	DEFB cc_addition	;;addition      x, y+1.
	DEFB cc_division	;;division      x/y+1.
	DEFB cc_atn	;;atn           a/2     (half the angle)
	DEFB cc_duplicate	;;duplicate     a/2, a/2.
	DEFB cc_addition	;;addition      a.
	DEFB cc_end_calc	;;end-calc      a.

	RET	; return.


; ------------------------
; THE 'ARCCOS' FUNCTION
; ------------------------
; (Offset $20: 'acs')
;   The inverse cosine function with the result in radians.
;   Error A unless the argument is between -1 and +1.
;   Result in range 0 to pi.
;   Derived from asn above which is in turn derived from the preceding atn. It 
;   could have been derived directly from atn using acs(x) = atn(sqr(1-x*x)/x).
;   However, as sine and cosine are horizontal translations of each other,
;   uses acs(x) = pi/2 - asn(x)

;   e.g. the arccosine of a known x value will give the required angle b in 
;   radians.
;   We know, from above, how to calculate the angle a using asn(x). 
;   Since the three angles of any triangle add up to 180 degrees, or pi radians,
;   and the largest angle in this case is a right-angle (pi/2 radians), then
;   we can calculate angle b as pi/2 (both angles) minus asn(x) (angle a).
; 
;
;            /|
;         1 /b|
;          /  |x
;         /a  |
;        /----|    
;          y

;; acs
acs:
	RST fp_calc	;; FP-CALC      x.
	DEFB cc_asn	;;asn           asn(x).
	DEFB cc_stk_pi_div_2	;;stk-pi/2      asn(x), pi/2.
	DEFB cc_subtract	;;subtract      asn(x) - pi/2.
	DEFB cc_negate	;;negate        pi/2 - asn(x) = acs(x).
	DEFB cc_end_calc	;;end-calc      acs(x)

	RET	; return.

; --------------------------
; THE 'SQUARE ROOT' FUNCTION
; --------------------------
; (Offset $25: 'sqr')
;   Error A if argument is negative.
;   This routine is remarkable for its brevity - 7 bytes.
;   The ZX81 code was originally 9K and various techniques had to be
;   used to shoe-horn it into an 8K Rom chip.


;; sqr
sqr:
	RST fp_calc	;; FP-CALC              x.
	DEFB cc_duplicate	;;duplicate             x, x.
	DEFB cc_fn_not	;;not                   x, 1/0
	DEFB cc_jump_true	;;jump-true             x, (1/0).
	DEFB last - $	;;to L1DFD, LAST        exit if argument zero
		;;                      with zero result.

;   else continue to calculate as x ** .5

	DEFB cc_stk_half	;;stk-half              x, .5.
	DEFB cc_end_calc	;;end-calc              x, .5.

; ------------------------------
; THE 'EXPONENTIATION' OPERATION
; ------------------------------
; (Offset $06: 'to-power')
;   This raises the first number X to the power of the second number Y.
;   As with the ZX80,
;   0 ** 0 = 1
;   0 ** +n = 0
;   0 ** -n = arithmetic overflow.

;; to-power
to_power:
	RST fp_calc	;; FP-CALC              X,Y.
	DEFB cc_exchange	;;exchange              Y,X.
	DEFB cc_duplicate	;;duplicate             Y,X,X.
	DEFB cc_fn_not	;;not                   Y,X,(1/0).
	DEFB cc_jump_true	;;jump-true
	DEFB xiso - $	;;forward to XISO if X is zero.

;   else X is non-zero. function 'ln' will catch a negative value of X.

	DEFB cc_ln	;;ln                    Y, LN X.
	DEFB cc_multiply	;;multiply              Y * LN X
	DEFB cc_end_calc	;;end-calc

	JP exp	; jump back to EXP routine.  ->

; ---

;   These routines form the three simple results when the number is zero.
;   begin by deleting the known zero to leave Y the power factor.

;; XISO
xiso:
	DEFB cc_delete	;;delete                Y.
	DEFB cc_duplicate	;;duplicate             Y, Y.
	DEFB cc_fn_not	;;not                   Y, (1/0).
	DEFB cc_jump_true	;;jump-true
	DEFB one - $	;;forward to L1DFB, ONE if Y is zero.

;   the power factor is not zero. If negative then an error exists.

	DEFB cc_stk_zero	;;stk-zero              Y, 0.
	DEFB cc_exchange	;;exchange              0, Y.
	DEFB cc_greater_0	;;greater-0             0, (1/0).
	DEFB cc_jump_true	;;jump-true             0
	DEFB last - $	;;to LAST        if Y was any positive
		;;                      number.

;   else force division by zero thereby raising an Arithmetic overflow error.
;   There are some one and two-byte alternatives but perhaps the most formal
;   might have been to use end-calc; rst 08; defb 05.

	DEFB cc_stk_one	;;stk-one               0, 1.
	DEFB cc_exchange	;;exchange              1, 0.
	DEFB cc_division	;;division              1/0    >> error

; ---

;; ONE
one:
	DEFB cc_delete	;;delete                .
	DEFB cc_stk_one	;;stk-one               1.

;; LAST
last:
	DEFB cc_end_calc	;;end-calc              last value 1 or 0.

	RET	; return.
