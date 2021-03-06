; zx81_ace.asm: Font derived from ZX81 and Jupiter Ace system fonts
; From the Open82 project
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
; E-mail: matthew@west.co.tt
; Postal address: 14 Daisy Hill Drive, Adlington, Chorley,
;                 Lancs, PR6 9NE, United Kingdom

; This is the font from the ZX81 with missing characters taken from a RAM
; dump of the Jupiter Ace (the Ace doesn't hold full characters in ROM).

	db 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	;
	db 0x00, 0x10, 0x10, 0x10, 0x10, 0x00, 0x10, 0x00	; !
	db 0x00, 0x24, 0x24, 0x00, 0x00, 0x00, 0x00, 0x00	; "
	db 0x00, 0x24, 0x7e, 0x24, 0x24, 0x7e, 0x24, 0x00	; #
	db 0x00, 0x08, 0x3e, 0x28, 0x3e, 0x0a, 0x3e, 0x08	; $
	db 0x00, 0x62, 0x64, 0x08, 0x10, 0x26, 0x46, 0x00	; %
	db 0x00, 0x10, 0x28, 0x10, 0x2a, 0x44, 0x3a, 0x00	; &
	db 0x00, 0x08, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00	; '
	db 0x00, 0x04, 0x08, 0x08, 0x08, 0x08, 0x04, 0x00	; (
	db 0x00, 0x20, 0x10, 0x10, 0x10, 0x10, 0x20, 0x00	; )
	db 0x00, 0x00, 0x14, 0x08, 0x3e, 0x08, 0x14, 0x00	; *
	db 0x00, 0x00, 0x08, 0x08, 0x3e, 0x08, 0x08, 0x00	; +
	db 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x08, 0x10	; ,
	db 0x00, 0x00, 0x00, 0x00, 0x3e, 0x00, 0x00, 0x00	; -
	db 0x00, 0x00, 0x00, 0x00, 0x00, 0x18, 0x18, 0x00	; .
	db 0x00, 0x00, 0x02, 0x04, 0x08, 0x10, 0x20, 0x00	; /
	db 0x00, 0x3c, 0x46, 0x4a, 0x52, 0x62, 0x3c, 0x00	; 0
	db 0x00, 0x18, 0x28, 0x08, 0x08, 0x08, 0x3e, 0x00	; 1
	db 0x00, 0x3c, 0x42, 0x02, 0x3c, 0x40, 0x7e, 0x00	; 2
	db 0x00, 0x3c, 0x42, 0x0c, 0x02, 0x42, 0x3c, 0x00	; 3
	db 0x00, 0x08, 0x18, 0x28, 0x48, 0x7e, 0x08, 0x00	; 4
	db 0x00, 0x7e, 0x40, 0x7c, 0x02, 0x42, 0x3c, 0x00	; 5
	db 0x00, 0x3c, 0x40, 0x7c, 0x42, 0x42, 0x3c, 0x00	; 6
	db 0x00, 0x7e, 0x02, 0x04, 0x08, 0x10, 0x10, 0x00	; 7
	db 0x00, 0x3c, 0x42, 0x3c, 0x42, 0x42, 0x3c, 0x00	; 8
	db 0x00, 0x3c, 0x42, 0x42, 0x3e, 0x02, 0x3c, 0x00	; 9
	db 0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x10, 0x00	; :
	db 0x00, 0x00, 0x10, 0x00, 0x00, 0x10, 0x10, 0x20	; ;
	db 0x00, 0x00, 0x04, 0x08, 0x10, 0x08, 0x04, 0x00	; <
	db 0x00, 0x00, 0x00, 0x3e, 0x00, 0x3e, 0x00, 0x00	; =
	db 0x00, 0x00, 0x10, 0x08, 0x04, 0x08, 0x10, 0x00	; >
	db 0x00, 0x3c, 0x42, 0x04, 0x08, 0x00, 0x08, 0x00	; ?
	db 0x00, 0x3c, 0x4a, 0x56, 0x5e, 0x40, 0x3c, 0x00	; @
	db 0x00, 0x3c, 0x42, 0x42, 0x7e, 0x42, 0x42, 0x00	; A
	db 0x00, 0x7c, 0x42, 0x7c, 0x42, 0x42, 0x7c, 0x00	; B
	db 0x00, 0x3c, 0x42, 0x40, 0x40, 0x42, 0x3c, 0x00	; C
	db 0x00, 0x78, 0x44, 0x42, 0x42, 0x44, 0x78, 0x00	; D
	db 0x00, 0x7e, 0x40, 0x7c, 0x40, 0x40, 0x7e, 0x00	; E
	db 0x00, 0x7e, 0x40, 0x7c, 0x40, 0x40, 0x40, 0x00	; F
	db 0x00, 0x3c, 0x42, 0x40, 0x4e, 0x42, 0x3c, 0x00	; G
	db 0x00, 0x42, 0x42, 0x7e, 0x42, 0x42, 0x42, 0x00	; H
	db 0x00, 0x3e, 0x08, 0x08, 0x08, 0x08, 0x3e, 0x00	; I
	db 0x00, 0x02, 0x02, 0x02, 0x42, 0x42, 0x3c, 0x00	; J
	db 0x00, 0x44, 0x48, 0x70, 0x48, 0x44, 0x42, 0x00	; K
	db 0x00, 0x40, 0x40, 0x40, 0x40, 0x40, 0x7e, 0x00	; L
	db 0x00, 0x42, 0x66, 0x5a, 0x42, 0x42, 0x42, 0x00	; M
	db 0x00, 0x42, 0x62, 0x52, 0x4a, 0x46, 0x42, 0x00	; N
	db 0x00, 0x3c, 0x42, 0x42, 0x42, 0x42, 0x3c, 0x00	; O
	db 0x00, 0x7c, 0x42, 0x42, 0x7c, 0x40, 0x40, 0x00	; P
	db 0x00, 0x3c, 0x42, 0x42, 0x52, 0x4a, 0x3c, 0x00	; Q
	db 0x00, 0x7c, 0x42, 0x42, 0x7c, 0x44, 0x42, 0x00	; R
	db 0x00, 0x3c, 0x40, 0x3c, 0x02, 0x42, 0x3c, 0x00	; S
	db 0x00, 0xfe, 0x10, 0x10, 0x10, 0x10, 0x10, 0x00	; T
	db 0x00, 0x42, 0x42, 0x42, 0x42, 0x42, 0x3c, 0x00	; U
	db 0x00, 0x42, 0x42, 0x42, 0x42, 0x24, 0x18, 0x00	; V
	db 0x00, 0x42, 0x42, 0x42, 0x42, 0x5a, 0x24, 0x00	; W
	db 0x00, 0x42, 0x24, 0x18, 0x18, 0x24, 0x42, 0x00	; X
	db 0x00, 0x82, 0x44, 0x28, 0x10, 0x10, 0x10, 0x00	; Y
	db 0x00, 0x7e, 0x04, 0x08, 0x10, 0x20, 0x7e, 0x00	; Z
	db 0x00, 0x0e, 0x08, 0x08, 0x08, 0x08, 0x0e, 0x00	; [
	db 0x00, 0x00, 0x40, 0x20, 0x10, 0x08, 0x04, 0x00	; \
	db 0x00, 0x70, 0x10, 0x10, 0x10, 0x10, 0x70, 0x00	; ]
	db 0x00, 0x10, 0x38, 0x54, 0x10, 0x10, 0x10, 0x00	; ^
	db 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xff	; _
	db 0x00, 0x1c, 0x22, 0x78, 0x20, 0x20, 0x7e, 0x00	; pound
	db 0x00, 0x00, 0x38, 0x04, 0x3c, 0x44, 0x3e, 0x00	; a
	db 0x00, 0x20, 0x20, 0x3c, 0x22, 0x22, 0x3c, 0x00	; b
	db 0x00, 0x00, 0x1c, 0x20, 0x20, 0x20, 0x1c, 0x00	; c
	db 0x00, 0x04, 0x04, 0x3c, 0x44, 0x44, 0x3e, 0x00	; d
	db 0x00, 0x00, 0x38, 0x44, 0x78, 0x40, 0x3c, 0x00	; e
	db 0x00, 0x0c, 0x10, 0x18, 0x10, 0x10, 0x10, 0x00	; f
	db 0x00, 0x00, 0x3c, 0x44, 0x44, 0x3c, 0x04, 0x38	; g
	db 0x00, 0x40, 0x40, 0x78, 0x44, 0x44, 0x44, 0x00	; h
	db 0x00, 0x10, 0x00, 0x30, 0x10, 0x10, 0x38, 0x00	; i
	db 0x00, 0x04, 0x00, 0x04, 0x04, 0x04, 0x24, 0x18	; j
	db 0x00, 0x20, 0x28, 0x30, 0x30, 0x28, 0x24, 0x00	; k
	db 0x00, 0x10, 0x10, 0x10, 0x10, 0x10, 0x0c, 0x00	; l
	db 0x00, 0x00, 0x68, 0x54, 0x54, 0x54, 0x54, 0x00	; m
	db 0x00, 0x00, 0x78, 0x44, 0x44, 0x44, 0x44, 0x00	; n
	db 0x00, 0x00, 0x38, 0x44, 0x44, 0x44, 0x38, 0x00	; o
	db 0x00, 0x00, 0x78, 0x44, 0x44, 0x78, 0x40, 0x40	; p
	db 0x00, 0x00, 0x3c, 0x44, 0x44, 0x3c, 0x04, 0x06	; q
	db 0x00, 0x00, 0x1c, 0x20, 0x20, 0x20, 0x20, 0x00	; r
	db 0x00, 0x00, 0x38, 0x40, 0x38, 0x04, 0x78, 0x00	; s
	db 0x00, 0x10, 0x38, 0x10, 0x10, 0x10, 0x0c, 0x00	; t
	db 0x00, 0x00, 0x44, 0x44, 0x44, 0x44, 0x3c, 0x00	; u
	db 0x00, 0x00, 0x44, 0x44, 0x28, 0x28, 0x10, 0x00	; v
	db 0x00, 0x00, 0x44, 0x54, 0x54, 0x54, 0x28, 0x00	; w
	db 0x00, 0x00, 0x44, 0x28, 0x10, 0x28, 0x44, 0x00	; x
	db 0x00, 0x00, 0x44, 0x44, 0x44, 0x3c, 0x04, 0x38	; y
	db 0x00, 0x00, 0x7c, 0x08, 0x10, 0x20, 0x7c, 0x00	; z
	db 0x00, 0x0e, 0x08, 0x30, 0x30, 0x08, 0x0e, 0x00	; {
	db 0x00, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x00	; |
	db 0x00, 0x70, 0x10, 0x0c, 0x0c, 0x10, 0x70, 0x00	; }
	db 0x32, 0x4c, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00	; ~
	db 0x3c, 0x42, 0x99, 0xa1, 0xa1, 0x99, 0x42, 0x3c	; (c)
