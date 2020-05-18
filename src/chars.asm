.assume adl=0

;
; Characters and strings. Relies on the defines in game.asm. Is not portable.
;

;Depends on:
;	Existence of page-aligned 8->16 bit expansion lookup tables
;	Text color defines
;	Screen buffer at address $8000
;
;Defines the following inline memory areas:
;	textcoords (LSB+X, MSB=Y)
;	textcolor	(two choices, each pointing to start of expansion LUTs)
;	
;Defines the following functions:
;	putchar					[in:A=char, dstr: AF DE HL]
;	putstr_centerpartial	[in:HL=str A=x_offset, out:HL=next_str, dstr: AF DE]
;	putstr_macro			[For use in macro. see routine. Also destroys IX]
;	putstr					[in:HL=str, dstr: AF DE]
;	putscore				[in:HL=adr_of_BE_BCD_num, dstr: AF BC DE HL]
;	getstrlen				[in:HL=str, out:A,B=strlen HL=str, dstr: N/A]
;	getstrfromidx			[in:HL=strarr B=idx, out:HL=str A,B=0]


;in: A=char;  out: character written to display
;rem: lazy will only do x in multiples of 4 px
;destroys: AF DE HL
putchar:
textcoords	.equ $+1
textpos		.equ $+1	; convenience equates
textx		.equ $+1	;
texty		.equ $+2	;
	ld hl,0		;SMC'd: H=Y, L=X
	ld e,L
	ld d,0		;DE=X offset
	ld L,80
	mlt hl
	add hl,de	;HL=offset into buffer
	set 7,h		;buffer starts at $8000
	push hl
	pop ix
	ld e,a
	ld hl,graphx_textdata
	ex de,hl
	add hl,hl
	add hl,hl
	add hl,hl	;x8
	add hl,de	;HL = address to character
	ld a,8
putchar_loop:
	ld e,(hl)			;Character data into LSB of pagealigned LUT
textcolor	.EQU $+1
	ld d,TEXT_BLACK		;MSB of LUT address div 2 into MSB DE.
	ex de,hl
	add hl,hl			;Shift left to complete the address.
	ld hl,(hl)			;Get 2bpp data from LUT
	ld (ix+0),hl		;and write it to the buffer
	ex de,hl
	inc hl				;point to next byte of character
	lea ix,ix+80		;point to next row in buffer
	dec a
	jr nz,putchar_loop
	ld hl,textcoords
	inc (hl)
	inc (hl)
	ret


;ok. can't get it to work with screen quartering as it is so we'll do the calc
;separately.
;inputs and outputs the same as putstr_centerpartial
putstr_centerquarter:
	ld b,-40+1
	jr putstr_centercontinue
;in: HL=strlen
;out: HL=adr_after_string
;Destroys: AF DE B
putstr_centerpartial:
	ld b,-80+1
putstr_centercontinue:
	push hl
		call getstrlen
		add a,a
		add a,b
		cpl
		srl a
		sbc a,0	;round down if a bit got shifted out
		ld hl,(textcoords)
		ld L,a
		ld a,h
		add a,12
		ld h,a
		ld (textcoords),hl
	pop hl
	jr putstr
	
;also destroys IX. BC is safe.
;call putstr_macro / .db XPOS,YPOS / .dw str_adr	
putstr_macro:
	pop ix
	ld hl,(ix+0+COMPILER_BUG_OFFSET)
	ld (textcoords),hl
	ld hl,(ix+2+COMPILER_BUG_OFFSET)
	pea ix+4+COMPILER_BUG_OFFSET
;in:  HL=zero_terminated_string
;out: HL=address_after_zero_terminator
;destroys: AF DE
putstr:
	ld a,(hl)
	inc hl
	or a
	ret z
	push hl
		call putchar
	pop hl
	jr putstr

	
;in: HL=4_byte_BCD_bigendian
;destroys: All but IY
putscore:
	xor a
	ld b,8
putscore_loop:
	push hl
		rld
		ld c,a
		add a,'0'
		call putchar
		ld a,c
	pop hl
	bit 0,b
	jr z,$+5
	rld			;To return (HL) to way it was before. Else corrupted scores
	inc hl
	djnz putscore_loop
	ret

;in:  HL=address_of_z-term_string
;out: HL is preserved, A= numchars string, zero flag set
getstrlen:
	ld a,-1
	push hl
getstrlen_loop:
		inc a
		inc (hl)
		dec (hl)	;frees up reg A for compare
		inc hl
		jr nz,getstrlen_loop
	pop hl
	ret

;in:	HL=start_of_string_array, B=index_to_get
;out:	HL=start_of_indexed_string
;destroys: A and B always zero.
;NOTE: This routine was used only once, so inlining it @ gametitle_printbottom
; getstrfromidx:
	; xor a
	; cp b
	; ret z
; getstrfromidx_loop:
	; cp (hl)
	; inc hl
	; jr nz,getstrfromidx_loop
	; djnz getstrfromidx_loop
	; ret

;zeroing out characters that aren't used to improve compression results
graphx_textdata		.EQU $-(8*' ')		;points to what would be start of ASCII
	.db	$00,$00,$00,$00,$00,$00,$00,$00 ;
	.db	$00,$00,$00,$00,$00,$00,$00,$00 ; !
	.db	$00,$00,$00,$00,$00,$00,$00,$00 ; "
	.db	$00,$00,$00,$00,$00,$00,$00,$00 ; #
	.db	$00,$00,$00,$00,$00,$00,$00,$00 ; $
	.db	$00,$00,$00,$00,$00,$00,$00,$00 ; %
	.db	$00,$00,$00,$00,$00,$00,$00,$00 ; &
	.db	$30,$30,$60,$00,$00,$00,$00,$00 ; '
	.db	$30,$60,$C0,$C0,$C0,$60,$30,$00 ; (
	.db	$C0,$60,$30,$30,$30,$60,$C0,$00 ; )
	.db	$00,$00,$00,$00,$00,$00,$00,$00 ; *
	.db	$00,$00,$00,$00,$00,$00,$00,$00 ; +
	.db	$00,$00,$00,$00,$00,$60,$60,$C0 ; ,
	.db	$00,$00,$00,$00,$00,$00,$00,$00 ; -
	.db	$00,$00,$00,$00,$00,$18,$18,$00 ; .
	.db	$06,$0C,$18,$30,$60,$C0,$80,$00 ; /
	.db	$7C,$CE,$DE,$F6,$E6,$C6,$7C,$00 ; 0
	.db	$30,$70,$30,$30,$30,$30,$FC,$00 ; 1
	.db	$7C,$C6,$06,$7C,$C0,$C0,$FE,$00 ; 2
	.db	$FC,$06,$06,$3C,$06,$06,$FC,$00 ; 3
	.db	$0C,$CC,$CC,$CC,$FE,$0C,$0C,$00 ; 4
	.db	$FE,$C0,$FC,$06,$06,$C6,$7C,$00 ; 5
	.db	$7C,$C0,$C0,$FC,$C6,$C6,$7C,$00 ; 6
	.db	$FE,$06,$06,$0C,$18,$30,$30,$00 ; 7
	.db	$7C,$C6,$C6,$7C,$C6,$C6,$7C,$00 ; 8
	.db	$7C,$C6,$C6,$7E,$06,$06,$7C,$00 ; 9
	.db	$00,$C0,$C0,$00,$00,$C0,$C0,$00 ; :
	.db	$00,$00,$00,$00,$00,$00,$00,$00 ; ;
	.db	$00,$00,$00,$00,$00,$00,$00,$00 ; <
	.db	$00,$00,$00,$00,$00,$00,$00,$00 ; =
	.db	$00,$00,$00,$00,$00,$00,$00,$00 ; >
	.db	$00,$00,$00,$00,$00,$00,$00,$00 ; ?
	.db	$00,$00,$00,$00,$00,$00,$00,$00 ; @
	.db	$38,$6C,$C6,$C6,$FE,$C6,$C6,$00 ; A
	.db	$FC,$C6,$C6,$FC,$C6,$C6,$FC,$00 ; B
	.db	$7C,$C6,$C0,$C0,$C0,$C6,$7C,$00 ; C
	.db	$F8,$CC,$C6,$C6,$C6,$CC,$F8,$00 ; D
	.db	$FE,$C0,$C0,$F8,$C0,$C0,$FE,$00 ; E
	.db	$FE,$C0,$C0,$F8,$C0,$C0,$C0,$00 ; F
	.db	$7C,$C6,$C0,$C0,$CE,$C6,$7C,$00 ; G
	.db	$C6,$C6,$C6,$FE,$C6,$C6,$C6,$00 ; H
	.db	$7E,$18,$18,$18,$18,$18,$7E,$00 ; I
	.db	$06,$06,$06,$06,$06,$C6,$7C,$00 ; J
	.db	$C6,$CC,$D8,$F0,$D8,$CC,$C6,$00 ; K
	.db	$C0,$C0,$C0,$C0,$C0,$C0,$FE,$00 ; L
	.db	$C6,$EE,$FE,$FE,$D6,$C6,$C6,$00 ; M
	.db	$C6,$E6,$F6,$DE,$CE,$C6,$C6,$00 ; N
	.db	$7C,$C6,$C6,$C6,$C6,$C6,$7C,$00 ; O
	.db	$FC,$C6,$C6,$FC,$C0,$C0,$C0,$00 ; P
	.db	$7C,$C6,$C6,$C6,$D6,$DE,$7C,$06 ; Q
	.db	$FC,$C6,$C6,$FC,$D8,$CC,$C6,$00 ; R
	.db	$7C,$C6,$C0,$7C,$06,$C6,$7C,$00 ; S
	.db	$FF,$18,$18,$18,$18,$18,$18,$00 ; T
	.db	$C6,$C6,$C6,$C6,$C6,$C6,$FE,$00 ; U
	.db	$C6,$C6,$C6,$C6,$C6,$7C,$38,$00 ; V
	.db	$C6,$C6,$C6,$C6,$D6,$FE,$6C,$00 ; W
	.db	$C6,$C6,$6C,$38,$6C,$C6,$C6,$00 ; X
	.db	$C6,$C6,$C6,$7C,$18,$30,$E0,$00 ; Y
	.db	$FE,$06,$0C,$18,$30,$60,$FE,$00 ; Z
	.db	$00,$00,$00,$00,$00,$00,$00,$00 ; [
	.db	$00,$00,$00,$00,$00,$00,$00,$00 ; \
	.db	$00,$00,$00,$00,$00,$00,$00,$00 ; ]
	.db	$00,$00,$00,$00,$00,$00,$00,$00 ; ^
	.db	$00,$00,$00,$00,$00,$00,$00,$00 ; _
	.db	$00,$00,$00,$00,$00,$00,$00,$00 ; `
	.db	$00,$00,$7C,$06,$7E,$C6,$7E,$00 ; a
	.db	$C0,$C0,$C0,$FC,$C6,$C6,$FC,$00 ; b
	.db	$00,$00,$7C,$C6,$C0,$C6,$7C,$00 ; c
	.db	$06,$06,$06,$7E,$C6,$C6,$7E,$00 ; d
	.db	$00,$00,$7C,$C6,$FE,$C0,$7C,$00 ; e
	.db	$1C,$36,$30,$78,$30,$30,$78,$00 ; f
	.db	$00,$00,$7E,$C6,$C6,$7E,$06,$FC ; g
	.db	$C0,$C0,$FC,$C6,$C6,$C6,$C6,$00 ; h
	.db	$18,$00,$38,$18,$18,$18,$3C,$00 ; i
	.db	$06,$00,$06,$06,$06,$06,$C6,$7C ; j
	.db	$C0,$C0,$CC,$D8,$F8,$CC,$C6,$00 ; k
	.db	$38,$18,$18,$18,$18,$18,$3C,$00 ; l
	.db	$00,$00,$CC,$FE,$FE,$D6,$D6,$00 ; m
	.db	$00,$00,$FC,$C6,$C6,$C6,$C6,$00 ; n
	.db	$00,$00,$7C,$C6,$C6,$C6,$7C,$00 ; o
	.db	$00,$00,$FC,$C6,$C6,$FC,$C0,$C0 ; p
	.db	$00,$00,$7E,$C6,$C6,$7E,$06,$06 ; q
	.db	$00,$00,$FC,$C6,$C0,$C0,$C0,$00 ; r
	.db	$00,$00,$7E,$C0,$7C,$06,$FC,$00 ; s
	.db	$30,$30,$FC,$30,$30,$30,$1C,$00 ; t
	.db	$00,$00,$C6,$C6,$C6,$C6,$7E,$00 ; u
	.db	$00,$00,$C6,$C6,$C6,$7C,$38,$00 ; v
	.db	$00,$00,$C6,$C6,$D6,$FE,$6C,$00 ; w
	.db	$00,$00,$C6,$6C,$38,$6C,$C6,$00 ; x
	.db	$00,$00,$C6,$C6,$C6,$7E,$06,$FC ; y
	.db	$00,$00,$FE,$0C,$38,$60,$FE,$00 ; z

