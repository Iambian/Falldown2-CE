#include "src/ti84pce_short.inc"

.assume adl=0
;Set to 0 when somebody fixes instruction width bug in Z80 mode.
#define COMPILER_BUG_OFFSET 0

#define COLOR_WHITE 00h
#define COLOR_DGRAY 01h
#define COLOR_LGRAY 02h
#define COLOR_BLACK 03h

#define SET_BRK(adr)	push af \ push hl \ push de \ scf \ sbc hl,hl \ ld de,adr \ ld (hl),2 \ pop de\ pop hl \ pop af
#define RES_BRK(adr)	push af \ push hl \ push de \ scf \ sbc hl,hl \ ld de,adr \ ld (hl),4 \ pop de\ pop hl \ pop af


#define PALETTE_GREEN 1000001111100000b
#define PALETTE_DGRAY 1010110101101011b

#define TEXT_GREEN (0F2h>>1)+0
#define TEXT_BLACK (0F4h>>1)+0
#define PUTSTR_XY(x,y,s)	call putstr_macro \.db x,y \.dw s

;The buffer is hard-coded to 8000h all over the program. DO NOT CHANGE THIS.
#define LCD_SCREEN 03500h
#define LCD_BUFFER 08000h

#define USERVARS_START	0F000h

;2b gap
score		.EQU 0F002h			;4, score in BCD big-endian format
subscore	.EQU 0F006h			;2. 8.8fp to allow progressive scoring
;4b gap
ball_y		.EQU 0F00Ch			;1, current ball y position (fine, 0 to (240-16-8))
ball_x		.EQU 0F00Dh			;1, current ball x position (granular, 0 to 80)
scrolldist	.EQU 0F00Eh			;1, amount of pixels to scroll each frame
curscroll	.EQU 0F00Fh			;1, current scroll away from adding new blocks
newscroll	.EQU 0F010h			;1, pixels between each row of blocks
narrowing	.EQU 0F011h			;1, decs on new blocks. on zero, decs newscroll
newnarrow	.EQU 0F012h			;1, next value on narrowing reset
rowgaps		.EQU 0F013h			;6, up to six gaps in the row heading
;2b gap
cursorpos	.EQU 0F01Ah			;1, 0-3 current cursor position.
ballcounter .EQU 0F01Bh			;1. all the flips


;-----------------------------------------------------------------------------
;DO NOT CHANGE THIS EQUATE. THIS ADDRESS WAS CAREFULLY CHOSEN AS PART
;OF ANOTHER INSTRUCTION
is_swrite	.EQU 0FB18h			;1, equ 1 when screen interrupt is acknowledged
;-----------------------------------------------------------------------------

;external file backup. Try to put to as close to end of memory as possible
versioncode	.EQU 0FDFEh			;1, reserved in case it's ever used.
speed		.EQU 0FDFFh			;1, 0-3 available speeds
hiscore0	.EQU 0FE00h			;4, BCD big-end fmt, hiscore for spd: slow
hiscore1	.EQU 0FE04h			;4, BCD big-end fmt, hiscore for spd: medium
hiscore2	.EQU 0FE08h			;4, BCD big-end fmt, hiscore for spd: fast
hiscore3	.EQU 0FE0Ch			;4, BCD big-end fmt, hiscore for spd: hyper

;lookup tables
widen01		.EQU 0F200h			;512 GREEN
widen11		.EQU 0F400h			;512 BLACK
widenall	.EQU 0F600h			;512 accelerates supersize_me routine

#define	start_r			00h
#define	getkbd_r		08h
#define getkbdwait_r	10h
#define setpalette_r	18h
#define screen_wait_r	20h
#define clearmem_r		28h


.org 0			;RST 00h -------------------------------------------------
init:
.assume adl=1
	jr protramStart_adl_frz80
.fill 0008h-$,0	;RST 08h -------------------------------------------------
getkbd:
	di
	call.il getkbd_adl
	jr lfsr2		;cycle through random values any time game waits on keys
.fill 0010h-$,0	;RST 10h -------------------------------------------------
getkbdwait:
	rst getkbd_r
	push af
		rst getkbd_r
		or a
		jr nz,$-2
	pop af
	ret	;8b
;--------------  RST 18h -------------------------------------------------
setpalette:
	jr setpalette_continue
.db 00h
.db 00h
.db 00h
	ei
	xor a
.db 021h		;(start of) ld hl,$FB18
screen_wait:
.db 018h		;RST 20h -------------------------------------------------
.db 0FBh		;;boundary straddles. Forms "jr $-3" at rst 20h.
	ld (hl),a	;3
	halt		;4
	cp (hl)		;5
	jr z,$-2	;7	
	ret			;8 doing this was incredibly stupid. why? stupid stupid stupid
;--------------	;RST 28h -------------------------------------------------
clearmem:		;have to manually decrement BC here.
	ld e,L
	ld d,H
	inc de
	ld (hl),0
	ldir
	ret			;8 bytes.
;--------------	;RST 30h -------------------------------------------------

.fill 0038h-$,0	;RST 38h -------------------------------------------------
interrupt_service_routine:
	push hl
		push af
			call.il isr_adl		;acknowledge interrupt(s) 
			ld a,(is_swrite)
			or a
			jr nz,isr_dont_copy		;don't write to screen if previous write not ackn
			push de
				push bc
					ld hl,LCD_BUFFER
					ld de,LCD_SCREEN
					ld bc,(240*320)/4
					ldir
				pop bc
			pop de
			inc a
			ld (is_swrite),a		;set to 1
isr_dont_copy:
		pop af
	pop hl
	ei
	ret
;-----------------------------------------------------------------------------
setpalette_continue:
	di
	call.il newPaletteEntry_adl
	ei
	ret
lfsr2:
	push af \ call lfsr \ pop af \ ei \ ret
;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------

protramStart_adl_frz80:
.assume adl=1
.org $ + 0D40000h
programStart_adl:
	ld.sis sp,0		;make sure SPS is set to a known value that works.
	;setup graphics
	ld de, lcdBpp2+lcdBgr+lcdBigEndianPixels+lcdPwr+lcdWatermark+lcdIntFront
	ld bc,0D43500h
	ld a,lcdIntVcomp
	call togglegfx
	ld hl,game_palette
	ld de,mplcdPalette
	ld bc,8
	ldir
	;load data from file, if it exists. if not, zero out then use that data.
	call getfile
	ex de,hl
	ld de,0D40000h | (versioncode-2)	;writeover stuffs.
	ld bc,(4*4)+2+2
	ldir
	;set up interrupts while they're disabled
	ld hl,mpIntMask
	ld de,(hl)
	push hl
		push de
			ld de,intLcd
			ld (hl),de				;set interrupts to accept only from LCD.
			or a
			sbc hl,hl
			set 3,h
			ld (mpIntMask),hl		;enable only LCD controller interrupt
			ld a,lcdIntVcomp
			ld (mpLcdImsc),a
			ld a,MB
			push af
				ld a,0D4h
				ld MB,a
				call.is programStart_z80
				di
			pop af
			ld MB,a
			call getfile
			inc de
			inc de
			ld bc,(4*4)+2
			ld hl,0D40000h | versioncode
			ldir	;write back to file
			;call _Arc_Unarc	;should this be archived???
		pop de
	pop hl		;interrupt mask
	ld iy,flags
	;undo changes to the interrupt controller
	ld (hl),de
	;reverse the changes to the lcd controller
	ld de,lcdNormalMode
	ld bc,0D40000h		;VRAM
togglegfx:
	ld hl,mpLcdBase
	ld (hl),bc
	ld l,lcdCtrl
	ld (hl),de
	ld l,lcdTiming0+1
	ld b,8
	ld de,gfx_settings
togglegfx_loop:
	ld a,(de)
	ld c,(hl)
	ex de,hl
	ld (de),a
	ld (hl),c
	ex de,hl
	djnz togglegfx_loop
	ret
	
;Returns exactly what chkfindsym would, but will do everything to ensure
;that the file both exists and is in RAM
getfile:
	ld hl,savefile_name
	call _Mov9ToOP1
	call _ChkFindSym
	jr nc,getfile_found
	ld hl,(4*4)+2+10	;10 more bytes for reserved
	push hl
		call _CreateAppVar
	pop bc
	ex de,hl
	inc hl
	inc hl
	call _MemClear
	jr getfile
getfile_found:
	call _ChkInRam
	ret z
	call _Arc_Unarc
	jr getfile


savefile_name:
.db 015h,"FalDDat2",0

getkbd_adl:
	ld ix,mpKeyMode
	lea hl,ix+0
	ld (hl),keyModeScanOnce
	xor a
	cp (hl)
	jr nz,$-1
	ld a,(ix+kbdG1-mpKeyRange)
	ld l,(ix+kbdG7-mpKeyRange)
	xor L
	and 11110000b	;mask out lower bits to let me shove dpad bits in
	xor L
	ret.l
isr_adl:
	ld hl,mpLcdMis
	ld a,(hl)
	ld l,mpLcdIcr&0FFh
	ld (hl),a
	ld a,0FFh
	ld hl,mpIntAck+1
	ld (hl),a		;I hope that acknowledges them all?
	ret.l
newPaletteEntry_adl:
	ld hl,mpLcdPalette+2
	ld (hl),e
	inc hl
	ld (hl),d
	ret.l
	
game_palette:
	.dw 1111111111111111b,1000001111100000b,0110001100011000b,0000000000000000b
gfx_settings:
;Copied from graphx.asm After having gone through what it does, 
;I probably wouldn't have changed anything anyway
;	.db	14 shl 2			; PPL shl 2
	.db	7					; HSW
	.db	87					; HFP
	.db	63					; HBP
	.dw	(0 >> 10)+319		; (VSW shl 10)+LPP
	.db	179					; VFP
	.db	0					; VBP
	.db	(0 >> 6)+(0 >> 5)+0	; (ACB shl 6)+(CLKSEL shl 5)+PCD_LO
;  H = ((PPL+1)*16)+(HSW+1)+(HFP+1)+(HBP+1) = 240+8+88+64 = 400
;  V = (LPP+1)+(VSW+1)+VFP+VBP = 320+1+179+0 = 500
; CC = H*V*PCD*2 = 400*500*2*2 = 800000
; Hz = 48000000/CC = 60


.org $-0D40000h

.assume adl=0
;-----------------------------------------------------------------------------

clearbuf:
	ld hl,8000h
	ld bc,((240*320)/4)-1
	rst clearmem_r
	ret

;out: HL=PRN_value, A=PRN_value_low8
lfsr:
lfsrseed .equ $+1
	ld hl,1    ;SMC'd back
	add hl,hl
	ld a,l
	jr nc,$+5
	xor 039h
	ld l,a
	ld (lfsrseed),hl
	ret

;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------
#include "src/chars.asm"
.assume adl=0
programStart_z80:
	call programStart_z80cont
	ret.l
programStart_z80cont:
	ld hl,0F000h
	ld bc,00C00h
	rst clearmem_r		;initialize program memory
	ld de,widen01
	;C will be zero at the start of this. This is good.
ps_widen_main:
	ld b,8
	ld a,c
ps_widen_sub:
	rlca
	adc hl,hl
	rrca
	rlca
	adc hl,hl
	djnz ps_widen_sub
	ld a,h	;reverse byte order
	and 01010101b
	ld (de),a
	inc de
	ld a,l
	and 01010101b
	ld (de),a
	dec de
	inc d
	inc d
	ld a,H
	ld (de),a
	inc de
	ld a,L
	ld (de),a
	inc de
	dec d
	dec d
	inc c
	jr nz,ps_widen_main		;s1: 39
;widening for all bits
	ld d,(widenall>>8)&0FFh	;E should be zero at this point.
ps_widenall_main:
	ld b,4
	ld a,c
ps_widenall_sub:
	rlca
	adc hl,hl
	rlca
	adc hl,hl
	rrca
	rrca
	rlca
	adc hl,hl
	rlca
	adc hl,hl
	djnz ps_widenall_sub
	ld a,h
	ld (de),a
	inc de
	ld a,L
	ld (de),a
	inc de
	inc c
	jr nz,ps_widenall_main
	rst getkbdwait_r		;wait until all keys are released
;=============================================================================	
gametitle: ;==================================================================
gametitle_loop:
	call clearbuf
	call gametitle_printtitle	;print early to avoid destroying registers
	rst getkbdwait_r
	;Logic for quit game
	bit kbitMode,a
	ret nz
	ld hl,cursorpos
	;Logic for pushing the 2nd button
	bit kbit2nd,a
	jr z,gametitle_checkdpad
	ld a,(hl)
	or a	;2nd button action: Start Game
	jp z,gamelogic
	dec a	;2nd button action: Change Speed
	jr nz,gametitle_skip2nd_changespeed
	ld a,(speed)
	inc a
	and 3
	ld (speed),a
	jr gametitle_skip_allbuttons
gametitle_skip2nd_changespeed:
	dec a	;2nd button action: About
	jp z,gameabout
	ret		;2nd button action: Quit Game
gametitle_checkdpad:
	;carry was cleared at kbd routine. still is cleared.
	;kbdDown .equ 1, kbdUp .equ 8
	;ld b,0	;b was already set to zero at the gametitle_printtitle routine
	rra
	rl b	;set b to 1 if kbitDown set.
	rra
	rra
	rra
	sbc a,a	;set a to -1 if kbitUp was set, else A=0.
	add a,b	;woo. 0000=0, 0001=1, 1000=-1, 1001=0
	add a,(hl)
	and 3
	ld (hl),a
gametitle_skip_allbuttons:
	ld a,(hl)	;cursorpos
	cpl			;0 to 3 -> -1 to -4
	add a,5+1	;-1 to -4 -> 4+1 to 1+1
	ld (gametitle_menuloop_smc),a
	PUTSTR_XY(0,01Ch,str_menu-1)	;1 byte cheaper than set texty to $1C and HL to str_menu
	ld c,4+1		;+1 dummy menu item to return text to black
gametitle_menuloop:
gametitle_menuloop_smc .EQU $+1
	ld a,0
	cp c
	ld a,TEXT_BLACK
	jr nz,$+4
	ld a,TEXT_GREEN
	ld (textcolor),a
	call putstr_centerquarter
	dec c
	jr nz,gametitle_menuloop
	call supersize_me
	call gametitle_printbottom
	rst screen_wait_r
	jr gametitle_loop
	

;=============================================================================
gameabout: ;==================================================================
	call clearbuf
	call gametitle_printtitle
	call supersize_me
	call gametitle_printbottom
	PUTSTR_XY(0,040h,str_about-1)	;size-optimizing HL and texty set
	ld c,8
gameabout_drawloop:
	call putstr_centerpartial
	dec c
	jr nz,gameabout_drawloop
gameabout_loop:
	rst screen_wait_r
	rst getkbdwait_r
	or a
	jr z,gameabout_loop
	jp gametitle
	
;=============================================================================
gameover:  ;==================================================================
	;jr $
	ld a,(speed)
	add a,a
	add a,a ;x4
	ld d,(hiscore0>>8)&0FFh	;assert: hiscores do not cross page boundaries
	ld e,a
	ld hl,score
	ld b,4
	push de
		push hl
gameover_checkhiscoreloop:
			ld a,(de)	;
			cp (hl)		;hiscore-score. carry, score was higher.
			jr nz,gameover_stop_checking
			inc hl
			inc de
			djnz gameover_checkhiscoreloop
gameover_stop_checking:
		pop hl
	pop de
	jr nc,gameover_not_a_hiscore
	ld bc,4
	ldir		;read from score, write to hiscore
gameover_not_a_hiscore:
	call clearbuf
	PUTSTR_XY(31,116,str_gameover)
	rst getkbdwait_r	;ensure you released the keys before polling for keyhit
gameover_loop:
	rst screen_wait_r	;show gameover screen while you're at it
	rst getkbdwait_r	;ensure you released the keys before polling for keyhit
	or a
	jr z,gameover_loop
	ld de,PALETTE_GREEN
	rst setpalette_r
	jp gametitle
	
;=============================================================================
gamelogic: ;==================================================================
	;init variables
	ld hl,score
	ld bc,6-1
	rst clearmem_r
	ld hl,02608h	;x=38, y=8
	ld (ball_y),hl
	ld hl,(speed)
	ld h,0
	ld de,gamelogic_speedtable
	add hl,de
	ld a,(hl)
	ld H,(hl)
	ld L,a
	ld (scrolldist),hl	;H=0 so a line will be genned immediately
	ld hl,08080h
	ld (newscroll),hl	;being lazy with narrowing. it'll fix itself.
	ld a,$40
	ld (newnarrow),a	;as high as it'll go to maximize game length w/ cur setup
	;init screen area
	call clearbuf
	rst screen_wait_r
	rst screen_wait_r
	ld de,PALETTE_DGRAY
	rst setpalette_r
gamelogic_loop:
	ld hl,$8000
	ld bc,(80*10)-1
	rst clearmem_r		;remove top 10 rows to prevent score text artifacting
	rst getkbd_r
	bit kbitMode,a
	jr nz,gameover		;gamemode MODE action: GAME OVER
	call calcballadr	;DE=XY HL=adr
	push hl	\ pop ix	;IX=balladr
	;erase ball sprite before trying to move it
	ld bc,12*256	;12 in B, 0 in C
	push de
		ld de,80-2
gamelogic_eraseball_loop:
		ld (hl),c
		inc hl
		ld (hl),c
		inc hl
		ld (hl),c
		add hl,de
		djnz gamelogic_eraseball_loop
	pop de
	;Begin test left/right keys
	bit kbitLeft,a
	jr z,gamelogic_skipleft
	;gamemode LEFT action
	push de \ call rotate_ball_ccw \ pop de
	inc d
	dec d
	jr z,gamelogic_moveball
	dec d
	lea ix,ix-1-80	;-80 for extended testing
	dec ix
	jr gamelogic_moveball
gamelogic_skipleft:
	bit kbitRight,a
	jr z,gamelogic_moveball
	;gamemode RIGHT action
	push de \ call rotate_ball_cw \ pop de
	ld a,d
	cp 80-3
	jr nc,gamelogic_moveball
	inc d
	lea ix,ix+3-80	;-80 for extended testing
gamelogic_moveball:
	;begin testing to see if ball can be moved
	xor a
	ld b,13		;Testing 1 px higher fixes screen bottom jumping bug
gamelogic_testmoveto:
	cp (ix+0)
	lea ix,ix+80
	jr nz,gamelogic_movecancel
	djnz gamelogic_testmoveto
	ld (ball_y),de		;if changed, make it stick
gamelogic_movecancel:
	call scrollit	;scroll screen up and generate new blocks if needed
	call nc,scrollit	;scroll again if we're at the bottom.
	call calcballadr
	ld de,ball
	ex de,hl		;HL=ballspr, DE=scrn_adr
	ld bc,$0CFF		;B=12, C sufficiently high to keep LDI from screwing us over
gamelogic_drawball_loop:
	ldi
	ldi
	ldi
	push hl
		ld hl,80-3
		add hl,de
		ex de,hl
	pop hl
	djnz gamelogic_drawball_loop
	;display the score, ensuring to pre-clear the space behind it first. maybe
	;direct writing. thankfully our crappy puts does that for us.
	PUTSTR_XY(1,2,str_score)
	ld hl,score
	call putscore
	rst screen_wait_r
	jp gamelogic_loop
	
;------------------------------
scrollit:
	;generate new possible row, even if it doesn't get used. If this impacts
	;performance too much, do the work to save register state, then do this
	ld c,6
	ld de,rowgaps
scrollit_newgaploop:
	ld b,8			;generate 8 new bits
	call lfsr
	djnz $-3
	ld L,20
	mlt hl
	ld a,h
	inc a		;prescale for loop counter being 1 to 20 instead of 0 to 19
	ld (de),a
	inc de
	dec c
	jr nz,scrollit_newgaploop
	;calculate new values for scrolldist, curscroll,
	;newscroll, narrowing, and newnarrow.
	ld ix,$0101	;sentinel values for deferring routines to end of calculations
	ld hl,(scrolldist)	;H=curscroll, L=scrolldist
	ld de,(newscroll)	;D=narrowing, E=newscroll
	ld a,h		;curscroll to subtract from
	sub l		;curscroll-scrolldist
	ld h,a		;save back in case no new rows needs to be generated.
	jr nc,scrollit_no_new_row
	;A row should be generated. reset scrolldist and determine offset to
	;place the new row.
	ld h,e		;doing this cheaply instead of considering gap offsets.
	add a,240-8	;Keep distance between rows consistent by offsetting row pos
	ld ixl,a	;A=row to draw new blocks to. keep safe in ixl.
	dec d		;Counts number of new rows left until we narrow the gap.
	jr nz,scrollit_no_new_row
	dec e		;Reduce the gap between rows (newscroll)
	jr nz,$+3	;But never allow it to hit zero or bad things will happen.
	inc e		;
	ld a,(newnarrow)
	ld d,a				;As we reset narrowing, we increment the new value
	inc a				;we give it in the hopes that it'll keep pace with
	ld (newnarrow),a	;how much more often we narrow the gaps.
scrollit_no_new_row:
	ld (scrolldist),hl
	ld (newscroll),de
;scrolls the screen. No validation, but should never need if speed stays in bounds
	ld c,80
	ld de,8000h		;DE (starting address): writeto
	ld h,c			;L=scrolldist, C=H=80
	ld a,L			;for later
	mlt hl			;80*scroll.
	push hl			;Is also amount of bytes to clear at end of buffer. Save.
		add hl,de	;HL (look ahead address): readfrom
		cpl
		add a,241	;neg = cpl+1, so this equiv to neg \ add a,240 (240-A)
		ld b,a		;(240-scroll) = all the other rows (leftovers)
		mlt bc		;leftovers*80. Gotta move all those bytes.
		ldir
	pop bc			;And now clear the remaining bytes.
	ld l,e
	ld h,d
	dec bc
	rst clearmem_r
	ld e,ixl
	dec e
	jr z,moveball_vertical
;generate new row if needed
	ld D,80
	mlt de
	set 7,d
	ld b,20
newrow_loop:
	push bc
		ld a,b
		ld hl,rowgaps
		ld bc,6
		cpir
		jr z,newrow_noblock
		ld a,8
		ld hl,sprite_block
		push de
newrow_subloop:
			ldi
			ldi
			ldi
			ldi
			ld bc,80-4
			ex de,hl
			add hl,bc
			ex de,hl
			dec a
			jr nz,newrow_subloop
		pop de
newrow_noblock:
		inc de
		inc de
		inc de
		inc de
	pop bc
	djnz newrow_loop
moveball_vertical:
	;Try to move ball down up to 4 pixels but could move up by up to 12.
	;ball_y cannot be equ or greater than 228 or will corrupt memory.
	;Call logic will be easier if ball_y is gt 228-4 (224), then clamp to
	;224 so the loop can always be 16. Recalc address.
	ld hl,(ball_y)		;HL=XY
	ld a,224
	cp l		;224-L. carry if L is gt 224 (NC if not. skip set if that)
	jr nc,$+3
	ld l,a		;L is now 224.
	call calcballadr_preload_ball	;DE=XY, HL=adr
	push hl
;CURRENT SCORE MODIFICATION: Might as well do it while we have ball_y 
;in register E. We just have to make sure it stays there at the end.
		ld c,e				;keep a backup of E.
		ld hl,(subscore)
		srl e
		ld d,0
		add hl,de			;resets carry as side-effect. important.	
		ld a,h				;A will be a value between 0 and 1. Definitely BCD.
		ld h,d
		ld (subscore),hl	;subscore updated, int portion moved to A and cleared
		ld hl,score+3
		ld b,4
scrollit_adjust_score:
		adc a,(hl)
		daa
		ld (hl),a
		ld a,d				;D=0, clears A to allow carry to cascade
		dec hl
		djnz scrollit_adjust_score
	pop ix
	ld b,14			;### MODIFIED -2 TO DROP BY 2, NOT 4
gamelogic_checkbelow_loop:
	ld a,(ix+0)
	or (ix+2)
	jr nz,gamelogic_checkbelow_collided
	lea ix,ix+80	;next row
	djnz gamelogic_checkbelow_loop
gamelogic_checkbelow_collided:
	ld a,b			;16  to  0
	cpl				;-17 to -1
	add a,5-2		;-12 to  4		### MODIFIED HERE TOO TO DROP BY 2
	add a,c			;240 is below bottom, which is also -16, giving us
	ld (ball_y),a	;save the new position of ball ypos and prepare to draw.
	cp 240			;headroom for comparison as max step backwards is -12 if 0.
	pop hl			;DOING THIS WILL PREVENT A MEMORY LEAK	
	jp nc,gameover	;SHOULD THE JUMP BE TAKEN
	cp 220			;guessing and testing this value allllllll dayyyy looong
	jp (hl)
	

;=============================================================================
;====================== BALLS BALLS BALLS BALLS BALLS BALLS ==================
;=============================================================================
;Defines the following functions that you should be interested in:
;	rotate_ball_ccw
;	rotate_ball_cw
;	calcballadr						[          out: DE=XY HL=adr]
;	calcballadr_preload_ball		[in:HL=XY, out: DE=XY HL=adr]

calcballadr:
	ld hl,(ball_y)
calcballadr_preload_ball:
	push hl
		ld d,0
		ld e,h
		ld h,80
		mlt hl
		add hl,de
		set 7,h
	pop de
	ret


rotate_ball_cw:
	ld hl,(ballcounter)
	ld a,(hl)
	xor 080h
	ld (hl),a
	jp m,ball_hflip
	jr ball_vflip
rotate_ball_ccw:
	ld hl,(ballcounter)		;here's hoping ballcounter started at zero...
	ld a,(hl)
	xor 080h
	ld (hl),a
	jp m,ball_vflip
;swaps all xcoords. (mirrors across y-axis)
ball_hflip:
	ld b,12
	ld hl,ball
ball_hflip_loop:
	ld a,(hl)
	inc hl
	call rev2bpp	;reverse byte +0 (lft)
	ld e,a
	ld a,(hl)
	call rev2bpp	;reverse byte +1 (ctr)
	ld (hl),a		;writeback result immediately
	inc hl
	ld a,(hl)
	call rev2bpp	;reverse byte +2 (rt)
	ld (hl),e		;write +0 to +2 spot
	dec hl
	dec hl
	ld (hl),a		;write +2 to +0 spot.
	inc hl
	inc hl
	inc hl			;advance pointer to next row
	djnz ball_hflip_loop
	ret ;31
	
;swaps all ycoords (mirrors across x-axis)
ball_vflip:
	ld hl,ball+(3*11)	;start of last row of ball
	ld de,ball			;start of first row of ball
	ld c,6				;swap 6 rows. HL moving up, DE moving down.
ball_vflip_loop:
	push bc
		ld b,3
ball_vflip_inner:
		ld c,(hl)		;get last row byte
		ld a,(de)		;and first row
		ex de,hl		;exchange first and last	
		ld (hl),c
		ld (de),a		;write back to swapped
		ex de,hl		;exchange back to original positions
		inc hl
		inc de			;get next byte pairs...
		djnz ball_vflip_inner
		ld bc,-6
		add hl,bc
	pop bc
	dec c
	jr nz,ball_vflip_loop
	ret

;A=byte to reverse. 76543210 -> 10325476
;destroys C
rev2bpp:
	ld c,a
	rlca
	rlca			;A=54321076
	rrc c
	rrc c			;C=10765432
	xor c           ;Masking out
	and 00110011b	;CCAACCAA
	xor c			;10325476
	ret
;-----------------------------------------------------------------------------
;basically data in code form.
gametitle_printtitle:
	PUTSTR_XY(0,0,str_title)
	jr supersize_me
	
gametitle_printbottom:
	PUTSTR_XY(1,230,str_hiscore)	;HL immediately after this is speed strtbl
	ld a,(speed)
	ld b,a
	add a,a
	add a,a
	ld c,a				;keep for later
	jr z,gametitle_pb_idxfound
	xor a
	cp (hl)
	inc hl
	jr nz,$-2
	djnz $-4
gametitle_pb_idxfound:
	call putstr			;print speed indicator
	ld hl,str_hiscorefinish
	call putstr
	ld hl,hiscore0
	add hl,bc			;B cleared in getstrfromidx, C=spd*4
	call putscore
	PUTSTR_XY(71,230,str_version)
	ret
	
gamelogic_speedtable:
.db 1,2,3,6
	
;it takes the top-left quarter of the buffer and zooms to fill the rest.
;destroys: All but IY
supersize_me:
	ld ix,$8000 + ((320*240)/4)
	ld hl,$8000 + ((320*240)/(4*2)) - 40
	ld b,0
supersize_me_mainloop:
	bit 7,h
	ret z		;quit if DE underflows buffer
	ld a,40		;number of rows to the subsection
supersize_me_subloop:
	lea ix,ix-2
	dec hl
	ld c,(hl)	;byte from subscreen
	ex de,hl
	ld hl,widenall
	add hl,bc
	add hl,bc
	ld hl,(hl)
	ld (ix+0),hl
	ld (ix-80),hl
	ex de,hl
	dec a
	jr nz,supersize_me_subloop
	ld de,-40
	add hl,de
	lea ix,ix-80
	jr supersize_me_mainloop
	;Terminating conditions found near jump location
	
	
	


;16 by 8 block. 4*8 bytes
sprite_block:
	.db 11111111b,11111111b,11111111b,11111111b
	.db 11010101b,01010101b,01010101b,01010111b
	.db 11011010b,10101010b,10101010b,10100111b
	.db 11011010b,10101010b,10101010b,10100111b
	.db 11011010b,10101010b,10101010b,10100111b
	.db 11011010b,10101010b,10101010b,10100111b
	.db 11010101b,01010101b,01010101b,01010111b
	.db 11111111b,11111111b,11111111b,11111111b
	
;12 by 12 block. 3*12 bytes.
ball:
	.db 00000000b,11111111b,00000000b
	.db 00001111b,01010101b,11110000b
	.db 00110101b,01101010b,01011100b
	.db 00110101b,01011010b,10011100b
	.db 11010101b,01010110b,10100111b
	.db 11010101b,01010101b,10100111b
	.db 11010101b,01010101b,01100111b
	.db 11010101b,01010101b,01010111b
	.db 00110101b,01010101b,01011100b
	.db 00110101b,01010101b,01011100b
	.db 00001111b,01010101b,11110000b
	.db 00000000b,11111111b,00000000b
	
	

str_score:
	.db "SCORE:",0
	
str_title:
	.db "FalldownCE",0
	
str_menu:
	.db "Start Game",0
	.db "Change Speed",0
	.db "About",0
	.db "Quit Game",0
	.db "",0		;extra entry to ensure text returns to black
	
str_hiscore:
	.db "High score (",0
	
str_speeds:
	.db "Slow",0
	.db "Medium",0
	.db "Fast",0
	.db "Hyper",0
	
str_hiscorefinish:
	.db "):",0
str_version:
	.db "v0.2",0
	
str_gameover:
	.db "Game Over",0
	
str_about:
;		0123456789012345678901234567890123456789	
	.db "Push LEFT/RIGHT to move the ball",0
	.db "and make it fall through the gaps.",0
	.db "Don't let the ball touch the top.",0
	.db "",0
	.db "Program by Rodger 'Iambian' Weisman",0
	.db "Released under the MIT License",0
	.db "File bug reports here:",0
	.db "https://cemete.ch/p264415",0,0,0,0,0,0,0,0
	
	
	
.echo "Relocatable stub size: ",$-init," bytes."