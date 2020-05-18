;
;Abbreviated version of ti84pce.inc to improve assemble times.
;See the included but not directly used ti84pce.inc for attribution.
;
;
_boot_ClearVRAM		.equ 0000374h
_Mov9ToOP1			.equ 0020320h
_ChkFindSym			.equ 002050Ch
_MemClear			.equ 00210DCh
_MemSet				.equ 00210E0h
_CreateAppVar		.equ 0021330h
_Arc_Unarc			.equ 0021448h
_ChkInRam			.equ 0021F98h

flags			.equ 0D00080h		; location of OS Flags (+-80h)
OP1				.equ 0D005F8h

userMem			.equ 0D1A881h


;LCD Ports
;--------------------------------
pLcdRange		.equ 04000h
mpLcdRange		.equ 0E30000h

lcdTiming0		.equ 0000h
pLcdTiming0		.equ pLcdRange + lcdTiming0
mpLcdTiming0		.equ mpLcdRange + lcdTiming0
lcdTiming1		.equ 0004h
pLcdTiming1		.equ pLcdRange + lcdTiming1
mpLcdTiming1		.equ mpLcdRange + lcdTiming1
lcdTiming2		.equ 0008h
pLcdTiming2		.equ pLcdRange + lcdTiming2
mpLcdTiming2		.equ mpLcdRange + lcdTiming2
lcdTiming3		.equ 000Ch
pLcdTiming3		.equ pLcdRange + lcdTiming3
mpLcdTiming3		.equ mpLcdRange + lcdTiming3

lcdBase			.equ 0010h
pLcdBase		.equ pLcdRange + lcdBase
mpLcdBase		.equ mpLcdRange + lcdBase
lcdUpbase		.equ 0010h
pLcdUpbase		.equ pLcdRange + lcdUpbase
mpLcdUpbase		.equ mpLcdRange + lcdUpbase
lcdLpbase		.equ 0014h
pLcdLpbase		.equ pLcdRange + lcdLpbase
mpLcdLpbase		.equ mpLcdRange + lcdLpbase

lcdCtrl			.equ 0018h
pLcdCtrl		.equ pLcdRange + lcdCtrl
mpLcdCtrl		.equ mpLcdRange + lcdCtrl
lcdEn			.equ 01h
lcdTft			.equ 20h
lcdBpp1			.equ 0000b | lcdTft | lcdEn
lcdBpp2			.equ 0010b | lcdTft | lcdEn
lcdBpp4			.equ 0100b | lcdTft | lcdEn
lcdBpp8			.equ 0110b | lcdTft | lcdEn
lcdBpp16Alt		.equ 1000b | lcdTft | lcdEn
lcdBpp24		.equ 1010b | lcdTft | lcdEn
lcdBpp16		.equ 1100b | lcdTft | lcdEn ; Default LCD mode
lcdBpp12		.equ 1110b | lcdTft | lcdEn
lcdBgr			.equ 100h
lcdBigEndian		.equ 200h
lcdBigEndianPixels	.equ 400h
lcdPwr			.equ 800h
lcdIntVSync		.equ 0000h
lcdIntBack		.equ 1000h
lcdIntActiveVideo	.equ 2000h
lcdIntFront		.equ 3000h
lcdWatermark		.equ 10000h
lcdNormalMode		.equ lcdPwr | lcdBgr | lcdBpp16

lcdImsc			.equ 001Ch
pLcdImsc		.equ pLcdRange + lcdImsc
mpLcdImsc		.equ mpLcdRange + lcdImsc
lcdRis			.equ 0020h
pLcdRis			.equ pLcdRange + lcdRis
mpLcdRis		.equ mpLcdRange + lcdRis
lcdMis			.equ 0024h
pLcdMis			.equ pLcdRange + lcdMis
mpLcdMis		.equ mpLcdRange + lcdMis
lcdIcr			.equ 0028h
pLcdIcr			.equ pLcdRange + lcdIcr
mpLcdIcr		.equ mpLcdRange + lcdIcr

bLcdIntFuf		.equ 1
lcdIntFuf		.equ 1 << bLcdIntFuf
bLcdIntLNBU		.equ 2
lcdIntLNBU		.equ 1 << bLcdIntLNBU
bLcdIntVcomp		.equ 3
lcdIntVcomp		.equ 1 << bLcdIntVcomp
bLcdIntMbErr		.equ 4
lcdIntMbErr		.equ 1 << bLcdIntMbErr

lcdCurr			.equ 002Ch
pLcdCurr		.equ pLcdRange + lcdCurr
mpLcdCurr		.equ mpLcdRange + lcdCurr
lcdUpcurr		.equ 002Ch
pLcdUpcurr		.equ pLcdRange + lcdUpcurr
mpLcdUpcurr		.equ mpLcdRange + lcdUpcurr
lcdLpcurr		.equ 0030h
pLcdLpcurr		.equ pLcdRange + lcdLpcurr
mpLcdLpcurr		.equ mpLcdRange + lcdLpcurr
lcdPalette		.equ 0200h
pLcdPalette		.equ pLcdRange + lcdPalette
mpLcdPalette		.equ mpLcdRange + lcdPalette

lcdCrsrImage		.equ 0800h
pLcdCrsrImage		.equ pLcdRange + lcdCrsrImage
mpLcdCrsrImage		.equ mpLcdRange + lcdCrsrImage
lcdCrsrCtrl		.equ 0C00h
pLcdCrsrCtrl		.equ pLcdRange + lcdCrsrCtrl
mpLcdCrsrCtrl		.equ mpLcdRange + lcdCrsrCtrl
lcdCrsrConfig		.equ 0C04h
pLcdCrsrConfig		.equ pLcdRange + lcdCrsrConfig
mpLcdCrsrConfig		.equ mpLcdRange + lcdCrsrConfig
lcdCrsrPalette0		.equ 0C08h
pLcdCrsrPalette0	.equ pLcdRange + lcdCrsrPalette0
mpLcdCrsrPalette0	.equ mpLcdRange + lcdCrsrPalette0
lcdCrsrPalette1		.equ 0C0Ch
pLcdCrsrPalette1	.equ pLcdRange + lcdCrsrPalette1
mpLcdCrsrPalette1	.equ mpLcdRange + lcdCrsrPalette1
lcdCrsrXY		.equ 0C10h
pLcdCrsrXY		.equ pLcdRange + lcdCrsrXY
mpLcdCrsrXY		.equ mpLcdRange + lcdCrsrXY
lcdCrsrClip		.equ 0C14h
pLcdCrsrClip		.equ pLcdRange + lcdCrsrClip
mpLcdCrsrClip		.equ mpLcdRange + lcdCrsrClip
lcdCrsrImsc		.equ 0C20h
pLcdCrsrImsc		.equ pLcdRange + lcdCrsrImsc
mpLcdCrsrImsc		.equ mpLcdRange + lcdCrsrImsc
lcdCrsrIcr		.equ 0C24h
pLcdCrsrIcr		.equ pLcdRange + lcdCrsrIcr
mpLcdCrsrIcr		.equ mpLcdRange + lcdCrsrIcr
lcdCrsrRis		.equ 0C28h
pLcdCrsrRis		.equ pLcdRange + lcdCrsrRis
mpLcdCrsrRis		.equ mpLcdRange + lcdCrsrRis
lcdCrsrMis		.equ 0C2Ch
pLcdCrsrMis		.equ pLcdRange + lcdCrsrMis
mpLcdCrsrMis		.equ mpLcdRange + lcdCrsrMis



;Interrupt Ports
;-----------------------------------
pIntRange		.equ 05000h
mpIntRange		.equ 0F00000h

intStat			.equ 0
pIntStat		.equ pIntRange + intStat
mpIntStat		.equ mpIntRange + intStat
intMask			.equ 4
pIntMask		.equ pIntRange + intMask
mpIntMask		.equ mpIntRange + intMask
intAck			.equ 8
pIntAck			.equ pIntRange + intAck
mpIntAck		.equ mpIntRange + intAck
intLatch		.equ 12
pIntLatch		.equ pIntRange + intLatch
mpIntLatch		.equ mpIntRange + intLatch
intInvert		.equ 16
pIntInvert		.equ pIntRange + intInvert
mpIntInvert		.equ mpIntRange + intInvert
intMasked		.equ 20
pIntMasked		.equ pIntRange + intMasked
mpIntMasked		.equ mpIntRange + intMasked

bIntOn			.equ 0
intOn			.equ 1 << bIntOn
bIntTmr1		.equ 1
intTmr1			.equ 1 << bIntTmr1
bIntTmr2		.equ 2
intTmr2			.equ 1 << bIntTmr2
bIntTmr3		.equ 3
intTmr3			.equ 1 << bIntTmr3
bIntOSTmr		.equ 4
intOSTmr		.equ 1 << bIntOSTmr
bIntKey			.equ 10
intKey			.equ 1 << bIntKey
bIntLcd			.equ 11
intLcd			.equ 1 << bIntLcd
bIntRtc			.equ 12
intRtc			.equ 1 << bIntRtc
bIntUsb			.equ 13
intUsb			.equ 1 << bIntUsb
intDefaultMask		.equ intOn | intTmr3 | intOSTmr | intRtc | intUsb

;Keypad Ports
;-----------------------------------
pKeyRange		.equ 0A000h
mpKeyRange		.equ 0F50000h

keyMode			.equ 0
pKeyMode		.equ pKeyRange + keyMode
mpKeyMode		.equ mpKeyRange + keyMode
keyRows			.equ 4
pKeyRows		.equ pKeyRange + keyRows
mpKeyRows		.equ mpKeyRange + keyRows
keyCols			.equ 5
pKeyCols		.equ pKeyRange + keyCols
mpKeyCols		.equ mpKeyRange + keyCols
keyIntStat		.equ 8
pKeyIntStat		.equ pKeyRange + keyIntStat
mpKeyIntStat		.equ mpKeyRange + keyIntStat
keyIntAck		.equ 8
pKeyIntAck		.equ pKeyRange + keyIntAck
mpKeyIntAck		.equ mpKeyRange + keyIntAck
keyIntMask		.equ 12
pKeyIntMask		.equ pKeyRange + keyIntMask
mpKeyIntMask		.equ mpKeyRange + keyIntMask
keyData			.equ 16
pKeyData		.equ pKeyRange + keyData
mpKeyData		.equ mpKeyRange + keyData
keyGpio			.equ 48
pKeyGpio		.equ pKeyRange + keyGpio
mpKeyGpio		.equ mpKeyRange + keyGpio

bKeyIntScanDone		.equ 0
keyIntScanDone		.equ 1 << bKeyIntScanDone
bKeyIntKeyChange	.equ 1
keyIntKeyChange		.equ 1 << bKeyIntKeyChange
bKeyIntKeyPress		.equ 2
keyIntKeyPress		.equ 1 << bKeyIntKeyPress

keyModeIdle		.equ 0
keyModeAny		.equ 1
keyModeScanOnce		.equ 2
keyModeScan		.equ 3

; DI Keyboard Equates
;---------------------------
DI_Mode		.equ 0F50000h
DI_Cntrl	.equ 0F50004h
DI_Int		.equ 0F50008h
DI_IntMask	.equ 0F5000Ch

kbdG1		.equ 0F50012h
;----------------------------
kbdGraph	.equ 00000001b
kbdTrace	.equ 00000010b
kbdZoom		.equ 00000100b
kbdWindow	.equ 00001000b
kbdYequ 	.equ 00010000b
kbd2nd		.equ 00100000b
kbdMode		.equ 01000000b
kbdDel		.equ 10000000b

kbitGraph	.equ 00h
kbitTrace	.equ 01h
kbitZoom	.equ 02h
kbitWindow	.equ 03h
kbitYequ	.equ 04h
kbit2nd		.equ 05h
kbitMode	.equ 06h
kbitDel		.equ 07h

kbdG2		.equ 0F50014h
;----------------------------
kbdStore	.equ 00000010b
kbdLn		.equ 00000100b
kbdLog		.equ 00001000b
kbdSquare	.equ 00010000b
kbdRecip	.equ 00100000b
kbdMath		.equ 01000000b
kbdAlpha	.equ 10000000b

kbitStore	.equ 01h
kbitLn		.equ 02h
kbitLog		.equ 03h
kbitSquare	.equ 04h
kbitRecip	.equ 05h
kbitMath	.equ 06h
kbitAlpha	.equ 07h

kbdG3		.equ 0F50016h
;----------------------------
kbd0		.equ 00000001b
kbd1		.equ 00000010b
kbd4		.equ 00000100b
kbd7		.equ 00001000b
kbdComma	.equ 00010000b
kbdSin		.equ 00100000b
kbdApps		.equ 01000000b
kbdGraphVar	.equ 10000000b

kbit0		.equ 00h
kbit1		.equ 01h
kbit4		.equ 02h
kbit7		.equ 03h
kbitComma	.equ 04h
kbitSin		.equ 05h
kbitApps	.equ 06h
kbitGraphVar	.equ 07h

kbdG4		.equ 0F50018h
;----------------------------
kbdDecPnt	.equ 00000001b
kbd2		.equ 00000010b
kbd5		.equ 00000100b
kbd8		.equ 00001000b
kbdLParen	.equ 00010000b
kbdCos		.equ 00100000b
kbdPgrm		.equ 01000000b
kbdStat		.equ 10000000b

kbitDecPnt	.equ 00h
kbit2		.equ 01h
kbit5		.equ 02h
kbit8		.equ 03h
kbitLParen	.equ 04h
kbitCos		.equ 05h
kbitPgrm	.equ 06h
kbitStat	.equ 07h

kbdG5		.equ 0F5001Ah
;----------------------------
kbdChs		.equ 00000001b
kbd3		.equ 00000010b
kbd6		.equ 00000100b
kbd9		.equ 00001000b
kbdRParen	.equ 00010000b
kbdTan		.equ 00100000b
kbdVars		.equ 01000000b

kbitChs		.equ 00h
kbit3		.equ 01h
kbit6		.equ 02h
kbit9		.equ 03h
kbitRParen	.equ 04h
kbitTan		.equ 05h
kbitVars	.equ 06h

kbdG6		.equ 0F5001Ch
;----------------------------
kbdEnter	.equ 00000001b
kbdAdd		.equ 00000010b
kbdSub		.equ 00000100b
kbdMul		.equ 00001000b
kbdDiv		.equ 00010000b
kbdPower	.equ 00100000b
kbdClear	.equ 01000000b

kbitEnter	.equ 00h
kbitAdd		.equ 01h
kbitSub		.equ 02h
kbitMul		.equ 03h
kbitDiv		.equ 04h
kbitPower	.equ 05h
kbitClear	.equ 06h

kbdG7		.equ 0F5001Eh
;----------------------------
kbdDown		.equ 00000001b
kbdLeft		.equ 00000010b
kbdRight	.equ 00000100b
kbdUp		.equ 00001000b

kbitDown	.equ 00h
kbitLeft	.equ 01h
kbitRight	.equ 02h
kbitUp		.equ 03h





