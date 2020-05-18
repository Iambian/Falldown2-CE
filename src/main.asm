#include "src/ti84pce_short.inc"
.org userMem-2
.db 0EFh, 07Bh
.assume adl=1
;Note. 2bpp screen mode. screen @ $D43500, buffer @D48000.

programStart:
	di
	ld hl,gameData
	ld de,0D40000h
	call zx7_decompress
	;jr $
	call 0D40000h
	jp _boot_ClearVRAM
#include "src/dzx7.asm"

.echo "Loader stub size: ",$-programStart," bytes"
gameData:
#import "obj/game.zx7"



.echo "Total file size: ",$-programStart," bytes"