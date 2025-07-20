
    processor 6502
    include "inc/vcs.h"
    include "inc/macro.h"
    include "inc/xmacro.h"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    seg.u Variables
    org $80

; ---- sprite pointers  
s1                .byte $00,$00
s2                .byte $00,$00
s3                .byte $00,$00
s4                .byte $00,$00
s5                .byte $00,$00
s6                .byte $00,$00
pf                .byte $00,$00


; ---- sprite variables
XPosition         .byte $00,$00
Direction         .byte $01

; ---- delay pointer
DelayPTR          .byte $00,$00

; ---- various shared variables
Temp              .byte $00
LoopCount         .byte $00

; ---- constants
THREE_COPIES    equ %011 ; for NUSIZ registers
SPRITE_HEIGHT   equ 93

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    seg Code
    org $f000

Start:
    CLEAN_START
    jsr InitSprites
    jsr LoadSprites

NextFrame:

    ; 1 + 3 lines of VSYNC
    ; enable VBLANK (disable output)
    ; at the beginning of the frame we set the VSYNC bit...
    lda #2
    sta VSYNC
    ; and hold it on for 3 scanlines...
    sta WSYNC
    sta WSYNC
    sta WSYNC
    ; now we turn VSYNC off
    lda #0
    sta VSYNC
    
    TIMER_SETUP 37
    TIMER_WAIT

    ; turn off VBLANK (enable output)
    lda #0
    sta VBLANK
    

    TIMER_SETUP 192
    
    ; set the background colour
    lda #$0a
    sta COLUBK

    ; y-position loop
    ldx #$20
.yPositionLoop:
    stx WSYNC
    dex
    bne .yPositionLoop

    ; main sprite loop    
    jsr DoPosition
    jsr DrawLargeSprite
    jsr CheckBounds
    
    TIMER_WAIT
    

    TIMER_SETUP 29

    ; black background
    lda #$00
    sta COLUBK

    ; Enable VBLANK again
    lda #2
    sta VBLANK

    ; do delay offset
    jsr DoDelayOffset
    TIMER_WAIT
    
    jmp NextFrame

; -------------------------------------------------
InitSprites:
    ; turn on virtual disp
    lda #$01    
    sta VDELP0
    sta VDELP1

    ; three copies into sprite num/size
    lda #THREE_COPIES    
    sta NUSIZ0
    sta NUSIZ1

    ; sprite colour is black
    lda #$01    
    sta COLUP0
    sta COLUP1
    
    ; set sprite x-position
    lda #50
    sta XPosition

    ; initialise the delay pointer
    lda #<JNDelay+39
    sta DelayPTR 
    lda #>JNDelay
    sta DelayPTR+1 

    ; mirrored PF
    lda #01
    sta CTRLPF

    rts

; -------------------------------------------------
LoadSprites:
    ; set sprite pointers
    lda #<Joe0
    sta s1
    lda #>Joe0
    sta s1+1
    lda #<Joe1
    sta s2
    lda #>Joe1
    sta s2+1
    lda #<Joe2
    sta s3
    lda #>Joe2
    sta s3+1
    lda #<Joe3
    sta s4
    lda #>Joe3
    sta s4+1
    lda #<MacPunch3
    sta s5
    lda #>MacPunch3
    sta s5+1
    lda #<MacPunch4
    sta s6
    lda #>MacPunch4
    sta s6+1

    ; set playfield pointer
    lda #<PFData
    sta pf
    lda #>PFData
    sta pf+1
    rts

; -------------------------------------------------
DrawLargeSprite:
    ; delay before displaying big sprite
    ; set the sprite size for us to loop each line
    lda #SPRITE_HEIGHT
    sta LoopCount

    sta WSYNC
    ldy #5 ; we need perfect timing here
.x2LargeSprite:    
    dey
    bpl .x2LargeSprite

    jmp (DelayPTR)
.largeSpriteDone:
    rts

    ; this next section needs to be cycle perfect.
    ; we jump to offset the delay cycles below
    ; based on the XPosition of the sprites
    align $100
JNDelay:        
    .byte      $c9,$c9,$c9,$c9,$c9,$c9,$c9,$c9,$c9
    .byte      $c9,$c9,$c9,$c9,$c9,$c9,$c9,$c9,$c9
    .byte      $c9,$c9,$c9,$c9,$c9,$c9,$c9,$c9,$c9
    .byte      $c9,$c9,$c9,$c9,$c9,$c9,$c9,$c9,$c9,$c5
    nop
    nop
    nop
    nop

; -------------------------------------------------
.drawLargeSpriteLoop:             
    ldy LoopCount
 
    lda (s1),Y   ; first two sprites
    sta GRP0
    lda (s2),Y
    sta GRP1
    
    lda (pf),Y   ; playfield
    sta PF0 
  
    lda (s3),Y   ; remaining sprites
    sta GRP0
    lda (s6),Y
    sta Temp   
    lax (s5),Y
    lda (s4),Y
    ldy Temp
    sta GRP1
    stx GRP0
    sty GRP1
    sta GRP0
    dec LoopCount
    bpl .drawLargeSpriteLoop
    jmp .largeSpriteDone

; -------------------------------------------------
DoPosition:
    lda XPosition
    sta WSYNC
    sta HMCLR     
    bit 0 ; exactly 3 cycles for timing!
.largeSpritePositionDivideLoop:
    sbc #15        
    bcs .largeSpritePositionDivideLoop
    eor #7
    asl        
    asl
    asl
    asl
    sta RESP0
    sta RESP1
    sta HMP0
    sbc #$f0
    sta HMP1
    sta WSYNC
    sleep 72    ; minimise whiskers
    sta HMOVE
    rts

; -------------------------------------------------
DoDelayOffset:
    ; large sprite delay calculation.
    ; this is done by dividing the xpos / 3
    ; we do this every frame
DivideBy3:
    ; rough approximation of A/3
    ; ((((A>>1)+A)>>2)+(A)>>2)-(A>>7)
    lda XPosition
    lsr
    lsr
    lsr
    lsr
    lsr
    lsr
    lsr
    sta Temp
    lda XPosition
    lsr
    adc XPosition
    lsr
    lsr
    adc XPosition
    lsr
    lsr
    sbc Temp
    tay
    sty Temp
    lda #<JNDelay+38    ; we load the delay pointer with the largest sprite delay
    sbc Temp            ; now substract the delay offset we need
    sta DelayPTR        ; and store it in the low byte of the delay pointer
    rts

; -------------------------------------------------
CheckBounds:
    lda #0
    ldy #1
    ldx XPosition
    cpx #5
    bcs .withinLeftPos
    sty Direction
.withinLeftPos:
    cpx #110
    bcc .withinRightPos
    sta Direction
.withinRightPos:
    lda Direction
    beq .moveLeft
    inc XPosition
    rts
.moveLeft:
    dec XPosition
    rts

; ---- playfield data

    align $100
PFData:
    .byte %00000000,%00000000,%00000000,%00000000
    .byte %00000000,%00010000,%00010000,%00010000
    .byte %00010000,%00010000,%00010000,%00010000
    .byte %00010000,%00010000,%00010000,%00010000
    .byte %00010000,%00010000,%00010000,%00010000
    .byte %00010000,%00010000,%00010000,%00010000
    .byte %00010000,%00010000,%00010000,%00010000
    .byte %00010000,%00010000,%00010000,%00010000
    .byte %00110000,%00110000,%00110000,%00110000
    .byte %00110000,%00110000,%00110000,%00110000
    .byte %00110000,%00110000,%00110000,%00110000
    .byte %00110000,%00110000,%00110000,%00110000
    .byte %00110000,%00110000,%00110000,%00110000
    .byte %00110000,%00110000,%00110000,%00110000
    .byte %01110000,%01110000,%01110000,%01110000
    .byte %01110000,%01110000,%01110000,%01110000
    .byte %01110000,%01110000,%01110000,%01110000
    .byte %01110000,%01110000,%01110000,%01110000
    .byte %01110000,%01110000,%01110000,%01110000
    .byte %11110000,%11110000,%11110000,%11110000
    .byte %11110000,%11110000,%11110000,%11110000
    .byte %11110000,%11110000,%11110000,%11110000
    .byte %11110000,%11110000,%11110000,%11110000
    .byte %11110000,%11110000,%11110000,%11110000
    .byte %00010000,%00110000,%01110000,%11110000
    .byte %00010000,%00110000,%01110000,%11110000
    .byte %00010000,%00110000,%01110000,%11110000
    .byte %00010000,%00110000,%01110000,%11110000
    .byte %00010000,%00110000,%01110000,%11110000
    .byte %00010000,%00010000,%11110000,%11110000
    .byte %11110000,%11110000,%11110000,%11110000


; ---- joe sprite data, stand
    align $100 
Joe0:
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$02,$03,$02,$02,$02,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$02,$02,$04,$04,$08,$08,$08,$08,$08,$07,$07,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0d,$04,$03,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
Joe1:
    .byte $00,$00,$00,$38,$3c,$3e,$3e,$2e,$1e,$17,$1f,$17,$1f,$16,$3e,$2e,$46,$72,$4a,$86,$05,$cd,$17,$05,$05,$09,$0b,$94,$e1,$82,$40,$40,$20,$20,$20,$13,$1d,$15,$17,$1c,$11,$e1,$20,$20,$20,$20,$60,$50,$50,$50,$60,$ae,$a1,$c1,$c1,$e1,$f1,$f1,$f1,$fd,$f1,$f5,$f1,$71,$e8,$04,$06,$09,$10,$10,$08,$0d,$0a,$08,$13,$14,$14,$0a,$08,$12,$12,$39,$3e,$3e,$3f,$7f,$7f,$3f,$3f,$3f,$1f,$0f,$07,$03        
    align $100    
Joe2:
    .byte $18,$3c,$3c,$64,$7c,$64,$7c,$64,$3c,$24,$3e,$22,$42,$5e,$62,$4a,$55,$41,$41,$81,$80,$41,$26,$98,$40,$a0,$40,$a0,$80,$40,$20,$00,$00,$00,$01,$e1,$5a,$56,$f5,$0e,$02,$04,$08,$0a,$12,$14,$14,$10,$10,$08,$78,$89,$09,$19,$1e,$3e,$7f,$ff,$ff,$ff,$ff,$fb,$fa,$f3,$e6,$7c,$24,$04,$88,$48,$28,$48,$4c,$4e,$0f,$87,$6b,$9f,$1e,$5a,$5a,$3e,$de,$1e,$1e,$9e,$be,$fe,$fe,$fe,$fc,$fc,$b8,$10
Joe3:
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$80,$80,$80,$40,$40,$40,$40,$40,$80,$80,$80,$80,$80,$80,$00,$00,$00,$00,$80,$40,$20,$20,$20,$10,$10,$10,$50,$50,$50,$90,$90,$10,$10,$10,$10,$10,$20,$20,$20,$20,$40,$40,$80,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

; ---- mac sprite data, punch
    align $100
MacPunch3:
    .byte $00,$07,$04,$05,$06,$05,$47,$b3,$d2,$ba,$df,$ba,$f2,$72,$13,$3c,$24,$48,$48,$40,$40,$41,$46,$2b,$37,$1f,$1c,$1c,$1c,$3c,$3c,$3c,$3e,$3e,$3c,$3c,$3b,$31,$56,$5f,$4f,$5f,$5f,$5f,$3f,$3f,$2f,$17,$13,$11,$09,$09,$07,$08,$08,$08,$08,$08,$04,$07
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00 ; bytes for padding only
MacPunch4:
    .byte $00,$00,$80,$40,$c0,$c0,$c0,$c0,$40,$20,$20,$a0,$10,$f0,$08,$08,$08,$08,$08,$10,$10,$50,$f0,$e0,$e0,$e0,$60,$10,$0c,$0a,$11,$31,$19,$11,$2a,$24,$20,$c0,$40,$40,$a0,$a0,$a0,$c0,$c0,$e0,$c0,$e0,$c0,$00,$00,$80,$c0,$40,$20,$a0,$a0,$c0,$40,$80
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00


;;
; Epilogue
    org $fffc
    .word Start
    .word Start
