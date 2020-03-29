;.define FIXED_PCM

;==============================================================
; WLA-DX banking setup
;==============================================================

.memorymap
defaultslot 0
slotsize $4000
slot 0 $0000
.endme

.rombankmap
bankstotal 1
banksize $4000
banks 1
.endro

;==============================================================
; SDSC tag and SMS rom header
;==============================================================

.sdsctag 9.0,"Video playback","Video playback demo","GliGli"

;==============================================================
; SMS defines
;==============================================================

.define VDPControl $bf
.define VDPData $be
.define VDPScanline $7e
.define PSGPort $7f
.define VRAMRead $0000
.define VRAMWrite $4000
.define CRAMWrite $c000
.define MapperSlot0 $fffd
.define MapperSlot1 $fffe
.define MapperSlot2 $ffff
.define TileSize 32
.define TileMapSize 1536
.define TilePaletteSize 16
.define BankSize_ 16384

;==============================================================
; Program defines
;==============================================================

.define DblBufTileOffset 49 * TileSize
.define FrameSampleCount 414 ; 344 cycles per PCM sample = one sample every 320 cycles
.define PCMBufferSizeShift 10
.define PCMBufferSize (1 << PCMBufferSizeShift)

.macro WaitVBlank args playSmp ; c11
    in a, (VDPControl)

---:
    .repeat 10
        inc iy
    .endr
    .ifeq playSmp 1
        PlaySampleSkew 125
    .endif
    in a, (VDPControl)
    or a  ; update flags
    jp p, ---
.endm

.macro SetVDPAddress args addr  ; c36
        ; Sets the VDP address
    ld a, <addr
    out (VDPControl),a
    ld a, >addr
    out (VDPControl),a
.endm

.macro CopyToVDP
; copies data to the VDP
; parameters: hl = data address, de = data length
; affects: a, hl, bc, de
    ld c, VDPData
-:  outi
    dec de
    ld a,d
    or e
    jp nz,-
.endm

.macro PlaySample
    exx
    outd
    exx
.endm

.macro PlaySampleSkew args skew
    ex af, af'
        ; this macro is 27 cycles when not playing
        ; one sample every 325 cycles (320 + 5 from jr not jumping)
    add a, (skew + 27) / 325 * 256
    jr nc, ++++
    PlaySample
++++:
    ex af, af'
.endm

.macro Unpack6Samples ; c153
    ld a, (bc)
    ld l, a

        ; High nibble, CH1
    ld h, >PCMUnpackLUT
    ld d, (hl)

        ; Low nibble, CH2
    inc h
    ld e, (hl)

        ; On to next sample pair
    push de
    inc bc
    ld a, (bc)
    ld l, a

        ; High nibble, CH3
    inc h
    ld d, (hl)

        ; Low nibble, CH1
    inc h
    ld e, (hl)

        ; On to next sample pair
    push de
    inc bc
    ld a, (bc)
    ld l, a

        ; High nibble, CH2
    inc h
    ld d, (hl)

        ; Low nibble, CH3
    inc h
    ld e, (hl)

        ; On to next sample pair
    push de
    inc bc
.endm

.macro TilesUploadSetTilePointer ; c72
        ; get a pointer on tile data from tile index

        ; upper bits of tile index select a rom bank
    ld a, h
    srl a ; pushes low bit into carry for use below

    ld (MapperSlot2), a

    sbc a, a ; get a mask of "carry" bits
    and $20 ; carry is $100 * Tilesize = $2000 bytes
    or $80 ; add pointer high bits ($8000)

        ; get pointer using LUT
    ld h, >TUTileIndexToOffsetLUT
    add a, (hl) ; add high byte from LUT
    inc h
    ld l, (hl) ; low byte from LUT
    ld h, a
.endm

.macro TilesUploadUpdateTilePointer args fast; c41
        ; update tile pointer

        ; get byte offset from "tile index difference" using LUT
    .ifeq fast 1
        dec h
        ld l, a
        ld a, (hl)
        dec h
        ld h, (hl)
        ld l, a
    .else
        inc h
        ld l, a
        ld a, (hl)
        inc h
        ld l, (hl)
        ld h, a
    .endif

        ; add to tile pointer
    add hl, de
.endm

.macro TilesUploadUnpack args fast ; c49
        ; tile pointer saved into de
    ex de, hl

        ; get next packed data
    dec sp ; we actually need to pop only one byte
    pop af

        ;  use jump table to handle it
    ld l, a
    ld h, >TUUnpackJumpTable + (1 - fast)
    ld h, (hl)
    ld l, 0
    jp (hl)
.endm

.macro TilesUploadTileToVRAMFast ; c192
    .repeat 20
        outi
    .endr
; c320
    PlaySample
    .repeat 12
        outi
    .endr
.endm

.macro TilesUploadTileToVRAMSlow ; c172
    .repeat 12
        outi
        inc iy ; timing
    .endr
    outi
; c328
    PlaySample
    .repeat 12
        outi
        dec iy ; timing
    .endr
; c312
    PlaySample
    .repeat 3
        outi
        inc iy ; timing
    .endr
    .repeat 3
        outi
        dec iy ; timing
    .endr
    outi
.endm

.macro TMCopy2CacheSlots args ps ; c83
        ; load both high nibbles
    ld a, (hl)
    inc hl
; c13
    .ifeq ps 0
        PlaySample
    .endif

        ; high nibble a
    ld (de), a
    inc e
; c24
    .ifeq ps 1
        PlaySample
    .endif

        ; low byte a
    ldi
; c40
    .ifeq ps 2
        PlaySample
    .endif

        ; high nibble b
    .repeat 4
        rrca
    .endr
    ld (de), a
    inc e
; c67
    .ifeq ps 3
        PlaySample
    .endif

        ; low byte b
    ldi
.endm

.macro TMProcessNextCommand args step, half; c36/26
        ; read next command
    .ifeq step 0
        pop de
        ld l, e
    .else
        ld l, d
    .endif

        ; jump to command code using jump table
    ld h, b
    ld h, (hl)
    ld l, half * $80 + (1 - step) * $40
    jp (hl)
.endm

.macro TMUploadCacheIndexMacro args cacheIdx, step ; c50
    ld hl, (TileMapCache + cacheIdx * 2)

        ; low byte of tilemap item
    out (c), h

    PlaySampleSkew 50+36-10*step

        ; high byte of tilemap item
    out (c), l

    inc ix
.endm

.macro TMUploadCacheRepeatMacro args rpt, step ; c52*(rpt-1)+38+40/c48*rpt+40+7
    .ifeq step 0
        ld a, d
    .else
        ld a, e
    .endif

        ; compute cache address
    and %00111110
    ld h, >TileMapCache
    ld l, a

        ; tilemap item into hl
    ld a, (hl)
    inc l
    ld h, (hl)

    .ifleeq rpt 4
        .repeat rpt index idx
                ; low byte of tilemap item
            out (c), h

            .ifeq idx 0
                PlaySampleSkew 52*(rpt-1)+38+40+36-10*step
            .else
                inc iy ; timing
                nop ; timing
            .endif

                ; high byte of tilemap item
            out (c), a

            inc ix
            nop ; timing
        .endr
    .else
        ld l, rpt
-:
            ; low byte of tilemap item
        out (c), h

        inc ix
        PlaySampleSkew 48+(40+7+36-10*step)/rpt ; distribute fixed cost across repeats

            ; high byte of tilemap item
        out (c), a

        dec l
        jp nz, -
    .endif
.endm

.macro TMUploadRawMacro args rpt, step, half ; c52*(rpt-1)+38+14*(1-step)
    .ifeq step 0
        ld a, d
        pop de
    .endif

    .repeat rpt index idx
            ; low byte of tilemap item
        .ifeq step 0
            out (c), e
        .else
            out (c), d
        .endif

        inc ix
	nop ; timing

            ; high byte of tilemap item
        .ifeq step 0
            out (c), a
        .else
            out (c), e
        .endif

        .ifeq idx 0
            PlaySampleSkew 52*(rpt-1)+38+14*(1-step)+26+10*step
        .else
            inc iy ; timing
            nop ; timing
        .endif
    .endr
.endm

.macro TMSkipMacro args step, half ; c95
    .ifeq step 0
        ld a, d
    .else
        ld a, e
    .endif

        ; command is skip count * 2
    rrca

        ; update TM offset (add skip count)
    ex de, hl
    ld d, 0
    ld e, a
    add ix, de
    ex de, hl

        ; update VRAM pointer (TM offset * 2 + ...)
    ld a, ixl
    rla
    out (VDPControl), a
    ld a, ixh
    rla
    or >VRAMWrite + half * $20
    out (VDPControl), a

    PlaySampleSkew 95+36-10*step
.endm

.macro TMSkip1Macro args step, half ; c32
        ; advance VRAM pointer one TM item

    in a, (VDPData)

    PlaySampleSkew 32+36-10*step

    in a, (VDPData)

    inc ix
.endm

;==============================================================
; RAM variables
;==============================================================

.enum $c000 export
    PSGBufferA        dsb PCMBufferSize
    PSGBufferB        dsb PCMBufferSize
    PCMUnpackLUT      dsb 1536
    TileMapCache      dsb 64 ; must be aligned on 256
    LocalPalette      dsb TilePaletteSize * 2
    FrameCount        dw
    CurFrameIdx       dw
.ende

;==============================================================
; Code
;==============================================================

.bank 0 slot 0
.org $0000
    di              ; disable interrupts
    im 1            ; Interrupt mode 1
    ; This maps the first 48K of ROM to $0000-$BFFF
    ld de, $FFFC
    ld hl, init_tab
    ld bc, $0004
    ldir

    jp main

init_tab: ; table must exist within first 1K of ROM
    .db $00, $00, $01, $02

.org $0038
    ei
    reti

.org $0066
    jp 0

main:
    ld sp, $dff0

    ; set up VDP registers

    ld hl,VDPInitData
    ld b,VDPInitDataEnd-VDPInitData
    ld c,VDPControl
    otir

    ; clear VRAM

    ; 1. cet VRAM write address to $0000
    SetVDPAddress $0000 | VRAMWrite
    ; 2. output 16KB of zeroes
    ld bc, $4000     ; counter for 16KB of VRAM
-:  xor a
    out (VDPData), a ; output to VRAM address, which is auto-incremented after each write
    dec bc
    ld a, b
    or c
    jr nz, -

    ; dummy Sprite table
    SetVDPAddress $600 | VRAMWrite
    ld a, $d0
    out (VDPData), a
    ld a, $06 << 1
    out (VDPControl), a
    ld a, $85
    out (VDPControl), a

    ; init current frame idx
    xor a
    ld (CurFrameIdx), a
    ld (CurFrameIdx + $01), a

    ; turn screen on
    ld a, %1100000
;          ||||||`- Zoomed sprites -> 16x16 pixels
;          |||||`-- Doubled sprites -> 2 tiles per sprite, 8x16
;          ||||`--- Mega Drive mode 5 enable
;          |||`---- 30 row/240 line mode
;          ||`----- 28 row/224 line mode
;          |`------ VBlank interrupts
;          `------- Enable display
    out (VDPControl), a
    ld a, $81
    out (VDPControl), a

    ; init PSG
    ld hl,PSGInit
    ld bc,(PSGInitEnd-PSGInit)<<8 + PSGPort
    otir

    ; 50Hz only
    call EnsureFifty

    ; clear PCM buffers
    ld hl, PSGBufferA
    ld b, 0
    ld a, $ff
-:  .repeat (PCMBufferSize*2)/256
        ld (hl), a
        inc hl
    .endr
    djnz -

    ; compute PCM unpack LUT
    ld l, 0
    ld b, 0 ; 256 iterations
-:
    inc l

    ld a, l
    and $0f
    ld e, a

    ld a, l
    .repeat 4
        srl a
    .endr
    ld d, a

    ld h, >PCMUnpackLUT
    ld a, d
    or $90
    ld (hl), a

    inc h
    ld a, e
    or $b0
    ld (hl), a

    inc h
    ld a, d
    or $d0
    ld (hl), a

    inc h
    ld a, e
    or $90
    ld (hl), a

    inc h
    ld a, d
    or $b0
    ld (hl), a

    inc h
    ld a, e
    or $d0
    ld (hl), a

    djnz -


    ; init PCM player
    exx
    ld c, PSGPort
    ld hl, PSGBufferB + PCMBufferSize - 1
    ld d, h
    exx

InitPlayer:
        ; algo expects VBlank state on start
    WaitVBlank 0

        ; map slot 1 to beginning of video data
    ld a, 1
    ld (MapperSlot1), a

        ; get first frame data pointers offset
    ld hl, BankSize_

        ; load frame count
    ld de, FrameCount
    ldi
    ldi

        ; load first frame bank index
    ld a, (hl)
    inc hl
    inc hl

        ; load frame data address into hl
    ld e, (hl)
    inc hl
    ld d, (hl)
    inc hl
    set 6, d; Add $4000
    ex de, hl

        ; map slot 1 & 2 to first frame data
    ld (MapperSlot1), a
    inc a
    ld (MapperSlot2), a

NextFrameLoad:
 ; c85 from jump

        ; are we using slot 2?
    bit 7, h
    jr z, NoSlotChange

        ; if so, move to next bank
    ld a, (MapperSlot2)
    ld (MapperSlot1), a
    inc a
    ld (MapperSlot2), a

        ; rewind to slot 1
    ld a, h
    sub $40
    ld h, a

    jp SlotEnd
; c312

NoSlotChange:

        ; ensure same timing as slot change
    .repeat 6
        inc iy
    .endr

SlotEnd:
; c165/168

    ; Load palette if frame contains one
    ld de, LocalPalette
    ld a, (hl)
    inc hl
    cp $00
    jp z, NoFramePalette

    .repeat 7
        ldi
    .endr
; c319
    PlaySample

    .repeat 20
        ldi
    .endr
; c320
    PlaySample

    .repeat 5
        ldi
    .endr

    jp PaletteEnd

NoFramePalette:

        ; ensure same timing as palette
    .repeat 7
        ld (0), hl ; timing
    .endr
; c319
    PlaySample

    .repeat 20
        ld (0), hl ; timing
    .endr
; c320
    PlaySample

    .repeat 5
        ld (0), hl ; timing
    .endr
    inc iy ; timing

PaletteEnd:
; c297

p1: ; Unpack tiles indexes and copy corresponding tiles to VRAM

        ; slot2 bank into ixl
    ld a, (MapperSlot2)
    ld ixl, a
; c318
    PlaySample

        ; should we seek to next bank start?
    ld a, (hl)
    inc hl
    cp $00
    jp z, NoBankChange

        ; if so, increment bank indexes
    ld a, ixl
    ld (MapperSlot1), a
    inc ixl

        ; reset frame data pointer
    ld hl, $4000

    jp BankChangeEnd

NoBankChange:

        ; ensure same timing as bank change
    .repeat 3
        ld (0), hl ; timing
    .endr

BankChangeEnd:
; c79/78

        ; Set tiles VRAM start address
    ld a, TileSize
    out (VDPControl), a
    ld a, (CurFrameIdx)
    ; we want to move bit 0 to bit 5
    rrca
    rrca
    rrca
    and %00100000
    or (DblBufTileOffset | VRAMWrite) >> 8
    out (VDPControl), a

        ; to indicate we start in "fast" VBlank
    ld ixh, 1

        ; prepare VRAM write register
    ld c, VDPData

        ; frame data pointer into sp
    ld sp, hl
; c175

        ; start unpacking tile indexes (in "fast" VBlank)
    TilesUploadUnpack 1

TilesUploadEnd:

; c175

        ;copy tilemap cache into ram
    ld de, TileMapCache
    TMCopy2CacheSlots -1
    TMCopy2CacheSlots 2 ; c308
    TMCopy2CacheSlots -1
    TMCopy2CacheSlots -1
    TMCopy2CacheSlots -1
    TMCopy2CacheSlots 1 ; c316
    TMCopy2CacheSlots -1
    TMCopy2CacheSlots -1
    TMCopy2CacheSlots -1
    TMCopy2CacheSlots 0 ; c321
    TMCopy2CacheSlots -1
    TMCopy2CacheSlots -1
    TMCopy2CacheSlots -1
    TMCopy2CacheSlots 0 ; c332
    TMCopy2CacheSlots -1
    TMCopy2CacheSlots -1
; c236

        ; sp will be used as a command pointer
    ld sp, hl

        ; Set tilemap VRAM pointer (also store it into ix)
    xor a
    out (VDPControl), a
    ld a, (CurFrameIdx)
    and 1
    ld l, a ; for use below
        ; we want to move bit 0 to bit 5
    rrca
    rrca
    rrca
    or VRAMWrite >> 8
    out (VDPControl), a

        ; jump table high byte
    ld b, >TMCommandsJumpTable

    ld c, VDPData

        ; ix will be used as a tilemap tile offset
    ld ix, 0

; c326
    PlaySample

        ; start unpacking tilemap
    xor a
    cp l
    jp nz, +
    TMProcessNextCommand 0, 0
+:
    TMProcessNextCommand 0, 1

TilemapUnpackEnd:

    ld b, h
    ld c, l

.ifdef FIXED_PCM
    ld iyh, b
    ld iyl, c
    ld bc, PCMData
.endif
    
    ; Unpack frame PCM data to RAM

        ; PSGBufferA for even frames, PSGBufferB for odd frames
    ld hl, PSGBufferA + PCMBufferSize
    ld a, (CurFrameIdx)
    and $01
    .repeat (PCMBufferSizeShift - 8)
        rla
    .endr
    add a, h
    ld h, a

        ; use SP as a PSGBuffer data pointer
    ld sp, hl

        ; 36 samples per iteration
    ld ixh, FrameSampleCount / 24
; c197

; c0

    PCMUnpackLoop:
        PlaySample
        Unpack6Samples
        Unpack6Samples
        cp 0 ; timing
        cp 0 ; timing
; c320
        PlaySample
        Unpack6Samples
        Unpack6Samples
    dec ixh
    jp nz, PCMUnpackLoop
; c322
        ; extra decoding
	
    PlaySample

    Unpack6Samples
    
        ; cleanup extra decoding
   pop hl
   pop hl
   ld l, h
   push hl
   push hl   
    
; c46

        ; restore frame data pointer into hl
.ifdef FIXED_PCM
    ld b, iyh
    ld c, iyl
    ld hl, FrameSampleCount / 2
    add hl, bc
.else
    ld h, b
    ld l, c
.endif
; c60

    PlaySampleSkew 220
p2:
        ; ensure the video frame lasted long enough by checking PCM buffer position
-:
    .repeat 10
        inc iy ; timing
    .endr

    PlaySampleSkew 159
    exx
    ld a, d
    and $fe
    ld ixh, a
    ld a, h
    exx

    cp ixh
        ; stop on buffer underflow
    jp c, +
        ; loop while there's still buffer left
    jp nz, -
+:

    WaitVBlank 1

p3:
        ; frame data pointer into de
    ex de, hl

    ; restart PCM player on other buffer
    exx
    ld (PSGBufferA), hl ; debug tool (shows how many samples actually played per frame)
    ld hl, PSGBufferA + PCMBufferSize - 1
    ld a, (CurFrameIdx)
    and $01
    .repeat (PCMBufferSizeShift - 8)
        rla
    .endr
    add a, h
    ld h, a
    ld d, a
    exx
    ; reset sample skew
    ex af, af'
    xor a
    ex af, af'
; c0

    ; tilemap swap
    ld a, (CurFrameIdx)
    and $01
    rla
    rla
    rla
    or $f1
    out (VDPControl), a
    ld a, $82
    out (VDPControl), a

    ld c, VDPData

    ; upload local palette to VDP

    SetVDPAddress $0000 | CRAMWrite
    ld hl, LocalPalette
; c121
    .repeat 12
        outi
    .endr
; c313
    PlaySample
    .repeat 20
        outi
    .endr
; c320

p4: ; Advance to next frame
    ld hl, (FrameCount)

; c336
    PlaySample

    ld bc, (CurFrameIdx)
    inc bc
    scf
    ccf
    sbc hl, bc
    jp nz, +

    ; If we're past last frame, rewind to first frame
    ld bc, 0
    ld (CurFrameIdx), bc
    jp InitPlayer

+:
    ld (CurFrameIdx), bc
        ; restore command pointer into hl
    ex de, hl
    jp NextFrameLoad
 ; c85
 
DetectTVType:
; Returns a=0 for NTSC, a=1 for PAL
-:
    in a, (VDPScanline)
    cp 192
    jp nz, -
    
    ld b, 0
    ld c, a
-:
    in a, (VDPScanline)
    cp c
    jp z, -
    inc b
    ld c, a
    cp 0
    jp nz, -    

    ld a, b
    cp 100
    ld a, 0
    jr c, +
    inc a
+:

    ret

EnsureFifty:
    call DetectTVType
    or a
    ret nz

    ; load tiles (Background)
    SetVDPAddress $2000 | VRAMWrite
    ld hl, FiftyTiles
    ld de, 96
    CopyToVDP
    
    ; load tilemap (Background)
    SetVDPAddress $2dc | VRAMWrite
    ld hl, FiftyTileMap
    ld de, 6
    CopyToVDP
    
    ; load palette (Background)
    SetVDPAddress $0000 | CRAMWrite
    ld hl, FiftyPalette
    ld de, 2
    CopyToVDP
    
-:
    jp -


;==============================================================
; Data
;==============================================================

PSGInit:
.db $9f $bf $df $ff $81 $00 $a1 $00 $c1 $00
PSGInitEnd:

; VDP initialisation data
VDPInitData:
.db $04,$80,$00,$81,$f1,$82,$ff,$85,$ff,$86,$ff,$87,$00,$88,$00,$89,$ff,$8a
VDPInitDataEnd:

.ifdef FIXED_PCM
PCMData:
.incbin "220hz344cy.bin"
.endif

;==============================================================
; Tiles upload fixed sequences (jump tables, LUTs)
;==============================================================

.org $0600
TUUnpackJumpTable:
    ; "fast" VBlank part
    .repeat 223
        .db >TUDoStandardFast
    .endr
    .db >TUDoDirectValueFast
    .repeat 30
        .db >TUDoRepeatFast
    .endr
    .db >TUDoTerminator
    .db >TUDoVBlankSwitch
    ; "slow" active display part
    .repeat 223
        .db >TUDoStandardSlow
    .endr
    .db >TUDoDirectValueSlow
    .repeat 30
        .db >TUDoRepeatSlow
    .endr
    .db >TUDoTerminator
    .db >TUDoVBlankSwitch

.org $0800
TUDoVBlankSwitch:
        ; tile pointer back into hl
    ex de, hl
    
    ld a, ixh
    or a
    jp z, +
    ld ixh, 0
    PlaySampleSkew 86
    TilesUploadUnpack 0
+:
    ld ixh, 1
    PlaySampleSkew 86
    TilesUploadUnpack 1
    
FiftyTiles:
.db $F3 $00 $00 $00 $84 $00 $00 $00 $85 $00 $00 $00 $F7 $00 $00 $00 $16 $00 $00 $00 $14 $00 $00 $00 $E3 $00 $00 $00 $00 $00 $00 $00
.db $12 $00 $00 $00 $92 $00 $00 $00 $92 $00 $00 $00 $9E $00 $00 $00 $92 $00 $00 $00 $92 $00 $00 $00 $12 $00 $00 $00 $00 $00 $00 $00
.db $F9 $00 $00 $00 $09 $00 $00 $00 $11 $00 $00 $00 $21 $00 $00 $00 $41 $00 $00 $00 $80 $00 $00 $00 $F9 $00 $00 $00 $00 $00 $00 $00
FiftyTileMap:
.dw $0100 $0101 $0102
FiftyPalette:
.db $00 $3F

.org $0900
TUDoStandardSlow:
        ; standard case, increment tile index

        ; increment tile pointer of "tile index difference" tiles (still de <-> hl)
    TilesUploadUpdateTilePointer 0

        ; upload tile
    TilesUploadTileToVRAMSlow

    PlaySampleSkew 262

    TilesUploadUnpack 0

.org $0a00 ; /!\ must stay between TUDoStandardSlow and TUDoStandardFast (cf. TilesUploadUpdateTilePointer)
TUTileIndexToOffsetLUT:
    .repeat 256 index idx
        .db (idx * TileSize) >> 8
    .endr
    .repeat 256 index idx
        .db (idx * TileSize) & $ff
    .endr

.org $0c00
TUDoStandardFast:
        ; standard case, increment tile index

        ; increment tile pointer of "tile index difference" tiles (still de <-> hl)
    TilesUploadUpdateTilePointer 1

        ; upload tile
    TilesUploadTileToVRAMFast

    PlaySampleSkew 282

    TilesUploadUnpack 1

.org $0d00
TUDoDirectValueFast:
    pop hl
    ld a, l
    out (VDPControl), a
    ld a, h
    out (VDPControl), a
    
    PlaySampleSkew 40

        ; direct value, load tile index from frame data pointer

    pop hl

        ; tile index to tile pointer
    TilesUploadSetTilePointer

        ; upload tile
    TilesUploadTileToVRAMFast

; c323
    PlaySample

    TilesUploadUnpack 1

.org $0e00
TUDoDirectValueSlow:
    pop hl
    ld a, l
    out (VDPControl), a
    ld a, h
    out (VDPControl), a

    PlaySampleSkew 20

        ; direct value, load tile index from frame data pointer

    pop hl

        ; tile index to tile pointer
    TilesUploadSetTilePointer

        ; upload tile
    TilesUploadTileToVRAMSlow

; c319
    PlaySample

    TilesUploadUnpack 0

.org $0f00
TUDoRepeatFast:
        ; tile pointer back into hl
    ex de, hl

        ; repeat, value - 223 times
    sub 223
    ld d, a

    PlaySampleSkew 64

@Loop:
    TilesUploadTileToVRAMFast
    PlaySampleSkew 206
    dec d
    jp nz, @Loop

    TilesUploadUnpack 1

.org $1000
TUDoRepeatSlow:
        ; tile pointer back into hl
    ex de, hl

        ; repeat, value - 223 times
    sub 223
    ld d, a

    PlaySampleSkew 64

@Loop:
    TilesUploadTileToVRAMSlow
    PlaySampleSkew 186
    dec d
    jp nz, @Loop

    TilesUploadUnpack 0

.org $1100
TUDoTerminator:
        ; value 224 is terminator

        ; tile pointer back into hl
    ex de, hl

        ; get remaining tile count
    dec sp ; we actually need to pop only one byte
    pop af

        ; restore mapper slot
    ld a, ixl
    ld (MapperSlot2), a

        ; frame data pointer from sp to hl
    ld hl, 0
    add hl, sp

    PlaySampleSkew 70

    jp TilesUploadEnd

;==============================================================
; Tilemap upload fixed sequences (jump tables, LUTs)
;==============================================================

.org $1200
TMCommandsJumpTable:
    ; $00
    .repeat 32 index idx
        .ifeq idx 0
            .db >TMTerminator
        .else
            .ifeq idx 1
                .db >TMSkip1
            .else
                .db >TMSkip
            .endif
        .endif
        .db (>TMCacheIndex + idx)
    .endr
    ; $40
    .repeat 32
        .db >TMCacheRepeat, (>TMCacheRepeat + 1)
    .endr
    ; $80
    .repeat 32
        .db (>TMCacheRepeat + 2), (>TMCacheRepeat + 3)
    .endr
    ; $C0
    .repeat 16
        .db >TMRaw
    .endr
    .repeat 16
        .db (>TMRaw + 1)
    .endr
    .repeat 16
        .db (>TMRaw + 2)
    .endr
    .repeat 16
        .db (>TMRaw + 3)
    .endr

.org $1400
TMCacheIndex:
.repeat 32 index idx
    .org $1400 + (idx * $100)
        TMUploadCacheIndexMacro idx, 0
        TMProcessNextCommand 0, 0
    .org $1440 + (idx * $100)
        TMUploadCacheIndexMacro idx, 1
        TMProcessNextCommand 1, 0
    .org $1480 + (idx * $100)
        TMUploadCacheIndexMacro idx, 0
        TMProcessNextCommand 0, 1
    .org $14c0 + (idx * $100)
        TMUploadCacheIndexMacro idx, 1
        TMProcessNextCommand 1, 1
.endr

.org $3400
TMCacheRepeat:
.repeat 5 index idxRpt
    .org $3400 + (idxRpt * $100)
        TMUploadCacheRepeatMacro (idxRpt + 2), 0
        TMProcessNextCommand 0, 0
    .org $3440 + (idxRpt * $100)
        TMUploadCacheRepeatMacro (idxRpt + 2), 1
        TMProcessNextCommand 1, 0
    .org $3480 + (idxRpt * $100)
        TMUploadCacheRepeatMacro (idxRpt + 2), 0
        TMProcessNextCommand 0, 1
    .org $34c0 + (idxRpt * $100)
        TMUploadCacheRepeatMacro (idxRpt + 2), 1
        TMProcessNextCommand 1, 1
.endr

.org $3900
TMRaw:
.repeat 4 index idxRpt
    .org $3900 + (idxRpt * $100)
        TMUploadRawMacro (idxRpt + 1), 0, 0
            ; TMUploadRawMacro consumed one more "command", so step in inverted
        TMProcessNextCommand 1, 0
    .org $3940 + (idxRpt * $100)
        TMUploadRawMacro (idxRpt + 1), 1, 0
        TMProcessNextCommand 0, 0
    .org $3980 + (idxRpt * $100)
        TMUploadRawMacro (idxRpt + 1), 0, 1
        TMProcessNextCommand 1, 1
    .org $39c0 + (idxRpt * $100)
        TMUploadRawMacro (idxRpt + 1), 1, 1
        TMProcessNextCommand 0, 1
.endr

.org $3d00
TMSkip1:
    TMSkip1Macro 0, 0
    TMProcessNextCommand 0, 0
.org $3d40
    TMSkip1Macro 1, 0
    TMProcessNextCommand 1, 0
.org $3d80
    TMSkip1Macro 0, 1
    TMProcessNextCommand 0, 1
.org $3dc0
    TMSkip1Macro 1, 1
    TMProcessNextCommand 1, 1

.org $3e00
TMSkip:
    TMSkipMacro 0, 0
    TMProcessNextCommand 0, 0
.org $3e40
    TMSkipMacro 1, 0
    TMProcessNextCommand 1, 0
.org $3e80
    TMSkipMacro 0, 1
    TMProcessNextCommand 0, 1
.org $3ec0
    TMSkipMacro 1, 1
    TMProcessNextCommand 1, 1

.org $3f00
TMTerminator:
        ; frame data pointer into hl
    ld hl, 0
    add hl, sp
    jp TilemapUnpackEnd
.org $3f40
    ld hl, -1 ; popped one byte too much
    add hl, sp
    jp TilemapUnpackEnd
.org $3f80
    ld hl, 0
    add hl, sp
    jp TilemapUnpackEnd
.org $3fc0
    ld hl, -1
    add hl, sp
    jp TilemapUnpackEnd
