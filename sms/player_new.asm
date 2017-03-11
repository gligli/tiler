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

.sdsctag 7.0,"Video playback","Video playback demo","GliGli"

;==============================================================
; SMS defines
;==============================================================

.define VDPControl $bf
.define VDPData $be
.define VDPScanline $7e
.define PSGPort $7f
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
.define FrameSampleCount 826 ; 344 cycles per PCM sample = one sample every 320 cycles
.define FirstVBlankScanline 192
.define LastVBlankScanline 252
.define MaxTilesPerVideoFrames 207

.macro WaitVBlank args playSmp ; c11
    in a, (VDPControl)

---:
    .ifeq playSmp 1
        PlaySampleSkew 25
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

.macro PlaySample
    exx
    outd
    exx
.endm

.macro PlaySampleSkew args skew
    ex af, af'
        ; this macro is 27 cycles when not playing
        ; one sample every 325 cycles (320 + 5 from jr not jumping)
        ; + 1 to round up
    add a, (skew + 27) / 325 * 256 + 1
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

.macro TilesUploadSetTilePointer ; c68
        ; get a pointer on tile data from tile index

        ; upper bits of tile index select a rom bank
    ld a, h
    rra ; incoming carry will always be 0; pushes low bit into carry for use below

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

.macro TMProcessNextCommand ; c34
        ; read next command
    ld a, (de)
    inc de

        ; jump to command code using jump table
    ld l, a
    ld h, b
    ld h, (hl)
    ld l, c
    jp (hl)
.endm

.macro TMUploadCacheRepeatMacro args rpt  ;c40+52*rpt
        ; compute cache address
    .ifeq rpt 1
        dec a
    .else
        and %00111110
    .endif
    ld h, >TileMapCache
    ld l, a

        ; tilemap item into hl
    ld a, (hl)
    inc l
    ld h, (hl)
    ld l, a

    .ifgreq rpt 5
        PlaySample
    .endif
    
    .repeat rpt index idx
            ; low byte of tilemap item
        ld a, h
        out (VDPData), a

        in a, (VDPScanline) ; timing

            ; high byte of tilemap item
        ld a, l
        out (VDPData), a
        push hl ; store tilemap item into LocalTileMap
    .endr

    .ifeq rpt 6
        PlaySampleSkew 66
    .else
        .ifeq rpt 5
            PlaySampleSkew 14
        .else
            PlaySampleSkew 52*rpt+40+34
        .endif
    .endif
.endm

.macro TMUploadCacheIndexMacro args cacheIdx ; c57
    ld hl, (TileMapCache + cacheIdx * 2)

        ; low byte of tilemap item
    ld a, h
    out (VDPData), a

    push hl ; store tilemap item into LocalTileMap

        ; high byte of tilemap item
    ld a, l
    out (VDPData), a

    PlaySampleSkew 57+34
.endm

.macro TMSkipMacro args half ; c54
        ; local tilemap pointer
    ld hl, -1
    add hl, sp
        ; loop counter (command is skip count * 2 already)
    ld b, a
        ; target register
    ld c, VDPData

        ; upload "skip count" prev items from local tilemap
-:
        ; tilemap item low byte
    outd
        ; tilemap item high byte
    .ifeq half 1
        set 0, (hl)
    .else
        res 0, (hl)
    .endif

    PlaySampleSkew 57

    outd

    jp nz, -

        ; sp (reverse local tilemap pointer) must be updated too
    ld sp, hl
    inc sp

        ; jump table offset back into bc
    ld bc, TMCommandsJumpTable + half * $80

    PlaySampleSkew 54+34
.endm

.macro TMUploadRawMacro args rpt, half ; c4+4*(1-half)+52*rpt
        ; high byte of tilemap item from command
    .ifeq half 0
        dec a
    .endif
    ld l, a

    .repeat rpt index idx
            ; low byte of tilemap item
        ld a, (de)
        ld h, a
        out (VDPData), a
        push hl ; store tilemap item into LocalTileMap

            ; high byte of tilemap item
        ld a, l
        out (VDPData), a
        .ifneq idx (rpt - 1)
            nop
        .else
            inc de
        .endif
    .endr

    PlaySampleSkew 52*rpt+4+4*(1-half)+34
.endm

;==============================================================
; RAM variables
;==============================================================

.enum $c000 export
    PSGBufferA        dsb 1024
    PSGBufferB        dsb 1024
    LocalTileMap      dsb TileMapSize
    LocalTileMapEnd   .
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
    retn

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

    ; Clear PCM buffers
    ld hl, PSGBufferA
    ld b, 0
    ld a, $ff
-:  .repeat 2048/256
        ld (hl), a
        inc hl
    .endr
    djnz -

        ; init PCM player
    exx
    ld c, PSGPort
    ld hl, PSGBufferB + 1024 - 1
    ld de, 0
    exx

        ; algo expects VBlank state on start
    WaitVBlank 0

InitPlayer:

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

        ; Set tilemap VRAM pointer (also store it into ix)
    xor a
    out (VDPControl), a
    ld a, (CurFrameIdx)
    and 1
        ; we want to move bit 0 to bit 5
    rrca
    ld c, a ; jump table jump low byte depending on VRAM "half" into c
    rrca
    rrca
    or VRAMWrite >> 8
    out (VDPControl), a

        ; jump table high byte
    ld b, >TMCommandsJumpTable

        ; save command pointer into de
    ex de, hl

        ; sp will be used as a pointer on a reversed local tilemap
    ld sp, LocalTileMapEnd
    
; c326
    PlaySample

        ; start unpacking tilemap
    TMProcessNextCommand

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
    ld hl, PSGBufferB + 1024
    ld a, (CurFrameIdx)
    and $01
    rla
    rla
    neg
    add a, h
    ld h, a

        ; use SP as a PSGBuffer data pointer
    ld sp, hl

        ; 36 samples per iteration
    ld ixh, FrameSampleCount / 36 + 0.5
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
        cp 0 ; timing
        cp 0 ; timing
; c320
        PlaySample
        Unpack6Samples
        Unpack6Samples
    dec ixh
    jp nz, PCMUnpackLoop
; c322
    PlaySample

        ; cleanup extra decoding
    pop hl
    ld hl, $ffff
    push hl
; c31

        ; restore frame data pointer into hl
.ifdef FIXED_PCM
    ld b, iyh
    ld c, iyl
    ld hl, FrameSampleCount / 2
    add hl, bc
.else
    dec bc
    ld h, b
    ld l, c
.endif
; c45

    PlaySampleSkew 56
p2:

    WaitVBlank 1

p3:
        ; frame data pointer into de
    ex de, hl

    ; restart PCM player on other buffer
    exx
    ld (PSGBufferA), hl ; debug tool (shows how many samples actually played per frame)
    ld hl, PSGBufferA + 1024 - 1
    ld a, (CurFrameIdx)
    and $01
    rla
    rla
    add a, h
    ld h, a
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

.ifdef FIXED_PCM
PCMData:
.incbin "220hz344cy.bin"
.endif

;==============================================================
; PCM fixed sequences (jump tables, LUTs)
;==============================================================

.org $0500:
PCMUnpackLUT:
    .repeat 256 index dat
        .db $90 | ((dat >> 4) & $0f)
    .endr
    .repeat 256 index dat
        .db $b0 | (dat & $0f)
    .endr
    .repeat 256 index dat
        .db $d0 | ((dat >> 4) & $0f)
    .endr
    .repeat 256 index dat
        .db $90 | (dat & $0f)
    .endr
    .repeat 256 index dat
        .db $b0 | ((dat >> 4) & $0f)
    .endr
    .repeat 256 index dat
        .db $d0 | (dat & $0f)
    .endr

;==============================================================
; Tiles upload fixed sequences (jump tables, LUTs)
;==============================================================

.org $0b00
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

.org $0d00
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

.org $0e00
TUDoStandardSlow:
        ; standard case, increment tile index

        ; increment tile pointer of "tile index difference" tiles (still de <-> hl)
    TilesUploadUpdateTilePointer 0

        ; upload tile
    TilesUploadTileToVRAMSlow

    PlaySampleSkew 262

    TilesUploadUnpack 0

.org $0f00 ; /!\ must stay between TUDoStandardSlow and TUDoStandardFast (cf. TilesUploadUpdateTilePointer)
TUTileIndexToOffsetLUT:
    .repeat 256 index idx
        .db (idx * TileSize) >> 8
    .endr
    .repeat 256 index idx
        .db (idx * TileSize) & $ff
    .endr

.org $1100
TUDoStandardFast:
        ; standard case, increment tile index

        ; increment tile pointer of "tile index difference" tiles (still de <-> hl)
    TilesUploadUpdateTilePointer 1

        ; upload tile
    TilesUploadTileToVRAMFast

    PlaySampleSkew 282

    TilesUploadUnpack 1

.org $1200
TUDoDirectValueFast:
        ; direct value, load tile index from frame data pointer

    pop hl

        ; tile index to tile pointer
    TilesUploadSetTilePointer

        ; upload tile
    TilesUploadTileToVRAMFast

; c319
    PlaySample

    TilesUploadUnpack 1

.org $1300
TUDoDirectValueSlow:
        ; direct value, load tile index from frame data pointer

    pop hl

        ; tile index to tile pointer
    TilesUploadSetTilePointer

        ; upload tile
    TilesUploadTileToVRAMSlow

    inc iy ; timing
    inc iy ; timing

; c319
    PlaySample

    TilesUploadUnpack 0

.org $1400
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

.org $1500
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

.org $1600
TUDoTerminator:
        ; value 224 is terminator

        ; tile pointer back into hl
    ex de, hl

        ; get remaining tile count
    dec sp ; we actually need to pop only one byte
    pop af

    cp 0
    jp z, @MaxUploaded

        ; ensure proper video timing my simulating the upload of remaining tiles
    ld d, a
@Loop:
    .repeat 32
        inc iy
    .endr
    PlaySample
    .repeat 19
        inc iy
    .endr
    PlaySampleSkew 204
    dec d
    jp nz, @Loop

@MaxUploaded:

        ; restore mapper slot
    ld a, ixl
    ld (MapperSlot2), a

        ; frame data pointer from sp to hl
    ld hl, 0
    add hl, sp

    PlaySampleSkew 89

    jp TilesUploadEnd

;==============================================================
; Tilemap upload fixed sequences (jump tables, LUTs)
;==============================================================

.org $1700
TMCommandsJumpTable:
    ; $00
    .db >TMTerminator
    .repeat 28 index idx
        .db (>TMCacheIndex + idx), >TMSkip
    .endr
    .repeat 3
        .db >TMCacheRpt1, >TMSkip
    .endr
    .db >TMCacheRpt1
    ; $40
    .repeat 32
        .db >TMCacheRpt2, >TMCacheRpt3
    .endr
    ; $80
    .repeat 32
        .db >TMCacheRpt4, >TMCacheRpt5
    .endr
    ; $C0
    .repeat 8
        .db >TMCacheRpt6, >TMRawRpt1
    .endr
    .repeat 8
        .db >TMCacheRpt6, >TMRawRpt2
    .endr
    .repeat 8
        .db >TMCacheRpt6, >TMRawRpt3
    .endr
    .repeat 8
        .db >TMCacheRpt6, >TMRawRpt4
    .endr

.org $1800
TMCacheIndex:
.repeat 28 index idx
    .org $1800 + (idx * $100)
        TMUploadCacheIndexMacro idx
        TMProcessNextCommand
    .org $1880 + (idx * $100)
        TMUploadCacheIndexMacro idx
        TMProcessNextCommand
.endr

.org $3400
TMRawRpt4:
    TMUploadRawMacro 4, 0
    TMProcessNextCommand
.org $3480
    TMUploadRawMacro 4, 1
    TMProcessNextCommand

.org $3500
TMRawRpt3:
    TMUploadRawMacro 3, 0
    TMProcessNextCommand
.org $3580
    TMUploadRawMacro 3, 1
    TMProcessNextCommand

.org $3600
TMRawRpt2:
    TMUploadRawMacro 2, 0
    TMProcessNextCommand
.org $3680
    TMUploadRawMacro 2, 1
    TMProcessNextCommand

.org $3700
TMRawRpt1:
    TMUploadRawMacro 1, 0
    TMProcessNextCommand
.org $3780
    TMUploadRawMacro 1, 1
    TMProcessNextCommand

.org $3800
TMCacheRpt6:
    TMUploadCacheRepeatMacro 6
    TMProcessNextCommand
.org $3880
    TMUploadCacheRepeatMacro 6
    TMProcessNextCommand

.org $3900
TMCacheRpt5:
    TMUploadCacheRepeatMacro 5
    TMProcessNextCommand
.org $3980
    TMUploadCacheRepeatMacro 5
    TMProcessNextCommand

.org $3a00
TMCacheRpt4:
    TMUploadCacheRepeatMacro 4
    TMProcessNextCommand
.org $3a80
    TMUploadCacheRepeatMacro 4
    TMProcessNextCommand

.org $3b00
TMCacheRpt3:
    TMUploadCacheRepeatMacro 3
    TMProcessNextCommand
.org $3b80
    TMUploadCacheRepeatMacro 3
    TMProcessNextCommand

.org $3c00
TMCacheRpt2:
    TMUploadCacheRepeatMacro 2
    TMProcessNextCommand
.org $3c80
    TMUploadCacheRepeatMacro 2
    TMProcessNextCommand

.org $3d00
TMCacheRpt1:
    TMUploadCacheRepeatMacro 1
    TMProcessNextCommand
.org $3d80
    TMUploadCacheRepeatMacro 1
    TMProcessNextCommand

.org $3e00
TMSkip:
    TMSkipMacro 0
    TMProcessNextCommand
.org $3e80
    TMSkipMacro 1
    TMProcessNextCommand

.org $3f00
TMTerminator:
    ex de, hl ; frame data pointer into hl
    jp TilemapUnpackEnd
.org $3f80
    ex de, hl ; frame data pointer into hl
    jp TilemapUnpackEnd

;==============================================================
; Data
;==============================================================

PSGInit:
.db $9f $bf $df $ff $81 $00 $a1 $00 $c1 $00
PSGInitEnd:

; VDP initialisation data
VDPInitData:
.db $04,$80,$00,$81,$f9,$82,$ff,$85,$ff,$86,$ff,$87,$00,$88,$00,$89,$ff,$8a
VDPInitDataEnd: