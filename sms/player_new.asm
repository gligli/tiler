.define FIXED_PCM

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

.macro WaitVBlank args playSmp ; c0
    in a, (VDPControl)

    .ifeq playSmp 1
        jp +++
---:
        PlaySample
+++:
; c0
         .repeat 19
             add iy, iy ; timing
         .endr
         inc iy ; timing
; c295
    .else
---:
    .endif
    in a, (VDPControl)
    or a  ; update flags
    jp p, ---
; c320
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
    add a, (skew + 25) / 320 * 256
    jp nc, ++++
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

.macro TilesUploadSetTilePointer ; c84
        ; VBlank bit preserve
    rr e

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

        ; VBlank bit restore
    rl e
.endm

.macro TilesUploadUpdateTilePointer ; c45
        ; update tile pointer

        ; get byte offset from "tile index difference" using LUT
    dec h ; ld h, (>TUTileIndexToOffsetLUT + 1)
    ld l, a
    ld a, (hl)
    dec h
    ld h, (hl)
    ld l, a

        ; VBlank bit preserve
    rra

        ; add to tile pointer
    add hl, de

        ; VBlank bit restore
    rla
.endm

.macro TilesUploadUnpack  ; c49
        ; tile pointer saved into de
    ex de, hl

        ; get next packed data
    pop hl
    dec sp ; we actually need to pop only one byte

        ;  use jump table to handle it
    ld a, l
    ld h, >TUUnpackJumpTable
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

.macro TilesUploadTileToVRAMSlow ; c172+10
    .repeat 12
        outi
        inc iy ; timing
    .endr
    outi
; c328
    PlaySample
    .repeat 11
        outi
        dec iy ; timing
    .endr
    outi
; c302
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

.macro DoTilesUploadOne args ps ; c220

        ; when in VBlank use fast upload
    jp c, ++

    TilesUploadTileToVRAMSlow
    
    .ifeq ps 1
        PlaySample
    .endif

        ; update VSync bit
        ; (detect active display -> blank transition)
    in a, (VDPScanline)
    add a, 256 - FirstVBlankScanline

    jp +++

++:

    TilesUploadTileToVRAMFast

    .ifeq ps 1
        PlaySample
    .endif

        ; update VSync bit
        ; (detect blank -> active display transition, accounting for delay before next upload)
    in a, (VDPScanline)
    cp LastVBlankScanline
+++:

.endm

.macro TMCopy2CacheSlots args ps
        ; load both high nibbles
    ld a, (hl)
    inc hl

    .ifeq ps 0
        PlaySample
    .endif

        ; high nibble a
    ld (de), a
    inc e

    .ifeq ps 1
        PlaySample
    .endif

        ; low byte a
    ldi

    .ifeq ps 2
        PlaySample
    .endif

        ; high nibble b
    .repeat 4
        rrca
    .endr
    ld (de), a
    inc e

    .ifeq ps 3
        PlaySample
    .endif

        ; low byte b
    ldi
.endm

.macro TMProcessNextCommand args half ; c40
        ; read next command
    ld a, (de)
    inc de

        ; jump to command code using jump table
    ld l, a
    ld h, >TMCommandsJumpTable
    ld h, (hl)
    ld l, half * $80
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

    .repeat rpt index idx
            ; low byte of tilemap item
        ld a, h
        out (VDPData), a

            ; sample skew
        .ifeq idx 0
            inc b
            inc b
            inc b
        .else
            inc b
            or 0 ; timing
        .endif

            ; high byte of tilemap item
        ld a, l
        out (VDPData), a
        push hl ; store tilemap item into LocalTileMap
    .endr
.endm

.macro TMUploadCacheIndexMacro args cacheIdx ; c57
    ld hl, (TileMapCache + cacheIdx * 2)

        ; low byte of tilemap item
    out (c), h

    push hl ; store tilemap item into LocalTileMap

        ; sample skew
    inc b

        ; high byte of tilemap item
    out (c), l

        ; sample skew
    inc b
.endm

.macro TMSkipMacro args half ; c54
        ; loop counter (command is skip count * 2 already)
    ld l, b ; cf. next code block
    ld b, a

        ; update sample skew (temporarily into a)
    rrca
    add a, l ; prev sample skew
    add a, 2

        ; local tilemap pointer
    ld hl, -1
    add hl, sp

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
    outd
    jp nz, -

        ; sp (reverse local tilemap pointer) must be updated too
    ld sp, hl
    inc sp

        ; sample skew back into b
    ld b, a
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
        .ifeq idx (rpt - 1)
                ; sample skew
            inc b
            out (c), l
        .else
            ld a, l
            out (VDPData), a
        .endif

            ; sample skew
        inc b
    .endr

        ; onto next command
    inc de
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

        ; algo expects VBlank state on start
    WaitVBlank 0

InitPlayer:
        ; init PCM player
    exx
    ld c, PSGPort
    ld hl, PSGBufferB + 1024 - 1
    ld de, 0
    exx

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
 ; c229 from jump

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

    ; Load palette if frame contains one
    ld de, LocalPalette
; c319/c322
    PlaySample

    ld a, (hl)
    inc hl
    cp $00
    jp z, NoFramePalette

    .repeat 18
        ldi
    .endr
; c315
    PlaySample

    .repeat 14
        ldi
    .endr

    jp PaletteEnd

NoFramePalette:

        ; ensure same timing as palette
    .repeat 18
        ld (0), hl ; timing
    .endr
; c320
    PlaySample

    .repeat 15
        ld (0), hl ; timing
    .endr

PaletteEnd:
; c234/c240

p1: ; Unpack tiles indexes and copy corresponding tiles to VRAM

        ; slot2 bank into ixl
    ld a, (MapperSlot2)
    ld ixl, a

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
; c320
    PlaySample

    jp BankChangeEnd

NoBankChange:

        ; ensure same timing as bank change
    ld (0), a ; timing
    ld (0), a ; timing

; c319
    PlaySample

    inc iy ; timing

BankChangeEnd:
; c10

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

        ; f carry bit = VBlank? , we start in VBlank
    scf

        ; prepare VRAM write register
    ld c, VDPData

        ; number of tiles uploaded
    ld ixh, 0

        ; frame data pointer into sp
    ld sp, hl
;c103

        ; start unpacking tile indexes
    TilesUploadUnpack

TilesUploadEnd:
        
; c185

        ;copy tilemap cache into ram
    ld de, TileMapCache
    TMCopy2CacheSlots 2
    TMCopy2CacheSlots -1
    TMCopy2CacheSlots -1
    TMCopy2CacheSlots -1
    TMCopy2CacheSlots 1
    TMCopy2CacheSlots -1
    TMCopy2CacheSlots -1
    TMCopy2CacheSlots -1
    TMCopy2CacheSlots 0
    TMCopy2CacheSlots -1
    TMCopy2CacheSlots -1
    TMCopy2CacheSlots -1
    PlaySample
    TMCopy2CacheSlots -1
    TMCopy2CacheSlots -1
    TMCopy2CacheSlots -1
    TMCopy2CacheSlots 3
; c16

        ; Set tilemap VRAM pointer (also store it into ix)
    xor a
    out (VDPControl), a
    ld a, (CurFrameIdx)
    and 1
    ld e, a
        ; we want to move bit 0 to bit 5
    rrca
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
; c106

        ; target register
    ld c, VDPData

        ; start unpacking tilemap
    bit 0, l
    jp nz, +
    TMProcessNextCommand 0
+:
    TMProcessNextCommand 1

TilemapUnpackEnd:
; c120
    PlaySampleSkew 197

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
    ld h, b
    ld l, c
.endif
; c55

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
; c129

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
; c204
    ; upload local palette to VDP

    SetVDPAddress $0000 | CRAMWrite
    ld hl, LocalPalette
; c250
    .repeat 4
        outi
    .endr
; c314
    PlaySample
    .repeat 20
        outi
    .endr
; c320
    PlaySample
    .repeat 8
        outi
    .endr
; c128

p4: ; Advance to next frame
    ld bc, (CurFrameIdx)
    inc bc
    ld hl, (FrameCount)
    sbc hl, bc
    jp nz, +

    ; If we're past last frame, rewind to first frame
    ld bc, 0
    ld (CurFrameIdx), bc
        ; restore command pointer into hl
    ex de, hl
    jp InitPlayer

+:
    ld (CurFrameIdx), bc
        ; restore command pointer into hl
    ex de, hl
    jp NextFrameLoad
 ; c229

.ifdef FIXED_PCM
PCMData:
.incbin "220hz344cy.bin"
.endif

;==============================================================
; PCM fixed sequences (jump tables, LUTs)
;==============================================================

.org $0700:
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

.org $0d00
TUUnpackJumpTable:
    .repeat 223
        .db >TUDoStandard
    .endr
    .db >TUDoDirectValue
    .db >TUDoTerminator
    .repeat 256 - 1 - 1 - 223
        .db >TUDoRepeat
    .endr

.org $0e00 ; /!\ must stay $200 below TUDoStandard (cf. TilesUploadUpdateTilePointer)
TUTileIndexToOffsetLUT:
    .repeat 256 index idx
        .db (idx * TileSize) >> 8
    .endr
    .repeat 256 index idx
        .db (idx * TileSize) & $ff
    .endr

.org $1000
TUDoStandard:
        ; standard case, increment tile index

        ; increment tile pointer of "tile index difference" tiles (still de <-> hl)
    TilesUploadUpdateTilePointer

        ; upload tile
    DoTilesUploadOne 1

        ; update "uploaded tiles" counter
    inc ixh

; c322
    ;PlaySample done by "DoTilesUploadOne 1"

    TilesUploadUnpack

.org $1100
TUDoDirectValue:
        ; direct value, load tile index from frame data pointer

    pop hl

        ; tile index to tile pointer
    TilesUploadSetTilePointer

    PlaySampleSkew 51

        ; upload tile
    DoTilesUploadOne 1

        ; update "uploaded tiles" counter
    inc ixh

    ;PlaySample done by "DoTilesUploadOne 1"

    TilesUploadUnpack

.org $1300
TUDoRepeat:
        ; tile pointer back into hl
    ex de, hl

        ; VBlank bit preserve
    rr e

        ; repeat, value - 223 times
    sub 223

    ld d, a

        ; update "uploaded tiles" counter
    add a, ixh
    ld ixh, a

        ; VBlank bit restore
    rl e

    PlaySampleSkew 96

@Loop:
    DoTilesUploadOne 0
    PlaySampleSkew 234
    dec d
    jp nz, @Loop

    TilesUploadUnpack

.org $1500
TUDoTerminator:
        ; value 224 is terminator

        ; tile pointer back into hl
    ex de, hl

        ; VBlank bit preserve
    rr e

        ; always upload max tiles to ensure proper video timing
    ld a, ixh
    sub MaxTilesPerVideoFrames
    jp z, @MaxUploaded

    ld d, a

        ; VBlank bit restore
    rl e
@Loop:
    DoTilesUploadOne 0
    PlaySampleSkew 234
    inc d
    jp nz, @Loop

@MaxUploaded:

        ; restore mapper slot
    ld a, ixl
    ld (MapperSlot2), a

        ; frame data pointer from sp to hl
    ld hl, 0
    add hl, sp

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
        TMProcessNextCommand 0
    .org $1880 + (idx * $100)
        TMUploadCacheIndexMacro idx
        TMProcessNextCommand 1
.endr

.org $3400
TMRawRpt4:
    TMUploadRawMacro 4, 0
    TMProcessNextCommand 0
.org $3480
    TMUploadRawMacro 4, 1
    TMProcessNextCommand 1

.org $3500
TMRawRpt3:
    TMUploadRawMacro 3, 0
    TMProcessNextCommand 0
.org $3580
    TMUploadRawMacro 3, 1
    TMProcessNextCommand 1

.org $3600
TMRawRpt2:
    TMUploadRawMacro 2, 0
    TMProcessNextCommand 0
.org $3680
    TMUploadRawMacro 2, 1
    TMProcessNextCommand 1

.org $3700
TMRawRpt1:
    TMUploadRawMacro 1, 0
    TMProcessNextCommand 0
.org $3780
    TMUploadRawMacro 1, 1
    TMProcessNextCommand 1

.org $3800
TMCacheRpt6:
    TMUploadCacheRepeatMacro 6
    TMProcessNextCommand 0
.org $3880
    TMUploadCacheRepeatMacro 6
    TMProcessNextCommand 1

.org $3900
TMCacheRpt5:
    TMUploadCacheRepeatMacro 5
    TMProcessNextCommand 0
.org $3980
    TMUploadCacheRepeatMacro 5
    TMProcessNextCommand 1

.org $3a00
TMCacheRpt4:
    TMUploadCacheRepeatMacro 4
    TMProcessNextCommand 0
.org $3a80
    TMUploadCacheRepeatMacro 4
    TMProcessNextCommand 1

.org $3b00
TMCacheRpt3:
    TMUploadCacheRepeatMacro 3
    TMProcessNextCommand 0
.org $3b80
    TMUploadCacheRepeatMacro 3
    TMProcessNextCommand 1

.org $3c00
TMCacheRpt2:
    TMUploadCacheRepeatMacro 2
    TMProcessNextCommand 0
.org $3c80
    TMUploadCacheRepeatMacro 2
    TMProcessNextCommand 1

.org $3d00
TMCacheRpt1:
    TMUploadCacheRepeatMacro 1
    TMProcessNextCommand 0
.org $3d80
    TMUploadCacheRepeatMacro 1
    TMProcessNextCommand 1

.org $3e00
TMSkip:
    TMSkipMacro 0
    TMProcessNextCommand 0
.org $3e80
    TMSkipMacro 1
    TMProcessNextCommand 1

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