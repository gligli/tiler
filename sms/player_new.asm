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
.define PCMSpeed 45

.macro WaitVBlank args playSmp
    in a, (VDPControl)
---:
    .ifeq playSmp 1
        PlaySample
    .endif
    in a, (VDPControl)
    or a  ; update flags
    jp p, ---
.endm

.macro SetVDPAddress args addr
        ; Sets the VDP address
    ld a, <addr
    out (VDPControl),a
    ld a, >addr
    out (VDPControl),a
.endm

.macro PlaySample
    ex af, af'
    ld a, r
    sub PCMSpeed
    jp m, +++
    exx

    outd

    ld r, a

    exx
+++:
    ex af, af'
.endm

.macro Unpack6Samples
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

.macro TilesUploadTileToVRAM args slow
    .repeat TileSize  / 2 - 1
        outi
        .ifeq slow 1
            ld (hl), 0 ; no effect (writes ROM)
        .endif
    .endr
    outi

    PlaySample

    .repeat TileSize  / 2 - 1
        outi
        .ifeq slow 1
            ld (hl), 0 ; no effect (writes ROM)
        .endif
    .endr
    outi

    PlaySample
.endm

.macro TilesUploadSetTilePointer
        ; get a pointer on tile data from tile index

        ; upper bits of tile index select a rom bank
    ld a, h
    rra ; incoming carry will always be 0; pushes low bit into carry for use below

    ld (MapperSlot2), a

        ; lower bits select an offset in that bank
        ; we want the low 9 bits of hl, x32, +$4000, in hl
        ; %-------a bcdefghi
        ;   to
        ; %10abcdef ghi00000
    ld a, l
    ld l, %00000010 ; to get the high bits we need
    .repeat 3
        rra     ; then rotate carry - a - l right three times
        rr l
    .endr
    ld h, a
.endm

.macro TilesUploadUpdateTilePointer
        ; update tile pointer

        ; get byte offset from "tile index difference" using LUT
    ld h, >TUTileIdxDiffToOffsetLUT
    ld l, a
    ld a, (hl)
    inc h
    ld h, (hl)
    ld l, a

        ; add to tile pointer
    add hl, de
.endm

.macro TilesUploadUnpack
        ; tile pointer saved into de
    ex de, hl

        ; get next packed data
    dec sp ; we actually need to pop only one byte
    pop af

        ;  use jump table to handle it
    ld l, a
    ld h, >TUUnpackJumpTable
    ld h, (hl)
    ld l, 0
    jp (hl)
.endm

.macro DoTilesUpload args many

        ; f' carry bit = VBlank?
    ex af, af'

@Again:
        ; when not in VBlank use slow upload
    jr nc, @Slow

    TilesUploadTileToVRAM 0

        ; update VSync bit
        ; (detect blank -> active display transition, accounting for delay before next upload)
    in a, (VDPScanline)
    .ifeq 0 0
        add a, 256 - 253
        rla ; push sign into carry
    .else
        ; faster but less safe?
        cp 253
    .endif

    .ifeq many 1
        dec e
        jp nz, @Again
    .endif

        ; VBlank bit preserve
    ex af, af'

    TilesUploadUnpack

@Slow:

    TilesUploadTileToVRAM 1

        ; update VSync bit
        ; (detect active display -> blank transition)
    in a, (VDPScanline)
    add a, 256 - 192

    .ifeq many 1
        dec e
        jp nz, @Again
    .endif

        ; VBlank bit preserve
    ex af, af'

    TilesUploadUnpack
.endm

.macro TMProcessNextCommand
    PlaySample

        ; read next command
    ld a, (de)
    inc de

        ; jump to command code using jump table
    ld l, a
    ld h, >TMCommandsJumpTable
    ld h, (hl)
    ld l, 0
    jp (hl)
.endm

.macro TMUploadCacheRepeatMacro args rpt
        ; compute cache address
    .ifeq rpt 1
        dec a
    .else
        and %00111110
    .endif
    ld h, >TileMapCache
    ld l, a

        ; tilemap item  low byte into b
    inc l
    ld b, (hl)

    .repeat rpt index idx
            ; low byte of tilemap item
        ld a, b
        out (VDPData), a

            ; high byte of tilemap item
        .ifeq idx 0
            dec l
        .else
            nop ; ensure min 26 cycles between VRAM writes
        .endif
        ld a, (hl)
        ld c, a
        out (VDPData), a
        push bc ; store tilemap item into LocalTileMap
    .endr
.endm

.macro TMUploadCacheIndexMacro args cacheIdx
    ld hl, (TileMapCache + cacheIdx * 2)

        ; low byte of tilemap item
    ld a, h
    out (VDPData), a

    push hl ; store tilemap item into LocalTileMap

        ; high byte of tilemap item
    ld a, l
    out (VDPData), a
.endm

.macro TMUploadSkipMacro
        ; local tilemap pointer
    ld hl, -1
    add hl, sp
        ; loop counter (command is skip count)
    ld b, a
        ; target register
    ld c, VDPData

        ; depending on VRAM "half", will call TMSkipHalfMacro 0 or 1
    jp (ix)
.endm

.macro TMSkipHalfMacro args half
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

    TMProcessNextCommand
.endm

.macro TMUploadRawMacro args rpt
        ; high byte of tilemap item from command
    and iyl  ; iyl = $0e or $0f depending on VRAM "side"
    ld c, a

    .repeat rpt index idx
            ; low byte of tilemap item
        ld a, (de)
        ld b, a
        out (VDPData), a
        push bc ; store tilemap item into LocalTileMap

            ; high byte of tilemap item
        ld a, c
        out (VDPData), a
        .ifneq idx (rpt - 1)
            nop
        .else
            inc de
        .endif
    .endr
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
;     WaitVBlank 1
;     WaitVBlank 1
;     WaitVBlank 1
;     jp TilemapUnpackEnd

        ; are we using slot 2?
    bit 7, h
    jp z, +

        ; if so, move to next bank
    ld a, (MapperSlot2)
    ld (MapperSlot1), a
    inc a
    ld (MapperSlot2), a

        ; rewind to slot 1
    ld a, h
    sub $40
    ld h, a

+:
    ; Load palette if frame contains one
    ld de, LocalPalette
    ld a, (hl)
    inc hl
    cp $00
    jp z, NoFramePalette

    .repeat TilePaletteSize * 2
        ldi
    .endr

NoFramePalette:

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

NoBankChange:


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

        ; f' carry bit = VBlank? , we start in VBlank
    ex af, af'
    scf
    ex af, af'

        ; Prepare VRAM write register
    ld c, VDPData

        ; frame data pointer into sp
    ld sp, hl

        ; start unpacking tile indexes
    TilesUploadUnpack

TilesUploadEnd:

        ;copy tilemap cache into ram
    ld de, TileMapCache
    .repeat 16
            ; load both high nibbles
        ld a, (hl)
        inc hl

            ; high nibble a
        ld (de), a
        inc e

            ; low byte a
        ldi

            ; high nibble b
        .repeat 4
            rrca
        .endr
        ld (de), a
        inc e

            ; low byte b
        ldi
    .endr

        ; Set tilemap VRAM pointer (also store it into ix)
    xor a
    out (VDPControl), a
    ld a, (CurFrameIdx)
    and 1
    ld c, a
        ; we want to move bit 0 to bit 5
    rrca
    ld ixl, a ; VRAM "half" in ixl bit 7
    rrca
    rrca
    or VRAMWrite >> 8
    out (VDPControl), a
    
    ld a, c
    or $0e
    ld iyl, a ; $e for first "half", $0f for second in iyl

        ; prepare (ix) jump table
    ld ixh, >TMSkipHalf0

        ; save command pointer into de
    ex de, hl

        ; sp will be used as a pointer on a reversed local tilemap
    ld sp, LocalTileMapEnd

        ; start unpacking tilemap
    TMProcessNextCommand

TilemapUnpackEnd:

    PlaySample

    ld b, h
    ld c, l
    ld iyh, b
    ld iyl, c

.ifdef FIXED_PCM
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

        ; 842 samples per video frame
        ; 24 samples per iteration
    ld ixh, 842 / 48

    PlaySample

    ;c+68

    PCMUnpackLoop:
        Unpack6Samples
        Unpack6Samples
        PlaySample
        Unpack6Samples
        Unpack6Samples
        PlaySample
        Unpack6Samples
        Unpack6Samples
        PlaySample
        Unpack6Samples
        Unpack6Samples
        PlaySample
    dec ixh
    jp nz, PCMUnpackLoop
    Unpack6Samples
    Unpack6Samples
    PlaySample
    Unpack6Samples
    Unpack6Samples
    PlaySample
    Unpack6Samples

        ; cleanup extra decoding
    pop hl
    pop hl
    ld hl, $ffff
    push hl
    push hl

        ; restore frame data pointer into hl
    ld a, iyh
    ld h, a
    ld a, iyl
    ld l, a

p2:

    WaitVBlank 1

p3:
        ; frame data pointer into de
    ex de, hl

    ; restart PCM player on other buffer
    exx
tstpcm:
    ld hl, PSGBufferA + 1024 - 1
    ld a, (CurFrameIdx)
    and $01
    rla
    rla
    add a, h
    ld h, a
    exx

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
    .repeat TilePaletteSize * 2
        outi
    .endr

    PlaySample

p4: ; Advance to next frame
    ld bc, (CurFrameIdx)
    inc bc
    ld hl, (FrameCount)
    sbc hl, bc
    jr nz, +

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

.ifdef FIXED_PCM
PCMData:
.incbin "200hz.bin"
.endif

;==============================================================
; PCM fixed sequences (jump tables, LUTs)
;==============================================================

.org $0900:
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

.org $0f00
TUUnpackJumpTable:
    .repeat 223
        .db >TUDoStandard
    .endr
    .db >TUDoDirectValue
    .db >TUDoTerminator
    .repeat 256 - 1 - 1 - 223
        .db >TUDoRepeat
    .endr

.org $1000
TUDoRepeat:
        ; tile pointer back into hl
    ex de, hl

        ; repeat, value - 223 times
    sub 223
    ld e, a

    DoTilesUpload 1

.org $1200
TUDoDirectValue:
        ; direct value, load tile index from frame data pointer

    pop hl

        ; tile index to tile pointer
    TilesUploadSetTilePointer

    DoTilesUpload 0

.org $1400
TUDoTerminator:
        ; value 224 is terminator

        ; restore mapper slot
    ld a, ixl
    ld (MapperSlot2), a

        ; frame data pointer from sp to hl
    ld hl, 0
    add hl, sp

    jp TilesUploadEnd

.org $1600
TUDoStandard:
        ; standard case, increment tile index

        ; increment tile pointer of "tile index difference" tiles (still de <-> hl)
    TilesUploadUpdateTilePointer

    DoTilesUpload 0

.org $1800
TUTileIdxDiffToOffsetLUT:
    .repeat 256 index idx
        .db (idx * TileSize) & $ff
    .endr
    .repeat 256 index idx
        .db (idx * TileSize) >> 8
    .endr

;==============================================================
; Tilemap upload fixed sequences (jump tables, LUTs)
;==============================================================

.org $1a00
TMCommandsJumpTable:
    ; $00
    .db >TMTerminator
    .repeat 24 index idx
        .db (>TMCacheIndex + idx), >TMSkip
    .endr
    .repeat 7 index idx
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

.org $1b00
TMCacheIndex:
.repeat 24 index idx
    .org $1b00 + (idx * 256)
        TMUploadCacheIndexMacro idx
        TMProcessNextCommand
.endr

.org $3300
TMTerminator:
        ; frame data pointer into hl
    ex de, hl

    jp TilemapUnpackEnd

.org $3400
TMRawRpt4:
    TMUploadRawMacro 4
    TMProcessNextCommand

.org $3500
TMRawRpt3:
    TMUploadRawMacro 3
    TMProcessNextCommand

.org $3600
TMRawRpt2:
    TMUploadRawMacro 2
    TMProcessNextCommand

.org $3700
TMRawRpt1:
    TMUploadRawMacro 1
    TMProcessNextCommand

.org $3800
TMSkip:
    TMUploadSkipMacro
    TMProcessNextCommand

.org $3900
TMCacheRpt6:
    TMUploadCacheRepeatMacro 6
    TMProcessNextCommand

.org $3a00
TMCacheRpt5:
    TMUploadCacheRepeatMacro 5
    TMProcessNextCommand

.org $3b00
TMCacheRpt4:
    TMUploadCacheRepeatMacro 4
    TMProcessNextCommand

.org $3c00
TMCacheRpt3:
    TMUploadCacheRepeatMacro 3
    TMProcessNextCommand

.org $3d00
TMCacheRpt2:
    TMUploadCacheRepeatMacro 2
    TMProcessNextCommand

.org $3e00
TMCacheRpt1:
    TMUploadCacheRepeatMacro 1
    TMProcessNextCommand

.org $3f00
TMSkipHalf0:
    TMSkipHalfMacro 0
    TMProcessNextCommand

.org $3f80
TMSkipHalf1:
    TMSkipHalfMacro 1
    TMProcessNextCommand
