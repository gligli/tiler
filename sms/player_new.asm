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

.macro WaitVBlank
    in a, (VDPControl)
@Wait:
    in a, (VDPControl)
    or a  ; update flags
    jp p, @Wait
.endm

.macro SetVDPAddress args addr
        ; Sets the VDP address
    ld a, <addr
    out (VDPControl),a
    ld a, >addr
    out (VDPControl),a
.endm

.macro TilesUploadTileToVRAM args slow
    .repeat TileSize - 1
        outi
        .ifeq slow 1
            ld (hl), 0 ; no effect (writes ROM)
        .endif
    .endr
    outi
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
    ld l, %00000010 ; to get the 10 high bits we need
    .repeat 3
        rra     ; then rotate carry - a - l right three times
        rr l
    .endr
    ld h, a
.endm

.macro TilesUploadUpdateTilePointer
        ; update tile pointer

        ; tile index increment << 5 into de
    ld e, 0
    .repeat 3
        rrca
        rr e
    .endr
    and %00011111
    ld d, a

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
    .ifeq 1 0
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

.macro TMUploadCacheMacro args rpt
        ; compute cache address
    .ifeq rpt 1
        dec a
    .else
        and %00111110
    .endif
    ld h, >TileMapCache
    ld l, a

        ; tilemap item  low byte into b
    ld b, (hl)

    .repeat rpt index idx
            ; low byte of tilemap item
        ld a, b
        out (VDPData), a

            ; high byte of tilemap item
        .ifeq idx 0
            inc l
        .else
            nop ; ensure min 26 cycles between VRAM writes
        .endif
        ld a, (hl)
        ld c, a
        out (VDPData), a
        push bc ; store tilemap item into LocalTileMap
    .endr
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
    LocalTileMap      dsb TileMapSize
    LocalTileMapEnd   .
    TileMapCache      dsb 64 ; must be aligned on 256
    LocalPalette      dsb TilePaletteSize * 2
    FrameCount        dw
    CurFrameIdx       dw
    SPSave            dw
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

init_tab: ; Table must exist within first 1K of ROM
    .db $00, $00, $01, $02

.org $0038
    ei
    reti

.org $0066
    retn

main:
    ld sp, $dff0

    ; Set up VDP registers

    ld hl,VDPInitData
    ld b,VDPInitDataEnd-VDPInitData
    ld c,VDPControl
    otir

    ; Clear VRAM

    ; 1. Set VRAM write address to $0000
    SetVDPAddress $0000 | VRAMWrite
    ; 2. Output 16KB of zeroes
    ld bc, $4000     ; Counter for 16KB of VRAM
-:  xor a
    out (VDPData), a ; Output to VRAM address, which is auto-incremented after each write
    dec bc
    ld a, b
    or c
    jr nz, -

    ; Dummy Sprite table
    SetVDPAddress $600 | VRAMWrite
    ld a, $d0
    out (VDPData), a
    ld a, $06 << 1
    out (VDPControl), a
    ld a, $85
    out (VDPControl), a

    ; Init current frame idx
    xor a
    ld (CurFrameIdx), a
    ld (CurFrameIdx + $01), a

    ; Turn screen on
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

        ; algo expects VBlank state on start
    WaitVBlank

InitPlayer:
    ; Map slot 1 to beginning of video data
    ld a, 1
    ld (MapperSlot1), a

    ; Get first frame data pointers offset
    ld hl, BankSize_

    ; Load frame count
    ld de, FrameCount
    ldi
    ldi

    ; Load first frame bank index
    ld a, (hl)
    inc hl
    inc hl

    ; Load frame data address into hl
    ld e, (hl)
    inc hl
    ld d, (hl)
    inc hl
    set 6, d; Add $4000
    ex de, hl

    ; Map slot 1 & 2 to first frame data
    ld (MapperSlot1), a
    inc a
    ld (MapperSlot2), a

NextFrameLoad:

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

        ; save stack pointer
    ld (SPSave), sp

        ; frame data pointer into sp
    ld sp, hl

        ; start unpacking tile indexes
    TilesUploadUnpack

TilesUploadEnd:

        ;copy tilemap cache into ram
    ld de, TileMapCache
    .repeat 64
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

        ; save stack pointer
    ld (SPSave), sp

        ; sp will be used as a pointer on a reversed local tilemap
    ld sp, LocalTileMapEnd

        ; start unpacking tilemap
    TMProcessNextCommand

TilemapUnpackEnd:

    ; command pointer still into de

p2:

    WaitVBlank

p3:
    ; Tilemap swap

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

    ; Upload local palette to VDP

    SetVDPAddress $0000 | CRAMWrite
    ld hl, LocalPalette
    .repeat TilePaletteSize * 2
        outi
    .endr

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
; Tiles upload fixed sequences (jump tables, LUTs)
;==============================================================

.org $2800
TUUnpackJumpTable:
    .repeat 223
        .db >TUDoStandard
    .endr
    .db >TUDoDirectValue
    .db >TUDoTerminator
    .repeat 256 - 1 - 1 - 223
        .db >TUDoRepeat
    .endr

.org $2900
TUDoDirectValue:
        ; direct value, load tile index from frame data pointer

    pop hl

        ; tile index to tile pointer
    TilesUploadSetTilePointer

    DoTilesUpload 0

.org $2b00
TUDoStandard:
        ; standard case, increment tile index

        ; tile pointer back into hl
    ex de, hl

        ; increment tile pointer of "tile index" tiles
    TilesUploadUpdateTilePointer

    DoTilesUpload 0

.org $2d00
TUDoRepeat:
        ; tile pointer back into hl
    ex de, hl

        ; repeat, value - 223 times
    sub 223
    ld e, a

    DoTilesUpload 1

.org $2e00
TUDoTerminator:
        ; value 224 is terminator

        ; restore mapper slot
    ld a, ixl
    ld (MapperSlot2), a

        ; frame data pointer from sp to hl
    ld hl, 0
    add hl, sp

        ; restore stack pointer
    ld sp, (SPSave)

    jp TilesUploadEnd

;==============================================================
; Tilemap upload fixed sequences (jump tables, LUTs)
;==============================================================

.org $3200
TMCommandsJumpTable:
    ; $00
    .db >TMTerminator
    .repeat 31
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

.org $3300
TMTerminator:
        ; restore stack pointer
    ld sp, (SPSave)

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
    TMUploadCacheMacro 6
    TMProcessNextCommand

.org $3a00
TMCacheRpt5:
    TMUploadCacheMacro 5
    TMProcessNextCommand

.org $3b00
TMCacheRpt4:
    TMUploadCacheMacro 4
    TMProcessNextCommand

.org $3c00
TMCacheRpt3:
    TMUploadCacheMacro 3
    TMProcessNextCommand

.org $3d00
TMCacheRpt2:
    TMUploadCacheMacro 2
    TMProcessNextCommand

.org $3e00
TMCacheRpt1:
    TMUploadCacheMacro 1
    TMProcessNextCommand

.org $3f00
TMSkipHalf0:
    TMSkipHalfMacro 0
    TMProcessNextCommand

.org $3f80
TMSkipHalf1:
    TMSkipHalfMacro 1
    TMProcessNextCommand

;==============================================================
; Data
;==============================================================

; VDP initialisation data
VDPInitData:
.db $04,$80,$00,$81,$f9,$82,$ff,$85,$ff,$86,$ff,$87,$00,$88,$00,$89,$ff,$8a
VDPInitDataEnd:
