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

.macro SetVDPAddress args addr
        ; Sets the VDP address
    ld a, <addr
    out (VDPControl),a
    ld a, >addr
    out (VDPControl),a
.endm

.macro TilesUploadTileToVRAM args slow
    set 5, b ; fixup for outi corrupting b (add TileSize)
    .repeat TileSize - 1
        outi
        .ifeq slow 1
            ld (hl), 0 ; no effect (writes ROM)
        .endif
    .endr
    outi
.endm

.macro TilesUploadPointOnTile
        ; Get a pointer on tile data from tile index

    ld l, e

        ; Upper bits of tile index select a rom bank
    ld a, d
    rra ; incoming carry will always be 0; pushes low bit into carry for use below

    ld (MapperSlot1), a

        ; Lower bits select an offset in that bank
        ; we want the low 9 bits of hl, x32, +$4000, in hl
        ; %-------a bcdefghi
        ;   to
        ; %01abcdef ghi00000
    ld a, l
    ld l, 1 ; to get the 01 high bits we need
    .repeat 3
        rra     ; then rotate carry - a - l right three times
        rr l
    .endr
    ld h, a
.endm

.macro TilesUploadScanlineJumpTable args many
        ; Jump to tile upload code (fast unrolled during VBlank, slower during display)
    in a, (VDPScanline)
    and $fc
    .ifeq many 1
        ld ixl, a
        jp (ix)
    .else
        ld iyl, a
        jp (iy)
    .endif
.endm

.macro TilesUploadUnpack
        ; restore mapper slot
    ld a, (MapperSlot2)
    dec a
    ld (MapperSlot1), a

        ; get next packed data
    pop hl
    ld a, l ; we need the data into a
    dec sp ; we actually needed only one byte unpacked

        ;  use jump table to handle it
    ld h, >TUUnpackJumpTable
    ld h, (hl)
    ld l, 0
    jp (hl)
.endm

.macro TMProcessNextCommand
        ; read next command
    ld a, (de)
    inc de

        ; store raw command into c
    ld c, a
        ; compute jump table offset
    and $c0
    ld l, a
    ld h, >TMCommandsJumpTable

        ; jump to commands table
    jp (hl)
.endm

.macro TMCommandCacheMacro
        ; cache pointer upper byte
    ld b, >TileMapCache

        ; compute cache offset and jump table offset from command using LUTs
    ld l, c
    inc h ; = >TMCommandCacheOffsetLUT
    ld c, (hl)
    inc h ; = >TMCommandCacheRepeatLUT
    ld l, (hl)
    inc h ; = >TMUploadCacheJumpTable

        ; jump to tilemap upload table
    jp (hl)
.endm

.define TMCS 11
.macro TMUploadCacheMacro args idx
        ; /!\ TMCS must stay equal to this macro length

        ; low byte of tilemap item
    ld a, (bc)
    out (VDPData), a
    inc c
    ld h, a

        ; high byte of tilemap item
    ld a, (bc)
    .ifneq idx 3
        dec c
    .endif
    ld l, a
    out (VDPData), a
    push hl ; store tilemap item into LocalTileMap
.endm

.macro TMCommandSkipMacro
        ; command is skip count
    ld a, c
    and $3f

        ; a skip of zero is termination
    jp z, TilemapUnpackEnd

        ; local tilemap pointer
    ld hl, -1
    add hl, sp
        ; loop counter (decremented x2 per loop)
    rlca
    ld b, a
        ; target register
    ld c, VDPData

        ; depending on VRAM "half", will call TMUploadSkipMacro 0 or 1
    jp (ix)
.endm

.macro TMUploadSkipMacro args half
        ; upload "skip count" prev items from local tilemap
-:
        ; tilemap item low byte
    outd
        ; tilemap item high byte
    ld a, (hl)
    .ifeq half 1
        or $01
    .else
        and $fe
    .endif
    dec hl
    out (VDPData), a
    djnz -

        ; sp (reverse local tilemap pointer) must be updated too
    ld sp, hl
    inc sp

    TMProcessNextCommand
.endm

.macro TMCommandRawMacro
        ; compute jump table offset from command repeat bits
    ld l, c
    ld h, >TMCommandRawLUT
    ld l, (hl)
    inc h ; = >TMUploadRawJumpTable

        ; jump to tilemap upload table
    jp (hl)
.endm

.define TMRS 9
.macro TMUploadRawMacro args idx
        ; /!\ TMRS must stay equal to this macro length

        ; low byte of tilemap item
    ld a, (de)
    ld b, a
    out (VDPData), a
    push bc ; store tilemap item into LocalTileMap

        ; high byte of tilemap item
    ld a, c
    out (VDPData), a
    .ifneq idx 3
        nop
    .else
        inc de
    .endif
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
    ld a, %1000000
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

    ; Enable ints
    ei

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
    jr z, +

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
    jr z, NoFramePalette

    .repeat TilePaletteSize * 2
        ldi
    .endr

NoFramePalette:

p1: ; Unpack tiles indexes and copy corresponding tiles to VRAM

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

        ; Prepare jump tables offsets
    ld ixh, >TUScanlineManyJumpTable
    ld iyh, >TUScanlineOneJumpTable

        ; Prepare VRAM write register
    ld c, VDPData

        ; save stack pointer
    ld (SPSave), sp

        ; frame data pointer into sp
    ld sp, hl

        ; start unpacking tile indexes
    TilesUploadUnpack

TilesUploadManySlow:
    TilesUploadTileToVRAM 1
    djnz TUMSRedo
    TilesUploadUnpack
TUMSRedo:
    TilesUploadScanlineJumpTable 1

TilesUploadManyFast:
    TilesUploadTileToVRAM 0
    djnz TUMFRedo
    TilesUploadUnpack
TUMFRedo:
    TilesUploadScanlineJumpTable 1

TilesUploadOneSlow:
    TilesUploadTileToVRAM 1
    TilesUploadUnpack

TilesUploadOneFast:
    TilesUploadTileToVRAM 0
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
    ; we want to move bit 0 to bit 5
    rrca
    ld ixl, a ; VRAM "half" in ixl bit 7
    rrca
    rrca
    or VRAMWrite >> 8
    out (VDPControl), a

        ; prepare (ix) jump table
    ld ixh, >TMUploadSkipJumpTable

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

        ; restore stack pointer
    ld sp, (SPSave)

p2: ; Wait vblank
-:  in a, (VDPScanline)
    cp 193
    jr c, -

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

.org $3200
TUDoDirectValue:
        ; direct value, load tile index from tile index pointer

    pop de

    TilesUploadPointOnTile
    TilesUploadScanlineJumpTable 0

.org $3300
TUDoStandard:
        ; standard case, increment tile index

    add a, e
    ld e, a
    adc a, d
    sub e
    ld d, a

    TilesUploadPointOnTile
    TilesUploadScanlineJumpTable 0

.org $3400
TUDoTerminator:
        ; value 224 is terminator

        ; frame data pointer from sp to hl
    ld hl, 0
    add hl, sp

        ; restore stack pointer
    ld sp, (SPSave)

    jp TilesUploadEnd

.org $3500
TUDoRepeat:
        ; repeat, value - 223 times
    sub 223
    ld b, a

        ; increment tile index once for TilesUploadPointOnTile
    inc de

    TilesUploadPointOnTile

        ; remainder of repeat count
    ld a, b
    dec a
        ; add to tile index
    add a, e
    ld e, a
    adc a, d
    sub e
    ld d, a

    TilesUploadScanlineJumpTable 1

.org $3600
TUUnpackJumpTable:
    .db >TUDoDirectValue
    .repeat 224 - 1
        .db >TUDoStandard
    .endr
    .db >TUDoTerminator
    .repeat 256 - 1 - 224
        .db >TUDoRepeat
    .endr

.org $3700
TUScanlineManyJumpTable:
    .repeat 192 / 4
        jp TilesUploadManySlow
        nop
    .endr
    .repeat (252 - 192) / 4
        jp TilesUploadManyFast
        nop
    .endr
    jp TilesUploadManySlow

.org $3800
TUScanlineOneJumpTable:
    .repeat 192 / 4
        jp TilesUploadOneSlow
        nop
    .endr
    .repeat (252 - 192) / 4
        jp TilesUploadOneFast
        nop
    .endr
    jp TilesUploadOneSlow

;==============================================================
; Tilemap upload fixed sequences (jump tables, LUTs)
;==============================================================

.org $3900
TMCommandsJumpTable:
    TMCommandCacheMacro
     
.org $3940
    TMCommandCacheMacro

.org $3980
    TMCommandSkipMacro

.org $39c0
    TMCommandRawMacro

.org $3a00
TMCommandCacheOffsetLUT:
    .repeat 4
        .repeat 32 index idx
            .db idx * 2
        .endr
    .endr

.org $3b00
TMCommandCacheRepeatLUT:
    .dsb 32, 0
    .dsb 32, TMCS
    .dsb 32, TMCS*2
    .dsb 32, TMCS*3

.org $3c00
TMUploadCacheJumpTable:
    .repeat 4 index idx
        TMUploadCacheMacro idx
    .endr
    TMProcessNextCommand

.org $3d00
TMUploadSkipJumpTable:
    TMUploadSkipMacro 0
    TMProcessNextCommand

.org $3d80
    TMUploadSkipMacro 1
    TMProcessNextCommand

.org $3e00
TMCommandRawLUT:
    .dsb 208, 0
    .dsb 16, TMRS
    .dsb 16, TMRS*2
    .dsb 16, TMRS*3

.org $3f00
TMUploadRawJumpTable:
    .repeat 4 index idx
        TMUploadRawMacro idx
    .endr
    TMProcessNextCommand

;==============================================================
; Data
;==============================================================

; VDP initialisation data
VDPInitData:
.db $04,$80,$00,$81,$f9,$82,$ff,$85,$ff,$86,$ff,$87,$00,$88,$00,$89,$ff,$8a
VDPInitDataEnd:
