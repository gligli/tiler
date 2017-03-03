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

;==============================================================
; Program defines
;==============================================================

.define DblBufTileOffset 49 * TileSize

.macro SetVDPAddress
; Sets the VDP address
; Parameters: de = address
    ld a,e
    out (VDPControl),a
    ld a,d
    out (VDPControl),a
.endm

;==============================================================
; RAM variables
;==============================================================

.enum $c000 export
    TileMapCache      dsb 64 ; must stay first
    LocalPalette      dsb TilePaletteSize * 2
    CurFrameIdx       dw
    CurVBlankIdx      dw
.ende

.bank 0 slot 0
.org $0000
.section "Boot section" force
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
.ends

.org $0038
.section "VDP int handler" force
    ex af, af' ; 4

    ; VDP int ack
    in a, (VDPControl) ; 11

    ; CurVBlankIdx update
    inc (iy + 0) ; 26

    ex af, af' ; 4

    ei
    reti
.ends

.org $0066
.section "Pause button handler" force
    retn
.ends

main:
    ld sp, $dff0

    ; Set up VDP registers

    ld hl,VDPInitData
    ld b,VDPInitDataEnd-VDPInitData
    ld c,VDPControl
    otir

    ; Clear VRAM

    ; 1. Set VRAM write address to $0000
    ld de, $0000 | VRAMWrite
    SetVDPAddress
    ; 2. Output 16KB of zeroes
    ld bc, $4000     ; Counter for 16KB of VRAM
-:  xor a
    out (VDPData), a ; Output to VRAM address, which is auto-incremented after each write
    dec bc
    ld a, b
    or c
    jr nz, -

    ; Dummy Sprite table
    ld de, $600 | VRAMWrite
    SetVDPAddress
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
    ld (CurVBlankIdx), a
    ld (CurVBlankIdx + $01), a

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

        ; will be incremented by VBlank int
    ld iy, CurVBlankIdx

    ; Enable ints
    ei

InitPlayer:

    ; Get first frame data pointers offset
    ld hl, VideoDataIndex + $02

    ; Map slot 1 & 2 to current frame data
    ld a, (hl)
    inc hl
    inc hl
    ld (MapperSlot1), a
    inc a
    ld (MapperSlot2), a

    ; Load frame data address into hl
    ld e, (hl)
    inc hl
    ld d, (hl)
    inc hl
    set 6, d; Add $4000
    ex de, hl

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

        ; Save current mapper slot
    ld a, (MapperSlot1)
    ld ixl, a

    jp TilesUploadUnpackStart

TilesUploadUnpackAgain

        ; we need tiles indexes pointer into hl and tile index into de
    pop hl

TilesUploadUnpackStart:
        ; Restore mapper slot
    ld a, ixl
    ld (MapperSlot1), a

    ld a, (hl)
    inc hl
    cp 0
    jp nz, +

        ; direct value, load tile index from tile index pointer
    ld e, (hl)
    inc hl
    ld d, (hl)
    inc hl
       ; count = 1
    ld c, 1
    jp ++

+:
    cp 224
    jp c, +

        ; value 224 is terminator
    jp z, TilesUploadEnd

        ; repeat, value - 223 times
    sub 223
    ld c, a
    inc de
    jp ++

+:
        ; standard case, increment tile index
    add a, e
    ld e, a
    adc a, d
    sub e
    ld d, a
       ; count = 1
    ld c, 1

++:
    push hl
    jp TilesUploadLoopStart

TilesUploadLoopAgain:
    ld c, a ; restore c
    dec c
    jr z, TilesUploadUnpackAgain

        ; to next tile
    inc de

TilesUploadLoopStart:

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

        ; Upload tile to VRAM (fast unrolled during VBlank, slower during display)

    in a, (VDPScanline)
    ld a, c ; save c
    ld c, VDPData
    cp 192
    jr c, TilesUploadSlow
    cp 253
    jr nc, TilesUploadSlow

TilesUploadFast:
    .repeat TileSize
        outi
    .endr
    jp TilesUploadLoopAgain

TilesUploadSlow:
    .repeat TileSize
        outi
        ld (hl), 0 ; no effect (writes ROM)
    .endr
    jp TilesUploadLoopAgain

TilesUploadEnd:

        ;copy tilemap cache into ram
    ld de, TileMapCache
    .repeat 64
        ldi
    .endr

        ; save command pointer into de
    ex de, hl

        ; Set tilemap VRAM pointer (stored into bc)
    xor a
    ld ixl, a
    out (VDPControl), a
    ld a, (CurFrameIdx)
    ; we want to move bit 0 to bit 5
    rrca
    rrca
    rrca
    and %00100000
    or VRAMWrite >> 8
    ld ixh, a
    out (VDPControl), a

        ; tilemap unpack code is at end of source
tm0:
    call TilemapUnpackStart
tm1:
        ; restore command pointer into hl
    ex de, hl

p2: ; Wait 4 VBlanks per frame (12.5 PAL fps)
-:  halt
    ld a, (CurVBlankIdx)
    and $03
    jr nz, -

p3:
    push hl

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

    ld de, $0000 | CRAMWrite
    SetVDPAddress
    ld hl, LocalPalette
    .repeat TilePaletteSize * 2
        outi
    .endr

p4: ; Advance to next frame
    ld bc, (CurFrameIdx)
    inc bc
    ld hl, (VideoDataIndex)
    sbc hl, bc
    jr nz, +

    ; If we're past last frame, rewind to first frame
    ld bc, 0
    ld (CurFrameIdx), bc
    pop hl
    jp InitPlayer

+:
    ld (CurFrameIdx), bc
    pop hl
    jp NextFrameLoad



.macro TMCommandCacheMacro
;jp TilemapUnpackStart

        ; compute cache offset lower byte
    ld a, b
    rlca
    and $3e
    ld c, a

        ; compute jump table offset from command repeat bits using LUT
    ld l, b
    inc h ; = >TMUploadLUT
    ld l, (hl)
    inc h ; = >TMUploadCacheJumpTable

        ; cache pointer upper byte
    ld b, >TileMapCache

        ; jump to tilemap upload table
    jp (hl)
.endm

.macro TMCommandSkipMacro
        ; command is skip count
    ld a, b
    and $3f

        ; a skip of zero is termination
    ret z

;jp TilemapUnpackStart
        ; double skip count and add it to local VRAM pointer
    rlca
    ld b, 0
    ld c, a
    add ix, bc

        ; update the VRAM pointer
    ld a, ixl
    out (VDPControl), a
    ld a, ixh
    out (VDPControl), a

    jp TilemapUnpackStart
.endm

.macro TMCommandRawMacro

        ; compute jump table offset from command repeat bits
    ld l, b
    inc h ; = >TMUploadLUT
    ld l, (hl)
    ld h, >TMUploadRawJumpTable

        ; jump to tilemap upload table
    jp (hl)
.endm

.macro TMUploadCacheMacro args end
        ; /!\ TMS must stay equal to this macro length

        ; low byte of tilemap item
    ld a, (bc)
    out (VDPData), a
        ; update local VRAM pointer
    inc ix
    inc c
        ; high byte of tilemap item
    ld a, (bc)
    out (VDPData), a
    inc ix
    .ifeq end 0
        dec c
    .endif
.endm

.macro TMUploadRawMacro args end
        ; /!\ TMS must stay equal to this macro length

        ; low byte of tilemap item
    ld a, (de)
    out (VDPData), a
        ; update local VRAM pointer
    inc ix
        ; encure 26 cycles between VRAM writes
    nop
        ; high byte of tilemap item
    ld a, b
    out (VDPData), a
    inc ix
    .ifeq end 0
        nop
    .else
        inc de
    .endif
.endm

.org $3c00
TMCommandsJumpTable:
    TMCommandCacheMacro

.org $3c40
    TMCommandCacheMacro

.org $3c80
    TMCommandSkipMacro

.org $3cc0
    TMCommandRawMacro

.org $3d00
    .define TMS 12
TMUploadLUT:
    .db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    .db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    .db TMS, TMS, TMS, TMS, TMS, TMS, TMS, TMS, TMS, TMS, TMS, TMS, TMS, TMS, TMS, TMS,
    .db TMS, TMS, TMS, TMS, TMS, TMS, TMS, TMS, TMS, TMS, TMS, TMS, TMS, TMS, TMS, TMS,
    .db TMS*2, TMS*2, TMS*2, TMS*2, TMS*2, TMS*2, TMS*2, TMS*2, TMS*2, TMS*2, TMS*2, TMS*2, TMS*2, TMS*2, TMS*2, TMS*2,
    .db TMS*2, TMS*2, TMS*2, TMS*2, TMS*2, TMS*2, TMS*2, TMS*2, TMS*2, TMS*2, TMS*2, TMS*2, TMS*2, TMS*2, TMS*2, TMS*2,
    .db TMS*3, TMS*3, TMS*3, TMS*3, TMS*3, TMS*3, TMS*3, TMS*3, TMS*3, TMS*3, TMS*3, TMS*3, TMS*3, TMS*3, TMS*3, TMS*3,
    .db TMS*3, TMS*3, TMS*3, TMS*3, TMS*3, TMS*3, TMS*3, TMS*3, TMS*3, TMS*3, TMS*3, TMS*3, TMS*3, TMS*3, TMS*3, TMS*3,
    .db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    .db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    .db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    .db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    .db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    .db TMS, TMS, TMS, TMS, TMS, TMS, TMS, TMS, TMS, TMS,TMS, TMS, TMS, TMS, TMS, TMS,
    .db TMS*2, TMS*2, TMS*2, TMS*2, TMS*2, TMS*2, TMS*2, TMS*2, TMS*2, TMS*2, TMS*2, TMS*2, TMS*2, TMS*2, TMS*2, TMS*2,
    .db TMS*3, TMS*3, TMS*3, TMS*3, TMS*3, TMS*3, TMS*3, TMS*3, TMS*3, TMS*3, TMS*3, TMS*3, TMS*3, TMS*3, TMS*3, TMS*3,

.org $3e00
TMUploadCacheJumpTable:
    TMUploadCacheMacro 0
    TMUploadCacheMacro 0
    TMUploadCacheMacro 0
    TMUploadCacheMacro 1

TilemapUnpackStart:
     ; read next command
    ld a, (de)
    inc de

        ; store raw command into b
    ld b, a
        ; compute jump table offset
    and $c0
    ld l, a
    ld h, >TMCommandsJumpTable

        ; jump to commands table
    jp (hl)

.org $3f00
TMUploadRawJumpTable:
    TMUploadRawMacro 0
    TMUploadRawMacro 0
    TMUploadRawMacro 0
    TMUploadRawMacro 1

    jp TilemapUnpackStart

.section "Data" free

; VDP initialisation data
VDPInitData:
.db $04,$80,$00,$81,$f9,$82,$ff,$85,$ff,$86,$ff,$87,$00,$88,$00,$89,$ff,$8a
VDPInitDataEnd:

VideoDataIndex:
.incbin "tiled/index.bin"

.ends