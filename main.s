; main.s
;
; Andrew Dawson <dawso.andrew@gmail.com>
;
; This is the main file for the game.

; iNES header...................................................................

.segment "HEADER"

INES_MAPPER = 0 ; 0 = NROM
INES_MIRROR = 1 ; 0 = horizontal mirroring, 1 = vertical mirroring
INES_SRAM   = 0 ; 1 = battery backed SRAM at $6000-7FFF

.byte 'N', 'E', 'S', $1A ; ID
.byte $02 ; 16k PRG chunk count
.byte $01 ; 8k CHR chunk count
.byte INES_MIRROR | (INES_SRAM << 1) | ((INES_MAPPER & $f) << 4)
.byte (INES_MAPPER & %11110000)
.byte $0, $0, $0, $0, $0, $0, $0, $0 ; padding

; CHR ROM.......................................................................

.segment "TILES"
.incbin "background.chr"
.incbin "sprite.chr"

; Music.........................................................................

.segment "RODATA"

song_silver_surfer:
.include "song_silver_surfer_ca65.s"

; vectors placed at top 6 bytes of memory area..................................

.segment "VECTORS"
.word nmi
.word reset
.word irq

; Famistudio....................................................................

.segment "CODE"

FAMISTUDIO_CFG_EXTERNAL       = 1
FAMISTUDIO_CFG_DPCM_SUPPORT   = 1
FAMISTUDIO_CFG_SFX_SUPPORT    = 1 
FAMISTUDIO_CFG_SFX_STREAMS    = 2
FAMISTUDIO_CFG_EQUALIZER      = 1
FAMISTUDIO_USE_VOLUME_TRACK   = 1
FAMISTUDIO_USE_PITCH_TRACK    = 1
FAMISTUDIO_USE_SLIDE_NOTES    = 1
FAMISTUDIO_USE_VIBRATO        = 1
FAMISTUDIO_USE_ARPEGGIO       = 1
FAMISTUDIO_CFG_SMOOTH_VIBRATO = 1
FAMISTUDIO_DPCM_OFF           = $e000

.define FAMISTUDIO_CA65_ZP_SEGMENT   ZEROPAGE
.define FAMISTUDIO_CA65_RAM_SEGMENT  BSS
.define FAMISTUDIO_CA65_CODE_SEGMENT CODE

.include "famistudio_ca65.s"

; reset routine.................................................................

.include "nes.inc"

.segment "CODE"
reset:
	sei       ; mask interrupts
	lda #0
	sta PPU_CONTROL1 ; disable NMI
	sta PPU_CONTROL2 ; disable rendering
	sta APU_CONTROL ; disable APU sound
	sta APU_DMC_CONTROL ; disable DMC IRQ
	lda #$40
	sta APU_FRAME_COUNTER ; disable APU IRQ
	cld       ; disable decimal mode
	ldx #$FF
	txs       ; initialize stack
	; wait for first vblank
	bit PPU_STATUS
	:
		bit PPU_STATUS
		bpl :-
	; clear all RAM to 0
	lda #0
	ldx #0
	:
		sta $0000, X
		sta $0100, X
		sta $0200, X
		sta $0300, X
		sta $0400, X
		sta $0500, X
		sta $0600, X
		sta $0700, X
		inx
		bne :-
	; place all sprites offscreen at Y=255
	lda #255
	ldx #0
	:
		sta oam, X
		inx
		inx
		inx
		inx
		bne :-
	; wait for second vblank
	:
		bit PPU_STATUS
		bpl :-
	; NES is initialized, ready to begin!
	; set up PPU and jump to our main program
	lda #(SPR_1000 | NMI_ENABLE)
	sta PPU_CONTROL1
	jmp main

; NMI routine...................................................................

.segment "ZEROPAGE"
nmi_lock:       .res 1 ; prevents NMI re-entry
nmi_count:      .res 1 ; is incremented every NMI
nmi_ready:      .res 1 ; set to 1 to push a PPU frame update, 2 to turn rendering off next NMI
nmt_update_len: .res 1 ; number of bytes in nmt_update buffer
scroll_x:       .res 1 ; x scroll position
scroll_y:       .res 1 ; y scroll position
scroll_nmt:     .res 1 ; nametable select (0-3 = $2000,$2400,$2800,$2C00)
temp:           .res 1 ; temporary variable

.segment "BSS"
nmt_update: .res 256 ; nametable update entry buffer for PPU update
palette:    .res 32  ; palette buffer for PPU update

.segment "OAM"
oam: .res 256        ; sprite OAM data to be uploaded by DMA

.segment "CODE"
nmi:
	; save registers
	pha
	txa
	pha
	tya
	pha
	; prevent NMI re-entry
	lda nmi_lock
	beq :+
		jmp @nmi_end
	:
	lda #1
	sta nmi_lock
	; increment frame counter
	inc nmi_count
	;
	lda nmi_ready
	bne :+ ; nmi_ready == 0 not ready to update PPU
		jmp @ppu_update_end
	:
	cmp #2 ; nmi_ready == 2 turns rendering off
	bne :+
		lda #%00000000
		sta PPU_CONTROL2
		ldx #0
		stx nmi_ready
		jmp @ppu_update_end
	:
	; sprite OAM DMA
	ldx #0
	stx SPR_RAM_ADDRESS
	lda #>oam
	sta SPRITE_DMA
	; palettes
	lda #(SPR_1000 | NMI_ENABLE)
	sta PPU_CONTROL1 ; set horizontal nametable increment
	lda PPU_STATUS
	lda #$3F
	sta VRAM_ADDRESS2
	stx VRAM_ADDRESS2 ; set PPU address to $3F00
	ldx #0
	:
		lda palette, X
		sta VRAM_IO
		inx
		cpx #32
		bcc :-
	; nametable update
	ldx #0
	cpx nmt_update_len
	bcs @scroll
	@nmt_update_loop:
		lda nmt_update, X
		sta VRAM_ADDRESS2
		inx
		lda nmt_update, X
		sta VRAM_ADDRESS2
		inx
		lda nmt_update, X
		sta VRAM_IO
		inx
		cpx nmt_update_len
		bcc @nmt_update_loop
	lda #0
	sta nmt_update_len
@scroll:
	lda scroll_nmt
	and #%00000011 ; keep only lowest 2 bits to prevent error
	ora #(SPR_1000 | NMI_ENABLE)
	sta PPU_CONTROL1
	lda scroll_x
	sta VRAM_ADDRESS1
	lda scroll_y
	sta VRAM_ADDRESS1
	; enable rendering
	lda #(BG_ENABLE | SPR_ENABLE | BG_SHOW_LEFT8 | SPR_SHOW_LEFT8)
	sta PPU_CONTROL2
	; flag PPU update complete
	ldx #0
	stx nmi_ready
@ppu_update_end:
	jsr famistudio_update
	; unlock re-entry flag
	lda #0
	sta nmi_lock
@nmi_end:
	; restore registers and return
	pla
	tay
	pla
	tax
	pla
	rti

; IRQ...........................................................................

.segment "CODE"
irq:
	rti

; Drawing utilities.............................................................

.segment "CODE"

; ppu_update: waits until next NMI, turns rendering on (if not already), uploads OAM, palette, and nametable update to PPU
ppu_update:
	lda #1
	sta nmi_ready
	:
		lda nmi_ready
		bne :-
	rts

; ppu_skip: waits until next NMI, does not update PPU
ppu_skip:
	lda nmi_count
	:
		cmp nmi_count
		beq :-
	rts

; ppu_off: waits until next NMI, turns rendering off (now safe to write PPU directly via $2007)
ppu_off:
	lda #2
	sta nmi_ready
	:
		lda nmi_ready
		bne :-
	rts

; ppu_address_tile: use with rendering off, sets memory address to tile at X/Y, ready for a $2007 write
;   Y =  0- 31 nametable $2000
;   Y = 32- 63 nametable $2400
;   Y = 64- 95 nametable $2800
;   Y = 96-127 nametable $2C00
ppu_address_tile:
	lda PPU_STATUS ; reset latch
	tya
	lsr
	lsr
	lsr
	ora #$20 ; high bits of Y + $20
	sta VRAM_ADDRESS2
	tya
	asl
	asl
	asl
	asl
	asl
	sta temp
	txa
	ora temp
	sta VRAM_ADDRESS2 ; low bits of Y + X
	rts

; ppu_update_tile: can be used with rendering on, sets the tile at X/Y to tile A next time you call ppu_update
ppu_update_tile:
	pha ; temporarily store A on stack
	txa
	pha ; temporarily store X on stack
	ldx nmt_update_len
	tya
	lsr
	lsr
	lsr
	ora #$20 ; high bits of Y + $20
	sta nmt_update, X
	inx
	tya
	asl
	asl
	asl
	asl
	asl
	sta temp
	pla ; recover X value (but put in A)
	ora temp
	sta nmt_update, X
	inx
	pla ; recover A value (tile)
	sta nmt_update, X
	inx
	stx nmt_update_len
	rts

; ppu_update_byte: like ppu_update_tile, but X/Y makes the high/low bytes of the PPU address to write
;    this may be useful for updating attribute tiles
ppu_update_byte:
	pha ; temporarily store A on stack
	tya
	pha ; temporarily store Y on stack
	ldy nmt_update_len
	txa
	sta nmt_update, Y
	iny
	pla ; recover Y value (but put in Y)
	sta nmt_update, Y
	iny
	pla ; recover A value (byte)
	sta nmt_update, Y
	iny
	sty nmt_update_len
	rts

; Gamepad.......................................................................

.segment "ZEROPAGE"
gamepad: .res 1

.segment "CODE"
; gamepad_poll: this reads the gamepad state into the variable labelled "gamepad"
;   This only reads the first gamepad, and also if DPCM samples are played they can
;   conflict with gamepad reading, which may give incorrect results.
gamepad_poll:
	; strobe the gamepad to latch current button state
	lda #1
	sta PAD1_STATE
	lda #0
	sta PAD1_STATE
	; read 8 bytes from the interface at $4016
	ldx #8
	:
		pha
		lda PAD1_STATE
		; combine low two bits and store in carry bit
		and #%00000011
		cmp #%00000001
		pla
		; rotate carry into gamepad variable
		ror
		dex
		bne :-
	sta gamepad
	rts

; Main..........................................................................

.segment "RODATA"
example_palette:
.byte $0F,$15,$26,$37 ; bg0 purple/pink
.byte $0F,$09,$19,$29 ; bg1 green
.byte $0F,$01,$11,$21 ; bg2 blue
.byte $0F,$00,$10,$30 ; bg3 greyscale
.byte $0F,$18,$28,$38 ; sp0 yellow
.byte $0F,$14,$24,$34 ; sp1 purple
.byte $0F,$1B,$2B,$3B ; sp2 teal
.byte $0F,$12,$22,$32 ; sp3 marine

.segment "ZEROPAGE"
cursor_x: .res 1
cursor_y: .res 1
temp_x:   .res 1
temp_y:   .res 1

.segment "CODE"
main:
	; setup 
	ldx #0
	:
		lda example_palette, X
		sta palette, X
		inx
		cpx #32
		bcc :-
	jsr setup_background
	; center the cursor
	lda #128
	sta cursor_x
	lda #120
	sta cursor_y
	; show the screen
	jsr draw_cursor
	jsr ppu_update
	; play a song
	lda #1 ; NTSC
	ldx #.lobyte(music_data_silver_surfer_c_stephen_ruddy)
    ldy #.hibyte(music_data_silver_surfer_c_stephen_ruddy)
    jsr famistudio_init
    lda #0 ; song index
    jsr famistudio_music_play
	; main loop
@loop:
	; read gamepad
	jsr gamepad_poll
	; respond to gamepad state
	lda gamepad
	and #PAD_START
	beq :+
		jsr push_start
		jmp @draw ; start trumps everything, don't check other buttons
	:
	jsr release_start ; releasing start restores scroll
	lda gamepad
	and #PAD_U
	beq :+
		jsr push_u
	:
	lda gamepad
	and #PAD_D
	beq :+
		jsr push_d
	:
	lda gamepad
	and #PAD_L
	beq :+
		jsr push_l
	:
	lda gamepad
	and #PAD_R
	beq :+
		jsr push_r
	:
	lda gamepad
	and #PAD_SELECT
	beq :+
		jsr push_select
	:
	lda gamepad
	and #PAD_B
	beq :+
		jsr push_b
	:
	lda gamepad
	and #PAD_A
	beq :+
		jsr push_a
	:
@draw:
	; draw everything and finish the frame
	jsr draw_cursor
	jsr ppu_update
	; keep doing this forever!
	jmp @loop

push_u:
	dec cursor_y
	; Y wraps at 240
	lda cursor_y
	cmp #240
	bcc :+
		lda #239
		sta cursor_y
	:
	rts

push_d:
	inc cursor_y
	; Y wraps at 240
	lda cursor_y
	cmp #240
	bcc :+
		lda #0
		sta cursor_y
	:
	rts

push_l:
	dec cursor_x
	rts

push_r:
	inc cursor_x
	rts

push_select:
	; turn off rendering so we can manually update entire nametable
	jsr ppu_off
	jsr setup_background
	; wait for user to release select before continuing
	:
		jsr gamepad_poll
		lda gamepad
		and #PAD_SELECT
		bne :-
	rts

push_start:
	inc scroll_x
	inc scroll_y
	; Y wraps at 240
	lda scroll_y
	cmp #240
	bcc :+
		lda #0
		sta scroll_y
	:
	; when X rolls over, toggle the high bit of nametable select
	lda scroll_x
	bne :+
		lda scroll_nmt
		eor #$01
		sta scroll_nmt
	:
	rts

release_start:
	lda #0
	sta scroll_x
	sta scroll_y
	sta scroll_nmt
	rts

push_b:
	jsr snap_cursor
	lda cursor_x
	lsr
	lsr
	lsr
	tax ; X = cursor_x / 8
	lda cursor_y
	lsr
	lsr
	lsr
	tay ; Y = cursor_y / 8
	lda #4
	jsr ppu_update_tile ; puts tile 4 at X/Y
	rts

push_a:
	jsr snap_cursor
	lda cursor_x
	lsr
	lsr
	lsr
	sta temp_x ; cursor_x / 8
	lda cursor_y
	lsr
	lsr
	lsr
	sta temp_y ; cursor_y / 8
	; draw a ring of 8 tiles around the cursor
	dec temp_x ; x-1
	dec temp_y ; y-1
	ldx temp_x
	ldy temp_y
	lda #5
	jsr ppu_update_tile
	inc temp_x ; x
	ldx temp_x
	ldy temp_y
	lda #6
	jsr ppu_update_tile
	inc temp_x ; x+1
	ldx temp_x
	ldy temp_y
	lda #5
	jsr ppu_update_tile
	dec temp_x
	dec temp_x ; x-1
	inc temp_y ; y
	ldx temp_x
	ldy temp_y
	lda #6
	jsr ppu_update_tile
	inc temp_x
	inc temp_x ; x+1
	ldx temp_x
	ldy temp_y
	lda #6
	jsr ppu_update_tile
	dec temp_x
	dec temp_x ; x-1
	inc temp_y ; y+1
	ldx temp_x
	ldy temp_y
	lda #5
	jsr ppu_update_tile
	inc temp_x ; x
	ldx temp_x
	ldy temp_y
	lda #6
	jsr ppu_update_tile
	inc temp_x ; x+1
	ldx temp_x
	ldy temp_y
	lda #5
	jsr ppu_update_tile
	rts

; snap_cursor: snap cursor to nearest tile
snap_cursor:
	lda cursor_x
	clc
	adc #4
	and #$F8
	sta cursor_x
	lda cursor_y
	clc
	adc #4
	and #$F8
	sta cursor_y
	; Y wraps at 240
	cmp #240
	bcc :+
		lda #0
		sta cursor_y
	:
	rts

draw_cursor:
	; four sprites centred around the currently selected tile
	; y position (note, needs to be one line higher than sprite's appearance)
	lda cursor_y
	sec
	sbc #5 ; Y-5
	sta oam+(0*4)+0
	sta oam+(1*4)+0
	lda cursor_y
	clc
	adc #3 ; Y+3
	sta oam+(2*4)+0
	sta oam+(3*4)+0
	; tile
	lda #1
	sta oam+(0*4)+1
	sta oam+(1*4)+1
	sta oam+(2*4)+1
	sta oam+(3*4)+1
	; attributes
	lda #%00000000 ; no flip
	sta oam+(0*4)+2
	lda #%01000000 ; horizontal flip
	sta oam+(1*4)+2
	lda #%10000000 ; vertical flip
	sta oam+(2*4)+2
	lda #%11000000 ; both flip
	sta oam+(3*4)+2
	; x position
	lda cursor_x
	sec
	sbc #4 ; X-4
	sta oam+(0*4)+3
	sta oam+(2*4)+3
	lda cursor_x
	clc
	adc #4 ; X+4
	sta oam+(1*4)+3
	sta oam+(3*4)+3
	rts

setup_background:
	; first nametable, start by clearing to empty
	lda PPU_STATUS ; reset latch
	lda #$20
	sta VRAM_ADDRESS2
	lda #$00
	sta VRAM_ADDRESS2
	; empty nametable
	lda #0
	ldy #30 ; 30 rows
	:
		ldx #32 ; 32 columns
		:
			sta VRAM_IO
			dex
			bne :-
		dey
		bne :--
	; set all attributes to 0
	ldx #64 ; 64 bytes
	:
		sta VRAM_IO
		dex
		bne :-
	; fill in an area in the middle with 1/2 checkerboard
	lda #1
	ldy #8 ; start at row 8
	:
		pha ; temporarily store A, it will be clobbered by ppu_address_tile routine
		ldx #8 ; start at column 8
		jsr ppu_address_tile
		pla ; recover A
		; write a line of checkerboard
		ldx #8
		:
			sta VRAM_IO
			eor #$3
			inx
			cpx #(32-8)
			bcc :-
		eor #$3
		iny
		cpy #(30-8)
		bcc :--
	; second nametable, fill with simple pattern
	lda #$24
	sta VRAM_ADDRESS2
	lda #$00
	sta VRAM_ADDRESS2
	lda #$00
	ldy #30
	:
		ldx #32
		:
			sta VRAM_IO
			clc
			adc #1
			and #3
			dex
			bne :-
		clc
		adc #1
		and #3
		dey
		bne :--
	; 4 stripes of attribute
	lda #0
	ldy #4
	:
		ldx #16
		:
			sta VRAM_IO
			dex
			bne :-
		clc
		adc #%01010101
		dey
		bne :--
	rts
