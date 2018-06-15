  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
  .inesmir 1   ; background mirroring
  

;;;;;;;;;;;;;;;

;; DECLARE SOME VARIABLES HERE
  .rsset $0000  ;;start variables at ram location 0

playerStatus .rs 1 ;bit 1: 0 is facing right, 1 is facing left
				   ;bit 0:0 is normal, 1 is up
buttons .rs 1
flipCooldown .rs 1

colBuffer .rs 30 ;Buffer filled with tile data when drawing metatiles

backgroundPointer .rs 2 ;Pointer to background to be drawn

metatilePointer .rs 2
metatileRepeat .rs 1 ;Number of repeat metatiles to draw
metatilesDrawn .rs 1 ;Number of metatiles drawn (for tracking when drawing repeat metatiles)

xData .rs 1 ;Place to store content of registers
yData .rs 1

playerX .rs 1 ;Player's current X and Y coordinates
playerY .rs 1

scroll .rs 1
nameTableAddress .rs 2
nameTable .rs 1

pointer1 .rs 2
level .rs 1 ;Level to load
colProgress .rs 1;Progress in column being loaded

;; DECLARE SOME CONSTANTS HERE
PPUCTRL = $2000
PPUMASK = $2001
PPUSTATUS = $2002
OAMADDR = $2003
PPUSCROLL = $2005
PPUADDR = $2006
PPUDATA = $2007
OAMDMA = $4014

PLAYERSPRITES = $0204 ;Location of first byte of the four sprites that constitute the player
;;;;;;;;;;;;
    
  .bank 0
  .org $C000 
RESET:
  SEI          ; disable IRQs
  CLD          ; disable decimal mode
  LDX #$40
  STX $4017    ; disable APU frame IRQ
  LDX #$FF
  TXS          ; Set up stack
  INX          ; now X = 0
  STX PPUCTRL   ; disable NMI
  STX PPUMASK   ; disable rendering
  STX $4010    ; disable DMC IRQs

vblankwait1:       ; First wait for vblank to make sure PPU is ready
  BIT $2002
  BPL vblankwait1

clrmem:
  LDA #$00
  STA $0000, x
  STA $0100, x
  STA $0300, x
  STA $0400, x
  STA $0500, x
  STA $0600, x
  STA $0700, x
  LDA #$FE
  STA $0200, x
  INX
  BNE clrmem
   
vblankwait2:      ; Second wait for vblank, PPU is ready after this
  BIT PPUSTATUS
  BPL vblankwait2


LoadPalettes:
  LDA PPUSTATUS           ; read PPU status to reset the high/low latch
  LDA #$3F
  STA PPUADDR             ; write the high byte of $3F00 address
  LDA #$00
  STA PPUADDR          ; write the low byte of $3F00 address
  LDX #$00              ; start out at 0
LoadPalettesLoop:
  LDA palette, x        ; load data from address (palette + the value in x)
  STA PPUDATA          ; write to PPU
  INX                   ; X = X + 1
  CPX #$20              ; Compare X to hex $10, decimal 16 - copying 16 bytes = 4 sprites
  BNE LoadPalettesLoop  ; Branch to LoadPalettesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down
  
  LDA #$00
  STA level
  JSR LoadBackground
						
  LDA #$80
  STA playerY
  STA playerX
  
  LDA #$00
  STA PLAYERSPRITES + 1
  STA PLAYERSPRITES + 2
  LDA #$80
  
  LDA #$01
  STA PLAYERSPRITES + 5
  LDA #$00
  STA PLAYERSPRITES + 6
  
  LDA #$10
  STA PLAYERSPRITES + 9
  LDA #$00
  STA PLAYERSPRITES + 10
  
  LDA #$11
  STA PLAYERSPRITES + 13
  LDA #$00
  STA PLAYERSPRITES + 14
  
  LDA #$00
  STA flipCooldown
  STA playerStatus
			
  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0
  STA PPUCTRL

  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA PPUMASK
  
Forever:
  JMP Forever
  
NMI:
  LDA #$00
  STA OAMADDR       ; set the low byte (00) of the RAM address
  LDA #$02
  STA OAMDMA       ; set the high byte (02) of the RAM address, start the transfer
  
  JSR ReadController

  LDA flipCooldown  ;don't flip if the player has flipped to recently
  BNE DontFlipDecTimer
  
  LDA buttons
  AND #%10000000
  BEQ DontFlip  ;A not pressed don't flip the player
    
  JSR FlipPlayer
  JMP DontFlip
DontFlipDecTimer:
  DEC flipCooldown
DontFlip:

  LDA buttons
  AND #%00000010 ;left pressed
  BEQ DontTurnLeft
  
  LDA playerStatus
  AND #%00000010
  BNE MoveLeft ;already facing left
  
  JSR TurnPlayer

MoveLeft:
  JSR MovePlayerLeft  

DontTurnLeft:

  LDA buttons
  AND #%00000001 ;right pressed
  BEQ DontTurnRight
  
  LDA playerStatus
  AND #%00000010
  BEQ MoveRight ;already facing right
  
  JSR TurnPlayer
  
MoveRight:
  JSR MovePlayerRight

DontTurnRight:

  JSR PlayerFall
  
  ;Make sure PPU is in desired state before drawing next frame
  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0
  STA PPUCTRL

  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA PPUMASK
  
  LDA scroll
  STA PPUSCROLL
  LDA #$00
  STA PPUSCROLL
  
  JSR UpdatePlayerSprites
  RTI
  
MovePlayerLeft:
  LDA playerX
  SEC
  SBC #$01
  STA playerX
  
  RTS
  
MovePlayerRight
  LDA playerX
  CLC
  ADC #$01
  STA playerX
  
  RTS
TurnPlayer:
 LDA PLAYERSPRITES + 1
 LDX PLAYERSPRITES + 5
 STA PLAYERSPRITES + 5
 STX PLAYERSPRITES + 1
 
 LDA PLAYERSPRITES + 9
 LDX PLAYERSPRITES + 13
 STA PLAYERSPRITES + 13
 STX PLAYERSPRITES + 9
 
 LDA PLAYERSPRITES + 2
 EOR #%01000000
 STA PLAYERSPRITES + 2
 STA PLAYERSPRITES + 6
 STA PLAYERSPRITES + 10
 STA PLAYERSPRITES + 14
 
 LDA playerStatus
 EOR #%00000010
 STA playerStatus
 
 RTS
  
FlipPlayer:
  
  LDA PLAYERSPRITES + 1 ;swap left column of sprites
  LDX PLAYERSPRITES + 9
  STA PLAYERSPRITES + 9
  STX PLAYERSPRITES + 1
  
  LDA PLAYERSPRITES + 5 ;swap right column of sprites
  LDX PLAYERSPRITES + 13
  STA PLAYERSPRITES + 13
  STX PLAYERSPRITES + 5

  LDA PLAYERSPRITES + 2 ;Change sprite data to flip vertically
  EOR #%10000000
  STA PLAYERSPRITES + 2
  STA PLAYERSPRITES + 6
  STA PLAYERSPRITES + 10
  STA PLAYERSPRITES + 14

  LDA playerStatus
  EOR #%00000001
  STA playerStatus
  
  LDA #$0C
  STA flipCooldown
  RTS
	
ReadController:
  LDA #$01
  STA $4016
  LDA #$00
  STA $4016
  LDX #$08
ReadControllerLoop:
  LDA $4016
  LSR A            ; bit0 -> Carry
  ROL buttons     ; bit0 <- Carry
  DEX
  BNE ReadControllerLoop
  RTS

LoadBackground:
  LDA PPUSTATUS
  LDA #$20
  STA PPUADDR ;Set PPU address to $2000
  LDA #$00
  STA PPUADDR
  LDA #%00000100 ;Put PPU in 32 increment mode
  STA PPUCTRL
  
  LDX #$00
  LDY #$00
  STY yData
  STY xData
  
  LDA level
  ASL A
  ASL A ;Each entry is 4 bytes long, multiply by 4 to get the appropriate location
  TAY
  LDA LevelIndex, Y
  STA pointer1
  INY
  LDA LevelIndex, Y
  STA pointer1 + 1  ;pointer1 now contains position of selected level's background index
  
  LDY #$00
  LDA [pointer1], Y
  STA backgroundPointer
  INY
  LDA [pointer1], Y
  STA backgroundPointer + 1;backgroundPointer now contains position of selected level's first screen's column index
  
LoadColumnLoop:
  LDA yData ;Current column
  ASL A
  TAY
  LDA [backgroundPointer], Y
  STA pointer1
  INY
  LDA [backgroundPointer], Y
  STA pointer1 + 1;Pointer 1 now has the column data address
  
  LDA #$00
  STA colProgress
  JSR LoadColumnData
  
  LDY yData
  INY
  STY yData
  CPY #$10
  BNE LoadColumnLoop
  
LoadBackgroundDone:
  RTS
  
LoadColumnData:
  LDY colProgress
  LDA [pointer1], Y
  
  ASL A
  TAY
  LDA Metatiles, Y 
  STA metatilePointer
  INY
  LDA Metatiles, Y
  STA metatilePointer + 1
  
  BCS MultipleRepeatSetup
  
SingleRepeatSetup:
  LDA #$01
  STA metatileRepeat
  JMP SetupDone
  
MultipleRepeatSetup:
  LDY colProgress
  INY
  STY colProgress
  LDA [pointer1], Y
  STA metatileRepeat
  
SetupDone:
  INC colProgress
  
  JSR LoadRepeatMetatileLoop
  
  LDA xData ;Whether or not the buffer is full (column loaded)
  CMP #$1E
  BNE LoadColumnData

  JSR LoadBuffer
  
  RTS
  
LoadRepeatMetatileLoop:
  LDY #$00
  LDX xData
  LDA [metatilePointer], Y ;Get tile number
  STA PPUDATA
  INY
  LDA [metatilePointer], Y
  STA PPUDATA
  INY
  LDA [metatilePointer], Y ;Store second row of tiles in buffer
  STA colBuffer, X
  INY
  INX
  LDA [metatilePointer], Y
  STA colBuffer, X
  INX
  STX xData
  LDY #$00
  
  INC metatilesDrawn
  ;LDA xData 
  ;CMP #$1E
  ;BNE ContinueRepeatMetatileLoop ;If rowBuffer is full and needs to be copied into the PPU
  
  ;JSR LoadBuffer

;ContinueRepeatMetatileLoop:
  LDA metatilesDrawn
  CMP metatileRepeat
  BNE LoadRepeatMetatileLoop
  
  RTS
  
LoadBuffer:
  LDY #$00
  STY xData ;Y will already be zeroed out so only setup left is to clear xData
LoadBufferLoop:
  LDA colBuffer, Y
  STA PPUDATA
  INY
  CPY #$1E
  BNE LoadBufferLoop
  
  LDY #$00
  RTS
  
UpdatePlayerSprites:
  
  LDA playerX
  STA PLAYERSPRITES + 3
  STA PLAYERSPRITES + 11
  CLC
  ADC #$08
  STA PLAYERSPRITES + 7
  STA PLAYERSPRITES + 15
  
  LDA playerY
  STA PLAYERSPRITES
  STA PLAYERSPRITES + 4
  CLC
  ADC #$08
  STA PLAYERSPRITES + 8
  STA PLAYERSPRITES + 12
  
  RTS
  
PlayerFall:
  
  LDA #$00
  STA nameTableAddress ;Clear high byte of address
  
  LDA playerX
  LSR A
  LSR A
  LSR A
  STA nameTableAddress + 1
  
  LDA playerStatus
  AND #%00000001
  BEQ IncrementPlayerY
  JMP DontIncrementPlayerY
  
IncrementPlayerY:
  LDA playerY
  CLC
  ADC #$12
  JMP IncrementDone
  
DontIncrementPlayerY:
  LDA playerY
  SEC
  SBC #$01
  
IncrementDone:
  
  AND #%11111000 ;Avoid having to LSR 3 times to get rid of that information since would then have to ASL 5 times
  ASL A
  ROL nameTableAddress
  ASL A
  ROL nameTableAddress ;Multiply by 4, move bits into high byte of address
  CLC
  ADC nameTableAddress + 1 ;Add result to existing address
  STA nameTableAddress + 1
  LDA nameTableAddress
  ADC #$00
  STA nameTableAddress ;Add any potential carry
  
  LDA nameTable
  ASL A
  ASL A
  CLC
  ADC nameTableAddress
  CLC 
  ADC #$20
  STA nameTableAddress
  
  LDA PPUSTATUS
  LDA nameTableAddress
  STA PPUADDR
  LDA nameTableAddress + 1
  STA PPUADDR
  
  LDA PPUDATA
  CMP #$07 ;Check if Player is on ground or not
  BNE FallDone
  
  LDA playerStatus
  AND #%00000001
  BEQ FallDown
  
FallUp:
  LDA playerY
  SEC
  SBC #$01
  STA playerY
  JMP FallDone
  
FallDown:
  LDA playerY
  CLC
  ADC #$01
  STA playerY
  
FallDone:
  
  RTS
  
  .bank 1
  .org $E000

LevelIndex:
  .dw TestLevelBackgroundIndex,TestLevelAttributeIndex
  
TestLevelBackgroundIndex:
  .dw TestLevel1
  
TestLevelAttributeIndex:
  .dw TestLevelAttribute1
  
TestLevel1:
  .dw Column1_1,Column1_2,Column1_3,Column1_4,Column1_5,Column1_6,Column1_7,Column1_8
  .dw Column1_9,Column1_10,Column1_11,Column1_12,Column1_13,Column1_14,Column1_15,Column1_16
  
Column1_1:
  .db $08,$0D,$83,$0A,$0B,$08
  
Column1_2:
  .db $08,$01,$89,$0A,$00,$08

Column1_3:
  .db $08,$01,$89,$0A,$00,$08

Column1_4:
  .db $08,$01,$89,$0A,$00,$08

Column1_5:
  .db $08,$01,$89,$0A,$00,$08

Column1_6:
  .db $08,$01,$89,$0A,$00,$08

Column1_7:
  .db $08,$01,$89,$0A,$00,$08

Column1_8:
  .db $08,$01,$89,$0A,$00,$08

Column1_9:
  .db $08,$01,$89,$0A,$00,$08

Column1_10:
  .db $08,$01,$89,$07,$06,$82,$01,$0D,$08
  
Column1_11:
  .db $08,$01,$89,$07,$00,$88,$03
  
Column1_12:
  .db $08,$01,$89,$07,$00,$88,$03

Column1_13:
  .db $08,$01,$89,$07,$00,$88,$03

Column1_14:
  .db $08,$01,$89,$07,$00,$88,$03

Column1_15:
  .db $08,$01,$89,$07,$00,$88,$03

Column1_16:
  .db $08,$01,$89,$07,$00,$88,$03
  
TestLevelAttribute1:
  .dw Attribute1_1,Attribute1_2,Attribute1_3,Attribute1_4,Attribute1_5,Attribute1_6,Attribute1_7,Attribute1_8
  
Attribute1_1:
  .db $00,$00,$00,$00,$00,$00,$00,$00

Attribute1_2:
  .db $00,$00,$00,$00,$00,$00,$00,$00

Attribute1_3:
  .db $00,$00,$00,$00,$00,$00,$00,$00

Attribute1_4:
  .db $00,$00,$00,$00,$00,$00,$00,$00
  
Attribute1_5:
  .db $00,$00,$00,$00,$00,$00,$00,$00

Attribute1_6:
  .db $00,$00,$00,$00,$00,$00,$00,$00
  
Attribute1_7:
  .db $00,$00,$00,$00,$00,$00,$00,$00

Attribute1_8:
  .db $00,$00,$00,$00,$00,$00,$00,$00
  
;Metatile lookup table
Metatiles:
  .dw GroundUp  ;$00
  .dw GroundDown  ;$01
  .dw GroundLeft  ;$02
  .dw GroundRight  ;$03
  .dw GroundBLCorner  ;$04
  .dw GroundBRCorner  ;$05
  .dw GroundTLCorner  ;$06
  .dw GroundTRCorner  ;$07
  .dw GroundInternal  ;$08
  .dw Blank  ;$09
  .dw InternalTLCorner  ;$0A
  .dw InternalTRCorner  ;$0B
  .dw InternalBLCorner  ;$0C
  .dw InternalBRCorner  ;$0D
  
GroundUp:
  .db $00,$01
  .db $10,$11
  
GroundDown:
  .db $10,$11
  .db $12,$13
  
GroundLeft:
  .db $02,$10
  .db $03,$11
  
GroundRight:
  .db $10,$06
  .db $11,$16
  
GroundBLCorner:
  .db $02,$10
  .db $14,$12
  
GroundBRCorner:
  .db $11,$16
  .db $12,$15
  
GroundTLCorner:
  .db $04,$01
  .db $03,$10
  
GroundTRCorner:
  .db $00,$05
  .db $11,$06
  
GroundInternal:
  .db $10,$11
  .db $11,$10
  
Blank:
  .db $09,$09
  .db $09,$09
  
InternalTLCorner:
  .db $07,$10
  .db $10,$11
 
InternalTRCorner:
  .db $11,$08
  .db $10,$11
  
InternalBLCorner:
  .db $17,$11
  .db $11,$10
  
InternalBRCorner:
  .db $10,$18
  .db $11,$10
  
palette:
  .db $0F,$2D,$3D,$1F,  $0F,$36,$17,$1F,  $0F,$30,$21,$1F,  $0F,$27,$17,$1F   ;;background palette
  .db $10,$0F,$15,$36,  $22,$02,$38,$3C,  $22,$1C,$15,$14,  $22,$02,$38,$3C   ;;sprite palette

  .org $FFFA     ;first of the three vectors starts here
  .dw NMI        ;when an NMI happens (once per frame if enabled) the 
                   ;processor will jump to the label NMI:
  .dw RESET      ;when the processor first turns on or is reset, it will jump
                   ;to the label RESET:
  .dw 0          ;external interrupt IRQ is not used in this tutorial
  
  
;;;;;;;;;;;;;;  
  
  
  .bank 2
  .org $0000
  .incbin "Graphics.chr"   ;includes 8KB graphics file from SMB1