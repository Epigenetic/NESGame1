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
rowBuffer .rs 32
backgroundPointer .rs 2
metatilePointer .rs 2
metatileRepeat .rs 1
metatilesDrawn .rs 1
xData .rs 1
yData .rs 1

;; DECLARE SOME CONSTANTS HERE
PPUCTRL = $2000
PPUMASK = $2001
PPUSTATUS = $2002
OAMADDR = $2003
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
  
  LDA #LOW(Background)
  STA backgroundPointer
  LDA #HIGH(Background)
  STA backgroundPointer+1
  JSR LoadBackground
						
  LDA #$80
  STA PLAYERSPRITES
  STA PLAYERSPRITES + 3
  LDA #$00
  STA PLAYERSPRITES + 1
  STA PLAYERSPRITES + 2
  LDA #$80
  
  LDA #$80
  STA PLAYERSPRITES + 4
  LDA #$01
  STA PLAYERSPRITES + 5
  LDA #$00
  STA PLAYERSPRITES + 6
  LDA #$88
  STA PLAYERSPRITES + 7
  
  LDA #$88
  STA PLAYERSPRITES + 8
  LDA #$10
  STA PLAYERSPRITES + 9
  LDA #$00
  STA PLAYERSPRITES + 10
  LDA #$80
  STA PLAYERSPRITES + 11
  
  LDA #$88
  STA PLAYERSPRITES + 12
  LDA #$11
  STA PLAYERSPRITES + 13
  LDA #$00
  STA PLAYERSPRITES + 14
  LDA #$88
  STA PLAYERSPRITES + 15
  
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
  
  RTI
  
MovePlayerLeft:
  LDA PLAYERSPRITES + 3
  SEC
  SBC #$01
  STA PLAYERSPRITES + 3
  STA PLAYERSPRITES + 11
  
  LDA PLAYERSPRITES + 7
  SEC
  SBC #$01
  STA PLAYERSPRITES + 7
  STA PLAYERSPRITES + 15
  RTS
  
MovePlayerRight
  LDA PLAYERSPRITES + 3
  CLC
  ADC #$01
  STA PLAYERSPRITES + 3
  STA PLAYERSPRITES + 11
  
  LDA PLAYERSPRITES + 7
  CLC
  ADC #$01
  STA PLAYERSPRITES + 7
  STA PLAYERSPRITES + 15
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
  
  LDX #$00
  LDY #$00
  STY yData
  STY xData
  
LoadBackgroundLoop:
  LDY yData
  LDA [backgroundPointer], Y ;Get metatitle number
  CMP #$FF ;Indicates end of background data, so loading this means there is no more background to load
  BEQ LoadBackgroundDone
  
  STY yData ;save place in background data
  
  ASL A ;Table of addresses so must jump by twos
  TAY
  LDA Metatiles, Y
  STA metatilePointer
  INY
  LDA Metatiles, Y
  STA metatilePointer+1
  
  LDY yData
  
  INY
  LDA [backgroundPointer], Y ;Get number of repetitions
  STA metatileRepeat
  INY
  STY yData
  LDY #$00
  LDX #$00
  STY metatilesDrawn
  
  
LoadRepeatMetatileLoop:
  
  LDX xData
  LDA [metatilePointer], Y ;Get tile number
  STA PPUDATA
  INY
  LDA [metatilePointer], Y
  STA PPUDATA
  INY
  LDA [metatilePointer], Y ;Store second row of tiles in buffer
  STA rowBuffer, X
  INY
  INX
  LDA [metatilePointer], Y
  STA rowBuffer, X
  INX
  STX xData
  LDY #$00
  
  INC metatilesDrawn
  LDA xData
  CMP #$20
  BNE ContinueRepeatMetatileLoop ;If rowBuffer is full and needs to be copied into the PPU
  
  JSR LoadBuffer

ContinueRepeatMetatileLoop:
  LDA metatilesDrawn
  CMP metatileRepeat
  BEQ LoadBackgroundLoop
  JMP LoadRepeatMetatileLoop
  
LoadBackgroundDone:
  RTS
  
LoadBuffer:
  ;LDY #$00
  STY xData ;Y will already be zeroed out so only setup left is to clear xData
LoadBufferLoop:
  LDA rowBuffer, Y
  STA PPUDATA
  INY
  CPY #$20
  BNE LoadBufferLoop
  
  RTS
  
  .bank 1
  .org $E000
  
Background:
  .db $08,$10 ;Row of ground internal
  .db $01,$10 ;Row of Ground down
  .db $09,$B0 ;Fill screen with blank
  .db $00,$10 ;Row of ground up
  .db $08,$10 ;Row of ground internal
  .db $FF ;End of data marker
  
;Metatile lookup table
Metatiles:
  .dw GroundUp
  .dw GroundDown
  .dw GroundLeft
  .dw GroundRight
  .dw GroundBLCorner
  .dw GroundBRCorner
  .dw GroundTLCorner
  .dw GroundTRCorner
  .dw GroundInternal
  .dw Blank
  
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
  .db $07,$07
  .db $07,$07
  
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