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

;; DECLARE SOME CONSTANTS HERE
PPUCTRL = $2000
PPUMASK = $2001
PPUSTATUS = $2002
OAMADDR = $2003
PPUADDR = $2006
PPUDATA = $2007
OAMDMA = $4014
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

  LDA #$80
  STA $0200
  STA $0203
  LDA #$00
  STA $0201
  STA $0202
  LDA #$80
  
  LDA #$80
  STA $0204
  LDA #$01
  STA $0205
  LDA #$00
  STA $0206
  LDA #$88
  STA $0207
  
  LDA #$88
  STA $0208
  LDA #$10
  STA $0209
  LDA #$00
  STA $020A
  LDA #$80
  STA $020B
  
  LDA #$88
  STA $020C
  LDA #$11
  STA $020D
  LDA #$00
  STA $020E
  LDA #$88
  STA $020F
  
  LDA #$00
  STA flipCooldown
  STA playerStatus
			
  LDA #%10000000   ; enable NMI, sprites from Pattern Table 0
  STA PPUCTRL

  LDA #%00010000   ; enable sprites
  STA PPUMASK
  
Forever:
  JMP Forever
  
NMI:
  LDA #$00
  STA OAMADDR       ; set the low byte (00) of the RAM address
  LDA #$02
  STA OAMDMA       ; set the high byte (02) of the RAM address, start the transfer
  
  JSR ReadController
  
  LDA flipCooldown
  BNE DontFlipDecTimer
  
  LDA buttons
  AND #%10000000
  BEQ DontFlip  ;A not pressed don't flip the player
    
  JSR FlipPlayer
  JMP DontFlip
DontFlipDecTimer:
  DEC flipCooldown
DontFlip:
  
  RTI
  
FlipPlayer:
  LDA playerStatus
  AND #%00000001
  BEQ UpToDown
  
  LDA $0201 ;swap left column of sprites
  LDX $0205
  STA $0205
  STX $0201
  
  LDA $0209 ;swap right column of sprites
  LDX $020D
  STA $020D
  STX $0209

  LDA $0202 ;Change sprite data to flip vertically
  EOR #%10000000
  STA $0202
  STA $0206
  STA $020A
  STA $020E

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

	
  .bank 1
  .org $E000
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