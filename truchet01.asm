; ==============================================================
; Truchet Tiles
;
; Port to Atari from "8-bit Show And Tell" C64 version.
; ==============================================================
;
; Ken Jennings
;
; Source uses MADS assembler syntax.
; %>  mads truchet01.asm -o:truchet01.xex
;
; Code now depends on the Atari includes for sensible 
; symbols and other goodies.   
; Get them here:
; https://github.com/kenjennings/Atari-Mads-Includes
;
; ==============================================================
;
; Version 00:  (See github.)
;
; Direct port of the C64 code with as few changes as possible.
; Seriously, intentionally going out of my way to make as few 
; changes as possible to the 6502 assembly code except where 
; necessary.
;
; ==============================================================
;
; Version 01 -- Atari-fication 
; 
; WORK IN PROGRESS . . .
;
; Updates:
;
; --------------------------------------------------------------
;
; 2025-08-19
;
; * Some easy stuff first: Deleted the C64-specific work.
;
; * Start using the include files and proper Atari symbols.
;
; * Use an overscan screen.  In memory this is a 48 character 
;   by 30 line text display. (1440 bytes)
;
; --------------------------------------------------------------
;
; 2025-08-24
;
; * Using more of the Atari system defines and (my) standard
;   variable naming conventions.
;
; * Changed one-time initialization code into 
;   direct-load-from-disk behavior, eliminating that code.
;
; * optimizations of the pattern list to eliminate math used
;   to reference data in structures.
;
; * optimized code around pointer use/math.  Optimizations have
;   reduced the assembled size by about 32 bytes.
;
; --------------------------------------------------------------
;
; 2025-08-30
;
; * Tile pattern can be changed to the next pattern by using the
;   right arrow, and previous pattern using the left arrow.
; 
; * Console Key Support:
;   Option = cycle to next base color
;   Select = Toggle dark/light v dark/light for tiles
;   Start  = Turn on the Attract mode (turned off by any 
;             regular keyboard key.)
; ==============================================================


; Let's Begin . . .

; System Includes

	ICL "ANTIC.asm"  ; Graphics
	ICL "GTIA.asm"   ; Color
	ICL "POKEY.asm"  ; Keyboard
	ICL "PIA.asm"    ; Joystick
	ICL "OS.asm"     ; Stuff, stuff, stuff
	ICL "DOS.asm"    ; LOMEM, start, and run addresses.

	ICL "macros.asm" ; Some memory tricks
	
	
; ==============================================================

; Program Defines

SCREEN_WIDTH    = 48 ; Screen width in characters.
SCREEN_HEIGHT   = 30 ; screen height in lines.

NUM_PATTERNS    = 30 ; number of tile patterns the program draws
NUM_TILE_IMAGES = 7  ; Number of images for tiles.


; ==============================================================

; We're going to use the Atari's segmented file format to 
; simultaneously declare symbols, define space, and pre-load
; data values.  And, since we're dropping all these working
; variables into page 0, the assembler will use page 0 
; references making the overall machine language smaller and
; faster. 

; The overscan display needs a custom display list.  
; Full overscan is 30 lines of text (times 8 scan lines 
; which is 240 scan lines).   This requires 1440 bytes of memory.
; We're abusing page 0 already, so let's just put the 
; display list there, too.  Because we can.  Just 35 bytes...
; Occupies $C0 through $E2. 

	ORG $C0 ; Yes, we are disk loading directly into page 0

; Defining, declaring, and loading data at the same time...

zDisplayList
	.byte $42                  ; Load Memory Scan + Mode 2 text
	.word gScreenMemory              ; Start reading the screen
	.byte $02,$02,$02,$02,$02,$02,$02,$02 ; 8 lines Mode 2 text
	.byte $02,$02,$02,$02,$02,$02,$02,$02 ; 8 lines Mode 2 text
	.byte $02,$02,$02,$02,$02,$02,$02,$02 ; 8 lines Mode 2 text
	.byte $02,$02,$02,$02,$02             ; 5 lines Mode 2 text
	.byte $41                        ; Do Vertical Blank and...
	.word zDisplayList ; ...start at the beginning of the list.

; ==============================================================

; Program Variables in Page 0

	ORG $ed

zTileImageNumber     .byte 0 ; at $ed - reference, current tile image number.
zSourcebit           .byte 0 ; at $ee - used for transforming/bit mirroring.
zDestbit             .byte 0 ; at $ef - used for transforming/bit mirroring.

zPatternNumber       .byte 0 ; at $f0 - reference, current pattern number
zPatternWidth        .byte 0 ; at $f1 - reference, width (X) of current pattern
zPatternHeight       .byte 0 ; at $f2 - reference, height (Y) of current pattern

zPatternX            .byte 0 ; at $f3 - working X position in pattern
zPatternY            .byte 0 ; at $f4 - working Y position in pattern
zPatternPointer      .word 0 ; at $f5/$f6 - working pointer to pattern. (reuse for transform)
zPatternPointerCopy  .word 0 ; at $f7/$f8 - backup used to reset pointer to start
zBaseColor           .byte 0 ; at $f9    - color for screen.
zTileColorType       .byte 0 ; at $fa    - 0 = Tiles dark/light. 1 = Tiles light/dark. 

; On Atari:
; 0 == internal code $4A -- lower left triangle.
; 1 == internal code $C8 -- upper left triangle (inverse video $48)
; 2 == internal code $CA -- upper right triangle (inverse video $4A)
; 3 == internal code $48 -- lower right triangle 

zaTileCodes
	.byte $4A,$C8,$CA,$48 ; at $fb to $fe


; The original code declared a pointer to screen memory.  The 
; Atari OS already gives us a page zero value for this, SAVMSC.
; Likewise, the original code declared current screen X and Y
; position values in page 0. The Atari OS already defines 
; values like these as ROWCRS (Y) and COLCRS (X).
;
; Since the program does not use OS routines for the S: device,
; the program is free to use/abuse these page zero values.
; Therefore, here repurpose the OS page 0 screen device  values 
; for the program's use:

zScreenPointer = SAVMSC ; word. Address of first byte of screen memory.
zScreenY       = ROWCRS ; byte. Current cursor row (Y) 
zScreenX       = COLCRS ; word, but we're only using low byte. Current cursor column (X)


; ==============================================================

; Now we can get to the real coding.

; Just pick any spot conveniently after DOS...

	ORG $3400 

; the part of the program that loops forever . . .

StartTruchet

; ==============================================================

	jsr InstallTileImage ; Load cset with tile images.

	; Set up the variables for this pattern (based on Pattern number)

	ldx zPatternNumber        ; X = Pattern Number  (to be used as array index) 
	
	lda gaPatternListLow,x    ; A = get low byte of pointer from array
	sta zPatternPointer       ; save in page 0 working variable
	sta zPatternPointerCopy   ; Save in the backup when needing to reset later.
	
	lda gaPatternListHigh,x   ; A = get high byte of pointer from array
	sta zPatternPointer+1     ; save in page 0 working variable
	sta zPatternPointerCopy+1 ; Save in the backup when needing to reset later.

	lda gaPatternListWidth,x  ; A = get pattern width from the array
	sta zPatternWidth         ; Save as the pattern width

	lda gaPatternListHeight,x ; A = get pattern height from the array
	sta zPatternHeight        ; Save as the pattern height


	; initialize/zero the working variables for drawing
	lda #0                    ; A = 0
	sta zScreenX
	sta zScreenY
	sta zPatternX
	sta zPatternY

	; Reset working screen pointer to the screen memory address
	lda #<gScreenMemory
	sta zScreenPointer
	lda #>gScreenMemory
	sta zScreenPointer+1


; ==============================================================

	; Draw patterns into screen memory until finished...
bDrawLoop

	; Get character from tile array and plot at current screen position...
	ldy zPatternX           ; Y = Pattern X position
	lda (zPatternPointer),Y ; A = *(Pattern Pointer + Y) -- tile ID number from pattern
;	tax                     ; X = tile ID number from pattern.
;	lda zaTileCodes,x       ; A = character from tile array ( tile[ x ] )
	ldy zScreenX            ; Y = Screen X position
	sta (zScreenPointer),y  ; Screen byte *(Screen Pointer + Y) = tile character 

	; Next pattern column
	inc zPatternX           ; Pattern X++
	lda zPatternX           ; A = Pattern X
	cmp zPatternWidth       ; If A == Pattern Width ?
	bcc bNextScreenColumn   ; No, Continue with next screen column
	lda #0                  ; A = 0 
	sta zPatternX           ; Reset Pattern X to start

	; Move to next screen column
bNextScreenColumn

	inc zScreenX            ; Screen X++
	lda zScreenX            ; A = Screen X
	cmp #SCREEN_WIDTH       ; If A == Screen X width ?
	bcc bDrawLoop           ; No, Continue with drawing the next position

	; Completed screen Row... Reset X positions for screen and pattern
	lda #0                  ; A = 0
	sta zPatternX           ; Reset Pattern X to start
	sta zScreenX            ; Reset Screen X to start

	; Completed screen Row... Move to next pattern row
	clc
	lda zPatternPointer      ; A = Pattern pointer low byte
	adc zPatternWidth        ; A = A + Pattern Width
	sta zPatternPointer      ; Save new  low byte in Pattern pointer
	bcc bNextPatternRow      ; If no carry, then skip high byte treatment
	inc zPatternPointer+1    ; Carry Set, so increment high byte.

	; Move to next pattern row.
bNextPatternRow

	inc zPatternY            ; Pattern Y++
	lda zPatternY            ; A = Pattern Y
	cmp zPatternHeight       ; If A == Pattern Y height ?
	bcc bNextScreenRow       ; No, Continue with drawing the next screen row

	; Completed pattern, reset to start again
	lda #0                   ; A = 0
	sta zPatternY            ; Reset Pattern Y to start

	lda zPatternPointerCopy  ; Copy the saved Pattern pointer...
	sta zPatternPointer      ; to the working variable.
	lda zPatternPointerCopy+1
	sta zPatternPointer+1

	; Move to next screen row
bNextScreenRow

	clc
	lda zScreenPointer      ; A = Screen pointer low byte
	adc #SCREEN_WIDTH       ; A = A + Screen Width
	sta zScreenPointer      ; Save new  low byte in Screen pointer
	bcc bNextScreenY        ; If no carry, then skip high byte treatment.
	inc zScreenPointer+1    ; Carry set, so increment high byte.

	; Update screen Y poisiton.
bNextScreenY

	inc zScreenY            ; Screen Y++
	lda zScreenY            ; A = Screen Y
	cmp #SCREEN_HEIGHT      ; If A == Screen Y height ?
	bcc bDrawLoop           ; No, Continue with drawing the next position

; ==============================================================

	; Completed drawing screen.   Wait for a keypress.
	; Quick and Dirty Version.  This needs to run a debounce 
	; behavior for the Console keys before accepting the input.
	
	; Note that when someone presses a key on the Atari's keyboard 
    ; the color cycling Attract Mode will also reset.

bMonitorInputLoop

	lda #255                ; Clear last keypress. 255 = No Key press.
	sta CH                  ; CH -- the most recent key pressed.  

	jsr WaitTOF             ; Wait for next frame to start.
	
; Checking the keys . . .	
	jsr KeyboardInput      ; Yes.  Skip out to processing the key.
	bne bGoToStart          ; 0 = no key input.   1= key input, so redraw.

bTestConsoleInput           ; No key input, so test console.
	jsr ProcessConsoleKeys ; Console changes color, so redraw is not needed.

	jmp bMonitorInputLoop

bGoToStart
	jmp StartTruchet    ; Back to the beginning



; ==============================================================
; KeyboardInput
; 
; Process Keycodes.
; Mask out shift and ctrl, so any form of the character works.
;
; Left Arrow  (+) aka #$06 == Previous tile Pattern
; Right arrow (*) aka #$07 == Next Tile Pattern
;
; Up Arrow    (-) aka #$0E  == Previous Tile Image
; Down Arrow  (=) aka #$0F == Next Tile Image

KeyboardInput
	lda CH                  ; A = CH -- Read OS register for last keypress
	cmp #255                ; Is a key pressed?  255 == No Key.
	bne bki_TestKeys        ; Something was pressed.

bki_ExitZero
	lda #0                  ; Nothing was pressed...
	rts                     ; so we're done here.

bki_TestKeys
	and #$3F                ; Remove Ctrl and Shift bits
; Testing Left Arrow...
	cmp #6                  ; Is it the left arrow?
	bne bki_TestRightArrow  ; No.
	
	dec zPatternNumber      ; Pattern Number--
	bpl bki_ExitInput       ; Positive value, so exit with input flag.
	lda #[NUM_PATTERNS-1]   ; Went negative.  Wrap to last pattern number.
	sta zPatternNumber      ; Reset pattern
	bpl bki_ExitInput       ; Exit with Input Flag.
	
bki_TestRightArrow
	cmp #7                  ; Is it Right Arrow?
	bne bki_TestUpArrow     ; No.
	
	inc zPatternNumber      ; Pattern Number++
	lda zPatternNumber      ; A = Pattern Number
	cmp #NUM_PATTERNS       ; If A == the end of the pattern list ?
	bcc bki_ExitInput       ; No.  Skip resetting the pattern number
	lda #0                  ; A = 0 -- go back to first pattern
	sta zPatternNumber      ; Save to Pattern Number
	beq bki_ExitInput       ; Exit with Input Flag. 

bki_TestUpArrow
	cmp #$0e                ; Is it Up Arrow?
	bne bki_TestDownArrow   ; No.
	dec zTileImageNumber    ; Image Number --
	bpl bki_ExitInput       ; Positive value, so exit with input flag.
	lda #[NUM_TILE_IMAGES-1] ; Went negative.  Wrap to last tile image number.
	sta zTileImageNumber    ; Reset image number
	bpl bki_ExitInput       ; Exit with Input Flag.

bki_TestDownArrow 
	cmp #$0F                ; Is it Down Arrow?
	bne bki_ExitZero        ; No. Exit with zero flagging no input.
	inc zTileImageNumber    ; Image Number --
	lda zTileImageNumber    ; A = Tile Image Number
	cmp #NUM_TILE_IMAGES    ; If A == the end of the tile image list ?
	bcc bki_ExitInput       ; No.  Skip resetting the tile image  number
	lda #0                  ; A = 0 -- go back to first tile image
	sta zTileImageNumber    ; Save to tile image Number

bki_ExitInput
	lda #1 

bki_Exit
	rts


; ==============================================================
; WaitTOF
;
; Delay until start of the next frame.
; This is an adequate pause for polling for input.

WaitTOF                     ; Where did that come from?  Hmmm.
	lda RTCLOK60            ; Get the Jiffy Clock
	
bwtof_FrameWait
	cmp RTCLOK60            ; Did the jiffy clock change?
	beq bwtof_FrameWait     ; Nope.  Keep looping through this frame.
	rts
	
	
; ==============================================================
; ProcessConsoleKeys 
;
; Console Key Support.
;
; Run debounce of Console Keys and require key be 
; released before accepting new input.
;
; Y is the key.  Do not change it during process.
; 0 = Start
; 1 = Select
; 2 = Option

ProcessConsoleKeys    ; Loop through the 3 keys
	ldy #2              ; 2, 1, 0...
bpc_LoopConsoleKeys
	jsr ProcessConsoleEvents ; Process Debounce and flag input
	bne bDoConsoleKey   ; !0 means this is an input event
	dey                 ; doing next key
	bpl bpc_LoopConsoleKeys 
	rts


; Process Input event for the Console Key.
; Y is the key.  Do not change it.
; 0 = Start  -- Turn on the Attract mode (turned off by any 
;               regular keyboard key.)
; 1 = Select -- Toggle dark/light v dark/light for tiles
; 2 = Option -- cycle to next base color


bDoConsoleKey 
	cpy #0   ; 0 = Start
	bne bdck_TestSelect ; Try next one
	lda #255
	sta ATRACT ; turn on color cycling.
	rts
	
bdck_TestSelect ; 1 = Select
	cpy #1
	bne bdck_TestOption ; Nope.  Try last one.
	inc zTileColorType
	lda zTileColorType
	and #1
	sta zTileColorType
	jmp bdck_SetColors
	
bdck_TestOption
	cpy #2   ; 2 = Option	
	bne bdck_Exit ; This should not happen.
	clc
	lda #16        ; Add 16 to the base color
	adc zBaseColor
	sta zBaseColor
	
bdck_SetColors
	lda zBaseColor
	ldx zTileColorType
	bne bdck_LightOnDark  ; 1 == Light Tile on Dark background
	ora #$e               ; 0 == Dark Tile on Light Background
	sta COLOR2            ; background color.
	lda #0 
	sta COLOR1            ; Tile (text) luminance.
	rts
	
bdck_LightOnDark	; 1 == Light Tile on Dark background
	sta COLOR2            ; background color.
	lda #$e
	sta COLOR1            ; Tile (text) luminance.

bdck_Exit
	rts                  ; done.


; ==============================================================
; ProcessConsoleEvents
;
; The OS handles normal Keyboard bounce and Input.
; It doesn't deal with the function keys.  
; To prevent a function key from registering as 
; continuous input when it is pressed, this function 
; analyzes state and only flags an input received 
; when conditions are met:
; The Console key must be pressed for several flames 
; before an input event is flagged. 
; The Console key must be released and pressed again 
; in order to cause a new Input event. 

; Y is the key.  Do not change it.
; 0 = Start
; 1 = Select
; 2 = Option

; CONSOLE_START =        %00000001 ; Start button
; CONSOLE_SELECT =       %00000010 ; Select button
; CONSOLE_OPTION =       %00000100 ; Option button

CONSOLE_DEBOUNCE_COUNT = 3  ; frame count to suppress intermittent contact.

; Convert Y value 2, 1, 0  into the bit in the CONSOLE register
gConsoleKeyBit
	.byte CONSOLE_START,CONSOLE_SELECT,CONSOLE_OPTION

; 0  Not pressed ; !0 = pressed
gConsoleKeyState 
	.byte 0,0,0

; Number of frames to count before accepting changes
gConsoleDelayCounter 
	.byte 0,0,0

; flag for input.   0 = No Input.  1 = Input occurred.  
gConsoleKeyInput
	.byte 0,0,0


ProcessConsoleEvents
	lda gConsoleKeyState,y
	beq bpce_LastStateOff      ; goto - No key was pressed on last scan
	; A key was pressed/ON in the last scan
	lda CONSOL    ; Test current key state
	eor #CONSOLE_KEYS ; flip the 0s to 1s
	and gConsoleKeyBit,Y
	bne bpce_StateOnKeyOn ; goto - Key continues to be pressed.
	; The key was on, but now is off.
	; Reset state to off and start counter to sustain off state
	lda #CONSOLE_DEBOUNCE_COUNT
	sta gConsoleDelayCounter,y
	lda #0
	sta gConsoleKeyState,y
	beq bpce_SetConsoleInputAndExit

bpce_StateOnKeyOn
	lda gConsoleDelayCounter,y
	beq bpce_SetConsoleInputAndExit
	
	jsr bpce_DecrementCounter    ; Decrement counter through X and update counter.
	cpx #0                       ; Set flags based on X again. 
	bne bpce_SetNoInputAndExit   ; State is On, Key On, Countdown still in progress
	; State is On, Key is On, Countdown ended.  Therefore Flag Input.
	lda #1
	bne bpce_SetConsoleInputAndExit

bpce_LastStateOff      ; No key was pressed on last scan
	lda gConsoleDelayCounter,y ; Is copunter in progress?
	beq bpce_StateOffCounterZero ; No input and no counter
	
	jsr bpce_DecrementCounter    ; Decrement counter through X and update counter.
	lda #0 ; Zero input enforced while counting.
	beq bpce_SetConsoleInputAndExit

bpce_StateOffCounterZero ; Counter ended
	lda CONSOL    ; Test current key state
	eor #CONSOLE_KEYS  ; flip the 0s to 1s
	and gConsoleKeyBit,Y
	beq bpce_SetConsoleInputAndExit ; No input, so return no input.
	; A key is pressed.
	lda #CONSOLE_DEBOUNCE_COUNT ; Start the bounce/delay counter
	sta gConsoleDelayCounter,y
	lda #1   ; Flag that input is now being tested
	sta gConsoleKeyState,y 

bpce_SetNoInputAndExit
	lda #0  ; But it is not yet an input event.

bpce_SetConsoleInputAndExit
	sta gConsoleKeyInput,y

bpce_Exit
	rts    ; Fini    bProcessConsoleEvents
	

bpce_DecrementCounter
;	stx absolute,y is not a thing. Therefore, the madness below....
	tax                          ; X = A
	dex                          ; X = X - 1
	txa                          ; A = X, because X can't write back the same way A read it
	sta gConsoleDelayCounter,y   ; Update the counter.
;	lda #0        ; Need to preset A for the exit when countdown remains in progress.
	rts
	

; ==============================================================
; InstalTileImage
;
; Copy tile image to position 1 in the redefined character set.

InstallTileImage

	ldy zTileImageNumber      ; Current tile image number
	lda gaTileImageListLow,Y  ; Get Low byte from list
	sta zPatternPointer       ; (Reusing for this tile copy)
	lda gaTileImageListHigh,Y ; Get High byte from list
	sta zPatternPointer+1     ; complete the pointer.
	
	ldy #7                    

biti_LoopBytes
	lda (zPatternPointer),y   ; Get a byte from memeory.
	sta gTileImage1,y         ; Put it into the forst character's image.
	dey
	bpl biti_LoopBytes        ; Continue until all 8 bytes copied.

	jsr TransformTile         ; Build tile images 2, 3, 4 from image 1.
	
	rts


; ==============================================================
; TransformTile
;
; Convert the tile in character position 1 into a horizontally 
; mirrored version that goes into position 4.
; Then the tiles in positions 1 and 4 are vertically flipped
; to create tiles 2 and 3 respectively.
;
; This allow the program to maintain data for only 1 tile 
; image, and then fill out the entire set from that data.
; Prior to calling this function it is expected that the image 
; has been copied into position 1.

TransformTile

	ldy #7   ; 7 to 0 bytes in the image.
	
btt_LoopBytes
	lda #0   ; Make the target zero if there are no bits to move.
	sta gTileImage4,y ; Destination of the horizontal mirror.
	
	lda #%10000000    ; High Bit  
	sta zSourceBit
	lda #%00000001    ; mirrors to Low bit.
	sta zDestBit
	
btt_LoopBits
	lda gTileImage1,y
	and zSourceBit
	beq btt_SkipBit
	
	lda gTileImage4,y
    ora zDestBit
	sta gTileImage4,y

btt_SkipBit
	lsr zSourceBit ; Move Bit ->
	asl zDestBit   ; Move Bit <-
	bcc btt_LoopBits 

	dey
	bpl btt_LoopBytes

; Now vertically flip tile 1 into 2, and tile 4 into 3....

	ldy #7
	ldx #0

btt_LoopByteCopy
	lda gTileImage1,Y
	sta gTileImage2,X
	lda gTileImage4,Y
	sta gTileImage3,X
	
	inx
	dey
	bpl btt_LoopByteCopy

	rts


; ==============================================================

; The original code used pointers with both the low bytes and 
; high bytes declared together. 
; Splitting up high byte and low byte into separate tables means 
; the index into the lists do not need to be multiplied times 2
; to account for the size of the pair of bytes.
;
; ALSO, the original code's pointer are the address of a 
; data structure for each tile pattern.   By separating the 
; horizontal(X_) and vertical (Y) into separate lists, the 
; pointers acquired here are just direct addresses to the 
; tile pattern.


; List of pointers to each of the patterns' tile descriptions.

gaPatternListLow
	mLowByte pa,pb,pc,pd,pe,pf,pg,ph
	mLowByte pi,pl,pm,pn,po,pp,pq,pr
	mLowByte ps,pt,pv,pu,px,py,pz,pamp
	mLowByte p1,p2,p3,p4,p5,p6

gaPatternListHigh
	mHighByte pa,pb,pc,pd,pe,pf,pg,ph
	mHighByte pi,pl,pm,pn,po,pp,pq,pr
	mHighByte ps,pt,pv,pu,px,py,pz,pamp
	mHighByte p1,p2,p3,p4,p5,p6


; The original code treated this as a structured data element:
; First Byte  = X width
; Second Byte = Y height
; Remaining   =  X * Y number of bytes containing the tile ID numbers.
;
; While that's a cool C-like thing, the 6502 doesn't fit so well, and 
; the code can be simplified by splitting up the structure parts into 
; separate lists.   All the X sizes in one list, all the Y sizes in
; another list.  Then all that is left is the string of tile pattern
; bytes.

gaPatternListWidth
	.byte 1,1,2,2,4,2,2,3
	.byte 4,4,2,4,4,2,4,2
	.byte 4,4,8,4,6,4,4,4
	.byte 8,6,10,10,6,12

gaPatternListHeight
	.byte 1,2,2,2,4,2,4,3
	.byte 4,4,2,4,2,2,2,1
	.byte 1,1,8,4,6,4,2,4
	.byte 8,6,10,10,6,12

;For Reference, the tile IDs again:
; On Atari:
; 0 == internal code $4A -- lower left triangle.
; 1 == internal code $C8 -- upper left triangle (inverse video $48)
; 2 == internal code $CA -- upper right triangle (inverse video $4A)
; 3 == internal code $48 -- lower right triangle 

; New Update... 
; Switching to a redefined character set means there is no 
; need to have a translation from tile number to a character 
; code corresponding to an ATASCII character.  The tile images
; can be moved to the start of the character set, and the 
; tile IDs can be the real internal character code in the 
; Atari character set.  
; 
; Therefore, the 0 to 3 values meant for the translation 
; table to lookup character code can be replaced with 
; values 1 to 4 corresponding to the actual character codes.

pa  ; 1,1
;	.byte 0
	.byte 1

pb  ; 1,2
;	.byte 0,2
	.byte 1,3

pc  ; 2,2
;	.byte 0,2
;	.byte 2,0
	.byte 1,3
	.byte 3,1

pd  ; 2,2
;	.byte 1,0
;	.byte 2,3
	.byte 2,1
	.byte 3,4

pe  ; 4,4
;	.byte 1,2,3,0
;	.byte 2,1,0,3
;	.byte 3,0,1,2
;	.byte 0,3,2,1
	.byte 2,3,4,1
	.byte 3,2,1,4
	.byte 4,1,2,3
	.byte 1,4,3,2
	

pf  ; 2,2
;	.byte 1,2
;	.byte 0,3
	.byte 2,3
	.byte 1,4

pg  ; 2,4
;	.byte 0,2
;	.byte 2,0
;	.byte 2,0
;	.byte 0,2
	.byte 1,3
	.byte 3,1
	.byte 3,1
	.byte 1,3

ph  ; 3,3
;	.byte 1,3,3
;	.byte 3,1,3
;	.byte 3,3,1
	.byte 2,4,4
	.byte 4,2,4
	.byte 4,4,2

pi  ; 4,4
;	.byte 2,0,0,2
;	.byte 0,2,2,0
;	.byte 0,2,2,0
;	.byte 2,0,0,2
	.byte 3,1,1,3
	.byte 1,3,3,1
	.byte 1,3,3,1
	.byte 3,1,1,3

pl  ; 4,4
;	.byte 3,0,3,0
;	.byte 2,3,0,1
;	.byte 3,2,1,0
;	.byte 2,1,2,1
	.byte 4,1,4,1
	.byte 3,4,1,2
	.byte 4,3,2,1
	.byte 3,2,3,2

pm  ; 2,2
;	.byte 1,2
;	.byte 2,1
	.byte 2,3
	.byte 3,2

pn  ; 4,4
;	.byte 1,0,3,2
;	.byte 2,3,0,1
;	.byte 3,2,1,0
;	.byte 0,1,2,3
	.byte 2,1,4,3
	.byte 3,4,1,2
	.byte 4,3,2,1
	.byte 1,2,3,4

po  ; 4,2
;	.byte 3,0,1,2
;	.byte 1,2,3,0
	.byte 4,1,2,3
	.byte 2,3,4,1

pp  ; 2,2
;	.byte 1,2
;	.byte 3,0
	.byte 2,3
	.byte 4,1

pq  ; 4,2
;	.byte 3,3,0,0
;	.byte 1,1,2,2
	.byte 4,4,1,1
	.byte 2,2,3,3

pr  ; 2,1
;	.byte 3,0
	.byte 4,1

ps  ; 4,1
;	.byte 3,0,1,2
	.byte 4,1,2,3

pt  ; 4,1
;	.byte 1,3,0,2
	.byte 2,4,1,3

pv  ; 8,8
;	.byte 2,0,3,0,2,0,1,0
;	.byte 0,3,0,2,0,1,0,2
;	.byte 3,0,2,0,1,0,2,0
;	.byte 0,2,0,1,0,2,0,3
;	.byte 2,0,1,0,2,0,3,0
;	.byte 0,1,0,2,0,3,0,2
;	.byte 1,0,2,0,3,0,2,0
;	.byte 0,2,0,3,0,2,0,1
	.byte 3,1,4,1,3,1,2,1
	.byte 1,4,1,3,1,2,1,3
	.byte 4,1,3,1,2,1,3,1
	.byte 1,3,1,2,1,3,1,4
	.byte 3,1,2,1,3,1,4,1
	.byte 1,2,1,3,1,4,1,3
	.byte 2,1,3,1,4,1,3,1
	.byte 1,3,1,4,1,3,1,2

pu  ; 4,4
;	.byte 1,3,0,2
;	.byte 3,0,2,1
;	.byte 0,2,1,3
;	.byte 2,1,3,0
	.byte 2,4,1,3
	.byte 4,1,3,2
	.byte 1,3,2,4
	.byte 3,2,4,1

px  ; 6,6
;	.byte 2,0,3,0,0,2
;	.byte 2,1,2,2,0,0
;	.byte 3,0,0,2,2,0
;	.byte 2,2,0,0,2,1
;	.byte 0,2,2,0,3,0
;	.byte 0,0,2,1,2,2
	.byte 3,1,4,1,1,3
	.byte 3,2,3,3,1,1
	.byte 4,1,1,3,3,1
	.byte 3,3,1,1,3,2
	.byte 1,3,3,1,4,1
	.byte 1,1,3,2,3,3

py  ; 4,4
;	.byte 1,3,0,2
;	.byte 3,1,2,0
;	.byte 2,0,3,1
;	.byte 0,2,1,3
	.byte 2,4,1,3
	.byte 4,2,3,1
	.byte 3,1,4,2
	.byte 1,3,2,4

pz  ; 4,2
;	.byte 1,2,0,3
;	.byte 3,1,2,0
	.byte 2,3,1,4
	.byte 4,2,3,1

pamp ; 4,4
;	.byte 1,3,2,0
;	.byte 3,1,0,2
;	.byte 0,2,3,1
;	.byte 2,0,1,3
	.byte 2,4,3,1
	.byte 4,2,1,3
	.byte 1,3,4,2
	.byte 3,1,2,4

p1  ; 8,8
;	.byte 0,2,0,2,1,3,1,3
;	.byte 1,3,1,3,0,2,0,2
;	.byte 3,1,3,1,2,0,2,0
;	.byte 1,3,1,3,0,2,0,2
;	.byte 3,1,3,1,2,0,2,0
;	.byte 2,0,2,0,3,1,3,1
;	.byte 0,2,0,2,1,3,1,3
;	.byte 2,0,2,0,3,1,3,1
	.byte 1,3,1,3,2,4,2,4
	.byte 2,4,2,4,1,3,1,3
	.byte 4,2,4,2,3,1,3,1
	.byte 2,4,2,4,1,3,1,3
	.byte 4,2,4,2,3,1,3,1
	.byte 3,1,3,1,4,2,4,2
	.byte 1,3,1,3,2,4,2,4
	.byte 3,1,3,1,4,2,4,2

p2  ; 6,6
;	.byte 3,0,2,1,3,0
;	.byte 0,2,0,3,1,3
;	.byte 2,0,1,2,3,1
;	.byte 3,1,0,3,2,0
;	.byte 1,3,1,2,0,2
;	.byte 2,1,3,0,2,1
	.byte 4,1,3,2,4,1
	.byte 1,3,1,4,2,4
	.byte 3,1,2,3,4,2
	.byte 4,2,1,4,3,1
	.byte 2,4,2,3,1,3
	.byte 3,2,4,1,3,2

p3  ; 10,10
;	.byte 1,2,1,2,0,2,3,0,1,3
;	.byte 3,0,3,0,2,0,2,1,3,1
;	.byte 1,2,1,2,0,2,0,3,1,3
;	.byte 0,1,2,3,1,3,1,2,0,2
;	.byte 1,0,3,2,0,2,0,3,1,3
;	.byte 0,3,0,3,1,3,1,2,0,2
;	.byte 2,1,2,1,3,1,3,0,2,0
;	.byte 0,3,0,3,1,3,2,1,0,2
;	.byte 2,1,2,1,3,0,3,0,3,0
;	.byte 3,0,3,0,2,1,2,1,2,1
	.byte 2,3,2,3,1,3,4,1,2,4
	.byte 4,1,4,1,3,1,3,2,4,2
	.byte 2,3,2,3,1,3,1,4,2,4
	.byte 1,2,3,4,2,4,2,3,1,3
	.byte 2,1,4,3,1,3,1,4,2,4
	.byte 1,4,1,4,2,4,2,3,1,3
	.byte 3,2,3,2,4,2,4,1,3,1
	.byte 1,4,1,4,2,4,3,2,1,3
	.byte 3,2,3,2,4,1,4,1,4,1
	.byte 4,1,4,1,3,2,3,2,3,2

p4  ; 10,10
;	.byte 1,3,0,2,0,2,2,1,1,3
;	.byte 3,1,2,0,2,0,2,1,3,1
;	.byte 1,1,2,2,0,2,0,3,1,3
;	.byte 1,3,0,2,2,0,2,1,3,1
;	.byte 0,2,1,3,3,1,3,0,2,0
;	.byte 0,0,3,3,1,3,1,2,0,2
;	.byte 2,0,3,1,3,1,3,0,2,0
;	.byte 0,2,1,3,1,3,3,0,0,2
;	.byte 2,0,3,1,3,3,1,2,0,0
;	.byte 3,1,2,0,2,2,0,3,1,1
	.byte 2,4,1,3,1,3,3,2,2,4
	.byte 4,2,3,1,3,1,3,2,4,2
	.byte 2,2,3,3,1,3,1,4,2,4
	.byte 2,4,1,3,3,1,3,2,4,2
	.byte 1,3,2,4,4,2,4,1,3,1
	.byte 1,1,4,4,2,4,2,3,1,3
	.byte 3,1,4,2,4,2,4,1,3,1
	.byte 1,3,2,4,2,4,4,1,1,3
	.byte 3,1,4,2,4,4,2,3,1,1
	.byte 4,2,3,1,3,3,1,4,2,2

p5  ; 6,6
;	.byte 3,1,2,1,2,0
;	.byte 1,3,0,3,0,2
;	.byte 0,2,3,0,1,3
;	.byte 1,3,2,1,0,2
;	.byte 0,2,1,2,1,3
;	.byte 2,0,3,0,3,1
	.byte 4,2,3,2,3,1
	.byte 2,4,1,4,1,3
	.byte 1,3,4,1,2,4
	.byte 2,4,3,2,1,3
	.byte 1,3,2,3,2,4
	.byte 3,1,4,1,4,2

p6  ; 12,12
;	.byte 0,3,0,1,2,1,2,1,2,3,0,3
;	.byte 2,2,1,0,3,3,0,0,3,2,1,1
;	.byte 0,2,0,1,3,1,2,0,2,3,1,3
;	.byte 1,3,1,0,2,0,3,1,3,2,0,2
;	.byte 3,3,0,1,2,2,1,1,2,3,0,0
;	.byte 1,2,1,0,3,0,3,0,3,2,1,2
;	.byte 2,1,2,3,0,3,0,3,0,1,2,1
;	.byte 0,0,3,2,1,1,2,2,1,0,3,3
;	.byte 2,0,2,3,1,3,0,2,0,1,3,1
;	.byte 3,1,3,2,0,2,1,3,1,0,2,0
;	.byte 1,1,2,3,0,0,3,3,0,1,2,2
;	.byte 3,0,3,2,1,2,1,2,1,0,3,0
	.byte 1,4,1,2,3,2,3,2,3,4,1,4
	.byte 3,3,2,1,4,4,1,1,4,3,2,2
	.byte 1,3,1,2,4,2,3,1,3,4,2,4
	.byte 2,4,2,1,3,1,4,2,4,3,1,3
	.byte 4,4,1,2,3,3,2,2,3,4,1,1
	.byte 2,3,2,1,4,1,4,1,4,3,2,3
	.byte 3,2,3,4,1,4,1,4,1,2,3,2
	.byte 1,1,4,3,2,2,3,3,2,1,4,4
	.byte 3,1,3,4,2,4,1,3,1,2,4,2
	.byte 4,2,4,3,1,3,2,4,2,1,3,1
	.byte 2,2,3,4,1,1,4,4,1,2,3,3
	.byte 4,1,4,3,2,3,2,3,2,1,4,1


; ==============================================================

; Tile graphics.  
;
; Define the first character image of a tile shape.  (8 bytes).
; The program will generate the other three characters on 
; demand by translating/flipping the first image.   
; 
; The characters are:
; |\    /|
; |1\  /4|
; |  \/  |
; |------|
; |  /\  |
; |2/  \3|
; |/    \|
;
;  Character tile 1 (lower left) is the one needing data.

gaTileImageListLow
	mLowByte gTile00,gTile01,gTile02,gTile03,gTile04,gTile05,gTile06

gaTileImageListHigh
	mHighByte gTile00,gTile01,gTile02,gTile03,gTile04,gTile05,gTile06

gTile00 ; Default.  Like the OS
	.byte %10000000
	.byte %11000000
	.byte %11100000
	.byte %11110000
	.byte %11111000
	.byte %11111100
	.byte %11111110
	.byte %11111111
	
gTile01 ; Like Default, but one less row
	.byte %00000000
	.byte %10000000
	.byte %11000000
	.byte %11100000
	.byte %11110000
	.byte %11111000
	.byte %11111100
	.byte %11111110

gTile02 ; Diagonal Line
	.byte %11000000
	.byte %11100000
	.byte %01110000
	.byte %00111000
	.byte %00011100
	.byte %00001110
	.byte %00000111
	.byte %00000011

gTile03 ; Corner 3
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %11110000
	.byte %11110000
	.byte %11110000
	.byte %11110000

gTile04 ; corner + outline
	.byte %00000000
	.byte %00000000
	.byte %11111110
	.byte %00000110
	.byte %00000110
	.byte %11100110
	.byte %11100110
	.byte %11100110

gTile05 ; Pointy out
	.byte %10000000
	.byte %11000000
	.byte %11111100
	.byte %11111100
	.byte %11111100
	.byte %11111100
	.byte %11111110
	.byte %11111111

gTile06 ; Round 
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %11100000
	.byte %00110000
	.byte %00011000
	.byte %00001100
	.byte %00001100

; ==============================================================

; We need space for the custom character set.   It must be 
; on a 1K boundary 

	.align 1024 

gCSetMemory
	.byte 0,0,0,0,0,0,0,0 ; guarantee that blank space is blank.

gTileImage1   ; Just declare where the first tile image goes.

gTileImage2 = gTileImage1 + 8; and the other images...
gTileImage3 = gTileImage2 + 8
gTileImage4 = gTileImage3 + 8


; ==============================================================

; Screen memory requires more than 1K now that overscan is 
; involved.  So to prevent crossing a 4K boundary, align the 
; next memory reference to a 2K boundary.   2K + 1440 bytes is 
; less than a 4K boundary.

	.align 2048 

; Since nothing follows the screen memory the program does not 
; need to explicitly reserve space or declare the .bytes used 
; for screen memory. 
; The display list will just start reading from this location 
; here and continue forward.   
; The tile writing code will do the same assuming this memory 
; is available.

gScreenMemory     ; Just defining where this occurs in memory.


; ==============================================================

; Codeless, Poking-From-Disk. 
;
; Exploit the Atari's segmented binary loading format to perform
; the one-time initialization code.   In the original code this
; was a series of LDA/STA instructions.   Here we are getting 
; the file loader to lay these bytes directly into memory from 
; the disk file.

; One-Time Initialization.

	ORG COLOR0           ; Load all the color registers.
	
	.byte $00            ; COLOR0/COLPF0 (not used in Mode 2).
	.byte COLOR_BLACK|$0 ; COLOR1/COLPF1 - text luminance in Mode 2. Color 0, Luminance $0 
	.byte COLOR_BLACK|$E ; COLOR2/COLPF2 - background in Mode 2.     Color 0, Luminance $E
	.byte $00            ; COLOR3/COLPF3 (not used in Mode 2).
	.byte COLOR_BLACK|$0 ; COLOR4/COLBK  - border in Mode 2.  Color 0, Luminance $0


; We have to start the Atari's custom screen display by updating
; the ANTIC Display List pointers.  To do this properly we have
; to guarantee the address bytes are updated in the same frame, 
; OR the screen DMA must be turned off.  
; It's easier to do the latter.

	mDiskPoke SDMCTL,DISABLE_DL_DMA             ; Turn off...screen DMA.

	mDiskDPoke SDLSTL,zDisplayList              ; Set new Display List address

	mDiskPoke SDMCTL,ENABLE_DL_DMA|PLAYFIELD_WIDTH_WIDE ; Turn on screen DMA. 

;	mDiskpoke CONSOL,0

	mDiskPoke CHBAS,>gCSetMemory                  ; Set custom character set

; This is how we make the executable automatically run on the 
; Atari.  Store the address of the start of the code to run in
; DOS_RUN_ADDR/$02E0.  When file loading completes DOS will 
; execute this address.


;	ORG DOS_RUN_ADDR

;	.word StartTruchet

	mDiskDPoke DOS_RUN_ADDR,StartTruchet

	END

; ==============================================================

