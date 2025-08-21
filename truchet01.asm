; ==============================================================
; Truchet Tiles
; ==============================================================
;
; Version 01 -- 2025-08-19 -- Ken Jennings
;
; WORK IN PROGRESS . . .
;
; Port to Atari from "8-bit Show And Tell" C64 version.
; Atari-fying the code and features.
;
; Source uses MADS assembler syntax.
; 
; Code now depends on the Atari includes for sensible 
; symbols and other goodies.   Get them here:
; https://github.com/kenjennings/Atari-Mads-Includes
;
; %>  mads truchet01.asm -o:truchet01.xex
;
; ==============================================================
;
; Version 00:
;
; Direct port of the C64 code with as few changes as possible.
; Seriously, intentionally going out of my way to make as few 
; changes as possible to the 6502 assembly code except where 
; necessary.
;
; ==============================================================
;
; Version 01:  WORK IN PROGRESS . . . 
;
; Atari-fication...
; 
; * Some easy stuff first: Deleted the C64-specific work.
;
; * Start using the include files and proper Atari symbols.
;
; * Use an overscan screen.  In memory this is a 48 character 
;   by 30 line text display. (1440 bytes)
;
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

SCREEN_WIDTH  = 48      ; Screen width in characters.
SCREEN_HEIGHT = 30      ; screen height in lines.

NUM_PATTERNS  = 30      ; number of tile patterns the program draws

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

DISPLAY_LIST
	.byte $42         ; Load Mamory Scan + Mode 2 text
	.word SCREEN_MEMORY ; Start reading the screen at $0400
	.byte $02,$02,$02,$02,$02,$02,$02,$02 ; 8 lines Mode 2 text
	.byte $02,$02,$02,$02,$02,$02,$02,$02 ; 8 lines Mode 2 text
	.byte $02,$02,$02,$02,$02,$02,$02,$02 ; 8 lines Mode 2 text
	.byte $02,$02,$02,$02,$02             ; 5 lines Mode 2 text
	.byte $41          ; Display List do Vertical Blank and...
	.word DISPLAY_LIST ; ...start at the beginning of the list.

; ==============================================================

; Program Variables in Page 0

	ORG $EE

SCREEN_X       .byte 0 ; at $ee - screen x position
SCREEN_Y       .byte 0 ; at $ef - screen y position
PATTERN_NUM    .byte 0 ; at $f0 - current pattern number
PATTERN_X      .byte 0 ; at $f1
PATTERN_Y      .byte 0 ; at $f2
PATTERN_WIDTH  .byte 0 ; at $f3
PATTERN_HEIGHT .byte 0 ; at $f4
PATTERN_LO     .byte 0 ; at $f5
PATTERN_HI     .byte 0 ; at $f6


; On Atari:
; 0 == internal code $4A -- lower left triangle.
; 1 == internal code $C8 -- upper left triangle (inverse video $48)
; 2 == internal code $CA -- upper right triangle (inverse video $4A)
; 3 == internal code $48 -- lower right triangle 

TILE_CODES  .byte $4A,$C8,$CA,$48 ; at $f7 to $fa

SCREEN_POINTER  .word 0 ; at $fb/$fc
PATTERN_POINTER .word 0 ; at $fd/$fe

; ==============================================================

; Now we can get to the real coding.

; Just pick any spot conveniently after DOS...

	ORG $3800 

gbStart

; One-Time Initialization.

;	 white background; black text

	 lda #$08    ; Color 0, luminance $8
	 sta $02c8   ; COLOR4/COLBK border in Mode 2
	 lda #$0E    ; Color 0, luminance $E
	 sta $02c6   ; COLOR2/COLPF2 background in Mode 2
	 lda #$00    ; Color 0, Luminance $0
	 sta $02c5   ; COLOR1/COLPF1 text luminance in Mode 2

; Last thing specific to the Atari...  
; We have to start the Atari's custom screen display by updating
; the ANTIC Display List pointers.  To do this properly we have
; to guarantee the address bytes are updated in the same frame, 
; OR the screen DMA must be turned off.  
; It's easier to do the latter.

	lda #DISABLE_DL_DMA             ; Turn off...
	sta SDMCTL          ; screen DMA.  Atari SDMCTL

	lda #<DISPLAY_LIST ; Set new Display List address
	sta SDLSTL          ; Atari SDLSTL
	lda #>DISPLAY_LIST
	sta SDLSTH          ; Atari SDLSTH

	lda #ENABLE_DL_DMA|PLAYFIELD_WIDTH_WIDE            ; Turn on...
	sta SDMCTL         ; screen DMA.  Atari SDMCTL

; ==============================================================

; the part of the program that loops forever . . .

	; get pattern pointer (array defined later)
initdraw
	lda PATTERN_NUM      ; A = Pattern Number  (to be used as array index) 
	asl             ; A = A * 2  (pointers are two byte addresses)
	tax             ; X = A
	lda PATTERN_LIST,x   ; A = get low byte of pointer from array
	sta PATTERN_POINTER      ; save in page 0 working variable
	lda PATTERN_LIST+1,x ; A = get high byte of pointer from array
	sta PATTERN_POINTER+1    ; save in page 0 working variable

	; get pattern width and height from the array
	ldy #0
	lda (PATTERN_POINTER),Y  ; A = *(PATTERN_POINTER + Y) - Get byte through page 0 pointer
	sta PATTERN_WIDTH        ; Save as the pattern width
	iny             ; increment to next position
	lda (PATTERN_POINTER),Y  ; A = *(PATTERN_POINTER + Y) - Get byte through page 0 pointer
	sta PATTERN_HEIGHT        ; Save as the pattern height

	; point to actual pattern -- Add 2 to the address to 
	; skip over the X,Y size values.
	clc
	lda PATTERN_POINTER      ; A = Pattern Pointer low byte
	adc #2          ; A = A + 2  (size of X and Y entries)
	sta PATTERN_POINTER      ; save adjusted low byte of pattern pointer
	sta PATTERN_LO       ; and save it again in working area
	; Since there has to be a load and store in two variables
	; for the high byte, there's little to be gained by 
	; optimizing the handling of high byte.
	lda PATTERN_POINTER+1    ; A = Pattern Pointer high byte
	adc #0          ; If carry is set this increments high byte
	sta PATTERN_POINTER+1    ; save adjusted high byte of pattern pointer
	sta PATTERN_HI       ; and save it again in working area.

	; initialize the variables for drawing
	lda #0          ; A = 0
	sta SCREEN_X        ; Screen X
	sta SCREEN_Y        ; Screen Y
	sta PATTERN_X        ; Pattern X
	sta PATTERN_Y        ; Pattern y

	; Reset working screen pointer to the screen memory address
	lda #<SCREEN_MEMORY
	sta SCREEN_POINTER
	lda #>SCREEN_MEMORY
	sta SCREEN_POINTER+1

; ==============================================================

	; Draw patterns into screen memory until finished...
drawloop
	ldy PATTERN_X        ; Y = Pattern X position
	lda (PATTERN_POINTER),Y  ; A = *(PATTERN_POINTER + Y) - tile ID number from pattern
	tax             ; X = tile ID number
	lda TILE_CODES,x     ; A =  character from tile array ( tile[ x ] )
	ldy SCREEN_X        ; Y = Screen X position
	sta (SCREEN_POINTER),y  ; Screen byte *(SCREEN_POINTER + Y) = tile character 

	; Next pattern column
	inc PATTERN_X        ; Pattern X++
	lda PATTERN_X        ; A = Pattern X
	cmp PATTERN_WIDTH        ; If A == Pattern Width ?
	bcc nextscrc    ; No, Continue with next screen column
	lda #0          ; A = 0 
	sta PATTERN_X        ; Reset Pattern X to start

	; Move to next screen column
nextscrc
	inc SCREEN_X        ; Screen X++
	lda SCREEN_X        ; A = Screen X
	cmp #SCREEN_WIDTH       ; If A == Screen X width ?
	bcc drawloop    ; No, Continue with drawing the next position

	; Completed screen Row... Reset X positions
	lda #0          ; A = 0
	sta PATTERN_X        ; Reset Pattern X to start
	sta SCREEN_X        ; Reset Screen X to start

	; Completed screen Row... Move to next pattern row
	clc
	lda PATTERN_POINTER      ; A = Pattern pointer low byte
	adc PATTERN_WIDTH        ; A = A + Pattern Width
	sta PATTERN_POINTER      ; Save new  low byte in Pattern pointer
	lda PATTERN_POINTER+1    ; A = Pattern pointer high bytye
	adc #0          ; If carry was set during low byte addition, add one
	sta PATTERN_POINTER+1    ; Save new  high byte in Pattern pointer

	inc PATTERN_Y        ; Pattern Y++
	lda PATTERN_Y        ; A = Pattern Y
	cmp PATTERN_HEIGHT        ; If A == Pattern Y height ?
	bcc nextscrr    ; No, Continue with drawing the next screen row

	; Completed pattern, reset to start again
	lda #0          ; A = 0
	sta PATTERN_Y        ; Reset Pattern Y to start

	lda PATTERN_LO       ; Copy Pattern pointer...
	sta PATTERN_POINTER      ; to low byte working pointer
	lda PATTERN_HI       ; Copy Pattern pointer...
	sta PATTERN_POINTER+1    ; to high byte working pointer

	; Move to next screen row
nextscrr
	clc
	lda SCREEN_POINTER      ; A = Screen pointer low byte
	adc #SCREEN_WIDTH       ; A = A + Screen Width
	sta SCREEN_POINTER      ; Save new  low byte in Screen pointer
	lda SCREEN_POINTER+1    ; A = Screen pointer high bytye
	adc #0          ; If carry was set during low byte addition, add one
	sta SCREEN_POINTER+1    ; Save new  high byte in Screen pointer

	inc SCREEN_Y        ; Screen Y++
	lda SCREEN_Y        ; A = Screen Y
	cmp #SCREEN_HEIGHT       ; If A == Screen Y height ?
	bcc drawloop    ; No, Continue with drawing the next position

; ==============================================================

	; Completed drawing screen.   Wait for a keypress.

	lda #255   ; Atari - Clear last keypress. 255 == No Key.
	sta 764    ; Atari - CH == Most recent key pressed.  

keywait
;	jsr $ffe4  - C64 wait for keypress
	lda 764         ; Atari - Read OS register for last keypress
	cmp #255        ; Is a key pressed?  255 == No Key.
	beq keywait     ; No, keep looping here.
; Note that when someone presses a key on the Atari's keyboard 
; the color cycling Attract Mode will also reset.

; ==============================================================

	; Run the next pattern after a key is pressed
	inc PATTERN_NUM      ; Pattern Number++
	lda PATTERN_NUM      ; A = Pattern Number
	cmp #NUM_PATTERNS     ; If A == the end of the pattern list ?
	bcc done1       ; No.  Skip resetting the pattern number

	; wrap back to the first pattern.
	lda #0          ; A = 0
	sta PATTERN_NUM      ; Save to Pattern Number

done1
	jmp initdraw    ; Back to the beginning

; ==============================================================

; List of pointers to each structure for the patterns' tile descriptions.
PATTERN_LIST
	.word pa,pb,pc,pd,pe,pf
	.word pg,ph,pi,pl,pm,pn
	.word po,pp,pq,pr,ps,pt
	.word pv,pu,px,py,pz,pamp
	.word p1,p2,p3,p4,p5,p6

; Each of the Patterns' tile descriptions.
; First Byte  = X width
; Second Byte = Y height
; Remaining   =  X * Y number of bytes containing the tile ID numbers.

;For Reference, the tile IDs again:
; On Atari:
; 0 == internal code $4A -- lower left triangle.
; 1 == internal code $C8 -- upper left triangle (inverse video $48)
; 2 == internal code $CA -- upper right triangle (inverse video $4A)
; 3 == internal code $48 -- lower right triangle 

pa
	.byte 1,1
	.byte 0
pb
	.byte 1,2
	.byte 0,2
pc
	.byte 2,2
	.byte 0,2
	.byte 2,0
pd
	.byte 2,2
	.byte 1,0
	.byte 2,3
pe
	.byte 4,4
	.byte 1,2,3,0
	.byte 2,1,0,3
	.byte 3,0,1,2
	.byte 0,3,2,1
pf
	.byte 2,2
	.byte 1,2
	.byte 0,3
pg
	.byte 2,4
	.byte 0,2
	.byte 2,0
	.byte 2,0
	.byte 0,2
ph
	.byte 3,3
	.byte 1,3,3
	.byte 3,1,3
	.byte 3,3,1
pi
	.byte 4,4
	.byte 2,0,0,2
	.byte 0,2,2,0
	.byte 0,2,2,0
	.byte 2,0,0,2
pl
	.byte 4,4
	.byte 3,0,3,0
	.byte 2,3,0,1
	.byte 3,2,1,0
	.byte 2,1,2,1
pm
	.byte 2,2
	.byte 1,2
	.byte 2,1
pn
	.byte 4,4
	.byte 1,0,3,2
	.byte 2,3,0,1
	.byte 3,2,1,0
	.byte 0,1,2,3
po
	.byte 4,2
	.byte 3,0,1,2
	.byte 1,2,3,0
pp
	.byte 2,2
	.byte 1,2
	.byte 3,0
pq
	.byte 4,2
	.byte 3,3,0,0
	.byte 1,1,2,2
pr
	.byte 2,1
	.byte 3,0
ps
	.byte 4,1
	.byte 3,0,1,2
pt
	.byte 4,1
	.byte 1,3,0,2
pv
	.byte 8,8
	.byte 2,0,3,0,2,0,1,0
	.byte 0,3,0,2,0,1,0,2
	.byte 3,0,2,0,1,0,2,0
	.byte 0,2,0,1,0,2,0,3
	.byte 2,0,1,0,2,0,3,0
	.byte 0,1,0,2,0,3,0,2
	.byte 1,0,2,0,3,0,2,0
	.byte 0,2,0,3,0,2,0,1
pu
	.byte 4,4
	.byte 1,3,0,2
	.byte 3,0,2,1
	.byte 0,2,1,3
	.byte 2,1,3,0
px
	.byte 6,6
	.byte 2,0,3,0,0,2
	.byte 2,1,2,2,0,0
	.byte 3,0,0,2,2,0
	.byte 2,2,0,0,2,1
	.byte 0,2,2,0,3,0
	.byte 0,0,2,1,2,2
py
	.byte 4,4
	.byte 1,3,0,2
	.byte 3,1,2,0
	.byte 2,0,3,1
	.byte 0,2,1,3
pz
	.byte 4,2
	.byte 1,2,0,3
	.byte 3,1,2,0
pamp
	.byte 4,4
	.byte 1,3,2,0
	.byte 3,1,0,2
	.byte 0,2,3,1
	.byte 2,0,1,3
p1
	.byte 8,8
	.byte 0,2,0,2,1,3,1,3
	.byte 1,3,1,3,0,2,0,2
	.byte 3,1,3,1,2,0,2,0
	.byte 1,3,1,3,0,2,0,2
	.byte 3,1,3,1,2,0,2,0
	.byte 2,0,2,0,3,1,3,1
	.byte 0,2,0,2,1,3,1,3
	.byte 2,0,2,0,3,1,3,1
p2
	.byte 6,6
	.byte 3,0,2,1,3,0
	.byte 0,2,0,3,1,3
	.byte 2,0,1,2,3,1
	.byte 3,1,0,3,2,0
	.byte 1,3,1,2,0,2
	.byte 2,1,3,0,2,1
p3
	.byte 10,10
	.byte 1,2,1,2,0,2,3,0,1,3
	.byte 3,0,3,0,2,0,2,1,3,1
	.byte 1,2,1,2,0,2,0,3,1,3
	.byte 0,1,2,3,1,3,1,2,0,2
	.byte 1,0,3,2,0,2,0,3,1,3
	.byte 0,3,0,3,1,3,1,2,0,2
	.byte 2,1,2,1,3,1,3,0,2,0
	.byte 0,3,0,3,1,3,2,1,0,2
	.byte 2,1,2,1,3,0,3,0,3,0
	.byte 3,0,3,0,2,1,2,1,2,1
p4
	.byte 10,10
	.byte 1,3,0,2,0,2,2,1,1,3
	.byte 3,1,2,0,2,0,2,1,3,1
	.byte 1,1,2,2,0,2,0,3,1,3
	.byte 1,3,0,2,2,0,2,1,3,1
	.byte 0,2,1,3,3,1,3,0,2,0
	.byte 0,0,3,3,1,3,1,2,0,2
	.byte 2,0,3,1,3,1,3,0,2,0
	.byte 0,2,1,3,1,3,3,0,0,2
	.byte 2,0,3,1,3,3,1,2,0,0
	.byte 3,1,2,0,2,2,0,3,1,1
p5
	.byte 6,6
	.byte 3,1,2,1,2,0
	.byte 1,3,0,3,0,2
	.byte 0,2,3,0,1,3
	.byte 1,3,2,1,0,2
	.byte 0,2,1,2,1,3
	.byte 2,0,3,0,3,1
p6
	.byte 12,12
	.byte 0,3,0,1,2,1,2,1,2,3,0,3
	.byte 2,2,1,0,3,3,0,0,3,2,1,1
	.byte 0,2,0,1,3,1,2,0,2,3,1,3
	.byte 1,3,1,0,2,0,3,1,3,2,0,2
	.byte 3,3,0,1,2,2,1,1,2,3,0,0
	.byte 1,2,1,0,3,0,3,0,3,2,1,2
	.byte 2,1,2,3,0,3,0,3,0,1,2,1
	.byte 0,0,3,2,1,1,2,2,1,0,3,3
	.byte 2,0,2,3,1,3,0,2,0,1,3,1
	.byte 3,1,3,2,0,2,1,3,1,0,2,0
	.byte 1,1,2,3,0,0,3,3,0,1,2,2
	.byte 3,0,3,2,1,2,1,2,1,0,3,0

; ==============================================================

; Screen memory requires more than 1K now.  So to prevent 
; crossing a 4K boundary, align the next memory reference to a
; 2K boundary.   2K + 1440 bytes is less than a 4K boundary.

	.align 2048 

; We don't need to reserve space, because the display list will
; just start reading here and keep on going.   The tile writing
; code will just do the same and assume this memory is available.

SCREEN_MEMORY 

; ==============================================================

; This is how we make the executable automatically run on the 
; Atari.  Store the address of the start of the code to run in
; DOS_RUN_ADDR/$02E0.  When file loading completes DOS will 
; execute this address.

	ORG $02e0    ; DOS_RUN_ADDR

	.word gbStart

	END

; ==============================================================

