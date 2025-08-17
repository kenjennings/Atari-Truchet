; ==============================================================
; Truchet Tiles
; ==============================================================
;
; Port to Atari from "8-bit Show And Tell" C64 version.
;
; Version 00 -- 2025-08-16 -- Ken Jennings
;
; Source uses MADS assembler syntax.
;
; %>  mads truchet00.asm -o:truchet.xex
;
; ==============================================================
;
; Direct port of the C64 code with as few changes as possible.
; Seriously, intentionally going out of my way to make as few 
; changes as possible to the 6502 assembly code except where 
; necessary.
;
; ==============================================================
;
; Some C64-exclusive behaviors, such as the way the machine code
; is attached to a BASIC program and started with a SYS command 
; naturally have to be bypassed.
;
; The original C64 program stuffs bytes directly to screen RAM.
; Since no OS printing occurs the Atari program can exploit some
; of its graphics flexibility to define a custom screen using 
; the same number of lines as the C64.  (Yes, the Atari can 
; display 25 lines of text, and more, no problem.)
; Therefore, the screen size/writing code will remain nearly 
; identical.  In fact, we can even have the Atari set up its
; custom screen using the same memory address as the C64.
;
; The Atari's segmented executable file format has the advantage
; of directing where values are loaded into memory without 
; needing to contain bytes for any intervening, unused space 
; between blocks of defined memory or code.  The program will 
; use this feature when declaring and defining space.  The 
; actual 6502 code will be as unchanged as possible.
;
; ==============================================================

; Let's Begin . . .

; The C64 code here to start the program from BASIC is not done 
; on the Atari.  Skip to the end of the assembly code to see how
; the Atari can load and automatically execute a machine 
; language program without the user needing to type anything.

	; * = $0801    -- C64 BASIC program start in memory

	; .word $080b  -- C64 BASIC's pointer to next line
	; .word 2024   -- C64 BASIC line number 2024
	; .byte $9e    -- C64 BASIC token for SYS command
	; .text "2061" -- "2061" is startin ML address. (aka, $080d)
	; .byte 0      -- C64 BASIC end of line byte
	; .word 0      -- C64 BASIC end of program (address)

	; jmp start    -- Discussed Above; Atari starts differently.

scr      = $0400   ; C64 default screen memory. Will also use for Atari.
col      = $d800   ; C64 color table.  Unused on Atari. 

scrw     = 40      ; Screen width in characters.
scrh     = 25      ; screen height in lines.

numpat   = 30      ; number of tile patterns the program draws

; The C64 version declares a few symbols for page 0 addresses
; and defines some space for working variables.
; We're going to use the Atari's segmented file format to 
; simultaneously declare symbols, define space, and pre-load
; data values.  And, since we're dropping all these working
; variables into page 0, the assembler will use page 0 
; references making the overall machine language smaller and
; faster. 

	ORG $EE   ; Yes, we are disk loading directly into page 0

; Defining, declaring, and loading data at the same time...

scrx   .byte 0 ; at $ee - screen x position
scry   .byte 0 ; at $ef - screen y position
patnum .byte 0 ; at $f0 - current pattern number
patx   .byte 0 ; at $f1
paty   .byte 0 ; at $f2
patw   .byte 0 ; at $f3
path   .byte 0 ; at $f4
patlo  .byte 0 ; at $f5
pathi  .byte 0 ; at $f6

; On C64:
; tiles .byte 223,105,95,233
; which is 
; 0 == lower left triangle,
; 1 == upper left triangle
; 2 == upper right triangle
; 3 == lower right triangle.

; On Atari:
; 0 == internal code $4A -- lower left triangle.
; 1 == internal code $C8 -- upper left triangle (inverse video $48)
; 2 == internal code $CA -- upper right triangle (inverse video $4A)
; 3 == internal code $48 -- lower right triangle 

tiles  .byte $4A,$C8,$CA,$48 ; at $f7 to $fa

scrptr .word 0 ; at $fb/$fc
patptr .word 0 ; at $fd/$fe

; This part is Atari-specific.
; Build a custom display list for a 25 text line display.
; And, we're abusing page 0 already, so let's just put the 
; display list there, too.  Because we can.  Just 33 bytes...

	ORG $C0

DISPLAY_LIST
	.byte $70,$70,$30 ; 8 + 8 + 4  blank scan lines
	.byte $42         ; Load Mamory Scan + Mode 2 text
	.word scr         ; Start reading the screen at $0400
	.byte $02,$02,$02,$02,$02,$02,$02,$02 ; 8 lines Mode 2 text
	.byte $02,$02,$02,$02,$02,$02,$02,$02 ; 8 lines Mode 2 text
	.byte $02,$02,$02,$02,$02,$02,$02,$02 ; 8 lines Mode 2 text
	.byte $41          ; Display List do Vertical Blank and...
	.word DISPLAY_LIST ; ...start at the beginning of the list.

; ==============================================================

; Now we can get to the real coding.

; The Atari does not tie machine language programs to BASIC.
; Machine language programs are stand-alone, so here set where 
; the machine language code will reside.

; Just pick any spot conveniently after DOS...

	ORG $3800 

start
	; C64-specific stuff is commented out. 
	; Code not needed becausde of the way the Atari works is 
	; also commented out.

	; start at pattern number 0
;	lda #0
;	sta patnum   ; On the Atari, 0 was established at load time.

	; make sure we're in upper case.
;	lda #21   ; Specific to C64.
;	sta 53272 ; Specific to C64.

	; med grey border, white background; black text
;	 lda #12
;	 sta $d020
;	 lda #1
;	 sta #d021
	 ; Atari version of setting colors
	 lda #$08    ; Color 0, luminance $8
	 sta $02c8   ; COLOR4/COLBK border in Mode 2
	 lda #$0E    ; Color 0, luminance $E
	 sta $02c6   ; COLOR2/COLPF2 background in Mode 2

	; This is only specific to C64...
	; set black characters  (Fill the color table)
;	lda #0        ; 0 is black
;	ldx #250      ; copying 250 times.
;loop1
;	dex           ; decrement loop
;	sta col,x     ; write 0 to color table (first section)
;	sta col+250,x ; write 0 to color table (first section)
;	sta col+500,x ; write 0 to color table (first section)
;	sta col+750,x ; write 0 to color table (first section)
;	bne loop1     ; Zero flag set by DEX, (it's not set by STA)

	 ; Setting Black Text on Atari:
	 lda #$00    ; Color 0, Luminance $0
	 sta $02c5   ; COLOR1/COLPF1 text luminance in Mode 2

; Last thing specific to the Atari...  
; We have to start the Atari's custom screen display by updating
; the ANTIC Display List pointers.  To do this properly we have
; to guarantee the address bytes are updated in the same frame, 
; OR the screen DMA must be turned off.  
; It's easier to do the latter.

	lda #0             ; Turn off...
	sta $022F          ; screen DMA.  Atari SDMCTL

	lda #<DISPLAY_LIST ; Set new Display List address
	sta $0230          ; Atari SDLSTL
	lda #>DISPLAY_LIST
	sta $0231          ; Atari SDLSTH

	lda #34            ; Turn on...
	sta $022F          ; screen DMA.  Atari SDMCTL

; ==============================================================

; the part of the program that loops forever . . .

	; get pattern pointer (array defined later)
initdraw
	lda patnum      ; A = Pattern Number  (to be used as array index) 
	asl             ; A = A * 2  (pointers are two byte addresses)
	tax             ; X = A
	lda patptrs,x   ; A = get low byte of pointer from array
	sta patptr      ; save in page 0 working variable
	lda patptrs+1,x ; A = get high byte of pointer from array
	sta patptr+1    ; save in page 0 working variable

	; get pattern width and height from the array
	ldy #0
	lda (patptr),Y  ; A = *(patptr + Y) - Get byte through page 0 pointer
	sta patw        ; Save as the pattern width
	iny             ; increment to next position
	lda (patptr),Y  ; A = *(patptr + Y) - Get byte through page 0 pointer
	sta path        ; Save as the pattern height

	; point to actual pattern -- Add 2 to the address to 
	; skip over the X,Y size values.
	clc
	lda patptr      ; A = Pattern Pointer low byte
	adc #2          ; A = A + 2  (size of X and Y entries)
	sta patptr      ; save adjusted low byte of pattern pointer
	sta patlo       ; and save it again in working area
	; Since there has to be a load and store in two variables
	; for the high byte, there's little to be gained by 
	; optimizing the handling of high byte.
	lda patptr+1    ; A = Pattern Pointer high byte
	adc #0          ; If carry is set this increments high byte
	sta patptr+1    ; save adjusted high byte of pattern pointer
	sta pathi       ; and save it again in working area.

	; initialize the variables for drawing
	lda #0          ; A = 0
	sta scrx        ; Screen X
	sta scry        ; Screen Y
	sta patx        ; Pattern X
	sta paty        ; Pattern y

	; Reset working screen pointer to the screen memory address
	lda #<scr
	sta scrptr
	lda #>scr
	sta scrptr+1

; ==============================================================

	; Draw patterns into screen memory until finished...
drawloop
	ldy patx        ; Y = Pattern X position
	lda (patptr),Y  ; A = *(patptr + Y) - tile ID number from pattern
	tax             ; X = tile ID number
	lda tiles,x     ; A =  character from tile array ( tile[ x ] )
	ldy scrx        ; Y = Screen X position
	sta (scrptr),y  ; Screen byte *(scrptr + Y) = tile character 

	; Next pattern column
	inc patx        ; Pattern X++
	lda patx        ; A = Pattern X
	cmp patw        ; If A == Pattern Width ?
	bcc nextscrc    ; No, Continue with next screen column
	lda #0          ; A = 0 
	sta patx        ; Reset Pattern X to start

	; Move to next screen column
nextscrc
	inc scrx        ; Screen X++
	lda scrx        ; A = Screen X
	cmp #scrw       ; If A == Screen X width ?
	bcc drawloop    ; No, Continue with drawing the next position

	; Completed screen Row... Reset X positions
	lda #0          ; A = 0
	sta patx        ; Reset Pattern X to start
	sta scrx        ; Reset Screen X to start

	; Completed screen Row... Move to next pattern row
	clc
	lda patptr      ; A = Pattern pointer low byte
	adc patw        ; A = A + Pattern Width
	sta patptr      ; Save new  low byte in Pattern pointer
	lda patptr+1    ; A = Pattern pointer high bytye
	adc #0          ; If carry was set during low byte addition, add one
	sta patptr+1    ; Save new  high byte in Pattern pointer

	inc paty        ; Pattern Y++
	lda paty        ; A = Pattern Y
	cmp path        ; If A == Pattern Y height ?
	bcc nextscrr    ; No, Continue with drawing the next screen row

	; Completed pattern, reset to start again
	lda #0          ; A = 0
	sta paty        ; Reset Pattern Y to start

	lda patlo       ; Copy Pattern pointer...
	sta patptr      ; to low byte working pointer
	lda pathi       ; Copy Pattern pointer...
	sta patptr+1    ; to high byte working pointer

	; Move to next screen row
nextscrr
	clc
	lda scrptr      ; A = Screen pointer low byte
	adc #scrw       ; A = A + Screen Width
	sta scrptr      ; Save new  low byte in Screen pointer
	lda scrptr+1    ; A = Screen pointer high bytye
	adc #0          ; If carry was set during low byte addition, add one
	sta scrptr+1    ; Save new  high byte in Screen pointer

	inc scry        ; Screen Y++
	lda scry        ; A = Screen Y
	cmp #scrh       ; If A == Screen Y height ?
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
	inc patnum      ; Pattern Number++
	lda patnum      ; A = Pattern Number
	cmp #numpat     ; If A == the end of the pattern list ?
	bcc done1       ; No.  Skip resetting the pattern number

	; wrap back to the first pattern.
	lda #0          ; A = 0
	sta patnum      ; Save to Pattern Number

done1
	jmp initdraw    ; Back to the beginning

; ==============================================================

; List of pointers to each structure for the patterns' tile descriptions.
patptrs
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

; This is how we make the executable automatically run on the 
; Atari.  Store the address of the start of the code to run in
; DOS_RUN_ADDR/$02E0.  When file loading completes DOS will 
; execute this address.

	ORG $02e0    ; DOS_RUN_ADDR

	.word start

	END

; ==============================================================

