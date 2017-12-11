.model  SMALL

.stack  100H			; start the stack at SS:100H

.data
			COMPARE_VAR 	DW	?
			SQRTBITS  db	00000000B
			ID				equ	385H
			LINEBITS	db	00000000B
.code

START:
	mov ax, @data						; initialize data
	mov ds, ax

; I'm going to use Newton's Square Root Approximation
;	which basically is new_approximation = ( (ID / old_approximation) + old_approximation ) / 2

	mov bx, 1								; let's start from old_approximation = 1

SQRT:
	mov dx, 0								; reset the remainder
	mov ax, ID							; mov last three digits of ID in ax register
	div bx									; do ax = ax / bx ; which is basically (ID / old_approximation)

	add ax, bx							; do ax = ax + bx ; which is ((ID / old_approximation) + old_approximation)
	shr ax, 1								; shifts all bits to right by 1 ; which means divide ax by 2

	mov COMPARE_VAR, ax			;	put new approximation in COMPARE_VAR

	sub COMPARE_VAR, bx			;	new_approximation - old_approximation ; diff between approximations
	xchg ax, bx							; swap ax with bx; so now bx is becoming an old approximation.
	cmp COMPARE_VAR, 0			; compare approximations diff with 0
	jne SQRT								; if it's not 0 then let's start from sqrt again
	mov SQRTBITS, bl				;	if it's 0 then let's store the last approximation result (which is bx) in SQRTBITS

SIMULATION:
	; So we have 5 lines of operations
	; This is the first one
	mov al, SQRTBITS				; start the simulation
	mov LINEBITS, 00000000B	;	reset LINEBITS
	call NOR								; do the first operation 
	call SAVELINE						; save 0, 1 bit	; the result of NOR
	shr al, 2								; remove bits 0, 1 ; those are used in NOR
	mov bl, al							; copy al in bl register
	and bl, 00000001B				; get only 2nd bit of SQRTBITS
	call SAVELINE						; save 2nd bit	; no operation on this bit
	shr al, 1								; remove 2nd bit ; that is already backed up
	mov bl, al							; copy al in bl register
	and bl, 00000001B				; get only 3nd bit of SQRTBITS
	call SAVELINE						; save 3rd bit	; no operation on this bit
	shr al, 1								; remove 3rd bit ; no operation on this bit
	call _XOR								; do XOR for 4th and 5th bits
	call SAVELINE						; save 4, 5 bit	; the result of XOR
	shr al, 2								; remove bits 4, 5 ; those are used in XOR
	mov bl, al							; copy al in bl register
	and bl, 00000001B				; get only 6th bit of SQRTBITS
	call SAVELINE						; save 6th bit	;	no operation on this bit
	shr al, 1								; remove 6th bit ; 
	mov bl, al							; copy al in bl register
	and bl, 00000001B				; get only 7th bit of SQRTBITS
	call SAVELINE						; save 7th bit	;	no operation on this bit
	; LINEBITS has 6 bits in it now
	; finished the first line, continue to the second
	mov al, LINEBITS				; get saved LINEBITS from the first line to al
	mov LINEBITS, 00000000B	;	reset LINEBITS
	call NAND								; call NAND for 0 and 1 bit of LINEBITS
	call SAVELINE						; save 0, 1 bits in LINEBITS
	shr al, 2								; remove 0, 1 bits that are used in NAND
	call _NOT								; call _NOT for 2nd bit of LINEBITS
	call SAVELINE						; save 2nd bit in LINEBITS
	shr al, 1								;	remove 2nd bit that is used in _NAND
	mov bl, al							; copy al in bl register
	and bl, 00000001B				; get only 3rd bit of LINEBITS
	call SAVELINE						; save 3rd bit ; no operation on this bit
	shr al, 1								; remove 3rd bit
	call NAND								; apply NAND on 4th and 5th bits of LINEBITS
	call SAVELINE						; save the result of NAND on 4th and 5 bits in LINEBITS	
	; updated LINEBITS has 4 bits now
	; finished the second line, continue to the third one
	mov al, LINEBITS				; get saved LINEBITS from the second line to al
	mov LINEBITS, 00000000B	; reset LINEBITS
	call _OR								; call _OR on 0 and 1 bit of LINEBITS
	call SAVELINE						; save the result of _OR on 0, 1 bits in LINEBITS
	shr al, 2								; remove 0, 1 bits from al that are used in OR
	mov bl, al							; copy al in bl register
	and bl, 00000001B				; get only the 2nd bit of bl
	call SAVELINE						; save the 2nd bit of LINEBITS
													; will not delete the 2nd bit now because we will use it in _XOR
	call _XOR								; call _XOR for 2nd and 3rd bit of LINEBITS
	call SAVELINE						; save the result of _XOR on 2nd and 3rd bits of LINEBITS
	; updated LINEBITS has 3 bits now
	; finished the third line, continue to the fourth one
	mov al, LINEBITS				; get saved LINEBITS from the third line to al
	mov LINEBITS, 00000000B	;	reset LINEBITS
	call NAND								; call NAND for 0 and 1 bit of LINEBITS
	call SAVELINE						; save 0, 1 bits in LINEBITS
	shr al, 2								; remove 0, 1 bits that are used in NAND
	call _NOT								; call _NOT for 2nd bit of LINEBITS
	call SAVELINE						; save result of _NOT on 2nd bit in LINEBITS
	; updated LINEBITS has 2 bits now
	; finished the fourth line, continue to the fifth one
	mov al, LINEBITS				; get saved LINEBITS from the fourth line to the fifth one
	mov LINEBITS, 00000000B	;	reset LINEBITS
	call NAND								; call NAND for 0 and 1 bits of LINEBITS
	jmp OUTPUT							; output and exit

SAVELINE:
  shl LINEBITS, 1					; shift left backup
  or LINEBITS, bl					; add the new result

NAND:
	mov bl,al           		; copy of input bits into BL
	mov cl,al           		; and another in CL
	and bl, 00000001B   		; mask off all bits except input bit 0
	and cl, 00000010B   		; mask off all bits except input bit 1
	shr cl,1            		; move bit 1 value into bit 0 of CL register
													; now we have the binary value of each bit in BL and CL, in bit 0 location
	and bl,cl           		; AND these two registers, result in BL
	not bl              		; invert bits for the not part of nand
	and bl, 00000001B   		; clear all upper bits positions leaving bit 0 either a zero or one

	mov ah, bl          		; copy answer into return value register
	ret                 		; uncomment for subroutine

NOR:
	mov bl, al
	mov cl, al
	and bl, 00000001B
	and cl, 00000010B
	shr cl, 1

	or  bl, cl
	not bl
	and bl, 00000001B

	mov ah, bl
	ret

_XOR:
	mov bl, al
	mov cl, al
	and bl, 00000001B
	and cl, 00000010B
	shr cl, 1
	
	xor bl, cl
	and bl, 00000001B
	
	mov ah, bl
	ret

_OR:
	mov bl, al
	mov cl, al
	and bl, 00000001B
	and cl, 00000010B
	shr cl, 1
	
	or  bl, cl
	and bl, 00000001B
	
	mov ah, bl
	ret

_NOT:
	mov bl, al
	and bl, 00000001B
	
	not  bl
	and bl, 00000001B

	mov ah, bl
	ret

OUTPUT:
	mov dl, ah          		; copy result into DL for DOS ASCII printout
	add dl, 30H         		; comment out for subroutine
	mov ah ,2            		; print result
	int 21H             		; to console via DOS call

EXIT:
  mov ah, 4CH							; setup to terminate program and
  int 21H									; return to the DOC prompt
END START