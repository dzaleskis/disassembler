; Programa: Nr. 3
; Užduoties sąlyga: 1
; Atliko: Deividas Zaleskis

;Apdoroti:
;MOV, OUT, NOT, RCR, XLAT.

.model small
.stack 100H

BUFFER_SIZE = 1000
UNSIGNED_BYTE_MAX = 255
SIGNED_BYTE_MAX = 127
UNSIGNED_WORD_MAX = 65535
SIGNED_WORD_MAX = 32767

.data

;vartotojo sasajai
help_msg db "programa keicia .com failo masinini koda i asemblerio instrukcijas(disassembliuoja)."
help_msg1 db "paleisdami pateikite .com failo varda ir norima isvesties failo varda", 13, 10, "$"
dest_file_error_msg db "ivyko klaida atidarant isvesties faila:", 13, 10, "$"
src_file_error_msg db "ivyko klaida atidarant ivesties faila:", 13, 10, "$"

error_01_msg db  "Invalid function number", 13, 10, "$"
error_02_msg db  "File not found", 13, 10, "$"
error_03_msg db "Path not found", 13, 10, "$"
error_04_msg db  "Too many open files (no handles left)", 13, 10, "$"
error_05_msg db "Access denied", 13, 10, "$"

newln db 13, 10
space db 32
tab db 9
plus db '+'
minus db '-'
mem_start db '['
mem_end db ']'
mem_sep db '0x'
left_arrow db " <-"
right_arrow db "->"

;darbui su simboliais
buffer db BUFFER_SIZE dup(?)
count dw 1
write_count dw 0

;darbui su failais
src_file db 12 dup (0)
src_file_handle	dw ?
dest_file db 12 dup (0)
dest_file_handle dw ?

;registru vardai
byte_regs	db	'AL','CL','DL','BL','AH','CH','DH','BH'
word_regs	db	'AX','CX','DX','BX','SP','BP','SI','DI'
seg_regs    db  'ES','CS','SS','DS'

mov_str db "MOV"
out_str db "OUT"
not_str db "NOT"
rcr_str db "RCR"
xlatb_str db "XLATB"
unrecognized_str db "unrecognized"

word_state db 0
direction_state db 0
immediate_size_state db 0
bit_push_state db 0

opcode db 0
addressing_mode db 0
mode db 0
reg db 0
regmem db 0
segment_register db 0
immediate_byte db 0
immediate_word dw 0
displacement_byte db 0
displacement_word dw 0
memory_word dw 0

instruction_type db 0

char_counter db 0
imm_char db 0
mem_char db 0

temp_byte db 0
temp_word dw 0
temp_al db 0

origin_offset dw 100h
last_cx_val dw 0
last_si_val dw 0
instruction_length dw 0

.code

start:
    mov	ax, @data
	mov	es, ax			; es kad galetume naudot stosb funkcija: Store AL at address ES:(E)DI
 
	mov	si, 81h        		; programos paleidimo parametrai rasomi segmente es pradedant 129 (arba 81h) baitu        
 
	call	skip_spaces
	
	mov	al, byte ptr ds:[si]	; nuskaityti pirma parametro simboli
	cmp	al, 13			; jei nera parametru
	je	help			; tai isvesti pagalba
	
	;; ar reikia isvesti pagalba
	mov	ax, word ptr ds:[si]
	cmp	ax, 3F2Fh        	; jei nuskaityta "/?" - 3F = '?'; 2F = '/'
	je	help                 	; rastas "/?", vadinasi reikia isvesti pagalba
	
	;; source failo pav. nuskaitymas
	lea	di, src_file
	call	read_filename		; perkelti is parametro i eilute
	cmp	byte ptr es:[src_file], '$' ; jei nieko nenuskaite
	je	help

	;; destination failo pavadinimo nusk.
	lea	di, dest_file
	call	read_filename		; perkelti is parametro i eilute

	push	ds si ; turbut kad pushina abu? paklaust destytojo
	

	mov	ax, @data
	mov	ds, ax
	
		;; skaitymui
	mov	dx, offset src_file	; ikelti i dx src failo pavadinima
	mov	ah, 3Dh			; ikelia komandos koda
	mov	al, 0   ; skaitymui
	int	21h			; INT 21h / AH= 3Dh - open existing file
	
	JC src_file_error ; jei ivyko klaida, bus nusettintas carry flagas
	
	mov	src_file_handle, ax		; issaugom handle, kadangi interruptas handle paduoda i ax
	
	;; rasymui
	mov	dx, offset dest_file	; ikelti i dx destination failo pavadinima
	mov	ah, 3Ch			; ikelia komandos koda
	mov	cx, 0			; normal - no attributes
	int	21h			; INT 21h / AH= 3Ch - create or truncate file.
	
	JC dest_file_error ; jei ivyko klaida, bus nusettintas carry flagas
	
	mov	dest_file_handle, ax		; issaugom handle, kadangi interruptas handle paduoda i ax
	;TODO - cia turetu buti error handlinimas su carry flag'u, dabar nesvarbu
	mov	dest_file_handle, ax		; issaugom handle, kadangi interruptas handle paduoda i ax
	JMP main_loop
help:
	mov	ax, @data
	mov	ds, ax
	
	mov	dx, offset help_msg         
	mov	ah, 09h
	int	21h
	JMP end_
dest_file_error:
    ; errors 03h, 04h, 05h
    ; if error occured in creating dest file, src file must be closed
    mov	dx, offset dest_file_error_msg         
	call print_string
	
	push ax
	MOV ah, 3Eh
    MOV bx, src_file_handle
    INT 21h
    pop ax
	CMP ax, 03h
	JE error_03
	CMP ax, 04h
	JE error_04
	CMP ax, 05h
	JE error_05
	JMP end_
src_file_error:
    ; errors (01h,02h,03h,04h,05h)
    mov	dx, offset src_file_error_msg         
	call print_string
	
	CMP ax, 01h
	JE error_01
	CMP ax, 02h
	JE error_02
	CMP ax, 03h
	JE error_03
	CMP ax, 04h
	JE error_04
	CMP ax, 05h
	JE error_05
	JMP end_
error_01:
    mov dx, offset error_01_msg
    call print_string
    JMP end_
error_02:
    mov dx, offset error_02_msg
    call print_string
    JMP end_
error_03:
    mov dx, offset error_03_msg
    call print_string
    JMP end_
error_04:
    mov dx, offset error_04_msg
    call print_string
    JMP end_
error_05:
    mov dx, offset error_05_msg
    call print_string
    JMP end_

main_loop:
    MOV bx, src_file_handle
    call read_file_to_buffer
    CMP [count], 0
    JE close_files
    
    push cx
    MOV si, offset buffer
    call char_by_char
    pop cx

    JMP main_loop


close_files:
    ;uzdaryti abu failus pries baigiant darba
  
    MOV ah, 3Eh
    MOV bx, src_file_handle
    INT 21h
    MOV bx, dest_file_handle
    INT 21h

end_:
    ;end program, return to DOS
    MOV ax, 4c00h 
	INT 21h


char_by_char PROC near

    MOV bx, dest_file_handle
    MOV cx, [count] ;iterate through all items in buffer
    
read_first_byte:
    MOV [last_cx_val], cx
    MOV [last_si_val], si

    MOV [word_state], 0
    MOV [direction_state], 0
    MOV [bit_push_state], 0
    MOV [immediate_size_state], 0
    
    MOV [addressing_mode], 0
    MOV [mode], 0
    MOV [reg], 0
    MOV [regmem], 0
    MOV [segment_register], 0
    
    MOV [immediate_byte], 0
    MOV [immediate_word], 0
    MOV [displacement_byte], 0
    MOV [displacement_word], 0
    MOV [instruction_type], 0

    push ax
    MOV ax, [count]
    SUB ax, cx
    ADD ax, [origin_offset]
    MOV [temp_al], al
    MOV al, ah
    call print_al_hex
    MOV al, [temp_al]
    call print_al_hex
    pop ax

    call print_tab

    LODSB
    MOV [opcode], al
    call parse_instruction
    DEC CX
    call print_instruction_hex
    call print_tab
    call print_tab
    call decode_instruction
    call print_newln

    CMP CX, 0
    JE end_char_by_char
    JMP read_first_byte
end_char_by_char:
    RET
    
char_by_char ENDP


;*******************


parse_instruction PROC near

step0:
;first check for one byte instructions, implied operands
;1110 111w – OUT
;1101 0111 - XLATB
check_xlatb:
    CMP al, 11010111b ; check for xlatb, simple as that
    JE found_in_step0
;then, check instructions with word byte
check_out0:
    call set_word_state
    CMP al, 11101111b
    JE found_in_step0
not_found_in_step0:
    call reset_word_state
    JMP step1
found_in_step0:
    MOV [instruction_type], 1
    ; maybe set some state that instruction found was of type 0
    RET
;check one byte instructions with immediate operands of 8 bits
;1110 011w portas – OUT portas
step1:
    call set_word_state
check_out1:
    CMP al, 11100111b ; need to parse imm value here
    JE found_in_step1
not_found_in_step1:
    call reset_word_state
    JMP step2
found_in_step1:
    MOV [instruction_type], 2
    call read_imm_byte
    RET
step2:
;one byte instructions, register mode, with w bit
;1011 wreg bojb [bovb] – MOV registras <- betarpiškas operandas
    push ax
    AND al, 00001000b
    SHR al, 3
    call set_word_state
    pop ax
    
    push ax
    AND al, 00000111b
    SHL al, 3
    MOV [reg], al
    pop ax
    
    AND al, 11111000b
check_mov2:
    CMP al, 10110000b
    JE found_in_step2
    CMP al, 10111000b
    JE found_in_step2
    JMP not_found_in_step2
found_in_step2:
    MOV [instruction_type], 3
    CMP [word_state], 0
    JNE read_immediate_word2
    call read_imm_byte
    RET
read_immediate_word2:
    call read_imm_word
    RET
not_found_in_step2:
    MOV [reg], 0
    call reset_word_state
    JMP step3
step3:
; one byte opcode, with memory and w bit
;1010 000w adrjb adrvb – MOV akumuliatorius <- atmintis
;1010 001w adrjb adrvb – MOV atmintis <- akumuliatorius
    call set_word_state
    call set_direction_state
check_mov3:
    ; CMP al, 10100001b
    ; JE found_in_step3
    CMP al, 10100011b
    JE found_in_step3
    JMP not_found_in_step3
found_in_step3:
    MOV [instruction_type], 4
    call read_mem_word
    RET
not_found_in_step3:
    call reset_word_state
    JMP step4
step4:
; two byte opcode with displacement, mem location and w bit
;1111 011w mod 010 r/m [poslinkis] – NOT registras/atmintis
    call set_word_state
check_not:
    CMP al, 11110111b
    JE found_in_step4
    JMP not_found_in_step4
found_in_step4:
    MOV [instruction_type], 5
    call read_addr_byte_no_reg
    call read_displacement
    RET
not_found_in_step4:
    call reset_word_state
    JMP step5
step5:
; two byte opcode with displacement, d bit and w bit
;1000 10dw mode reg r/m [poslinkis] – MOV registras <-> registras/atmintis
    call set_word_state
    call set_direction_state
check_mov5:
    CMP al, 10001011b
    JE found_in_step5
    JMP not_found_in_step5
found_in_step5:
    MOV [instruction_type], 6
    call read_addr_byte
    call read_displacement
    RET

not_found_in_step5:
    call reset_word_state
    call reset_direction_state
    JMP step6
step6:
; two byte opcode with displacement, immediate value and w bit
;1100 011w mode 000 r/m [poslinkis] bojb [bovb] – MOV registras/atmintis <- betarpiškas operandas
    call set_word_state
check_mov6:
    CMP al, 11000111b
    JE found_in_step6
    JMP not_found_in_step6
found_in_step6:
    MOV [instruction_type], 7
    call read_addr_byte_no_reg
    call read_displacement
    call read_immediate
    RET
not_found_in_step6:
    call reset_word_state
    JMP step7
step7:
;two byte instruction with segment register, displacement and direction bit
;1000 11d0 mode 0sr r/m [poslinkis] – MOV segmento registras <-> registras/atmintis
    call set_direction_state
check_mov7:
    CMP al, 10001110b
    JE found_in_step7
    JMP not_found_in_step7
found_in_step7:
    MOV [instruction_type], 8
    call read_addr_byte
    call read_displacement
    RET
not_found_in_step7:
    call reset_direction_state
    JMP step8
step8:
; two byte opcode with displacement, v bit and w bit
;1101 00vw mod 011 r/m [poslinkis] – RCR registras/atmintis, {1; CL}
    call set_word_state
    call set_bit_push_state
check_rcr:
    CMP al, 11010011b
    JE found_in_step8
    JMP not_found_in_step8
found_in_step8:
    MOV [instruction_type], 9
    call read_addr_byte
    call read_displacement
    RET
not_found_in_step8:
    call reset_word_state
    call reset_bit_push_state
    
byte_not_recognized:
    RET

parse_instruction ENDP

;*******************

decode_instruction PROC near
    
    MOV dl, [instruction_type]
    CMP dl, 0
    JNE instruction_type_valid
instruction_type_invalid:
    call print_unrecognized
    RET
instruction_type_valid:
    call print_instruction
    ;push these values to the right end, makes it easier later
    SHR [reg], 3
    SHR [mode], 6
check1:
    CMP dl, 1
    JNE check2
    JMP decode_type1
check2:
    CMP dl, 2
    JNE check3
    JMP decode_type2
check3:
    CMP dl, 3
    JNE check4 
    JMP decode_type3
check4:
    CMP dl, 4
    JNE check5
    JMP decode_type4
check5:
    CMP dl, 5
    JNE check6
    JMP decode_type5
check6:
    CMP dl, 6
    JNE check7
    JMP decode_type6
check7:
    CMP dl, 7
    JNE check8
    JMP decode_type7
check8:
    CMP dl, 8
    JNE check9
    JMP decode_type8
check9:
    JMP decode_type9

decode_type1:
    CMP al, 11010111b
    JNE cont_1
    RET
cont_1:
    mov write_count, 2
    mov dx, offset word_regs + 4
    call write_from_dx
    call print_tab
    CMP [word_state], 1
    call print_register
    RET
decode_type2:
    call print_immediate
    call print_tab
    call print_register
    RET
decode_type3:
    call print_register
    call print_tab
    call print_immediate
    RET
decode_type4:
    call print_memory
    call print_space
    call print_direction
    call print_tab
    call print_register
    RET
decode_type5:
    call print_second_operand
    RET
decode_type6:
    call print_register
    call print_space
    call print_direction
    call print_tab
    call print_second_operand
    RET
decode_type7:
    call print_first_operand
    call print_tab
    call print_immediate
    RET
decode_type8:
    MOV [word_state], 1
    call print_segment_register
    call print_space
    call print_direction
    call print_tab
    call print_second_operand
    RET
decode_type9:
    call print_second_operand
    call print_tab
    MOV [word_state], 0
    CMP [bit_push_state], 1
    JE state_push
    MOV [immediate_byte], 1
    call print_immediate
    RET
state_push:
    MOV [reg], 1 ; 1 is value of cl
    call print_register
    RET
end_decode:
    RET
decode_instruction ENDP

;*******************

print_instruction_hex PROC near

    push cx
    MOV cx, [last_cx_val]
    MOV [instruction_length], cx
    pop cx
    SUB [instruction_length], cx
    
    push ax si
    MOV si, [last_si_val]
load_from_si:
    LODSB
    call print_al_hex
check_if_end:
    DEC [instruction_length]
    CMP [instruction_length], 0
    JE end_print_hex
    JMP load_from_si
end_print_hex:
	pop si ax
	RET

print_instruction_hex ENDP

;*******************
print_al_hex PROC near

    push ax bx cx dx
    MOV cl, 16	
hex_div:
    XOR ah, ah
    DIV cl
    push ax
    XOR ah, ah
    DIV cl
    push ax
    ADD [char_counter], 2
print_hex_from_stack:
    pop ax
    CMP ah, 9
    JNA skip_make_hex
    ADD ah, 7
skip_make_hex:
    ADD ah, 48
    MOV [temp_byte], ah
    MOV dx, offset temp_byte
    MOV write_count, 1
	call write_from_dx
	DEC [char_counter]
	CMP [char_counter], 0
	JNE print_hex_from_stack
    pop dx cx bx ax
    RET

print_al_hex ENDP
;*******************

print_direction PROC near

    CMP [direction_state], 1
    JE print_left_arrow
print_right_arrow:
    MOV dx, offset right_arrow
    MOV write_count, 2
    call write_from_dx
    RET
print_left_arrow:
    MOV dx, offset left_arrow
    MOV write_count, 3
    call write_from_dx
    RET

print_direction ENDP

;*******************

print_segment_register PROC near
    MOV [write_count], 2
    mov dx, offset seg_regs
    push ax cx
    MOV al, [reg] 
    XOR ah, ah
    MOV cl, 2
    MUL cl
    ADD dx, ax
    pop cx ax 
    call write_from_dx
    RET
print_segment_register ENDP

;*******************

print_first_operand PROC near
; addressing modes:
;0 - register addressing
;1 - memory direct addressing
;2 - one byte displacement addressing 
;3 - two byte displacement addressing
;4 - indirect register addressing

    CMP [addressing_mode], 0
    JE first_operand_is_reg
    CMP [addressing_mode], 1
    JE first_operand_is_mem
    CMP [addressing_mode], 2
    JE first_operand_is_reg_disp_byte
    CMP [addressing_mode], 3
    JE first_operand_is_reg_disp_word
    CMP [addressing_mode], 4
    JE first_operand_is_reg_indirect

    RET
first_operand_is_reg:
    call print_register
    RET
first_operand_is_mem:
    call print_memory
    RET
first_operand_is_reg_disp_byte:
    call print_mem_start
    call print_register_indirect
    call print_displacement
    call print_mem_end
    RET
first_operand_is_reg_disp_word:
    call print_mem_start
    call print_register_indirect
    call print_displacement
    call print_mem_end
    RET
first_operand_is_reg_indirect:
    call print_mem_start
    call print_register_indirect
    call print_mem_end
    RET
print_first_operand ENDP

;*******************

print_second_operand PROC near

    CMP [addressing_mode], 0
    JE second_operand_is_reg
    CMP [addressing_mode], 1
    JE second_operand_is_mem
    CMP [addressing_mode], 2
    JE second_operand_is_reg_disp_byte
    CMP [addressing_mode], 3
    JE second_operand_is_reg_disp_word
    CMP [addressing_mode], 4
    JE second_operand_is_reg_indirect

    RET
second_operand_is_reg:
    push ax
    MOV al, [regmem]
    MOV [reg], al
    pop ax
    call print_register
    RET
second_operand_is_mem:
    call print_memory
    RET
second_operand_is_reg_disp_byte:
    call print_mem_start
    call print_register_indirect
    call print_displacement
    call print_mem_end
    RET
second_operand_is_reg_disp_word:
    call print_mem_start
    call print_register_indirect
    call print_displacement
    call print_mem_end
    RET
second_operand_is_reg_indirect:
    call print_mem_start
    call print_register_indirect
    call print_mem_end
    RET

print_second_operand ENDP

;*******************

print_register_indirect PROC near
    MOV [write_count], 2
    mov dx, offset word_regs
    
    push ax cx
    MOV al, [reg] 

    CMP [regmem],3
    JNA skip_offset
    ADD al, 2

    skip_offset:
    XOR ah, ah
    MOV cl, 2
    MUL cl
    ADD dx, ax
    pop cx ax 
    call write_from_dx
    RET
print_register_indirect ENDP

;*******************

print_register PROC near
    CMP [word_state], 1
    JE word_reg
byte_reg:
    call print_byte_register
    RET
word_reg:    
    call print_word_register
    RET
print_register ENDP

;*******************

print_byte_register PROC near
    MOV [write_count], 2
    mov dx, offset byte_regs
    push ax cx
    XOR ax, ax
    MOV al, [reg] 
    MOV cl, 2
    MUl cl
    ADD dx, ax
    pop cx ax
    call write_from_dx
    RET
print_byte_register ENDP

;*******************

print_word_register PROC near
    MOV [write_count], 2
    mov dx, offset word_regs
    push ax cx
    MOV al, [reg] 
    XOR ah, ah
    MOV cl, 2
    MUL cl
    ADD dx, ax
    pop cx ax 
    call write_from_dx
    RET
print_word_register ENDP

;*******************


print_displacement PROC near
    push ax bx cx dx
    MOV cx, 10	
	MOV [char_counter], 0
    CMP [addressing_mode], 3
    JE case_disp_word
case_disp_byte:
    XOR ax,ax
    MOV al, displacement_byte
    CMP al, SIGNED_BYTE_MAX
    JA disp_byte_is_negative
disp_byte_is_positive:
    call print_plus
    JMP disp_division_loop
disp_byte_is_negative:
    call print_minus
    MOV [temp_byte], UNSIGNED_BYTE_MAX
    SUB [temp_byte], al
    MOV al, [temp_byte]
    ADD al,1 ;value adjustment
    JMP disp_division_loop
case_disp_word:
    mov ax, displacement_word		
    CMP ax, SIGNED_WORD_MAX
    JA disp_word_is_negative
disp_word_is_positive:
    call print_plus
    JMP disp_division_loop
disp_word_is_negative:
    call print_minus
    MOV [temp_word], UNSIGNED_BYTE_MAX
    SUB [temp_word], ax
    MOV ax, [temp_word]
    ADD ax,1 ;value adjustment
    JMP disp_division_loop
disp_division_loop:
	XOR dx, dx
	DIV cx
	push dx	
	INC [char_counter]
	CMP ax, 0
	JNE disp_division_loop
print_disp_from_stack:
    pop dx
    ADD dl, 48
	MOV [imm_char], dl
	MOV dx, offset imm_char
	MOV write_count, 1
	call write_from_dx
	DEC [char_counter]
	CMP [char_counter], 0
	JNE print_disp_from_stack
end_print_disp:
	pop dx cx bx ax
	RET
print_displacement ENDP

;*******************

print_immediate PROC near
    push ax bx cx dx
    MOV cx, 10	
	MOV [char_counter], 0
    CMP [immediate_size_state], 1
    JE case_imm_word
case_imm_byte:
    XOR ax,ax
    MOV al, immediate_byte
    JMP imm_division_loop
case_imm_word:
    mov ax, immediate_word		
imm_division_loop:
	XOR dx, dx
	DIV cx
	push dx	
	INC [char_counter]
	CMP ax, 0
	JNE imm_division_loop
print_imm_from_stack:
    pop dx
    ADD dl, 48
	MOV [imm_char], dl
	MOV dx, offset imm_char
	MOV write_count, 1
	call write_from_dx
	DEC [char_counter]
	CMP [char_counter], 0
	JNE print_imm_from_stack
end_print_imm:
	pop dx cx bx ax
	RET
print_immediate ENDP

;*******************


print_mem_start PROC near
    MOV dx, offset mem_start
	MOV write_count, 1
	call write_from_dx
    RET
print_mem_start ENDP

;*******************


print_mem_separator PROC near
    MOV dx, offset mem_sep
	MOV write_count, 2
	call write_from_dx
    RET
print_mem_separator ENDP

;*******************


print_mem_end PROC near
    MOV dx, offset mem_end
	MOV write_count, 1
	call write_from_dx
    RET
print_mem_end ENDP

;*******************


print_memory PROC near
    push ax bx cx dx
    call print_mem_start
    call print_mem_separator
    MOV cx, 16	
	MOV [char_counter], 0
    MOV ax, [memory_word]	
mem_division_loop:
	XOR dx, dx
	DIV cx
	push dx	
	INC [char_counter]
	CMP ax, 0
	JNE mem_division_loop
print_mem_from_stack:
    pop dx
    CMP dl, 9
    JNA make_mem_char
    ADD dl, 7 ;makes char hex if value exceeds 10
make_mem_char:
    ADD dl, 48
	MOV [mem_char], dl
	MOV dx, offset mem_char
	MOV write_count, 1
	call write_from_dx
	DEC [char_counter]
	CMP [char_counter], 0
	JNE print_mem_from_stack
end_print_mem:
    call print_mem_end
	pop dx cx bx ax
	RET
print_memory ENDP

;*******************


print_instruction PROC near
    
    push ax dx
    MOV al, [opcode]
    
    CMP al, 11010111b
    JE print_xlatb
    
    OR al, 00000001b

    CMP al, 11101111b
    JE print_out

    CMP al, 11100111b
    JE print_out
    
    CMP al, 11110111b
    JE print_not
    
    OR al, 00000010b
    
    CMP al, 11010011b
    JE print_rcr
    
    ;all other possible cases are mov, so just jump to that
    JMP print_mov

print_xlatb:
    mov	dx, offset xlatb_str     
    mov write_count, 5
	call write_from_dx
    JMP end_print_instruction
print_out:
    mov	dx, offset out_str   
    mov write_count, 3
	call write_from_dx
    JMP end_print_instruction
print_not:
    mov	dx, offset not_str 
    mov write_count, 3
	call write_from_dx
    JMP end_print_instruction
print_rcr:
    mov	dx, offset rcr_str      
    mov write_count, 3
	call write_from_dx
    JMP end_print_instruction
print_mov:
    mov	dx, offset mov_str      
    mov write_count, 3
	call write_from_dx
    JMP end_print_instruction
end_print_instruction:
    mov	dx, offset tab
    mov write_count, 1
	call write_from_dx
    pop dx ax
    RET
print_instruction ENDP


;*******************
print_unrecognized PROC near
    MOV dx, offset unrecognized_str
    MOV write_count, 12
    call write_from_dx
    RET
print_unrecognized ENDP
;*******************
print_plus PROC near
    MOV dx, offset plus
    MOV write_count, 1
    call write_from_dx
    RET
print_plus ENDP
;*******************
print_minus PROC near
    MOV dx, offset minus
    MOV write_count, 1
    call write_from_dx
    RET
print_minus ENDP
;*******************
print_tab PROC near
    MOV dx, offset tab
    MOV write_count, 1
    call write_from_dx
    RET
print_tab ENDP
;*******************
print_space PROC near
    MOV dx, offset space
    MOV write_count, 1
    call write_from_dx
    RET
print_space ENDP
;*******************
print_newln PROC near
    MOV dx, offset newln
    MOV write_count, 2
    call write_from_dx
    RET
print_newln ENDP
;*******************

read_immediate PROC near

    CMP [word_state], 0
    JNE read_immediate_word
    call read_imm_byte
    RET
read_immediate_word:
    call read_imm_word
    RET

read_immediate ENDP

;*******************


read_displacement PROC near

    ;check for one byte displacement
    CMP [mode], 01000000b
    JE read_displacement_byte
    ;check for two byte displacement
    CMP [mode], 10000000b
    JE read_displacement_word
    ;check for indirect addresing
    CMP [mode], 00000000b
    JE check_for_mem
    ;else, mod is 11, direct addresing
    RET
read_displacement_byte:
    MOV [addressing_mode], 2
    call read_disp_byte
    RET
read_displacement_word:
    MOV [addressing_mode], 3
    call read_disp_word
    RET
check_for_mem:
    ;check for memory direct addressing
    CMP [regmem], 00000110b
    JE read_mem
    ;if it isnt displacement only addressing, must be indirect addressing
    MOV [addressing_mode], 4
    RET
read_mem:
    MOV [addressing_mode], 1
    call read_mem_word
    RET
read_displacement ENDP

;*******************
set_word_state PROC near
    push ax
    AND al, 00000001b
    MOV [word_state], al ;set word state to 0 or 1 (byte or word)
    pop ax
    OR al,  00000001b
    RET
set_word_state ENDP

;*******************

reset_word_state PROC near
    MOV [word_state], 0 ;reset
    MOV al, [opcode]
    RET
reset_word_state ENDP
;*******************
set_direction_state PROC near
    push ax
    AND al, 00000010b
    SHR al, 1
    MOV [direction_state], al ;set direction_state to 0 or 1 (byte or word)
    pop ax
    OR al,  00000010b
    RET
set_direction_state ENDP

;*******************

reset_direction_state PROC near
    MOV [direction_state], 0 ;reset
    MOV al, [opcode]
    RET
reset_direction_state ENDP

;*******************

set_bit_push_state PROC near
    push ax
    AND al, 00000010b
    SHR al, 1
    MOV [bit_push_state], al ;set bit_push_state to 0 or 1 (push by 1 or by value of cl)
    pop ax
    OR al,  00000010b
    RET
set_bit_push_state ENDP

;*******************

reset_bit_push_state PROC near
    MOV [bit_push_state], 0 ;reset
    MOV al, [opcode]
    RET
reset_bit_push_state ENDP

;*******************

read_addr_byte PROC near
    
    push ax
    LODSB
    DEC cx
    
    push ax
    AND al, 11000000b
    MOV [mode], al
    pop ax
    
    push ax
    AND al, 00111000b
    MOV [reg], al
    pop ax
    
    push ax
    AND al, 00000111b
    MOV [regmem], al
    pop ax
    
    pop ax
    RET

read_addr_byte ENDP
;*******************

read_addr_byte_no_reg PROC near
    ;this is used when the reg field has a value, which leaves the regmem field in place of it
    push ax
    LODSB
    DEC cx
    
    push ax
    AND al, 11000000b
    MOV [mode], al
    pop ax
    
    push ax
    AND al, 00000111b
    MOV [regmem], al
    SHL al, 3
    MOV [reg], al
    pop ax
    
    pop ax
    RET

read_addr_byte_no_reg ENDP
;*******************

read_imm_byte PROC near
    push ax
    LODSB
    DEC cx
    MOV [immediate_byte], al
    MOV [immediate_size_state], 0
    pop ax
    RET
read_imm_byte ENDP
;*******************

read_imm_word PROC near
    push ax
    LODSW
    SUB cx, 2
    MOV [immediate_word], ax
    MOV [immediate_size_state], 1
    pop ax
    RET
read_imm_word ENDP
;*******************

read_disp_byte PROC near
    push ax
    LODSB
    DEC cx
    MOV [displacement_byte], al
    pop ax
    RET
read_disp_byte ENDP
;*******************

read_disp_word PROC near
    push ax
    LODSW
    SUB cx, 2
    MOV [displacement_word], ax
    pop ax
    RET
read_disp_word ENDP
;*******************
read_mem_word PROC near
    push ax
    LODSW
    SUB cx, 2
    MOV [memory_word], ax
    pop ax
    RET
read_mem_word ENDP


;*******************

set_segment_register PROC near

    push ax
    MOV al, [reg]
    SHR al, 3
    MOV [segment_register], al
    pop ax
    RET
set_segment_register ENDP

write_from_buffer PROC near
    push ax cx dx
write_buffer_content:
    mov dx, offset buffer
    
    mov cx, count
    MOV ah, 40h
    INT 21h

write_from_buffer_end:
    pop dx cx ax
    RET
    
write_from_buffer ENDP

;*******************

write_from_dx PROC near

    push cx ax
    mov cx, write_count
    MOV ah, 40h
    INT 21h
    pop ax cx
    RET
    
write_from_dx ENDP

;*******************

skip_spaces PROC near

skip_spaces_loop:
	cmp byte ptr ds:[si], ' '
	jne skip_spaces_end
	inc si
	jmp skip_spaces_loop
skip_spaces_end:
	ret
	
skip_spaces ENDP


;###################

read_filename PROC near

	push	ax
	call	skip_spaces
read_filename_start:
	cmp	byte ptr ds:[si], 13	; jei nera parametru
	je	read_filename_end	; tai baigtas failo vedimas
	cmp	byte ptr ds:[si], ' '	; tikriname ar yra tarpas
	jne	read_filename_next	; jei nera tarpo, tai skaito kita simboli, jei yra - skaitymas baigtas
read_filename_end:
	mov	al, '$'			; irasyti '$' gale
	stosb                           ; Store AL at address ES:(E)DI, di = di + 1
	pop	ax ; ax grizta i busena, buvusia pries subroutine call'a
	ret ;return to caller
read_filename_next:
	lodsb				; Load byte at DS:[SI] into AL. Update SI.
	stosb                           ; Store AL at address ES:(E)DI, update SI
	jmp read_filename_start

read_filename ENDP

;###################

read_file_to_buffer PROC near
    
    MOV bx, src_file_handle
    MOV dx, offset buffer
    MOV cx, BUFFER_SIZE
    
    MOV ah, 3Fh
    INT 21h
    MOV [count], ax    
    RET

read_file_to_buffer ENDP

;###################

print_string PROC near
    push ax
	mov	ah, 09h
	int	21h
	pop ax
    RET
print_string ENDP

;###################
	


end start