;====================================================================
;MICROCONTROLADORES 2017-1
;ALUNO: DIEGO RAMON BEZERRA DA SILVA
;MATRICULA: 11228382
;DATA: 23/10/2017
;NOME DO PROJETO: PROTOCOLO I2C
;DESCRIÇÃO DO PROJETO: EXERCÍCIO DE MANIPULAÇÃO E IMPLEMENTAÇÃO DO
;PROTOCOLO DE COMUNICAÇÃO I2C.
;NOTA: NÃO FUNCIONA NO SIMULADOR
;====================================================================

;====================================================================
; DEFINITIONS
;====================================================================

#INCLUDE P16F628A.INC                ; INCLUDE REGISTER DEFINITION FILE

#DEFINE SDA 1                        ; LINHA DE DADOS I2C
#DEFINE SCL 2                        ; LINHA DE CLOCK I2C
#DEFINE TENSAO PORTA, RA0            ; ENTRADA DE TENSÃO MEDIDA
;====================================================================
; VARIABLES
;====================================================================
 CBLOCK  0x20
        ;IC2
        COUNT           ;LOOP DO I2C(WRITE)
        BUFFER          ;BUFFER DO I2C

        ;VOLTIMETRO
        COM_LEITURA     ;VALOR LIDO DO ADC
        UNIDADE         ;UNIDADE
        DEZENA          ;DEZENA
        CENTENA         ;CENTENA
        ACM_L           ;ACUMULA LEITURAS DO ADC PARTE BAIXA
        ACM_H           ;ACUMULA LEITURAS DO ADC PARTE ALTA
        LEITURA         ;NÚMERO DE LEITURAS FEITAS
        MUL_L           ;PARTE BAIXA DO WORD
        MUL_H           ;PARTE ALTA DO WORD
        MULCND          ;MULTIPLICANDO OU DIVIDENDO
        MULPLR          ;MULTIPLICADOR OU DIVISOR

        ;DELAY
        D1              ;CONTADOR DE DELAY
        D2              ;CONTADOR DE DELAY
        D3              ;CONTADOR DE DELAY

 ENDC
;====================================================================
; RESET AND INTERRUPT VECTORS
;====================================================================

      ; RESET VECTOR
RST   CODE  0X0
      GOTO  START

;====================================================================
; CODE SEGMENT
;====================================================================

;====================================================================
;CONVERSÃO BINÁRIO EM DECIMAL(8 BITS )
;SEPARA UM BYTE(0 ~ 255) EM CENTENA, DEZENA E UNIDADE
;====================================================================
BIN2DEC
    MOVWF   UNIDADE             ;COPIA O NÚMERO A SER CONVERTIDO PARA VARIÁVEL
    CLRF    DEZENA              ;LIMPA DEZENAS
    CLRF    CENTENA             ;LIMPA CENTENAS
BIN2DEC_0
    MOVLW   .100                ;SUBTRAI 100 DECIMAL
    SUBWF   UNIDADE, W          ;DO VALOR A SER CONVERTIDO E GUARDA EM W
    BTFSS   STATUS, C           ;O RESULTADO É POSITIVO OU ZERO?
    GOTO    BIN2DEC_1           ;NÃO, VAI PARA PRÓXIMA PARTE
    MOVWF   UNIDADE             ;SIM, ENTÃO COPIA O VALOR PARA AS UNIDADES
    INCF    CENTENA, F          ;INCREMENTA UM NAS CENTENAS
    GOTO    BIN2DEC_0           ;CONTINUA
BIN2DEC_1
    MOVLW   .10                 ;SUBTRAI 10 DECIMAL
    SUBWF   UNIDADE, W          ;DO VALOR A SER CONVERTIDO E GUARDA EM W
    BTFSS   STATUS, C           ;O RESULTADO É POSITIVO OU ZERO?
    RETURN                      ;NÃO, TERMINOU
    MOVWF   UNIDADE             ;SIM, ENTÃO COPIA O VALOR PARA AS UNIDADES
    INCF    DEZENA, F           ;INCREMENTA UM NAS DEZENAS
    GOTO    BIN2DEC_1           ;CONTINUA

;====================================================================
;MULTIPLICAÇÃO 8X8
;====================================================================
MUL8X8
    CLRF    MUL_L       ;LIMPA BYTE BAIXO DO RESULTADO
    CLRF    MUL_H       ;LIMPA BYTE ALTO DO RESULTADO

MUL8X8_HAS_ZERO         ;SE UM DOS OPERANDOS FOR ZERO JÁ SAI DIRETO
    MOVF    MULCND, W
    IORWF   0X0, F
    BTFSC   STATUS, Z   ;ZERO?
    GOTO    MUL8X8_END  ;SIM, FINALIZA
    MOVF    MULPLR, W
    IORWF   0X0, F
    BTFSC   STATUS, Z   ;ZERO?
    GOTO    MUL8X8_END  ;SIM, FINALIZA
MUL8X8_LOOP
    MOVF    MULCND, W   ;MOVE MULTIPLICANDO PARA WORK
    ADDWF   MUL_L, F    ;SOMA MULTIPLCANDO AO BYTE BAIXO RESULTADO
    BTFSC   STATUS, C   ;HOUVE CARRY?
    INCF    MUL_H, F    ;SIM, INCREMENTA BYTE ALTO
    DECFSZ  MULPLR, F   ;NÃO, MULTIPLICAÇÃO TERMINOU?
    GOTO    MUL8X8_LOOP ;NÃO, CONTINUA

MUL8X8_END
    RETURN              ;SIM, FINALIZA

;====================================================================
;DIVISÃO 8X8
;====================================================================
DIV8X8
    CLRF    MUL_L       ;LIMPA BYTE BAIXO DO RESULTADO
DIV8X8_LOOP
    MOVFW   MULPLR      ;MOVE DIVISOR PARA WORK
    SUBWF   MULCND, F   ;MULPLR - MULCND
    BTFSS   STATUS, C   ;CARRY?
    GOTO    DIV8X8_END  ;DIVIDENDO > 0, FINALIZA
    INCF    MUL_L, F    ;SENÃO, INCREMENTA RESULTADO
    GOTO    DIV8X8_LOOP ;CONTINUA
DIV8X8_END
    RETURN              ;FINALIZA

;====================================================================
;DIVISÃO 16 BITS POR 32(CONSTANTE)
;====================================================================
DIV16X32
    BCF     STATUS, C   ;DIVIDE POR 2
    RRF     ACM_H, F
    RRF     ACM_L, F
    BCF     STATUS, C   ;DIVIDE POR 4
    RRF     ACM_H, F
    RRF     ACM_L, F
    BCF     STATUS, C   ;DIVIDE POR 8
    RRF     ACM_H, F
    RRF     ACM_L, F
    BCF     STATUS, C   ;DIVIDE POR 16
    RRF     ACM_H, F
    RRF     ACM_L, F
    BCF     STATUS, C   ;DIVIDE POR 32
    RRF     ACM_H, F
    RRF     ACM_L, F
    RETURN

;====================================================================
;DIVISÃO 16 BITS POR 256(CONSTANTE)
;====================================================================

DIV16X256
    BCF     STATUS, C   ;DIVIDE POR 2
    RRF     MUL_H, F
    RRF     MUL_L, F
    BCF     STATUS, C   ;DIVIDE POR 4
    RRF     MUL_H, F
    RRF     MUL_L, F
    BCF     STATUS, C   ;DIVIDE POR 8
    RRF     MUL_H, F
    RRF     MUL_L, F
    BCF     STATUS, C   ;DIVIDE POR 16
    RRF     MUL_H, F
    RRF     MUL_L, F
    BCF     STATUS, C   ;DIVIDE POR 32
    RRF     MUL_H, F
    RRF     MUL_L, F
    BCF     STATUS, C   ;DIVIDE POR 64
    RRF     MUL_H, F
    RRF     MUL_L, F
    BCF     STATUS, C   ;DIVIDE POR 128
    RRF     MUL_H, F
    RRF     MUL_L, F
    BCF     STATUS, C   ;DIVIDE POR 256
    RRF     MUL_H, F
    RRF     MUL_L, F
    RETURN

;====================================================================
;DELAY DE 1s
;====================================================================
DELAY_1s
    MOVLW   0X07
    MOVWF   D1
    MOVLW   0X2F
    MOVWF   D2
    MOVLW   0X03
    MOVWF   D3
DELAY_0
    DECFSZ  D1, F
    GOTO    $+2
    DECFSZ  D2, F
    GOTO    $+2
    DECFSZ  D3, F
    GOTO    DELAY_0
    GOTO    $+1
    GOTO    $+1
    GOTO    $+1
    RETURN

;====================================================================
;DELAY DO I2C  (PARA GERAR CLOCK DE APROX 33 kHz)
;====================================================================
I2C_DELAY
    MOVLW   0X9E
    MOVWF   D1
    MOVLW   0X10
    MOVWF   D2
ATRASO_20ms_0
    DECFSZ  D1, F
    GOTO    $+2
    DECFSZ  D2, F
    GOTO    ATRASO_20ms_0
    GOTO    $+1
    NOP
    RETURN

;====================================================================
;START DO I2C
;====================================================================
I2C_START
    BSF STATUS, RP0
    BCF TRISB, SDA
    BCF TRISB, SCL
    BCF STATUS, RP0

    BSF PORTB, SDA
    BSF PORTB, SCL
    CALL I2C_DELAY
    BCF PORTB, SDA
    CALL I2C_DELAY
    BCF PORTB, SCL
    CALL I2C_DELAY
    RETURN

;====================================================================
; STOP DO I2C
;====================================================================
I2C_STOP
    BSF STATUS, RP0
    BCF TRISB, SDA
    BCF STATUS, RP0

    BCF PORTB, SCL
    BCF PORTB, SDA
    CALL I2C_DELAY
    BSF PORTB, SCL
    CALL I2C_DELAY
    BSF PORTB, SDA
    CALL I2C_DELAY
    RETURN

;====================================================================
;WRITE DO I2C
;====================================================================
I2C_WRITE
    MOVWF   BUFFER
    MOVLW   .8
    MOVWF   COUNT
    BSF     STATUS, RP0
    BCF     TRISB, SDA     ;SDA = 0
    BCF     STATUS, RP0

I2C_WRITE_0
    BCF     PORTB, SCL
    RLF     BUFFER, F
    BCF     PORTB, SDA
    BTFSC   STATUS, C
    BSF     PORTB, SDA
    CALL    I2C_DELAY
    BSF     PORTB, SCL
    CALL    I2C_DELAY
    DECFSZ  COUNT, F
    GOTO    I2C_WRITE_0
ACK
    RETURN

;====================================================================
;INICIO
;====================================================================
PGM   CODE
START
    CLRF    PORTB
    MOVLW   0X07            ;DESATIVA COMPARADORES
    MOVLW   CMCON
    BSF     STATUS, RP0     ;BANK1
    MOVLW   B'00000000'     ;PORTB COMO SAIDA
    MOVWF   TRISB
    BCF     STATUS, RP0     ;BANK0
LOOP
    CLRF    PORTA

    MOVLW   B'00000100'     ;ATIVA COMPARADORES
    MOVLW   CMCON
    BSF     STATUS, RP0     ;BANK1
    MOVLW   B'01000000'
    MOVWF   PIE1
    BCF     STATUS, RP0     ;BANK0

    MOVLW   B'10100000'     ;CONFIG REF. INTERNA DO COMPARADOR
    MOVWF   VRCON

COMPARA
    BTFSS   CMCON, C1OUT    ;Vin > Vref
    GOTO    LOOP_0          ;SIM, AUMENTA Vref
    GOTO    LOOP_1          ;NÃO, ASSUME TENSÃO

LOOP_0
    MOVF    VRCON, W        ;AUMENTA
    ADDLW   .1              ;TENSÃO DE REF
    MOVWF   VRCON           ;POR UMA UNIDADE
    GOTO    COMPARA         ;VOLTA PARA COMPARAÇÃO
LOOP_1
    MOVF    VRCON, W        ;MOVE VALOR DE REF PARA WORK
    MOVWF   COM_LEITURA
    MOVLW   B'00000111'     ;SEPARA OS BITS DE REF
    ANDWF   COM_LEITURA, F  ;DOS BITS DE CONTROLE

    MOVFW   COM_LEITURA     ;DIVIDE
    MOVWF   MULCND          ;ESSE VALOR
    MOVLW   .24             ;POR
    MOVWF   MULPLR          ;24
    CALL    DIV8X8

    MOVLW   .5              ;MULTIPLICA
    MOVWF   MULCND          ;ESSE VALOR
    MOVFW   MUL_L           ;POR
    MOVWF   MULPLR          ;5
    CALL    MUL8X8

    MOVF    MUL_L, W        ;SEPARA
    CALL    BIN2DEC         ;VALOR EM UNIDADE, DEZENA E CENTENA

    CALL    I2C_START       ;INICIA O I2C
    MOVLW   B'00000110'     ;ENVIA ENDEREÇO DO MODULO LCD
    CALL    I2C_WRITE

    MOVLW   'T'             ;ESCREVE MENSAGEM "TENSÃO:"
    CALL    I2C_WRITE
    MOVLW   'E'
    CALL    I2C_WRITE
    MOVLW   'N'
    CALL    I2C_WRITE
    MOVLW   'S'
    CALL    I2C_WRITE
    MOVLW   'A'
    CALL    I2C_WRITE
    MOVLW   'O'
    CALL    I2C_WRITE
    MOVLW   ':'
    CALL    I2C_WRITE
    MOVLW   ' '
    CALL    I2C_WRITE

    MOVLW   B'10001000'      ;ESCREVE TENSÃO
    CALL    I2C_WRITE

    MOVF    DEZENA, W        ;ESCREVE DEZENA
    CALL    I2C_WRITE

    MOVF    UNIDADE, W       ;ESCREVE UNIDADE
    CALL    I2C_WRITE

    MOVLW   'V'
    CALL    I2C_WRITE

    CALL    I2C_STOP        ;PARA O I2C

    CALL    DELAY_1s        ;ATRASO DE 2s
    CALL    DELAY_1s        ;ENTRE AS MEDIÇÕES

    GOTO    LOOP
;====================================================================
      END
