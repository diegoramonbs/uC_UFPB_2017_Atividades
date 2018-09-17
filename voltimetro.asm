;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*              MODIFICAÇÕES PARA USO COM 12F675                   *
;*                FEITAS PELO PROF. MARDSON                        *
;*                    FEVEREIRO DE 2016                            *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*MICROCONTROLADORES 2017-1                                        *
;*ALUNO: DIEGO RAMON BEZERRA DA SILVA                              *
;*MATRICULA: 11228382                                              *
;*DATA: 18/09/2017                                                 *
;*NOME DO PROJETO: MEDIÇÃO DE TENSÃO E INDICAÇÃO NO LCD            *
;*DESCRIÇÃO DO PROJETO: EXERCÍCIO DE MANIPULAÇÃO DO CONVERSOR A/D  *
;*TENDO COMO OBJETIVO FAZER A MEDIÇÃO DE UMA TENSÃO USANDO O ADC   *
;*E POR FIM, FAZER A VISUALIZAÇÃO DO RESULTADO NO DISPLAY LCD      *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                     ARQUIVOS DE DEFINIÇÕES                      *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#INCLUDE <P12F675.INC>  ;ARQUIVO PADRÃO MICROCHIP PARA 12F675

    __CONFIG _BODEN_OFF & _CP_OFF & _PWRTE_ON & _WDT_OFF & _MCLRE_ON & _INTRC_OSC_NOCLKOUT

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                    PAGINAÇÃO DE MEMÓRIA                         *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;DEFINIÇÃO DE COMANDOS DE USUÁRIO PARA ALTERAÇÃO DA PÁGINA DE MEMÓRIA
#DEFINE BANK0   BCF STATUS,RP0  ;SETA BANK 0 DE MEMÓRIA
#DEFINE BANK1   BSF STATUS,RP0  ;SETA BANK 1 DE MAMÓRIA

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                         VARIÁVEIS                               *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; DEFINIÇÃO DOS NOMES E ENDEREÇOS DE TODAS AS VARIÁVEIS UTILIZADAS
; PELO SISTEMA

    CBLOCK  0x20        ;ENDEREÇO INICIAL DA MEMÓRIA DE
                        ;USUÁRIO
        W_TEMP          ;REGISTRADORES TEMPORÁRIOS PARA USO
        STATUS_TEMP     ;JUNTO ÀS INTERRUPÇÕES

        ;VARIÁVEIS DO LOCD
        TEMPO1          ;VARIÁVEL USADA NO DELAY
        TEMPO2          ;VARIÁVEL USADA NO DELAY
        TEMPO3          ;VARIÁVEL USADA NO DELAY
        BUFFER          ;BUFFER DE DADOS USADO PARA ENVIO DE DADOS
        COMANDO         ;REGISTRADOR DE COMANDO SERIAL
        LCD_ASCII       ;REGISTRADOR DE DADOS LCD
        CONT_SERIAL     ;CONTADOR DE ENVIO DE BITS
        FLAGS           ;REGISTRADOR FLAGS

        ;VARIÁVEIS DA CONVERSÃO A/D E ARITMÉTICAS
        ADC_LEITURA     ;VALOR LIDO DO ADC
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


    ENDC                ;FIM DO BLOCO DE MEMÓRIA
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                        FLAGS INTERNOS                           *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; DEFINIÇÃO DE TODOS OS FLAGS UTILIZADOS PELO SISTEMA

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                         CONSTANTES                              *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; DEFINIÇÃO DE TODAS AS CONSTANTES UTILIZADAS PELO SISTEMA

#DEFINE LCD_RS  FLAGS, 3

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                           ENTRADAS                              *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; DEFINIÇÃO DE TODOS OS PINOS QUE SERÃO UTILIZADOS COMO ENTRADA
; RECOMENDAMOS TAMBÉM COMENTAR O SIGNIFICADO DE SEUS ESTADOS (0 E 1)

#DEFINE ADC GPIO, 2         ;ENTRADA DO ANALÓGICA DO CONVERSOR ADC

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                           SAÍDAS                                *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; DEFINIÇÃO DE TODOS OS PINOS QUE SERÃO UTILIZADOS COMO SAÍDA
; RECOMENDAMOS TAMBÉM COMENTAR O SIGNIFICADO DE SEUS ESTADOS (0 E 1)
; PINOS DEFINIDOS COM BASE NO CIRCUITO FORNECIDO

#DEFINE LCD_CLK GPIO, 0     ;CLOCK DO DESLOCADOR DE 8 BITS
#DEFINE LCD_DAT GPIO, 4     ;PINO DE DADOS PARA O DESLOCADOR
#DEFINE LCD_EN  GPIO, 5     ;PINO PARA O ENABLE DO LCD

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                           MACROS                                *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; DEFINIÇÃO DE TODAS AS MACROS UTILIZADAS PELO SISTEMA


;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                       VETOR DE RESET                            *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

    ORG 0x00            ;ENDEREÇO INICIAL DE PROCESSAMENTO
    GOTO    INICIO

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                    INÍCIO DA INTERRUPÇÃO                        *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; ENDEREÇO DE DESVIO DAS INTERRUPÇÕES. A PRIMEIRA TAREFA É SALVAR OS
; VALORES DE "W" E "STATUS" PARA RECUPERAÇÃO FUTURA

    ORG 0x04            ;ENDEREÇO INICIAL DA INTERRUPÇÃO
    MOVWF   W_TEMP      ;COPIA W PARA W_TEMP
    SWAPF   STATUS,W
    MOVWF   STATUS_TEMP ;COPIA STATUS PARA STATUS_TEMP

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                    ROTINA DE INTERRUPÇÃO                        *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; AQUI SERÁ ESCRITA AS ROTINAS DE RECONHECIMENTO E TRATAMENTO DAS
; INTERRUPÇÕES

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                 ROTINA DE SAÍDA DA INTERRUPÇÃO                  *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; OS VALORES DE "W" E "STATUS" DEVEM SER RECUPERADOS ANTES DE
; RETORNAR DA INTERRUPÇÃO

SAI_INT
    SWAPF   STATUS_TEMP,W
    MOVWF   STATUS      ;MOVE STATUS_TEMP PARA STATUS
    SWAPF   W_TEMP,F
    SWAPF   W_TEMP,W    ;MOVE W_TEMP PARA W
    RETFIE

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                   ROTINAS E SUBROTINAS                          *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; CADA ROTINA OU SUBROTINA DEVE POSSUIR A DESCRIÇÃO DE FUNCIONAMENTO
; E UM NOME COERENTE ÀS SUAS FUNÇÕES.

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;* INICIALIZA O LCD COM BARRAMENTO DE 4 BITS                       *
;* SEQUÊNCIA DE INICIALIZAÇÃO DO DISPLAY LCD CONFORME DATASHEET    *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
LCD_INICIA_4BIT
    BCF     LCD_EN          ;HABILITA LCD
    MOVLW   B'00000000'     ;MODO COMANDO(RS = 0)
    CALL    ENVIA_DADOS     ;ENVIA DADOS PARA DESLOCADOR
    CALL    ATRASO_20ms     ;TEMPO DE SETUP
    MOVLW   B'00110000'     ;BYTE QUE SERÁ ENVIADO NO WORK
    CALL    ENVIA_DADOS     ;ENVIA DADOS PARA DESLOCADOR
    CALL    LCD_ESCREVE     ;ESCREVE NO LCD
    CALL    ATRASO_5ms      ;TEMPO DE SETUP, MAIOR QUE 4.1ms
    MOVLW   B'00110000'     ;BYTE QUE SERÁ ENVIADO NO WORK
    CALL    ENVIA_DADOS     ;ENVIA DADOS PARA DESLOCADOR
    CALL    LCD_ESCREVE     ;ESCREVE NO LCD
    MOVLW   B'00110000'     ;BYTE QUE SERÁ ENVIADO NO WORK
    CALL    ENVIA_DADOS     ;ENVIA DADOS PARA DESLOCADOR
    CALL    LCD_ESCREVE     ;ESCREVE NO LCD
    MOVLW   B'00100000'     ;BYTE QUE SERÁ ENVIADO NO WORK
    CALL    ENVIA_DADOS     ;ENVIA DADOS PARA DESLOCADOR
    CALL    LCD_ESCREVE     ;ESCREVE NO LCD
    RETURN

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;* ENVIA DADOS PARA O SHIFT DE 8 BITS                              *
;* ROTINA RESPONSÁVEL POR ENVIAR OS DADOS PARA O DESLOCADOR E,     *
;* CONSEQUENTEMENTE, PARA O DISPLAY LCD.                           *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
ENVIA_DADOS
    MOVWF   BUFFER          ;DADOS(COMANDO/DADOS) QUE SERÁ ENVIADO
    MOVLW   .8              ;LOOP USADO
    MOVWF   CONT_SERIAL     ;PARA ENVIAR 8 BITS, 1 POR 1
ENVIA_DADOS_0

    BTFSS   BUFFER, 7       ;VERIFICA VAI ENVIAR 1(HIGH)
    GOTO    ENVIA_DADOS_1       ;SENÃO, ENVIA 0(LOW)
    BSF     LCD_DAT         ;COLOCA A LIHA DE DADOS EM 1(HIGH)
    BSF     LCD_CLK         ;AVANÇA
    BCF     LCD_CLK         ;UM CICLO DE CLOCK DO DESLOCADOR
    GOTO    ENVIA_DADOS_2       ;VAI PARA VERIFICAÇÃO DE TÉRMINO
ENVIA_DADOS_1
    BCF     LCD_DAT         ;ENVIA 0(LOW)
    BSF     LCD_CLK         ;AVANÇA
    BCF     LCD_CLK         ;UM CICLO DE CLOCK DO DESLOCADOR
ENVIA_DADOS_2
    RLF     BUFFER, F       ;FAZ O DESLOCAMENTO PARA ESQUERDA
    DECFSZ  CONT_SERIAL, F  ;AINDA TEM BITS PARA ENVIAR?
    GOTO    ENVIA_DADOS_0       ;SIM, ENVIA PRÓXIMO BIT
    RETURN

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;* HABILITA O LCD                                                  *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
LCD_HABILITA
    BSF     LCD_EN          ;HABILITA LCD
    BCF     LCD_EN          ;DESABILITA LCD
    RETURN

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;* ESCREVE NO LCD                                                  *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
LCD_ESCREVE
    CALL    LCD_HABILITA        ;DÁ UM PULSO NO LCD ENABLE
    CALL    ATRASO_1ms      ;TEMPO DE SETUP
    RETURN

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;* ATRASO DE 20 ms                                                 *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
ATRASO_20ms
    MOVLW   0X9E
    MOVWF   TEMPO1
    MOVLW   0X10
    MOVWF   TEMPO2
ATRASO_20ms_0
    DECFSZ  TEMPO1, F
    GOTO    $+2
    DECFSZ  TEMPO2, F
    GOTO    ATRASO_20ms_0
    GOTO    $+1
    NOP
    RETURN

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;* ATRASO DE 1 ms                                                  *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
ATRASO_1ms
    MOVLW   0XC6
    MOVWF   TEMPO1
    MOVLW   0X01
    MOVWF   TEMPO2
ATRASO_1ms_0
    DECFSZ  TEMPO1, F
    GOTO    $+2
    DECFSZ  TEMPO2, F
    GOTO    ATRASO_1ms_0
    GOTO    $+1
    NOP
    RETURN

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;* ATRASO DE 5 ms                                                  *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
ATRASO_5ms
    MOVLW   0XE6
    MOVWF   TEMPO1
    MOVLW   0X04
    MOVWF   TEMPO2
ATRASO_5ms_0
    DECFSZ  TEMPO1, F
    GOTO    $+2
    DECFSZ  TEMPO2, F
    GOTO    ATRASO_5ms_0
    GOTO    $+1
    NOP
    RETURN

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;* EXECULTA UM COMANDOO                                            *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
LCD_COMANDO
    MOVWF   COMANDO         ;O BYTE DE CONTROLE ESTÁ NO WORK
    CALL    ATRASO_5ms      ;TEMPO DE SETUP, MAIOR QUE 4.1ms
    MOVF    COMANDO, W      ;MOVE O BYTE DE CONTROLE DE VOLTA PARA O WORK
    ANDLW   B'11110000'     ;MÁSCARA PARA SEPARAR O HIGH NIBBLE DO COMANDO
    MOVWF   FLAGS           ;O LOW NIBBLE CONTEM FLAGS DE CONTROLE
    BCF     LCD_RS          ;COLOCA O BIT DE LCD RS EM 0(LOW)
    MOVF    FLAGS, W        ;MOVE DE VOLTA PARA WORK
    CALL    ENVIA_DADOS     ;ENVIA HIGH NIBBLE DO COMANDO
    CALL    LCD_ESCREVE     ;ESCREVE NO LCD
    MOVF    COMANDO, W      ;MOVE BYTE DE COMANDO ORIGINAL PARA O WOK
    SWAPF   COMANDO, W      ;ROTACIONA OS NIBBLES DO BYTE DE COMANDO
    ANDLW   B'11110000'     ;MÁSCARA PARA SEPARAR O LOW NIBBLE DO COMANDO
    MOVWF   FLAGS           ;O LOW NIBBLE CONTEM FLAGS DE CONTROLE
    BCF     LCD_RS          ;COLOCA O BIT DE LCD RS EM 0(LOW)
    MOVF    FLAGS, W        ;MOVE DE VOLTA PARA WORK
    CALL    ENVIA_DADOS     ;ENVIA COMANDO LOW NIBBLE DO COMANDO
    CALL    LCD_ESCREVE     ;ESCREVE NO LCD
    RETURN


;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*INICIALIZA LCD                                                   *
;*CONFIGURA: INTERFACE, NÚMERO DE LINHAS, FONTE, DIREÇÃO DE SHIFT, *
;*RETORNA CURSOR PARA INICIO E LIMPA O LCD.                        *
;*BYTES DE COMANDOS EXTRAÍDOS DO DATASHEET DO LCD                  *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
LCD_CONFIGURA
    MOVLW   B'00101100'     ;CONFIGURA A INTERFACE 4 BITS, 2 LINHAS E FONTE 1:5 X 10 DOTS
    CALL    LCD_COMANDO     ;ENVIA COMANDO PARA LCD
    MOVLW   B'00000110'     ;SHIFT DO CURSOR PARA DIREITA
    CALL    LCD_COMANDO     ;ENVIA COMANDO PARA LCD
    MOVLW   B'00001111'     ;DISPLAY ON, BLINK ON, BLINK ON
    CALL    LCD_COMANDO     ;ENVIA COMANDO PARA LCD
    MOVLW   B'00000110'     ;SETA DIREÇÃO DO CURSOR DE SHIFT PARA DIREITA
    CALL    LCD_COMANDO     ;ENVIA COMANDO PARA LCD
    MOVLW   B'00000010'     ;RETORNA O CURSOR PARA O INICIO
    CALL    LCD_COMANDO     ;ENVIA COMANDO PARA LCD
    MOVLW   B'00000001'     ;LIMPA LCD
    CALL    LCD_COMANDO     ;ENVIA COMANDO PARA LCD
    MOVLW   B'00001100'     ;DESATIVA CURSOR
    CALL    LCD_COMANDO     ;ENVIA COMANDO PARA LCD
    RETURN

LCD_LIMPA
    MOVLW   B'00000001'     ;LIMPA LCD
    CALL    LCD_COMANDO     ;ENVIA COMANDO PARA LCD
    RETURN
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*ESCREVE DADOS NO LCD                                             *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
LCD_DADOS
    MOVWF   LCD_ASCII           ;O BYTE DO W PARA UM REGISTRADOR AUXILIAR
    CALL    ATRASO_5ms       ;TEMPO DE SETUP
    MOVF    LCD_ASCII,W     ;MOVE O BYTE DE VOLTA PARA O WORK
    ANDLW   B'11110000'     ;MÁSCARA PARA SEPARAR O HIGH NIBBLE
    MOVWF   FLAGS           ;O LOW NIBBLE CONTEM FLAGS DE CONTROLE
    BSF     LCD_RS          ;COLOCA O BIT DE LCD RS EM 1(HIGH)
    MOVF    FLAGS, W        ;MOVE DE VOLTA PARA WORK
    CALL    ENVIA_DADOS     ;ENVIA O HIGH NIBBLE
    CALL    LCD_ESCREVE     ;ESCREVE NO LCD
    MOVF    LCD_ASCII,W     ;MOVE O BYTE PARA WORK
    SWAPF   LCD_ASCII,W     ;FAZ O SWAP DOS NIBBLES
    ANDLW   B'11110000'     ;MÁSCARA PARA SEPARAR O LOW NIBBLE
    MOVWF   FLAGS           ;O LOW NIBBLE CONTEM FLAGS DE CONTROLE
    BSF     LCD_RS          ;COLOCA O BIT DE LCD RS EM 1(HIGH)
    MOVF    FLAGS, W        ;MOVE DE VOLTA PARA WORK
    CALL    ENVIA_DADOS     ;ENVIA O LOW NIBBLE
    CALL    LCD_ESCREVE     ;ESCREVE NO LCD
    RETURN


;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*CONVERSÃO BINÁRIO EM DECIMAL(8 BITS )                            *
;*SEPARA UM BYTE(0 ~ 255) EM CENTENA, DEZENA E UNIDADE             *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
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


;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*ESCREVE DADOS DA LEITURA DO ADC NO LCD                            *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
ESCREVE_DADOS
    MOVWF   ADC_LEITURA
    MOVLW   B'10000000'
    CALL    LCD_COMANDO
                            ;ESCREVE MENSAGEM "TENSÃO:"
    MOVLW   'T'
    CALL    LCD_DADOS
    MOVLW   'E'
    CALL    LCD_DADOS
    MOVLW   'N'
    CALL    LCD_DADOS
    MOVLW   'S'
    CALL    LCD_DADOS
    MOVLW   'A'
    CALL    LCD_DADOS
    MOVLW   '0'
    CALL    LCD_DADOS
    MOVLW   ':'
    CALL    LCD_DADOS
    MOVLW   ' '
    CALL    LCD_DADOS

    MOVLW   B'10001000'         ;ESCREVE TENSÃO
    CALL    LCD_COMANDO
    MOVF    ADC_LEITURA, W
    CALL    BIN2DEC             ;SEPARA VALOR EM CENTENA, DEZENA E UNIDADE
    MOVF    DEZENA, W           ;ESCREVE DEZENA
    ADDLW   0X30                ;CONVERTE PARA ASCII
    CALL    LCD_DADOS

    MOVLW   '.'
    CALL    LCD_DADOS

    MOVF    UNIDADE, W          ;ESCREVE UNIDADE
    ADDLW   0X30                ;CONVERTE PARA ASCII
    CALL    LCD_DADOS

    MOVLW   'V'
    CALL    LCD_DADOS
    RETURN

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                  MULTIPLICAÇÃO 8X8                              *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
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


;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                  DIVISÃO 8X8                                    *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
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


;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*           DIVISÃO 16 BITS POR 32(CONSTANTE)                     *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
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

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*           DIVISÃO 16 BITS POR 256(CONSTANTE)                    *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

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

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                     INICIO DO PROGRAMA                          *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

INICIO
    BANK1                   ;ALTERA PARA O BANCO 1
    MOVLW   B'00000100'     ;CONFIGURA PORTA GPIO 2 COMO ENTRADA(ADC)
    MOVWF   TRISIO          ;E O RESTO COMO SAÍDAS
    MOVLW   B'00010100'     ;SELECIONA AN2 E FOSC/8
    MOVWF   ANSEL
    MOVLW   B'00000100'
    MOVWF   OPTION_REG      ;DEFINE OPÇÕES DE OPERAÇÃO
    MOVLW   B'00000000'
    MOVWF   INTCON          ;DEFINE OPÇÕES DE INTERRUPÇÕES
    BANK0                   ;RETORNA PARA O BANCO
    MOVLW   B'00000111'
    MOVWF   CMCON           ;DEFINE O MODO DE OPERAÇÃO DO COMPARADOR ANALÓGICO
    MOVLW   B'00001001'     ;ATIVA ADC E SELECIONA AN2
    MOVWF   ADCON0

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                     INICIALIZAÇÃO DAS VARIÁVEIS                 *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    CLRF    GPIO            ;LIMPA O GPIO
    CALL    ATRASO_5ms      ;TEMPO DE SETUP

    CALL    LCD_INICIA_4BIT ;INICIALIZA O LCD NO MODO 4 BIT
    CALL    LCD_CONFIGURA   ;CONFIGURA LCD


;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                     ROTINA PRINCIPAL                            *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
MAIN
    BSF     ADCON0, GO      ;INICIA CONVERSÃO
    BTFSC   ADCON0, GO      ;HÁ DADOS LIDOS?
    GOTO    $-1             ;NÃO, ESPERA
    MOVF    ADRESH, W       ;MOVE VALOR LIDO PARA WORK
    MOVWF   ADC_LEITURA     ;MOVE VALOR LIDO PARA VARIÁVEL

    CLRF    ACM_L           ;LIMPA VARIÁVEL ACUMULADORA
    CLRF    ACM_H
    MOVLW   .32             ;FAZ 32 LEITURAS
    MOVWF   LEITURA
NOVA_LEITURA
    BSF     ADCON0, GO      ;INICIA CONVERSÃO
    BTFSC   ADCON0, GO      ;HÁ DADOS LIDOS?
    GOTO    $-1             ;NÃO, ESPERA
    MOVF    ADRESH, W       ;MOVE VALOR LIDO PARA WORK
    ADDWF   ACM_L, F        ;SOMA LEITURA NA VARIÁVEL ACUMULADORA
    BTFSC   STATUS, C       ;TEM CARRY?
    INCF    ACM_H, F        ;SIM, INCREMENTA NA PARTE ALTA
    DECFSZ  LEITURA, F      ;LEITURAS ACABARAM?
    GOTO    NOVA_LEITURA    ;NÃO, FAZ NOVA LEITURA

    CALL    DIV16X32        ;FAZ A DIVISÃO POR 32
    MOVF    ACM_L, W        ;MOVE RESULTADO PARA WORK
    MOVWF   MULCND          ;MULTIPLICANDO VALOR LIDO
    MOVLW   .50
    MOVWF   MULPLR          ;MULTIPLICADOR IGUAL 50( 5v X 10 PARA PRECISÃO DE 1 CASA)
    CALL    MUL8X8          ;FAZ A MULTIPLICAÇÃO
    CALL    DIV16X256       ;FAZ A DIVISÃO(VALOR_ADC*50)/255

    MOVLW   .100
    ADDWF   MUL_L, W        ;SOMA 100 AO RESULTADO
    CALL    ESCREVE_DADOS   ;ESCREVE DADOS NO LCD

    GOTO MAIN               ;LOOP



;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                       FIM DO PROGRAMA                           *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

    END