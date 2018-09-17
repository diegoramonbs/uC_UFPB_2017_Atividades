;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*              MODIFICA��ES PARA USO COM 12F675                   *
;*                FEITAS PELO PROF. MARDSON                        *
;*                    FEVEREIRO DE 2016                            *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

;MICROCONTROLADORES 2017-1
;ALUNO: DIEGO RAMON BEZERRA DA SILVA
;MATRICULA: 11228382
;DATA: 11/10/2017
;NOME DO PROJETO: CONTROLE DE PORTAS X FREQU�NCIA DE S�IDA
;DESCRI��O DO PROJETO: COMUNICA��O DO PIC12F65 COM UM PERIF�RICO
;EXTERNO. IMPLEMENTA��O DE UM TERMOHIGR�METRO COM SENSOR DHT-11

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                     ARQUIVOS DE DEFINI��ES                      *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#INCLUDE <P12F675.INC>  ;ARQUIVO PADR�O MICROCHIP PARA 12F675

    __CONFIG _BODEN_OFF & _CP_OFF & _PWRTE_ON & _WDT_OFF & _MCLRE_ON & _INTRC_OSC_NOCLKOUT

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                    PAGINA��O DE MEM�RIA                         *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;DEFINI��O DE COMANDOS DE USU�RIO PARA ALTERA��O DA P�GINA DE MEM�RIA
#DEFINE BANK0   BCF STATUS,RP0  ;SETA BANK 0 DE MEM�RIA
#DEFINE BANK1   BSF STATUS,RP0  ;SETA BANK 1 DE MAM�RIA

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                         VARI�VEIS                               *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; DEFINI��O DOS NOMES E ENDERE�OS DE TODAS AS VARI�VEIS UTILIZADAS 
; PELO SISTEMA

    CBLOCK  0x20    ;ENDERE�O INICIAL DA MEM�RIA DE
                    ;USU�RIO
        W_TEMP      ;REGISTRADORES TEMPOR�RIOS PARA USO
        STATUS_TEMP ;JUNTO �S INTERRUP��ES

        ;NOVAS VARI�VEIS
        TEMPO1      ;VARI�VEL USADA NO ATRASO
        TEMPO2      ;VARI�VEL USADA NO ATRASO
        TEMPO3      ;VARI�VEL USADA NO ATRASO
        BUFFER      ;BUFFER DE DADOS USADO PARA ENVIO DE DADOS  
        COMMAND     ;REGISTRADOR DE COMANDO SERIAL
        LCDDATA     ;REGISTRADOR DE DADOS LCD
        CONT_SERIAL ;CONTADOR DE ENVIO DE BITS
        FLAGS       ;REGISTRADOR FLAGS
        
        ;VARI�VEIS SENSOR DHT-11
        TEMP_I      ;TEMPERATURA INTEIRO
        TEMP_D      ;TEMPERATURA DECIMAL
        UMID_I      ;UMIDADE INTEIRO
        UMID_D      ;UMIDADE DECIMAL
		CHECK		;RESPOSTA CHECADA
		UNIDADE     ;UNIDADE
        DEZENA      ;DEZENA
        CENTENA     ;CENTENA
		BYTE		

    ENDC            ;FIM DO BLOCO DE MEM�RIA
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                        FLAGS INTERNOS                           *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; DEFINI��O DE TODOS OS FLAGS UTILIZADOS PELO SISTEMA

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                         CONSTANTES                              *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; DEFINI��O DE TODAS AS CONSTANTES UTILIZADAS PELO SISTEMA

#DEFINE LCD_RS  FLAGS, 3

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                           ENTRADAS                              *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; DEFINI��O DE TODOS OS PINOS QUE SER�O UTILIZADOS COMO ENTRADA
; RECOMENDAMOS TAMB�M COMENTAR O SIGNIFICADO DE SEUS ESTADOS (0 E 1)

#DEFINE DHT_DATA GPIO, 2
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                           SA�DAS                                *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; DEFINI��O DE TODOS OS PINOS QUE SER�O UTILIZADOS COMO SA�DA
; RECOMENDAMOS TAMB�M COMENTAR O SIGNIFICADO DE SEUS ESTADOS (0 E 1)

; PINOS DEFINIDOS COM BASE NO CIRCUITO FORNECIDO
#DEFINE LCD_CLK GPIO, 0     ;CLOCK DO DESLOCADOR DE 8 BITS
#DEFINE LCD_DAT GPIO, 4     ;PINO DE DADOS PARA O DESLOCADOR
#DEFINE LCD_EN  GPIO, 5     ;PINO PARA O ENABLE DO LCD


;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                       VETOR DE RESET                            *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

    ORG 0x00            ;ENDERE�O INICIAL DE PROCESSAMENTO
    GOTO    INICIO
    
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                    IN�CIO DA INTERRUP��O                        *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; ENDERE�O DE DESVIO DAS INTERRUP��ES. A PRIMEIRA TAREFA � SALVAR OS
; VALORES DE "W" E "STATUS" PARA RECUPERA��O FUTURA

    ORG 0x04            ;ENDERE�O INICIAL DA INTERRUP��O
    MOVWF   W_TEMP      ;COPIA W PARA W_TEMP
    SWAPF   STATUS,W
    MOVWF   STATUS_TEMP ;COPIA STATUS PARA STATUS_TEMP

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                    ROTINA DE INTERRUP��O                        *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; AQUI SER� ESCRITA AS ROTINAS DE RECONHECIMENTO E TRATAMENTO DAS
; INTERRUP��ES

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                 ROTINA DE SA�DA DA INTERRUP��O                  *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; OS VALORES DE "W" E "STATUS" DEVEM SER RECUPERADOS ANTES DE 
; RETORNAR DA INTERRUP��O

SAI_INT
    SWAPF   STATUS_TEMP,W
    MOVWF   STATUS      ;MOVE STATUS_TEMP PARA STATUS
    SWAPF   W_TEMP,F
    SWAPF   W_TEMP,W    ;MOVE W_TEMP PARA W
    RETFIE

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                   ROTINAS E SUBROTINAS                      *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; CADA ROTINA OU SUBROTINA DEVE POSSUIR A DESCRI��O DE FUNCIONAMENTO
; E UM NOME COERENTE �S SUAS FUN��ES.

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;* INICIALIZA O LCD COM BARRAMENTO DE 4 BITS                       *
;* SEQU�NCIA DE INICIALIZA��O DO DISPLAY LCD CONFORME DATASHEET    *                                           
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
LCD_INICIA_4BIT
    BCF     LCD_EN          ;HABILITA LCD
    MOVLW   B'00000000'     ;MODO COMANDO(RS = 0)
    CALL    ENVIA_DADOS       ;ENVIA DADOS PARA DESLOCADOR
    CALL    ATRASO_20ms      ;TEMPO DE SETUP 
    MOVLW   B'00110000'     ;BYTE QUE SER� ENVIADO NO WORK
    CALL    ENVIA_DADOS       ;ENVIA DADOS PARA DESLOCADOR
    CALL    LCD_ESCREVE       ;ESCREVE NO LCD
    CALL    ATRASO_5ms       ;TEMPO DE SETUP, MAIOR QUE 4.1ms
    MOVLW   B'00110000'     ;BYTE QUE SER� ENVIADO NO WORK
    CALL    ENVIA_DADOS       ;ENVIA DADOS PARA DESLOCADOR
    CALL    LCD_ESCREVE       ;ESCREVE NO LCD
    MOVLW   B'00110000'     ;BYTE QUE SER� ENVIADO NO WORK
    CALL    ENVIA_DADOS       ;ENVIA DADOS PARA DESLOCADOR
    CALL    LCD_ESCREVE       ;ESCREVE NO LCD
    MOVLW   B'00100000'     ;BYTE QUE SER� ENVIADO NO WORK
    CALL    ENVIA_DADOS       ;ENVIA DADOS PARA DESLOCADOR
    CALL    LCD_ESCREVE       ;ESCREVE NO LCD
    RETURN                  
    
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;* ENVIA DADOS PARA O SHIFT DE 8 BITS                              *
;* ROTINA RESPONS�VEL POR ENVIAR OS DADOS PARA O DESLOCADOR E,     *
;* CONSEQUENTEMENTE, PARA O DISPLAY LCD.                           *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
ENVIA_DADOS
    MOVWF   BUFFER          ;DADOS(COMANDO/DADOS) QUE SER� ENVIADO
    MOVLW   .8              ;LOOP USADO
    MOVWF   CONT_SERIAL     ;PARA ENVIAR 8 BITS, 1 POR 1
ENVIA_DADOS_0
            
    BTFSS   BUFFER, 7       ;VERIFICA VAI ENVIAR 1(HIGH)
    GOTO    ENVIA_DADOS_1     ;SEN�O, ENVIA 0(LOW)
    BSF     LCD_DAT         ;COLOCA A LIHA DE DADOS EM 1(HIGH)
    BSF     LCD_CLK         ;AVAN�A
    BCF     LCD_CLK         ;UM CICLO DE CLOCK DO DESLOCADOR
    GOTO    ENVIA_DADOS_2     ;VAI PARA VERIFICA��O DE T�RMINO
ENVIA_DADOS_1
    BCF     LCD_DAT         ;ENVIA 0(LOW)
    BSF     LCD_CLK         ;AVAN�A
    BCF     LCD_CLK         ;UM CICLO DE CLOCK DO DESLOCADOR
ENVIA_DADOS_2
    RLF     BUFFER, F       ;FAZ O DESLOCAMENTO PARA ESQUERDA
    DECFSZ  CONT_SERIAL, F  ;AINDA TEM BITS PARA ENVIAR?
    GOTO    ENVIA_DADOS_0     ;SIM, ENVIA PR�XIMO BIT
    RETURN                  ;SEN�O, RETONA


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
    CALL    LCD_HABILITA      ;D� UM PULSO NO LCD ENABLE
    CALL    ATRASO_1ms       ;TEMPO DE SETUP
    RETURN                  
    
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;* ATRASO DE 20 ms                                                 *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
ATRASO_20ms
    MOVLW   0X9E
    MOVWF   TEMPO1
    MOVLW   0X10
    MOVWF   TEMPO2
ATRASO_20MS_0
    DECFSZ  TEMPO1, F
    GOTO    $+2
    DECFSZ  TEMPO2, F
    GOTO    ATRASO_20MS_0
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
ATRASO_1MS_0
    DECFSZ  TEMPO1, F
    GOTO    $+2
    DECFSZ  TEMPO2, F
    GOTO    ATRASO_1MS_0
    GOTO    $+1
    NOP
    RETURN
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;* ATRASO DE 2 ms                                                  *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
ATRASO_2ms
    MOVLW   0X8E
    MOVWF   TEMPO1
    MOVLW   0X02
    MOVWF   TEMPO2
ATRASO_2MS_0
    DECFSZ  TEMPO1, F
    GOTO    $+2
    DECFSZ  TEMPO2, F
    GOTO    ATRASO_2MS_0
    GOTO    $+1
    NOP

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;* ATRASO DE 5 ms                                                  *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
ATRASO_5ms
    MOVLW   0XE6
    MOVWF   TEMPO1
    MOVLW   0X04
    MOVWF   TEMPO2
ATRASO_5MS_0
    DECFSZ  TEMPO1, F
    GOTO    $+2
    DECFSZ  TEMPO2, F
    GOTO    ATRASO_5MS_0
    GOTO    $+1
    NOP
    RETURN
    
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;* ATRASO DE 18 ms                                                 *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
ATRASO_18ms
    MOVLW   0X0E
    MOVWF   TEMPO1
    MOVLW   0X0F
    MOVWF   TEMPO2
ATRASO_18MS_0
    DECFSZ  TEMPO1, F
    GOTO    $+2
    DECFSZ  TEMPO2, F
    GOTO    ATRASO_18MS_0
    GOTO    $+1
    NOP
    RETURN
	
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;* ATRASO DE 30 us                                                 *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *	
ATRASO_30us
    MOVLW   0X08
    MOVWF   TEMPO1
DELAY_30US_0
    DECFSZ  TEMPO1, F
    GOTO    DELAY_30US_0
    NOP
    RETURN

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;* ATRASO DE 40 us                                                 *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
ATRASO_40us
    MOVLW   0X0B
    MOVWF   TEMPO1
ATRASO_40US_0
    DECFSZ  TEMPO1, F
    GOTO    ATRASO_40US_0
    GOTO    $+1
    RETURN  
 
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;* ATRASO DE 80 us                                                 *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
ATRASO_80us
    MOVLW   0X19
    MOVWF   TEMPO1
ATRASO_80US_0
    DECFSZ  TEMPO1, F
    GOTO   ATRASO_80US_0
    RETURN 

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;* ATRASO DE 1s                                                    *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *	
ATRASO_1s
    MOVLW   0X07
    MOVWF   TEMPO1
    MOVLW   0X2F
    MOVWF   TEMPO2
    MOVLW   0X03
    MOVWF   TEMPO3
DELAY_1S_0
    DECFSZ  TEMPO1, F
    GOTO    $+2
    DECFSZ  TEMPO2, F
    GOTO    $+2
    DECFSZ  TEMPO3, F
    GOTO    DELAY_1S_0
    GOTO    $+1
    GOTO    $+1
    GOTO    $+1
    RETURN
    
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;* EXECULTA UM COMMANDO                                            *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
LCD_COMANDO
    MOVWF   COMMAND         	;O BYTE DE CONTROLE EST� NO WORK
    CALL    ATRASO_5ms      	;TEMPO DE SETUP, MAIOR QUE 4.1ms
    MOVF    COMMAND, W      	;MOVE O BYTE DE CONTROLE DE VOLTA PARA O WORK
    ANDLW   B'11110000'     	;M�SCARA PARA SEPARAR O HIGH NIBBLE DO COMANDO
    MOVWF   FLAGS           	;O LOW NIBBLE CONTEM FLAGS DE CONTROLE
    BCF     LCD_RS          	;COLOCA O BIT DE LCD RS EM 0(LOW)
    MOVF    FLAGS, W        	;MOVE DE VOLTA PARA WORK
    CALL    ENVIA_DADOS     	;ENVIA HIGH NIBBLE DO COMANDO
    CALL    LCD_ESCREVE     	;ESCREVE NO LCD
    MOVF    COMMAND, W      	;MOVE BYTE DE COMANDO ORIGINAL PARA O WOK
    SWAPF   COMMAND, W      	;ROTACIONA OS NIBBLES DO BYTE DE COMANDO
    ANDLW   B'11110000'     	;M�SCARA PARA SEPARAR O LOW NIBBLE DO COMANDO   
    MOVWF   FLAGS           	;O LOW NIBBLE CONTEM FLAGS DE CONTROLE
    BCF     LCD_RS          	;COLOCA O BIT DE LCD RS EM 0(LOW)
    MOVF    FLAGS, W        	;MOVE DE VOLTA PARA WORK
    CALL    ENVIA_DADOS     	;ENVIA COMANDO LOW NIBBLE DO COMANDO
    CALL    LCD_ESCREVE     	;ESCREVE NO LCD
    RETURN                  


;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*INICIALIZA LCD                                                   *
;*CONFIGURA: INTERFACE, N�MERO DE LINHAS, FONTE, DIRE��O DE SHIFT, *
;*RETORNA CURSOR PARA INICIO E LIMPA O LCD.                        *
;*BYTES DE COMANDOS EXTRA�DOS DO DATASHEET DO LCD                  *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
LCD_CONFIGURA
    MOVLW   B'00101100'     	;CONFIGURA A INTERFACE 4 BITS, 2 LINHAS E FONTE 1:5 X 10 DOTS
    CALL    LCD_COMANDO     	;ENVIA COMANDO PARA LCD
    MOVLW   B'00000110'     	;SHIFT DO CURSOR PARA DIREITA
    CALL    LCD_COMANDO     	;ENVIA COMANDO PARA LCD
    MOVLW   B'00001111'     	;DISPLAY ON, BLINK ON, BLINK ON
    CALL    LCD_COMANDO     	;ENVIA COMANDO PARA LCD
    MOVLW   B'00000110'     	;SETA DIRE��O DO CURSOR DE SHIFT PARA DIREITA
    CALL    LCD_COMANDO     	;ENVIA COMANDO PARA LCD
    MOVLW   B'00000010'     	;RETORNA O CURSOR PARA O INICIO
    CALL    LCD_COMANDO     	;ENVIA COMANDO PARA LCD
    MOVLW   B'00000001'     	;LIMPA LCD
    CALL    LCD_COMANDO     	;ENVIA COMANDO PARA LCD
    MOVLW   B'00001100'     	;DESATIVA CURSOR
    CALL    LCD_COMANDO     	;ENVIA COMANDO PARA LCD
    RETURN                  

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*LIMPA DADOS NO LCD                                               *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
LCD_LIMPA
    MOVLW   B'00000001'     	;LIMPA LCD
    CALL    LCD_COMANDO     	;ENVIA COMANDO PARA LCD
    RETURN

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*ESCREVE DADOS NO LCD                                             *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
LCD_DADOS
    MOVWF   LCDDATA         	;O BYTE DO W PARA UM REGISTRADOR AUXILIAR
    CALL    ATRASO_5ms      	;TEMPO DE SETUP
    MOVF    LCDDATA,W       	;MOVE O BYTE DE VOLTA PARA O WORK
    ANDLW   B'11110000'     	;M�SCARA PARA SEPARAR O HIGH NIBBLE
    MOVWF   FLAGS           	;O LOW NIBBLE CONTEM FLAGS DE CONTROLE
    BSF     LCD_RS          	;COLOCA O BIT DE LCD RS EM 1(HIGH)
    MOVF    FLAGS, W        	;MOVE DE VOLTA PARA WORK
    CALL    ENVIA_DADOS     	;ENVIA O HIGH NIBBLE
    CALL    LCD_ESCREVE     	;ESCREVE NO LCD
    MOVF    LCDDATA,W       	;MOVE O BYTE PARA WORK  
    SWAPF   LCDDATA,W       	;FAZ O SWAP DOS NIBBLES
    ANDLW   B'11110000'     	;M�SCARA PARA SEPARAR O LOW NIBBLE
    MOVWF   FLAGS           	;O LOW NIBBLE CONTEM FLAGS DE CONTROLE
    BSF     LCD_RS          	;COLOCA O BIT DE LCD RS EM 1(HIGH)
    MOVF    FLAGS, W        	;MOVE DE VOLTA PARA WORK
    CALL    ENVIA_DADOS     	;ENVIA O LOW NIBBLE
    CALL    LCD_ESCREVE     	;ESCREVE NO LCD
    RETURN                  

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*CENTRALIZA CURSOR NO LCD                                         *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *    
LCD_CENTRALIZA
    MOVLW   B'00011100'     	;CURSOR SHIFT PARA DIREITA
    CALL    LCD_COMANDO     	;ENVIA COMANDO PARA LCD
    MOVLW   B'00011100'     	;CURSOR SHIFT PARA DIREITA
    CALL    LCD_COMANDO     	;ENVIA COMANDO PARA LCD
    MOVLW   B'00011100'     	;CURSOR SHIFT PARA DIREITA
    CALL    LCD_COMANDO     	;ENVIA COMANDO PARA LCD
    MOVLW   B'00011100'     	;CURSOR SHIFT PARA DIREITA
    CALL    LCD_COMANDO     	;ENVIA COMANDO PARA LCD
    MOVLW   B'00011100'     	;CURSOR SHIFT PARA DIREITA
    CALL    LCD_COMANDO     	;ENVIA COMANDO PARA LCD
    RETURN                  
    
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*CONVERS�O BIN�RIO EM DECIMAL(8 BITS )                            *
;*SEPARA UM BYTE(0 ~ 255) EM CENTENA, DEZENA E UNIDADE             *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
BIN2ASCII
    MOVWF   UNIDADE             ;COPIA O N�MERO A SER CONVERTIDO PARA VARI�VEL
    CLRF    DEZENA              ;LIMPA DEZENAS
    CLRF    CENTENA             ;LIMPA CENTENAS
BIN2DEC_0
    MOVLW   .100                ;SUBTRAI 100 DECIMAL
    SUBWF   UNIDADE, W          ;DO VALOR A SER CONVERTIDO E GUARDA EM W
    BTFSS   STATUS, C           ;O RESULTADO � POSITIVO OU ZERO?
    GOTO    BIN2DEC_1           ;N�O, VAI PARA PR�XIMA PARTE
    MOVWF   UNIDADE             ;SIM, ENT�O COPIA O VALOR PARA AS UNIDADES
    INCF    CENTENA, F          ;INCREMENTA UM NAS CENTENAS
    GOTO    BIN2DEC_0           ;CONTINUA
BIN2DEC_1
    MOVLW   .10                 ;SUBTRAI 10 DECIMAL
    SUBWF   UNIDADE, W          ;DO VALOR A SER CONVERTIDO E GUARDA EM W
    BTFSS   STATUS, C           ;O RESULTADO � POSITIVO OU ZERO?
    GOTO	TO_ASCII            ;N�O, TERMINOU
    MOVWF   UNIDADE             ;SIM, ENT�O COPIA O VALOR PARA AS UNIDADES
    INCF    DEZENA, F           ;INCREMENTA UM NAS DEZENAS
    GOTO    BIN2DEC_1           ;CONTINUA
	
TO_ASCII
	BCF		STATUS, C			;LIMPA CARRY
	MOVLW	.48
	ADDWF	UNIDADE, F			;CONVERTE VALOR HEX PARA ASCII
	MOVLW	.48
	ADDWF	DEZENA, F			;CONVERTE VALOR HEX PARA ASCII
	MOVLW	.48
	ADDWF	CENTENA, F			;CONVERTE VALOR HEX PARA ASCII
	BCF		STATUS, C			;LIMPA CARRY
	RETURN

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*INICIALIZA O SENSOR DHT11                                        *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
DTH11_INICIALIZA
    BANK1
    MOVLW   B'00000000'      ;SELECIONA GPIO-2 COMO SA�DA
    MOVWF   TRISIO
    BANK0
    BCF     DHT_DATA
    CALL    ATRASO_18ms
    BSF     DHT_DATA
    CALL    ATRASO_40us
    BANK1
    MOVLW   B'00000100'      ;SELECIONA GPIO-2 COMO ENTRADA
    MOVWF   TRISIO
    BANK0
    RETURN
	
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*CHECA A RESPOSTA DO SENSOR DHT11                                 *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
DHT11_RESPOSTA
	CLRF	CHECK			;LIMPA FLAG DE CHECAGEM
	CALL	ATRASO_40us
	BTFSS	DHT_DATA
	CALL	ATRASO_80us
	BTFSS	DHT_DATA
	RETURN
	COMF 	CHECK
	CALL	ATRASO_40us
	RETURN

	
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*IDENTIFICA BIT 0 OU 1 BASEADO NA LARGURA DO PULSO (TIMER1)       * 
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *	
LER_BYTE_T
	CLRF	BYTE			;LIMPA REGISTRADOR
	MOVLW   .8              ;LOOP USADO
    MOVWF   CONT_SERIAL     ;PARA LER 8 BITS, 1 POR 1
LER_BYTE_T_0
	RLF     BYTE, F         ;FAZ O DESLOCAMENTO PARA ESQUERDA
	CLRF	TMR1L			;LIMPA TIMER1 LOW
	BTFSS	DHT_DATA		;EST� EM HIGH
	GOTO	$-1				;ESPERA
	BSF		T1CON, TMR1ON	;INICIA TIMER1
	BTFSC	DHT_DATA		;EST� EM LOW
	GOTO	$-1				;ESPERA
	BCF		T1CON, TMR1ON	;PARA TIMER1
	
	MOVFW	TMR1L
	SUBLW	.30				;SE TRM1L > 30, ENT�O BIT = 1
	BTFSS	STATUS, C
	BSF		BYTE, 0
	BCF		STATUS, C		;LIMPA CARRY

	DECFSZ  CONT_SERIAL, F  ;AINDA TEM BITS PARA RECEBER?
	GOTO	LER_BYTE_T_0	;SIM, CONTINUA
	RETURN
	

	
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*LER DADOS DO SENSOR DHT11                                        *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *	
DHT11_LER_DADOS

    CLRF 	UMID_I      	;LIMPA REGISTRADOR
	CALL	LER_BYTE_T		;LER UM BYTE DO SENSOR
	MOVFW	BYTE			;MOVE FALOR LIDO PARA REGISTRADOR 
	MOVWF	UMID_I

	CLRF 	UMID_D      	;LIMPA REGISTRADOR	
	CALL	LER_BYTE_T		;LER UM BYTE DO SENSOR
	MOVFW	BYTE			;MOVE FALOR LIDO PARA REGISTRADOR 
	MOVWF	UMID_D
		
	CLRF	TEMP_I      	;LIMPA REGISTRADOR
	CALL	LER_BYTE_T		;LER UM BYTE DO SENSOR
	MOVFW	BYTE			;MOVE FALOR LIDO PARA REGISTRADOR 
	MOVWF	TEMP_I
	
	CLRF 	TEMP_D      	;LIMPA REGISTRADOR
	CALL	LER_BYTE_T		;LER UM BYTE DO SENSOR
	MOVFW	BYTE			;MOVE FALOR LIDO PARA REGISTRADOR 
	MOVWF	TEMP_D
	

	RETURN
	

   
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*ESCREVER DADOS NO LCD                                        	   *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *	
LCD_ESCREVE_DADOS
	MOVLW   B'10000000'
    CALL    LCD_COMANDO
								;ESCREVE MENSAGEM "DIEGO"
	MOVLW	'D'
	CALL	LCD_DADOS
	MOVLW	'I'
	CALL	LCD_DADOS
	MOVLW	'E'
	CALL	LCD_DADOS
	MOVLW	'G'
	CALL	LCD_DADOS
	MOVLW	'O'
	CALL	LCD_DADOS
	
	MOVLW   B'11000000'         ;ESCREVE TEMPERATURA
    CALL    LCD_COMANDO
	
	MOVLW	'T'
	CALL	LCD_DADOS
	MOVLW	'e'
	CALL	LCD_DADOS
	MOVLW	'm'
	CALL	LCD_DADOS
	MOVLW	'p'
	CALL	LCD_DADOS
	MOVLW	'='
	CALL	LCD_DADOS
	
    MOVF    TEMP_I, W
	CALL    BIN2ASCII        ;SEPARA VALOR EM CENTENA, DEZENA E UNIDADE

	MOVFW   DEZENA         	;ESCREVE DEZENA
    CALL    LCD_DADOS
	
	MOVFW   UNIDADE        	;ESCREVE DEZENA
    CALL    LCD_DADOS

	MOVLW   'C'
    CALL    LCD_DADOS
	
	MOVLW   ' '       		;ESCREVE UMIDADE
    CALL    LCD_DADOS

							;ESCREVE MENSAGEM "UR="
	MOVLW	'U'
	CALL	LCD_DADOS
	MOVLW	'R'
	CALL	LCD_DADOS
	MOVLW	'='
	CALL	LCD_DADOS

	MOVLW   B'11001100'    	;ESCREVE UMIDADE
    CALL    LCD_COMANDO
	
    MOVF    UMID_I, W
	CALL    BIN2ASCII     	;SEPARA VALOR EM CENTENA, DEZENA E UNIDADE
	
	MOVFW   DEZENA       	;ESCREVE DEZENA
    CALL    LCD_DADOS
	
	MOVFW   UNIDADE      	;ESCREVE DEZENA
    CALL    LCD_DADOS

	MOVLW   '%'
    CALL    LCD_DADOS


	RETURN  
  
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                     INICIO DO PROGRAMA                          *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
INICIO
    BANK1                       ;ALTERA PARA O BANCO 1
    MOVLW   B'00000000'         ;CONFIGURA TODAS AS PORTAS DO GPIO (PINOS)
    MOVWF   TRISIO              ;COMO SA�DAS
    CLRF    ANSEL               ;DEFINE PORTAS COMO Digital I/O
    MOVLW   B'00000100'
    MOVWF   OPTION_REG          ;DEFINE OP��ES DE OPERA��O
    MOVLW   B'11000000'
    MOVWF   INTCON              ;DEFINE OP��ES DE INTERRUP��ES
	
	;CALL	3FFh				;CALIBRA O OSCILADOR INTERNO
	;MOVWF	OSCCAL
	
    BANK0                       ;RETORNA PARA O BANCO
    MOVLW   B'00000111'
    MOVWF   CMCON               ;DEFINE O MODO DE OPERA��O DO COMPARADOR ANAL�GICO
	
	MOVLW   B'00000001'
    MOVWF   T1CON              	;DEFINE O MODO DE OPERA��O TIMER 1

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                     INICIALIZA��O DAS VARI�VEIS                 *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    CLRF    GPIO                ;LIMPA GPIO
    CALL    ATRASO_5ms          ;TEMPO DE SETUP
    
    CALL    LCD_INICIA_4BIT  	;INICIALIZA O LCD NO MODO 4 BIT
    CALL    LCD_CONFIGURA       ;CONFIGURA LCD 
	
    
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                     ROTINA PRINCIPAL                            *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
MAIN

	CALL    DTH11_INICIALIZA    ;INICIALIZA O SENSOR DHT11
	CALL	DHT11_RESPOSTA		;VERIFICA REPOSTA DO SENSOR
	
	BTFSS	CHECK, 0			;DHT11 RESPONDEU?
	GOTO	ESTADO_ERROR		;N�O
	GOTO	ESTADO_OK			;SIM

ESTADO_OK
	CALL	DHT11_LER_DADOS		;SE SIM, LER OS DADOS
	CALL	LCD_ESCREVE_DADOS	;ENT�O, ESCREVE NO LCD
	GOTO	FIM					
ESTADO_ERROR
	NOP							;SE HOUVE ERRO, N�O FAZ NADA
FIM
	CALL	ATRASO_1s			;REALIZA UMA MEDI��O A CADA 1S
    GOTO 	MAIN				;E REENICIA O CICLO.

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                       FIM DO PROGRAMA                           *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


    END