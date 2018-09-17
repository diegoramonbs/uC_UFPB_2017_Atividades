;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*              MODIFICAÇÕES PARA USO COM 12F675                   *
;*                FEITAS PELO PROF. MARDSON                        *
;*                    FEVEREIRO DE 2016                            *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

;MICROCONTROLADORES 2017-1
;ALUNO: DIEGO RAMON BEZERRA DA SILVA
;MATRICULA: 11228382
;DATA: 07/09/2017
;NOME DO PROJETO: MANIPULAÇÃO DO LCD COM PIC 12F675
;DESCRIÇÃO DO PROJETO: EXERCÍCIO DE MANIPULAÇÃO DO LCD
;TENDO COMO OBJETIVO FAZER A ESCRITA DO NOME NO LCD.

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                     ARQUIVOS DE DEFINIÇÕES                      *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#INCLUDE <P12F675.INC>	;ARQUIVO PADRÃO MICROCHIP PARA 12F675

	__CONFIG _BODEN_OFF & _CP_OFF & _PWRTE_ON & _WDT_OFF & _MCLRE_ON & _INTRC_OSC_NOCLKOUT

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                    PAGINAÇÃO DE MEMÓRIA                         *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;DEFINIÇÃO DE COMANDOS DE USUÁRIO PARA ALTERAÇÃO DA PÁGINA DE MEMÓRIA
#DEFINE	BANK0	BCF STATUS,RP0	;SETA BANK 0 DE MEMÓRIA
#DEFINE	BANK1	BSF STATUS,RP0	;SETA BANK 1 DE MAMÓRIA

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                         VARIÁVEIS                               *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; DEFINIÇÃO DOS NOMES E ENDEREÇOS DE TODAS AS VARIÁVEIS UTILIZADAS 
; PELO SISTEMA

	CBLOCK	0x20	;ENDEREÇO INICIAL DA MEMÓRIA DE
					;USUÁRIO
		W_TEMP		;REGISTRADORES TEMPORÁRIOS PARA USO
		STATUS_TEMP	;JUNTO ÀS INTERRUPÇÕES

		;NOVAS VARIÁVEIS
		TEMPO1		;VARIÁVEL USADA NO DELAY
		TEMPO2		;VARIÁVEL USADA NO DELAY
		TEMPO3		;VARIÁVEL USADA NO DELAY
		BUFFER		;BUFFER DE DADOS USADO PARA ENVIO DE DADOS	
		COMMAND		;REGISTRADOR DE COMANDO SERIAL
		LCDDATA		;REGISTRADOR DE DADOS LCD
		CONT_SERIAL	;CONTADOR DE ENVIO DE BITS
		FLAGS		;REGISTRADOR FLAGS
		

	ENDC			;FIM DO BLOCO DE MEMÓRIA
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                        FLAGS INTERNOS                           *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; DEFINIÇÃO DE TODOS OS FLAGS UTILIZADOS PELO SISTEMA

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                         CONSTANTES                              *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; DEFINIÇÃO DE TODAS AS CONSTANTES UTILIZADAS PELO SISTEMA

#DEFINE LCD_RS	FLAGS, 3

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                           ENTRADAS                              *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; DEFINIÇÃO DE TODOS OS PINOS QUE SERÃO UTILIZADOS COMO ENTRADA
; RECOMENDAMOS TAMBÉM COMENTAR O SIGNIFICADO DE SEUS ESTADOS (0 E 1)

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                           SAÍDAS                                *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; DEFINIÇÃO DE TODOS OS PINOS QUE SERÃO UTILIZADOS COMO SAÍDA
; RECOMENDAMOS TAMBÉM COMENTAR O SIGNIFICADO DE SEUS ESTADOS (0 E 1)

; PINOS DEFINIDOS COM BASE NO CIRCUITO FORNECIDO
#DEFINE	LCD_CLK	GPIO, 0		;CLOCK DO DESLOCADOR DE 8 BITS
#DEFINE LCD_DAT	GPIO, 4		;PINO DE DADOS PARA O DESLOCADOR
#DEFINE LCD_EN	GPIO, 5		;PINO PARA O ENABLE DO LCD


;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                       VETOR DE RESET                            *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

	ORG	0x00			;ENDEREÇO INICIAL DE PROCESSAMENTO
	GOTO	INICIO
	
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                    INÍCIO DA INTERRUPÇÃO                        *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; ENDEREÇO DE DESVIO DAS INTERRUPÇÕES. A PRIMEIRA TAREFA É SALVAR OS
; VALORES DE "W" E "STATUS" PARA RECUPERAÇÃO FUTURA

	ORG	0x04			;ENDEREÇO INICIAL DA INTERRUPÇÃO
	MOVWF	W_TEMP		;COPIA W PARA W_TEMP
	SWAPF	STATUS,W
	MOVWF	STATUS_TEMP	;COPIA STATUS PARA STATUS_TEMP

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
	SWAPF	STATUS_TEMP,W
	MOVWF	STATUS		;MOVE STATUS_TEMP PARA STATUS
	SWAPF	W_TEMP,F
	SWAPF	W_TEMP,W	;MOVE W_TEMP PARA W
	RETFIE

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*	            	 ROTINAS E SUBROTINAS                      *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; CADA ROTINA OU SUBROTINA DEVE POSSUIR A DESCRIÇÃO DE FUNCIONAMENTO
; E UM NOME COERENTE ÀS SUAS FUNÇÕES.

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;* INICIALIZA O LCD COM BARRAMENTO DE 4 BITS                       *
;* SEQUÊNCIA DE INICIALIZAÇÃO DO DISPLAY LCD CONFORME DATASHEET    *                                           
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
LCD_INIT_MODE_4BIT
	BCF		LCD_EN			;HABILITA LCD
	MOVLW	B'00000000'		;MODO COMANDO(RS = 0)
	CALL	SEND_DATA		;ENVIA DADOS PARA DESLOCADOR
	CALL	DELAY_20ms		;TEMPO DE SETUP	
	MOVLW	B'00110000'		;BYTE QUE SERÁ ENVIADO NO WORK
	CALL	SEND_DATA		;ENVIA DADOS PARA DESLOCADOR
	CALL	LCD_WRITE		;ESCREVE NO LCD
	CALL	DELAY_5ms		;TEMPO DE SETUP, MAIOR QUE 4.1ms
	MOVLW	B'00110000'		;BYTE QUE SERÁ ENVIADO NO WORK
	CALL	SEND_DATA		;ENVIA DADOS PARA DESLOCADOR
	CALL	LCD_WRITE		;ESCREVE NO LCD
	MOVLW	B'00110000'		;BYTE QUE SERÁ ENVIADO NO WORK
	CALL	SEND_DATA		;ENVIA DADOS PARA DESLOCADOR
	CALL	LCD_WRITE		;ESCREVE NO LCD
	MOVLW	B'00100000'		;BYTE QUE SERÁ ENVIADO NO WORK
	CALL	SEND_DATA		;ENVIA DADOS PARA DESLOCADOR
	CALL	LCD_WRITE		;ESCREVE NO LCD
	RETURN					
	
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;* ENVIA DADOS PARA O SHIFT DE 8 BITS                              *
;* ROTINA RESPONSÁVEL POR ENVIAR OS DADOS PARA O DESLOCADOR E,     *
;* CONSEQUENTEMENTE, PARA O DISPLAY LCD.                           *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
SEND_DATA
	MOVWF	BUFFER			;DADOS(COMANDO/DADOS) QUE SERÁ ENVIADO
	MOVLW	.8				;LOOP USADO
	MOVWF	CONT_SERIAL		;PARA ENVIAR 8 BITS, 1 POR 1
SEND_DATA_0
			
	BTFSS	BUFFER, 7		;VERIFICA VAI ENVIAR 1(HIGH)
	GOTO	SEND_DATA_1		;SENÃO, ENVIA 0(LOW)
	BSF		LCD_DAT			;COLOCA A LIHA DE DADOS EM 1(HIGH)
	BSF		LCD_CLK			;AVANÇA
	BCF		LCD_CLK			;UM CICLO DE CLOCK DO DESLOCADOR
	GOTO	SEND_DATA_2		;VAI PARA VERIFICAÇÃO DE TÉRMINO
SEND_DATA_1
	BCF		LCD_DAT			;ENVIA 0(LOW)
	BSF		LCD_CLK			;AVANÇA
	BCF		LCD_CLK			;UM CICLO DE CLOCK DO DESLOCADOR
SEND_DATA_2
	RLF		BUFFER, F		;FAZ O DESLOCAMENTO PARA ESQUERDA
	DECFSZ	CONT_SERIAL, F	;AINDA TEM BITS PARA ENVIAR?
	GOTO	SEND_DATA_0		;SIM, ENVIA PRÓXIMO BIT
	RETURN					;SENÃO, RETONA


;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;* HABILITA O LCD                                                  *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
LCD_ENABLE
	BSF		LCD_EN			;HABILITA LCD
	BCF		LCD_EN			;DESABILITA LCD
	RETURN					
	
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;* ESCREVE NO LCD                                                  *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *	
LCD_WRITE
	CALL	LCD_ENABLE		;DÁ UM PULSO NO LCD ENABLE
	CALL	DELAY_1ms		;TEMPO DE SETUP
	RETURN					
	
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;* ATRASO DE 20 ms                                                 *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
DELAY_20ms
    MOVLW   0X9E
    MOVWF   TEMPO1
    MOVLW   0X10
    MOVWF   TEMPO2
DELAY_20MS_0
    DECFSZ  TEMPO1, F
    GOTO    $+2
    DECFSZ  TEMPO2, F
    GOTO    DELAY_20MS_0
    GOTO    $+1
    NOP
	RETURN
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;* ATRASO DE 1 ms                                                  *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *	
DELAY_1ms
    MOVLW   0XC6
    MOVWF   TEMPO1
    MOVLW   0X01
    MOVWF   TEMPO2
DELAY_1MS_0
    DECFSZ  TEMPO1, F
    GOTO    $+2
    DECFSZ  TEMPO2, F
    GOTO    DELAY_1MS_0
    GOTO    $+1
    NOP
    RETURN
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;* ATRASO DE 2 ms                                                  *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
DELAY_2ms
    MOVLW   0X8E
    MOVWF   TEMPO1
    MOVLW   0X02
    MOVWF   TEMPO2
DELAY_2MS_0
    DECFSZ  TEMPO1, F
    GOTO    $+2
    DECFSZ  TEMPO2, F
    GOTO    DELAY_2MS_0
    GOTO    $+1
    NOP

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;* ATRASO DE 5 ms                                                  *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
DELAY_5ms
    MOVLW   0XE6
    MOVWF   TEMPO1
    MOVLW   0X04
    MOVWF   TEMPO2
DELAY_5MS_0
    DECFSZ  TEMPO1, F
    GOTO    $+2
    DECFSZ  TEMPO2, F
    GOTO    DELAY_5MS_0
    GOTO    $+1
    NOP
    RETURN
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;* EXECULTA UM COMMANDO                         				   *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
LCD_COMMAND
	MOVWF	COMMAND			;O BYTE DE CONTROLE ESTÁ NO WORK
	CALL	DELAY_5ms		;TEMPO DE SETUP, MAIOR QUE 4.1ms
	MOVF	COMMAND, W		;MOVE O BYTE DE CONTROLE DE VOLTA PARA O WORK
	ANDLW	B'11110000'		;MÁSCARA PARA SEPARAR O HIGH NIBBLE DO COMANDO
	MOVWF	FLAGS			;O LOW NIBBLE CONTEM FLAGS DE CONTROLE
	BCF		LCD_RS			;COLOCA O BIT DE LCD RS EM 0(LOW)
	MOVF    FLAGS, W		;MOVE DE VOLTA PARA WORK
	CALL	SEND_DATA		;ENVIA HIGH NIBBLE DO COMANDO
	CALL	LCD_WRITE		;ESCREVE NO LCD
	MOVF	COMMAND, W		;MOVE BYTE DE COMANDO ORIGINAL PARA O WOK
	SWAPF	COMMAND, W		;ROTACIONA OS NIBBLES DO BYTE DE COMANDO
	ANDLW	B'11110000'		;MÁSCARA PARA SEPARAR O LOW NIBBLE DO COMANDO	
	MOVWF	FLAGS			;O LOW NIBBLE CONTEM FLAGS DE CONTROLE
	BCF		LCD_RS			;COLOCA O BIT DE LCD RS EM 0(LOW)
	MOVF    FLAGS, W		;MOVE DE VOLTA PARA WORK
	CALL	SEND_DATA		;ENVIA COMANDO LOW NIBBLE DO COMANDO
	CALL	LCD_WRITE		;ESCREVE NO LCD
	RETURN					


;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*INICIALIZA LCD                                                   *
;*CONFIGURA: INTERFACE, NÚMERO DE LINHAS, FONTE, DIREÇÃO DE SHIFT, *
;*RETORNA CURSOR PARA INICIO E LIMPA O LCD.                        *
;*BYTES DE COMANDOS EXTRAÍDOS DO DATASHEET DO LCD                  *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
LCD_CONFIGURE
	MOVLW	B'00101100'		;CONFIGURA A INTERFACE 4 BITS, 2 LINHAS E FONTE 1:5 X 10 DOTS
	CALL	LCD_COMMAND		;ENVIA COMANDO PARA LCD
	MOVLW	B'00000110'		;SHIFT DO CURSOR PARA DIREITA
	CALL	LCD_COMMAND		;ENVIA COMANDO PARA LCD
	MOVLW	B'00001111'		;DISPLAY ON, BLINK ON, BLINK ON
	CALL	LCD_COMMAND		;ENVIA COMANDO PARA LCD
	MOVLW	B'00000110'		;SETA DIREÇÃO DO CURSOR DE SHIFT PARA DIREITA
	CALL	LCD_COMMAND		;ENVIA COMANDO PARA LCD
	MOVLW	B'00000010'		;RETORNA O CURSOR PARA O INICIO
	CALL	LCD_COMMAND		;ENVIA COMANDO PARA LCD
	MOVLW	B'00000001'		;LIMPA LCD
	CALL	LCD_COMMAND		;ENVIA COMANDO PARA LCD
	MOVLW	B'00001100'		;DESATIVA CURSOR
	CALL	LCD_COMMAND		;ENVIA COMANDO PARA LCD
	RETURN					


;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*ESCREVE DADOS NO LCD                        					   *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
LCD_DATA
	MOVWF	LCDDATA			;O BYTE DO W PARA UM REGISTRADOR AUXILIAR
	CALL    DELAY_5ms       ;TEMPO DE SETUP
	MOVF    LCDDATA,W		;MOVE O BYTE DE VOLTA PARA O WORK
	ANDLW   B'11110000'		;MÁSCARA PARA SEPARAR O HIGH NIBBLE
	MOVWF	FLAGS			;O LOW NIBBLE CONTEM FLAGS DE CONTROLE
	BSF		LCD_RS			;COLOCA O BIT DE LCD RS EM 1(HIGH)
	MOVF    FLAGS, W		;MOVE DE VOLTA PARA WORK
	CALL    SEND_DATA		;ENVIA O HIGH NIBBLE
	CALL	LCD_WRITE		;ESCREVE NO LCD
	MOVF    LCDDATA,W		;MOVE O BYTE PARA WORK	
	SWAPF   LCDDATA,W		;FAZ O SWAP DOS NIBBLES
	ANDLW   B'11110000'     ;MÁSCARA PARA SEPARAR O LOW NIBBLE
	MOVWF	FLAGS			;O LOW NIBBLE CONTEM FLAGS DE CONTROLE
	BSF		LCD_RS			;COLOCA O BIT DE LCD RS EM 1(HIGH)
	MOVF    FLAGS, W		;MOVE DE VOLTA PARA WORK
	CALL    SEND_DATA		;ENVIA O LOW NIBBLE
	CALL	LCD_WRITE		;ESCREVE NO LCD
	RETURN					

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*CENTRALIZA CURSOR NO LCD                       				   *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *	
CENTRALIZE
	MOVLW	B'00011100'		;CURSOR SHIFT PARA DIREITA
	CALL	LCD_COMMAND		;ENVIA COMANDO PARA LCD
	MOVLW	B'00011100'		;CURSOR SHIFT PARA DIREITA
	CALL	LCD_COMMAND		;ENVIA COMANDO PARA LCD
	MOVLW	B'00011100'		;CURSOR SHIFT PARA DIREITA
	CALL	LCD_COMMAND		;ENVIA COMANDO PARA LCD
	MOVLW	B'00011100'		;CURSOR SHIFT PARA DIREITA
	CALL	LCD_COMMAND		;ENVIA COMANDO PARA LCD
	MOVLW	B'00011100'		;CURSOR SHIFT PARA DIREITA
	CALL	LCD_COMMAND		;ENVIA COMANDO PARA LCD
	RETURN					
	
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*ESCREVE MEU NOME NO LCD                       				   *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
WRITE_MY_NAME
	CALL	CENTRALIZE		;CENTRALIZA
	MOVLW	B'01000100'		;CARREGA ASCII DO D 
	CALL	LCD_DATA		;ESCREVE NO LCD
	MOVLW	B'01001001'		;CARREGA ASCII DO I 
	CALL	LCD_DATA		;ESCREVE NO LCD
	MOVLW	B'01000101'		;CARREGA ASCII DO E 
	CALL	LCD_DATA		;ESCREVE NO LCD
	MOVLW	B'01000111'		;CARREGA ASCII DO G 
	CALL	LCD_DATA		;ESCREVE NO LCD
	MOVLW	B'01001111'		;CARREGA ASCII DO 0 
	CALL	LCD_DATA		;ESCREVE NO LCD
	RETURN
	
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                     INICIO DO PROGRAMA                          *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	
INICIO
	BANK1						;ALTERA PARA O BANCO 1
	MOVLW	B'00000000' 		;CONFIGURA TODAS AS PORTAS DO GPIO (PINOS)
	MOVWF	TRISIO				;COMO SAÍDAS
	CLRF	ANSEL 				;DEFINE PORTAS COMO Digital I/O
	MOVLW	B'00000100'
	MOVWF	OPTION_REG			;DEFINE OPÇÕES DE OPERAÇÃO
	MOVLW	B'00000000'
	MOVWF	INTCON				;DEFINE OPÇÕES DE INTERRUPÇÕES
	BANK0						;RETORNA PARA O BANCO
	MOVLW	B'00000111'
	MOVWF	CMCON				;DEFINE O MODO DE OPERAÇÃO DO COMPARADOR ANALÓGICO

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                     INICIALIZAÇÃO DAS VARIÁVEIS                 *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	CLRF	GPIO				;LIMPA GPIO
	CALL	DELAY_5ms			;TEMPO DE SETUP
	
	CALL	LCD_INIT_MODE_4BIT	;INICIALIZA O LCD NO MODO 4 BIT
	CALL	LCD_CONFIGURE		;CONFIGURA LCD 
	
	CALL	WRITE_MY_NAME		;CHAMA A ROTINA QUE ESCREVE MEU NOME
	
	
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                     ROTINA PRINCIPAL                            *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
MAIN

	GOTO MAIN

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                       FIM DO PROGRAMA                           *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

	END
