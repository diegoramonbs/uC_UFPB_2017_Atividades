;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*              MODIFICA��ES PARA USO COM 12F675                   *
;*                FEITAS PELO PROF. MARDSON                        *
;*                    FEVEREIRO DE 2016                            *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

;MICROCONTROLADORES 2017-1
;ALUNO: DIEGO RAMON BEZERRA DA SILVA
;MATRICULA: 11228382
;DATA: 10/08/2017
;NOME DO PROJETO: SEM�FORO DE TR�NSITO
;DESCRI��O DO PROJETO: CONTROLAR O SEM�FORO DE UM CRUZAMENTO SIMPLES

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                     ARQUIVOS DE DEFINI��ES                      *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#INCLUDE <P12F675.INC>	;ARQUIVO PADR�O MICROCHIP PARA 12F675

	__CONFIG _BODEN_OFF & _CP_OFF & _PWRTE_ON & _WDT_OFF & _MCLRE_ON & _INTRC_OSC_NOCLKOUT

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                    PAGINA��O DE MEM�RIA                         *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;DEFINI��O DE COMANDOS DE USU�RIO PARA ALTERA��O DA P�GINA DE MEM�RIA
#DEFINE	BANK0	BCF STATUS,RP0	;SETA BANK 0 DE MEM�RIA
#DEFINE	BANK1	BSF STATUS,RP0	;SETA BANK 1 DE MAM�RIA

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                         VARI�VEIS                               *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; DEFINI��O DOS NOMES E ENDERE�OS DE TODAS AS VARI�VEIS UTILIZADAS 
; PELO SISTEMA

	CBLOCK	0x20	;ENDERE�O INICIAL DA MEM�RIA DE
					;USU�RIO
		W_TEMP		;REGISTRADORES TEMPOR�RIOS PARA USO
		STATUS_TEMP	;JUNTO �S INTERRUP��ES

		;NOVAS VARI�VEIS
		TEMPO1		;VARI�VEL USADA COMO MULTIPLICADOR NO DELAY

	ENDC			;FIM DO BLOCO DE MEM�RIA
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                        FLAGS INTERNOS                           *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; DEFINI��O DE TODOS OS FLAGS UTILIZADOS PELO SISTEMA

#DEFINE	FLAG INTCON, T0IF	;FLAG DE INTERRUP��O DO TIMER0

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                         CONSTANTES                              *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; DEFINI��O DE TODAS AS CONSTANTES UTILIZADAS PELO SISTEMA

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                           ENTRADAS                              *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; DEFINI��O DE TODOS OS PINOS QUE SER�O UTILIZADOS COMO ENTRADA
; RECOMENDAMOS TAMB�M COMENTAR O SIGNIFICADO DE SEUS ESTADOS (0 E 1)

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                           SA�DAS                                *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; DEFINI��O DE TODOS OS PINOS QUE SER�O UTILIZADOS COMO SA�DA
; RECOMENDAMOS TAMB�M COMENTAR O SIGNIFICADO DE SEUS ESTADOS (0 E 1)

;TODAS AS PORTAS REPRESENTAM LED'S 
;	0 <- DESLIGADO 
;	1 <- LIGADO

#DEFINE AMARELO1		GPIO, 0
#DEFINE AMARELO2	 	GPIO, 1
#DEFINE VERMELHO		GPIO, 2
#DEFINE VERDE			GPIO, 4


;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                       VETOR DE RESET                            *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

	ORG	0x00			;ENDERE�O INICIAL DE PROCESSAMENTO
	GOTO	INICIO
	
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                    IN�CIO DA INTERRUP��O                        *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; ENDERE�O DE DESVIO DAS INTERRUP��ES. A PRIMEIRA TAREFA � SALVAR OS
; VALORES DE "W" E "STATUS" PARA RECUPERA��O FUTURA

	ORG	0x04			;ENDERE�O INICIAL DA INTERRUP��O
	MOVWF	W_TEMP		;COPIA W PARA W_TEMP
	SWAPF	STATUS,W
	MOVWF	STATUS_TEMP	;COPIA STATUS PARA STATUS_TEMP

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
	SWAPF	STATUS_TEMP,W
	MOVWF	STATUS		;MOVE STATUS_TEMP PARA STATUS
	SWAPF	W_TEMP,F
	SWAPF	W_TEMP,W	;MOVE W_TEMP PARA W
	RETFIE

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*	            	 ROTINAS E SUBROTINAS                          *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; CADA ROTINA OU SUBROTINA DEVE POSSUIR A DESCRI��O DE FUNCIONAMENTO
; E UM NOME COERENTE �S SUAS FUN��ES.

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*	            	   ATRASO DE 500ms                             *
;* REALIZA UM ATRSO DE 500ms USANDO O TIMER0 DO PIC12F675          *
;* FAZENDO USO DO PRESCALER 1:8,E DE UM CONTADOR EXTERNO           *
;* 250*250*8us = 500ms                                             *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

DELAY_500ms
	BANK1					;ALTERA PARA O BANCO 1
	MOVLW	B'00000010'		;PRESCALER 1:8
	MOVWF	OPTION_REG		;AS DEMAIS CONFIG. S�O IRRLEVANTES
	BANK0					;ALTERA PARA O BANCO 0
	MOVLW	.250			;TEMPO1 
	MOVWF	TEMPO1			;� INICIALIZADO COM 250
DELAY_500ms_0
	BCF		FLAG			;LIMPA A FLAG T0IF
	MOVLW	.5			
	MOVWF 	TMR0			;MOVE 5 PARA TMR0
	BTFSS	FLAG			;HOUVE ESTOURO?
	GOTO	$-1				;N�O,RETORNA 1 INSTRU��O
	DECFSZ	TEMPO1			;SIM, DECREMENTA O CONTADOR, � ZERO?
	GOTO	DELAY_500ms_0	;N�O,PULA PARA DELAY_500ms_0
	RETURN					;SIM, RETONE


;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                     INICIO DO PROGRAMA                          *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	
INICIO
	BANK1					;ALTERA PARA O BANCO 1
	MOVLW	B'00000000' 	;CONFIGURA A PORTA 4
	MOVWF	TRISIO			;COMO SA�DA
	CLRF	ANSEL 			;DEFINE PORTAS COMO Digital I/O
	MOVLW	B'00000000' 	;PRESCALER 1:2 NO TMR0
							;AS DEMAIS CONFIG. S�O IRRLEVANTES

	CALL	3FFh			;CARREGA CALIBRA��O DE F�BRICA
	MOVWF	OSCCAL			;CALIBRA OSCILADOR INTERNO

	MOVWF	OPTION_REG		;DEFINE OP��ES DE OPERA��O
	MOVLW	B'10000000'
	MOVWF	INTCON			;DEFINE OP��ES DE INTERRUP��ES

	BANK0					;RETORNA PARA O BANCO
	MOVLW	B'00000111'
	MOVWF	CMCON			;DEFINE O MODO DE OPERA��O DO COMPARADOR ANAL�GICO


;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                     INICIALIZA��O DAS VARI�VEIS                 *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                     ROTINA PRINCIPAL                            *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
MAIN

	MOVLW	B'00010000'	;RED = 1, GREEN = 1, YELLOW1 = 0, YELLOW2 = 0 
	MOVWF	GPIO
	CALL	DELAY_500ms

	MOVLW	B'00010000'	;RED = 1, GREEN = 1, YELLOW1 = 0, YELLOW2 = 0 
	MOVWF	GPIO
	CALL	DELAY_500ms

	MOVLW	B'00010000'	;RED = 1, GREEN = 1, YELLOW1 = 0, YELLOW2 = 0 
	MOVWF	GPIO
	CALL	DELAY_500ms

	MOVLW	B'00010010'	;RED = 1, GREEN = 1, YELLOW1 = 0, YELLOW2 = 1 
	MOVWF	GPIO
	CALL	DELAY_500ms

	CLRF	GPIO		;LIMPA GPIO

	MOVLW	B'00000100'	;RED = 1, GREEN = 1, YELLOW1 = 0, YELLOW2 = 0 
	MOVWF	GPIO
	CALL	DELAY_500ms

	MOVLW	B'00000100'	;RED = 1, GREEN = 1, YELLOW1 = 0, YELLOW2 = 0 
	MOVWF	GPIO
	CALL	DELAY_500ms

	MOVLW	B'00000100'	;RED = 1, GREEN = 1, YELLOW1 = 0, YELLOW2 = 0 
	MOVWF	GPIO
	CALL	DELAY_500ms

	MOVLW	B'00000101'	;RED = 1, GREEN = 1, YELLOW1 = 1, YELLOW2 = 0 
	MOVWF	GPIO
	CALL	DELAY_500ms


	GOTO MAIN

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                       FIM DO PROGRAMA                           *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

	END
