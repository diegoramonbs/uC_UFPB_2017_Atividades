;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*              MODIFICA��ES PARA USO COM 12F675                   *
;*                FEITAS PELO PROF. MARDSON                        *
;*                    FEVEREIRO DE 2016                            *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                    ROTINA DE ATRASO 31.125 MS                   *
;*                            									   *
;*   AUTOR: DIEGO RAMON BEZERRA DA SILVA                           *
;*   VERS�O: 1.0                           DATA: 03/08/17          *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*   MODELO PARA O PIC 12F675                                      *
;*                                                                 *
;*                                                                 *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

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

	CBLOCK	0x20	;ENDERE�O INICIAL DA MEM�RIA DE USU�RIO
		W_TEMP		;REGISTRADORES TEMPOR�RIOS PARA USO
		STATUS_TEMP	;JUNTO �S INTERRUP��ES

		contador1	;VARI�VEL USADO COMO CONTADOR DE LOOP		
		contador2	;VARI�VEL USADO COMO CONTADOR DE LOOP	

	ENDC			;FIM DO BLOCO DE MEM�RIA
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                        FLAGS INTERNOS                           *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; DEFINI��O DE TODOS OS FLAGS UTILIZADOS PELO SISTEMA

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

#define LED GPIO, 5		;A SA�DA DO PROGRAMA SER� NA PORTA GPIO 5
						;0 -> QUE O LED ESTER� DESLIGADO, SENDO O ESTADO INICIAL
						;1 -> QUE O LED ESTAR� LIGADO

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
;*	            	 ROTINAS E SUBROTINAS                      *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; CADA ROTINA OU SUBROTINA DEVE POSSUIR A DESCRI��O DE FUNCIONAMENTO
; E UM NOME COERENTE �S SUAS FUN��ES.

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;* 						ROTINA ATRASO 31.125 ms			           *
;*                                                                 *                                             *
;*                                                                 *
;* DESCRI��O: REALIZA UM ATRASO DE APROXIMADAMENTE 31.125 ms       *
;* USANDO O TEMPO DE EXECU��O DAS INTRU��ES, COMO SER� UTILIZADO   *
;* O CLOCK INTERNO DE 4MHz, COM UMA INSTRU��O A CADA 1 us,         *
;* RESULTANDO EM 31125 INSTRU��ES PARA COMPUTAR O ATRASO REQUERIDO *
;*																   * 
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


ATRASO
	MOVLW	.243 			; MOVE O VALOR PARA O REGISTRADOR W
	MOVWF   contador1		; MOVE O VALOR PARA A VARI�VEL CONTADOR
	NOP						; N�O FAZ NADA
	NOP						; N�O FAZ NADA
	NOP						; N�O FAZ NADA
	NOP						; N�O FAZ NADA
	NOP						; N�O FAZ NADA
	NOP						; N�O FAZ NADA
	NOP						; N�O FAZ NADA
	NOP						; N�O FAZ NADA
	NOP						; N�O FAZ NADA
	NOP						; N�O FAZ NADA
	NOP						; N�O FAZ NADA
	NOP						; N�O FAZ NADA
	NOP						; N�O FAZ NADA
	NOP						; N�O FAZ NADA
ATRASO_31_125_MS_0			; R�TULO PARA REALIZA��O DO LA�O
	CALL ATRASO_125_US		; ATRASO DE 125 MS
	NOP						; N�O FAZ NADA
	DECFSZ	contador1, F	; DECREMENTA O CONTADOR, E VERIFICA	
	GOTO ATRASO_31_125_MS_0 ; SE N�O ZEROU, CONTINUA NO LA�O, SALTANDO PARA O R�TULO
	RETURN					; SE SIM, FINALIZA A ROTINA

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;* 						ROTINA ATRASO 125 us			           *
;*                                                                 *                                             *
;*                                                                 *
;* DESCRI��O: REALIZA UM ATRASO DE APROXIMADAMENTE 125 us          *
;* USANDO O TEMPO DE EXECU��O DAS INTRU��ES, COMO SER� UTILIZADO   *
;* O CLOCK INTERNO DE 4MHz, COM UMA INSTRU��O A CADA 1 us,         *
;* RESULTANDO EM 125 INSTRU��ES PARA COMPUTAR O ATRASO REQUERIDO   *
;* 123 DA ROTINA MAIS 2 DO CALL   								   *
;*																   * 
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


ATRASO_125_US:
	MOVLW	.23  			; MOVE O VALOR PARA O REGISTRADOR W
	MOVWF   contador2		; MOVE O VALOR PARA A VARI�VEL CONTADOR
	NOP 					; N�O FAZ NADA
	NOP						; N�O FAZ NADA
	NOP						; N�O FAZ NADA
	NOP						; N�O FAZ NADA
ATRASO_2_5US_0				; R�TULO PARA REALIZA��O DO LA�O
	NOP						; N�O FAZ NADA
	NOP						; N�O FAZ NADA				
	DECFSZ	contador2, F	; DECREMENTA O CONTADOR, E VERIFICA	
	GOTO ATRASO_2_5US_0		; SE N�O ZEROU, CONTINUA NO LA�O, SALTANDO PARA O R�TULO
	RETURN					; SE SIM, FINALIZA A ROTINA
				


;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                     INICIO DO PROGRAMA                          *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	
INICIO
	BANK1				;ALTERA PARA O BANCO 1
	MOVLW	B'00011111' ;CONFIGURA A PORTA 5 DO GPIO (PINO) COM SA�DA
	MOVWF	TRISIO		;COMO SA�DAS
	CLRF	ANSEL 		;DEFINE PORTAS COMO Digital I/O
	MOVLW	B'00000100'
	MOVWF	OPTION_REG	;DEFINE OP��ES DE OPERA��O
	MOVLW	B'00000000'
	MOVWF	INTCON		;DEFINE OP��ES DE INTERRUP��ES

	CALL	3FFh		;OBT�M O VALOR DE CALIBRA��O DE F�BRICA

	MOVWF	OSCCAL		;CALIBRA O OSCILADOR INTERNO
	BANK0				;RETORNA PARA O BANCO
	MOVLW	B'00000111'
	MOVWF	CMCON		;DEFINE O MODO DE OPERA��O DO COMPARADOR ANAL�GICO

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                     INICIALIZA��O DAS VARI�VEIS                 *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

	BCF			LED			;DESLIGA O LED(ESTADO INICIAL)

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                     ROTINA PRINCIPAL                            *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
MAIN:
	; NO LOOP A PORTA GPIO 5 � CHEVEADA A CADA 31.125MS

	BSF		LED			;LIGA O LED
	CALL 	ATRASO		;PAUSA 31.125MS

	BCF 	LED			;DESLIGA O LED

	CALL 	ATRASO		;PAUSA 31.125MS
	GOTO MAIN			;SALTA PARA LOOP PRINCIPAL

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                       FIM DO PROGRAMA                           *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

	END