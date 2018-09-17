;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*              MODIFICAÇÕES PARA USO COM 12F675                   *
;*                FEITAS PELO PROF. MARDSON                        *
;*                    FEVEREIRO DE 2016                            *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

;MICROCONTROLADORES 2017-1
;ALUNO: DIEGO RAMON BEZERRA DA SILVA
;MATRICULA: 11228382
;DATA: 10/08/2017
;NOME DO PROJETO: SEMÁFORO DE TRÂNSITO
;DESCRIÇÃO DO PROJETO: CONTROLAR O SEMÁFORO DE UM CRUZAMENTO SIMPLES

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
		TEMPO1		;VARIÁVEL USADA COMO MULTIPLICADOR NO DELAY

	ENDC			;FIM DO BLOCO DE MEMÓRIA
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                        FLAGS INTERNOS                           *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; DEFINIÇÃO DE TODOS OS FLAGS UTILIZADOS PELO SISTEMA

#DEFINE	FLAG INTCON, T0IF	;FLAG DE INTERRUPÇÃO DO TIMER0

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                         CONSTANTES                              *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; DEFINIÇÃO DE TODAS AS CONSTANTES UTILIZADAS PELO SISTEMA

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
;*	            	 ROTINAS E SUBROTINAS                          *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; CADA ROTINA OU SUBROTINA DEVE POSSUIR A DESCRIÇÃO DE FUNCIONAMENTO
; E UM NOME COERENTE ÀS SUAS FUNÇÕES.

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*	            	   ATRASO DE 500ms                             *
;* REALIZA UM ATRSO DE 500ms USANDO O TIMER0 DO PIC12F675          *
;* FAZENDO USO DO PRESCALER 1:8,E DE UM CONTADOR EXTERNO           *
;* 250*250*8us = 500ms                                             *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

DELAY_500ms
	BANK1					;ALTERA PARA O BANCO 1
	MOVLW	B'00000010'		;PRESCALER 1:8
	MOVWF	OPTION_REG		;AS DEMAIS CONFIG. SÃO IRRLEVANTES
	BANK0					;ALTERA PARA O BANCO 0
	MOVLW	.250			;TEMPO1 
	MOVWF	TEMPO1			;É INICIALIZADO COM 250
DELAY_500ms_0
	BCF		FLAG			;LIMPA A FLAG T0IF
	MOVLW	.5			
	MOVWF 	TMR0			;MOVE 5 PARA TMR0
	BTFSS	FLAG			;HOUVE ESTOURO?
	GOTO	$-1				;NÃO,RETORNA 1 INSTRUÇÃO
	DECFSZ	TEMPO1			;SIM, DECREMENTA O CONTADOR, É ZERO?
	GOTO	DELAY_500ms_0	;NÃO,PULA PARA DELAY_500ms_0
	RETURN					;SIM, RETONE


;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                     INICIO DO PROGRAMA                          *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	
INICIO
	BANK1					;ALTERA PARA O BANCO 1
	MOVLW	B'00000000' 	;CONFIGURA A PORTA 4
	MOVWF	TRISIO			;COMO SAÍDA
	CLRF	ANSEL 			;DEFINE PORTAS COMO Digital I/O
	MOVLW	B'00000000' 	;PRESCALER 1:2 NO TMR0
							;AS DEMAIS CONFIG. SÃO IRRLEVANTES

	CALL	3FFh			;CARREGA CALIBRAÇÃO DE FÁBRICA
	MOVWF	OSCCAL			;CALIBRA OSCILADOR INTERNO

	MOVWF	OPTION_REG		;DEFINE OPÇÕES DE OPERAÇÃO
	MOVLW	B'10000000'
	MOVWF	INTCON			;DEFINE OPÇÕES DE INTERRUPÇÕES

	BANK0					;RETORNA PARA O BANCO
	MOVLW	B'00000111'
	MOVWF	CMCON			;DEFINE O MODO DE OPERAÇÃO DO COMPARADOR ANALÓGICO


;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                     INICIALIZAÇÃO DAS VARIÁVEIS                 *
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
