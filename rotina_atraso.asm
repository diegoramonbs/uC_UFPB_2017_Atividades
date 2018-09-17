;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*              MODIFICAÇÕES PARA USO COM 12F675                   *
;*                FEITAS PELO PROF. MARDSON                        *
;*                    FEVEREIRO DE 2016                            *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                    ROTINA DE ATRASO 31.125 MS                   *
;*                            									   *
;*   AUTOR: DIEGO RAMON BEZERRA DA SILVA                           *
;*   VERSÃO: 1.0                           DATA: 03/08/17          *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*   MODELO PARA O PIC 12F675                                      *
;*                                                                 *
;*                                                                 *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

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

	CBLOCK	0x20	;ENDEREÇO INICIAL DA MEMÓRIA DE USUÁRIO
		W_TEMP		;REGISTRADORES TEMPORÁRIOS PARA USO
		STATUS_TEMP	;JUNTO ÀS INTERRUPÇÕES

		contador1	;VARIÁVEL USADO COMO CONTADOR DE LOOP		
		contador2	;VARIÁVEL USADO COMO CONTADOR DE LOOP	

	ENDC			;FIM DO BLOCO DE MEMÓRIA
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                        FLAGS INTERNOS                           *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; DEFINIÇÃO DE TODOS OS FLAGS UTILIZADOS PELO SISTEMA

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

#define LED GPIO, 5		;A SAÍDA DO PROGRAMA SERÁ NA PORTA GPIO 5
						;0 -> QUE O LED ESTERÁ DESLIGADO, SENDO O ESTADO INICIAL
						;1 -> QUE O LED ESTARÁ LIGADO

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
;* 						ROTINA ATRASO 31.125 ms			           *
;*                                                                 *                                             *
;*                                                                 *
;* DESCRIÇÃO: REALIZA UM ATRASO DE APROXIMADAMENTE 31.125 ms       *
;* USANDO O TEMPO DE EXECUÇÃO DAS INTRUÇÕES, COMO SERÁ UTILIZADO   *
;* O CLOCK INTERNO DE 4MHz, COM UMA INSTRUÇÃO A CADA 1 us,         *
;* RESULTANDO EM 31125 INSTRUÇÕES PARA COMPUTAR O ATRASO REQUERIDO *
;*																   * 
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


ATRASO
	MOVLW	.243 			; MOVE O VALOR PARA O REGISTRADOR W
	MOVWF   contador1		; MOVE O VALOR PARA A VARIÁVEL CONTADOR
	NOP						; NÃO FAZ NADA
	NOP						; NÃO FAZ NADA
	NOP						; NÃO FAZ NADA
	NOP						; NÃO FAZ NADA
	NOP						; NÃO FAZ NADA
	NOP						; NÃO FAZ NADA
	NOP						; NÃO FAZ NADA
	NOP						; NÃO FAZ NADA
	NOP						; NÃO FAZ NADA
	NOP						; NÃO FAZ NADA
	NOP						; NÃO FAZ NADA
	NOP						; NÃO FAZ NADA
	NOP						; NÃO FAZ NADA
	NOP						; NÃO FAZ NADA
ATRASO_31_125_MS_0			; RÓTULO PARA REALIZAÇÃO DO LAÇO
	CALL ATRASO_125_US		; ATRASO DE 125 MS
	NOP						; NÃO FAZ NADA
	DECFSZ	contador1, F	; DECREMENTA O CONTADOR, E VERIFICA	
	GOTO ATRASO_31_125_MS_0 ; SE NÃO ZEROU, CONTINUA NO LAÇO, SALTANDO PARA O RÓTULO
	RETURN					; SE SIM, FINALIZA A ROTINA

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;* 						ROTINA ATRASO 125 us			           *
;*                                                                 *                                             *
;*                                                                 *
;* DESCRIÇÃO: REALIZA UM ATRASO DE APROXIMADAMENTE 125 us          *
;* USANDO O TEMPO DE EXECUÇÃO DAS INTRUÇÕES, COMO SERÁ UTILIZADO   *
;* O CLOCK INTERNO DE 4MHz, COM UMA INSTRUÇÃO A CADA 1 us,         *
;* RESULTANDO EM 125 INSTRUÇÕES PARA COMPUTAR O ATRASO REQUERIDO   *
;* 123 DA ROTINA MAIS 2 DO CALL   								   *
;*																   * 
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


ATRASO_125_US:
	MOVLW	.23  			; MOVE O VALOR PARA O REGISTRADOR W
	MOVWF   contador2		; MOVE O VALOR PARA A VARIÁVEL CONTADOR
	NOP 					; NÃO FAZ NADA
	NOP						; NÃO FAZ NADA
	NOP						; NÃO FAZ NADA
	NOP						; NÃO FAZ NADA
ATRASO_2_5US_0				; RÓTULO PARA REALIZAÇÃO DO LAÇO
	NOP						; NÃO FAZ NADA
	NOP						; NÃO FAZ NADA				
	DECFSZ	contador2, F	; DECREMENTA O CONTADOR, E VERIFICA	
	GOTO ATRASO_2_5US_0		; SE NÃO ZEROU, CONTINUA NO LAÇO, SALTANDO PARA O RÓTULO
	RETURN					; SE SIM, FINALIZA A ROTINA
				


;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                     INICIO DO PROGRAMA                          *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	
INICIO
	BANK1				;ALTERA PARA O BANCO 1
	MOVLW	B'00011111' ;CONFIGURA A PORTA 5 DO GPIO (PINO) COM SAÍDA
	MOVWF	TRISIO		;COMO SAÍDAS
	CLRF	ANSEL 		;DEFINE PORTAS COMO Digital I/O
	MOVLW	B'00000100'
	MOVWF	OPTION_REG	;DEFINE OPÇÕES DE OPERAÇÃO
	MOVLW	B'00000000'
	MOVWF	INTCON		;DEFINE OPÇÕES DE INTERRUPÇÕES

	CALL	3FFh		;OBTÉM O VALOR DE CALIBRAÇÃO DE FÁBRICA

	MOVWF	OSCCAL		;CALIBRA O OSCILADOR INTERNO
	BANK0				;RETORNA PARA O BANCO
	MOVLW	B'00000111'
	MOVWF	CMCON		;DEFINE O MODO DE OPERAÇÃO DO COMPARADOR ANALÓGICO

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                     INICIALIZAÇÃO DAS VARIÁVEIS                 *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

	BCF			LED			;DESLIGA O LED(ESTADO INICIAL)

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                     ROTINA PRINCIPAL                            *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
MAIN:
	; NO LOOP A PORTA GPIO 5 É CHEVEADA A CADA 31.125MS

	BSF		LED			;LIGA O LED
	CALL 	ATRASO		;PAUSA 31.125MS

	BCF 	LED			;DESLIGA O LED

	CALL 	ATRASO		;PAUSA 31.125MS
	GOTO MAIN			;SALTA PARA LOOP PRINCIPAL

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                       FIM DO PROGRAMA                           *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

	END