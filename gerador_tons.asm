;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*              MODIFICAÇÕES PARA USO COM 12F675                   *
;*                FEITAS PELO PROF. MARDSON                        *
;*                    FEVEREIRO DE 2016                            *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

;MICROCONTROLADORES 2017-1
;ALUNO: DIEGO RAMON BEZERRA DA SILVA
;MATRICULA: 11228382
;DATA: 16/08/2017
;NOME DO PROJETO: GERADOR DE TOM
;DESCRIÇÃO DO PROJETO: GERADOR DE TOM EM DIFERENTES FREQUÊNCIAS
;FAZENDO USO DO TIMER E INTERRUPÇÕES


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

    CBLOCK  0x20    ;ENDEREÇO INICIAL DA MEMÓRIA DE
                    ;USUÁRIO
        W_TEMP      ;REGISTRADORES TEMPORÁRIOS PARA USO
        STATUS_TEMP ;JUNTO ÀS INTERRUPÇÕES
        
        ;NOVAS VARIÁVEIS

		MASK		;MÁSCARA USADA PARA DECODIFICAR O TOM
        GPIO_STATUS ;CÓPIA ESTADO DO GPIO ATUAL
        TEMPO1		;VARIÁVEL USADO NO TIMER


    ENDC            ;FIM DO BLOCO DE MEMÓRIA
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                        FLAGS INTERNOS                           *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; DEFINIÇÃO DE TODOS OS FLAGS UTILIZADOS PELO SISTEMA

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                         CONSTANTES                              *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; DEFINIÇÃO DE TODAS AS CONSTANTES UTILIZADAS PELO SISTEMA

;VALORES DE INICIALIZAÇÃO DO TIMER1 PARA OBTER AS FREQUÊNCIAS
;ESPECIFICADAS PARA CADA TOM, ESSAS CONSTANTES SÃO CALCULADAS
;DA SEGUINTE FORMA: N = 65536 - (1/2*F), ONDE F É A FREQUÊNCIA
;DESEJADA, SENDO:
;	 HIGH = N >> 8
;	 LOW = N & 0XFF
;APÓS ISSO, TAMBÉM FOI NECESSÁRIO UMA CALIBRAÇÃO MANUAL NESSES
;VALORES PARA ATINGIR AS FREQUÊNCIAS ESPECIFICADAS, NO SIMULADOR.

FREQ262_L   EQU     0xA8
FREQ262_H   EQU     0xF8
FREQ349_L   EQU     0x84
FREQ349_H   EQU     0xFA
FREQ440_L   EQU     0xAD
FREQ440_H   EQU     0xFB
FREQ523_L   EQU     0x62
FREQ523_H   EQU     0xFC


;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                           ENTRADAS                              *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; DEFINIÇÃO DE TODOS OS PINOS QUE SERÃO UTILIZADOS COMO ENTRADA
; RECOMENDAMOS TAMBÉM COMENTAR O SIGNIFICADO DE SEUS ESTADOS (0 E 1)

#DEFINE BOTAO0  GPIO, 0     ;{0, 1} -> {SOLTO, PRESSIONADO}
#DEFINE BOTAO1  GPIO, 1     ;{0, 1} -> {SOLTO, PRESSIONADO}
#DEFINE BOTAO2  GPIO, 2     ;{0, 1} -> {SOLTO, PRESSIONADO}
#DEFINE SAIDA   GPIO, 4     ;{0, 1} -> OSCILA COM DETERMINADA FREQUÊNCIA 

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                           SAÍDAS                                *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; DEFINIÇÃO DE TODOS OS PINOS QUE SERÃO UTILIZADOS COMO SAÍDA
; RECOMENDAMOS TAMBÉM COMENTAR O SIGNIFICADO DE SEUS ESTADOS (0 E 1)

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
;AQUI SERÁ ESCRITA AS ROTINAS DE RECONHECIMENTO E TRATAMENTO DAS
;INTERRUPÇÕES

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*      TRATAMENTO DA INTERRUPÇÃO POR MUDANÇA DE ESTADO            *
;*                                                                 *
;* SE FOR UMA INTERRUPÇÃO DE MUDANÇA DE ESTADO,  SERÁ NECESSÁRIO   *
;* RECONFIGURAR O TIMER, PARA  ATINGIR O TOM CERTO ATRÁVES DO      *
;* CHAVEAMENTO DA PORTA GPIO.                                      *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

GPIO_INT
    BTFSS   INTCON, GPIF    ;INTERRUPÇÃO POR MUDANÇA DE ESTADO?
    GOTO    TIMER_INT       ;NÃO É
                            ;TRATA INTERRUPÇÃO DE MUDANÇA DE ESTADO
                            
							;PRIMEIRO É PRECISO DECODIFICAR O TOM ESCOLHIDO
							;ATRÁVES DOS ESTADOS DAS ENTRADAS GPIO 0, 1 E 2
    
    MOVLW   B'00000000'     ;LIMPA A MÁSCARA
    MOVWF   MASK
    MOVF    GPIO, W         ;MOVE ESTADO DO GPIO PARA W
    MOVWF   GPIO_STATUS     ;SALVA O ESTADO DA GPIO NUMA VARIÁVEL AUXILIAR
    BCF     INTCON, GPIF    ;LIMPA A FLAG DE INTERRUPÇÃO 
    XORWF   MASK, F         ;SEPARA OS BITS DE MUDANÇA DE ESTADO
    
    BTFSS   MASK, 0         ;BIT 0 ESTÁ SETADO?
    GOTO    STATE_LOW       ;NÃO, VAI PARA ESTADO LOW   -> XXXXXXX0
    GOTO    TEST_2BIT       ;SENÃO TESTA SEGUNDO BIT    -> XXXXXXX1
    
TEST_2BIT   
    BTFSS   MASK, 1         ;BIT 1 ESTÁ SETADO?
    GOTO    TEST_3BIT_A     ;NÃO, TESTA TERCEIRO BIT    -> XXXXXX01
    GOTO    TEST_3BIT_B     ;SIM, TESTA TERCEIRO BIT    -> XXXXXX11
    
TEST_3BIT_A
    BTFSS   MASK, 2         ;BIT 2 ESTÁ SETADO?
    GOTO    STATE_262       ;NÃO, VAI PARA 262Hz        -> XXXXX001
    GOTO    STATE_349       ;SIM, VAI PARA 349Hz        -> XXXXX101

TEST_3BIT_B 
    BTFSS   MASK, 2         ;BIT 2 ESTÁ SETADO?
    GOTO    STATE_440       ;NÃO, VAI PARA 440Hz        -> XXXXX110
    GOTO    STATE_523       ;SIM, VAI PARA 523Hz        -> XXXXX111
    
STATE_LOW
    ;FAZ A CONFIGURAÇÃO PARA O ESTADO LOW
    GOTO    LOW_RESET
    
STATE_262
    ;FAZ A CONFIGURAÇÃO PARA O ESTADO 262, SE NECESSARIO
	;E SALTA PARA O RESET DO TIMER DESSA FREQUENCIA
    GOTO    RESET_262
STATE_349
    ;FAZ A CONFIGURAÇÃO PARA O ESTADO 349, SE NECESSARIO
	;E SALTA PARA O RESET DO TIMER DESSA FREQUENCIA
    GOTO    RESET_349
    
STATE_440
    ;FAZ A CONFIGURAÇÃO PARA O ESTADO 440, SE NECESSARIO
	;E SALTA PARA O RESET DO TIMER DESSA FREQUENCIA
    GOTO    RESET_440
    
STATE_523
    ;FAZ A CONFIGURAÇÃO PARA O ESTADO 523, SE NECESSARIO
	;E SALTA PARA O RESET DO TIMER DESSA FREQUENCIA
    GOTO    RESET_523
    

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*               TRATAMENTO DA INTERRUPÇÃO DO TIMER                *
;*                                                                 *
;* SE FOR UMA INTERRUPÇÃO DO TIMER 0, SÓ SERÁ NECESSÁRIO FAZER     *
;* O RESET DO TIMER, E FAZER O CHAVEAMENTO DA PORTA DE SAÍDA       *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
TIMER_INT
    BTFSS   PIR1, TMR1IF   	;INTERRUPÇÃO DO TIMER 1?
    GOTO    SAI_INT         ;NÃO É, SAI DO TRATAMENTO DE INTERRUPÇÕES

    BCF  	PIR1, TMR1IF    ;LIMPA FLAG DO TIMER                  
    MOVLW   B'00000000'     ;LIMPA A MÁSCARA
    MOVWF   MASK
    MOVF    GPIO, W         ;MOVE ESTADO DO GPIO PARA W
    XORWF   MASK, F         ;SEPARA OS BITS DE MUDANÇA DE ESTADO
    
    BTFSS   MASK, 0         ;BIT 0 ESTÁ SETADO?
    GOTO    LOW_RESET       ;NÃO, VAI PARA ESTADO LOW    -> XXXXXXX0
    GOTO    TEST_2BIT_2     ;SENÃO, TESTA SEGUNDO BIT    -> XXXXXXX1
    
TEST_2BIT_2   
    BTFSS   MASK, 1         ;BIT 1 ESTÁ SETADO?
    GOTO    TEST_3BIT_A_2   ;NÃO, TESTA TERCEIRO BIT    -> XXXXXX01
    GOTO    TEST_3BIT_B_2   ;SIM, TESTA TERCEIRO BIT    -> XXXXXX11
    
TEST_3BIT_A_2
    BTFSS   MASK, 2         ;BIT 2 ESTÁ SETADO?
    GOTO    RESET_262       ;NÃO, VAI PARA 262Hz        -> XXXXX001
    GOTO    RESET_349       ;SIM, VAI PARA 349Hz        -> XXXXX101

TEST_3BIT_B_2 
    BTFSS   MASK, 2         ;BIT 2 ESTÁ SETADO?
    GOTO    RESET_440       ;NÃO, VAI PARA 440Hz        -> XXXXX110
    GOTO    RESET_523       ;SIM, VAI PARA 523Hz        -> XXXXX111
    
LOW_RESET
    ;RESET O TIMER
    GOTO    SAIDA_OFF       ;VAI PARA LOW
    
RESET_262   
    ;RESET DO TIMER

	MOVLW	FREQ262_L		;REINICIA O REGISTRADOR
	MOVWF	TMR1L 			
	MOVLW	FREQ262_H		;REINICIA O REGISTRADOR
	MOVWF	TMR1H
	
    BCF     PIR1, TMR1IF    ;LIMPA A FLAG DO TIMER
    BTFSS   SAIDA           ;TESTA O ESTADO SA SAÍDA
    GOTO    SAIDA_ON        ;SE FOR LOW, VAI PARA HIGH
    GOTO    SAIDA_OFF       ;SE FOR HIGH, VAI PARA LOW
    
RESET_349       
   ;RESET DO TIMER
	
	MOVLW	FREQ349_L		;REINICIA O REGISTRADOR
	MOVWF	TMR1L 
	MOVLW	FREQ349_H		;REINICIA O REGISTRADOR
	MOVWF	TMR1H
	
    BCF     PIR1, TMR1IF    ;LIMPA A FLAG DO TIMER
    BTFSS   SAIDA           ;TESTA O ESTADO SA SAÍDA
    GOTO    SAIDA_ON        ;SE FOR LOW, VAI PARA HIGH
    GOTO    SAIDA_OFF       ;SE FOR HIGH, VAI PARA LOW
    
RESET_440   
   ;RESET DO TIMER
	
	MOVLW	FREQ440_L		;REINICIA O REGISTRADOR
	MOVWF	TMR1L 
	MOVLW	FREQ440_H		;REINICIA O REGISTRADOR
	MOVWF	TMR1H
	
    BCF     PIR1, TMR1IF    ;LIMPA A FLAG DO TIMER
    BTFSS   SAIDA           ;TESTA O ESTADO SA SAÍDA
    GOTO    SAIDA_ON        ;SE FOR LOW, VAI PARA HIGH
    GOTO    SAIDA_OFF       ;SE FOR HIGH, VAI PARA LOW
    
    
RESET_523       
   ;RESET DO TIMER
	
	MOVLW	FREQ523_L		;REINICIA O REGISTRADOR
	MOVWF	TMR1L 
	MOVLW	FREQ523_H		;REINICIA O REGISTRADOR
	MOVWF	TMR1H
	
    BCF     PIR1, TMR1IF    ;LIMPA A FLAG DO TIMER
    BTFSS   SAIDA           ;TESTA O ESTADO SA SAÍDA
    GOTO    SAIDA_ON        ;SE FOR LOW, VAI PARA HIGH
    GOTO    SAIDA_OFF       ;SE FOR HIGH, VAI PARA LOW
    
    
SAIDA_ON
    BSF     SAIDA           ;SETA SAÍDA
    GOTO    SAI_INT         ;SAI DO TRATAMENTO DE INTERRUPÇÕES
SAIDA_OFF
    BCF     SAIDA           ;LIMPA SAÍDA
    GOTO    SAI_INT         ;SAI DO TRATAMENTO DE INTERRUPÇÕES
      

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
;*                   ROTINAS E SUBROTINAS                      *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; CADA ROTINA OU SUBROTINA DEVE POSSUIR A DESCRIÇÃO DE FUNCIONAMENTO
; E UM NOME COERENTE ÀS SUAS FUNÇÕES.


;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                     INICIO DO PROGRAMA                          *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

INICIO
    BANK1           	;ALTERA PARA O BANCO 1
    MOVLW   B'00000111' ;CONFIGURA 0, 1 E 2 COMO ENTRADA 
    MOVWF   TRISIO      ;4 E 5 COMO SAÍDA
    CLRF    ANSEL       ;DEFINE PORTAS COMO Digital I/O
    MOVLW   B'00000000' ;PRESCALER 1:2
    MOVWF   OPTION_REG  ;DEFINE OPÇÕES DE OPERAÇÃO
    
    MOVLW   B'00000111' ;CONFIGURA PORTAS MUDANÇA DE ESTADO DE GPIO
    MOVWF   IOC     	;PORTAS 0, 1 E 2
    
    MOVLW   B'11001000'
    MOVWF   INTCON      ;DEFINE OPÇÕES DE INTERRUPÇÕES
	
	MOVLW	B'00000001'	;CONFIGURA TIMER 1
	MOVWF	PIE1
	
	CALL	3FFh		;CARREGA A CALIBRAÇÃO DE FÁBRICA
	MOVWF	OSCCAL		;CALIBRA O OSCILADOR INTERNO
    
    BANK0           	;RETORNA PARA O BANCO
    MOVLW   B'00000111'
    MOVWF   CMCON       ;DEFINE O MODO DE OPERAÇÃO DO COMPARADOR ANALÓGICO
	
	MOVLW	B'00000001'	;CONFIGURA TIMER 1
	MOVWF	T1CON
    

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                     INICIALIZAÇÃO DAS VARIÁVEIS                 *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                     ROTINA PRINCIPAL                            *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
MAIN

    ;CORPO DA ROTINA PRINCIPAL
	;TODO O PROGRAMA ESTÁ BASEADO INTERRUPÇÕES, LOGO SOMENTE UM LOOP

    GOTO MAIN

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                       FIM DO PROGRAMA                           *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

    END
