;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*              MODIFICAÇÕES PARA USO COM 12F675                   *
;*                FEITAS PELO PROF. MARDSON                        *
;*                    FEVEREIRO DE 2016                            *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                       NOME DO PROJETO                           *
;*                           CLIENTE                               *
;*         DESENVOLVIDO PELA MOSAICO ENGENHARIA E CONSULTORIA      *
;*   VERSÃO: 1.0                           DATA: 17/06/03          *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                     DESCRIÇÃO DO ARQUIVO                        *
;*-----------------------------------------------------------------*
;*   MODELO PARA O PIC 12F675                                      *
;*                                                                 *
;*                                                                 *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                     ARQUIVOS DE DEFINIÇÕES                      *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#INCLUDE <P12F675.INC>  ;ARQUIVO PADRÃO MICROCHIP PARA 12F675

    __CONFIG _BODEN_OFF & _CP_OFF & _PWRTE_ON & _WDT_ON & _MCLRE_ON & _INTRC_OSC_CLKOUT

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
        D1
        D2

    ENDC            ;FIM DO BLOCO DE MEMÓRIA
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

#DEFINE CLOCK   GPIO, 4

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
;*                   ROTINAS E SUBROTINAS                      *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
; CADA ROTINA OU SUBROTINA DEVE POSSUIR A DESCRIÇÃO DE FUNCIONAMENTO
; E UM NOME COERENTE ÀS SUAS FUNÇÕES.

ATRASO_50ms
    MOVLW   0X0E
    MOVWF   D1
    MOVLW   0X28
    MOVWF   D2
ATRASO_50MS_0
    DECFSZ  D1, F
    GOTO    $+2
    DECFSZ  D2, F
    GOTO    ATRASO_50MS_0
    GOTO    $+1
    NOP
    RETURN

APOS_ACORDAR
    BSF     GPIO, 0
    CALL    ATRASO_50ms
    BCF     GPIO, 0

    BSF     GPIO, 1
    CALL    ATRASO_50ms
    BCF     GPIO, 1

    BSF     GPIO, 2
    CALL    ATRASO_50ms
    BCF     GPIO, 2

    BSF     GPIO, 5
    CALL    ATRASO_50ms
    BCF     GPIO, 5

    RETURN

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                     INICIO DO PROGRAMA                          *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

INICIO
    BANK1               ;ALTERA PARA O BANCO 1
    MOVLW   B'00000000' ;CONFIGURA TODAS AS PORTAS DO GPIO (PINOS)
    MOVWF   TRISIO      ;COMO SAÍDAS
    CLRF    ANSEL       ;DEFINE PORTAS COMO Digital I/O

    MOVLW   B'00001111' ;WDT COM DELAY DE 2.3s
    MOVWF   OPTION_REG  ;DEFINE OPÇÕES DE OPERAÇÃO

    MOVLW   B'00000000'
    MOVWF   INTCON      ;DEFINE OPÇÕES DE INTERRUPÇÕES
    BANK0               ;RETORNA PARA O BANCO
    MOVLW   B'00000111'
    MOVWF   CMCON       ;DEFINE O MODO DE OPERAÇÃO DO COMPARADOR ANALÓGICO

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                     INICIALIZAÇÃO DAS VARIÁVEIS                 *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    CLRF    GPIO
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                     ROTINA PRINCIPAL                            *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
MAIN

    SLEEP
    CALL    APOS_ACORDAR

    GOTO    MAIN

;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;*                       FIM DO PROGRAMA                           *
;* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

    END