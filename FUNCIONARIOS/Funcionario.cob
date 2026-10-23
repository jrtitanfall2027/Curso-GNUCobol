       IDENTIFICATION DIVISION.
       PROGRAM-ID. ContagemFuncionarios.
       AUTHOR. Reginaldo Junior.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FUNCIONARIOS ASSIGN TO "FUNCIONARIOS.DATA"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD FUNCIONARIOS.
       01 DETALHEFUNCIONARIO.
           88 FINALREGISTRO VALUE HIGH-VALUES.
           05 MATRICULA-FUNCIONARIO PIC 9(5).
           05 NOME-FUNCIONARIO.
               10 PRIMEIRO-NOME       PIC X(20).
               10 ULTIMO-NOME         PIC X(20).
           05 GENERO                  PIC X(1).

       WORKING-STORAGE SECTION.
       01 CONTADORES.
           05 TOTAL-HOMENS            PIC 9(3) VALUE 0.
           05 TOTAL-MULHERES          PIC 9(3) VALUE 0.

       01 LEITURA-FINALIZADA          PIC X VALUE "N".

       PROCEDURE DIVISION.
       INICIO.
           DISPLAY "===============================".
           DISPLAY " Contagem de Funcionarios".
           DISPLAY "===============================".

      * Abrir o arquivo
           OPEN INPUT FUNCIONARIOS.

      * Cabecalho das colunas
           DISPLAY "Matricula | Sexo | Nome".

      * Processamento dos registros do arquivo
           PERFORM UNTIL LEITURA-FINALIZADA = "S"
               READ FUNCIONARIOS INTO DETALHEFUNCIONARIO
                   AT END
                       MOVE "S" TO LEITURA-FINALIZADA
                   NOT AT END
                       DISPLAY MATRICULA-FUNCIONARIO "     | " GENERO
                           "    | " FUNCTION TRIM(PRIMEIRO-NOME) " "
                           FUNCTION TRIM(ULTIMO-NOME)

                       IF GENERO = "M"
                           ADD 1 TO TOTAL-HOMENS
                       ELSE
                           IF GENERO = "F"
                               ADD 1 TO TOTAL-MULHERES
                           END-IF
                       END-IF
               END-READ
           END-PERFORM.

      * Fechar arquivo
           CLOSE FUNCIONARIOS.

      * Exibir o resumo
           DISPLAY "=============================".
           DISPLAY "Resumo:".
           DISPLAY " Total de Homens..: " TOTAL-HOMENS.
           DISPLAY " Total de Mulheres: " TOTAL-MULHERES.
           DISPLAY "=============================".

           STOP RUN.
