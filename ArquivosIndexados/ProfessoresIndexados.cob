       IDENTIFICATION DIVISION.
           PROGRAM-ID. ProfessoresIndexados.
           AUTHOR. Reginaldo Junior.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-PROFESSOR ASSIGN TO "professores.idx"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PRO-MATRICULA
               FILE STATUS IS WS-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD ARQ-PROFESSOR.
       01 REG-PROFESSOR.
           05 PRO-MATRICULA    PIC X(8).
           05 PRO-NOME         PIC X(30).

       WORKING-STORAGE SECTION.
       01 WS-OPCAO             PIC 9.
       01 WS-STATUS            PIC X(2).
       01 WS-SUFIXO            PIC X(3).
       01 WS-VALIDO            PIC X.
       01 WS-MATRICULA-LIVRE   PIC X.
       01 WS-CONFIRMACAO       PIC X.
       01 WS-ENCONTRADO        PIC X.
       01 WS-FIM-CONFIRMACAO   PIC X.
       01 WS-FIM-LEITURA       PIC X.
       01 WS-NAVEGACAO         PIC X.
       01 WS-LIMITE-ATINGIDO   PIC X.
       01 WS-QTD-REGISTROS     PIC 9(4).
       01 WS-PAGINA-INICIO     PIC 9(4).
       01 WS-INDICE            PIC 9(4).
       01 WS-TAM-PAGINA        PIC 9(2) VALUE 3.

       01 WS-PROFESSOR.
           05 WS-MATRICULA     PIC X(8).
           05 WS-NOME          PIC X(30).

       01 WS-TABELA-REGISTROS.
           05 WS-ITEM-REGISTRO OCCURS 1000 TIMES.
               10 WS-ITEM-MATRICULA PIC X(8).
               10 WS-ITEM-NOME      PIC X(30).

       PROCEDURE DIVISION.
       INICIO.
           DISPLAY "--------------------------------".
           DISPLAY "Menu do Professor".
           DISPLAY "--------------------------------".
           DISPLAY " 1 - Cadastrar Professor".
           DISPLAY " 2 - Mostrar Professores".
           DISPLAY " 3 - Modificar Professor".
           DISPLAY " 4 - Excluir Professor".
           DISPLAY " 5 - Sair".
           DISPLAY "--------------------------------".
           DISPLAY "Escolha uma opcao: ".
           ACCEPT WS-OPCAO.

           EVALUATE WS-OPCAO
               WHEN 1
                   PERFORM CADASTRAR
               WHEN 2
                   PERFORM MOSTRAR
               WHEN 3
                   PERFORM MODIFICAR
               WHEN 4
                   PERFORM EXCLUIR
               WHEN 5
                   STOP RUN
               WHEN OTHER
                   DISPLAY "Opcao invalida!"
                   PERFORM INICIO
           END-EVALUATE.
           
       ABRIR-ARQUIVO.
           OPEN I-O ARQ-PROFESSOR.
           IF WS-STATUS = "35"
               OPEN OUTPUT ARQ-PROFESSOR
               CLOSE ARQ-PROFESSOR
               OPEN I-O ARQ-PROFESSOR.
       
       FECHAR-ARQUIVO.
           CLOSE ARQ-PROFESSOR.

       CADASTRAR.
           DISPLAY "--------------------------------".
           DISPLAY "Cadastrar Professor".
           DISPLAY "--------------------------------".
           PERFORM ABRIR-ARQUIVO.
           MOVE "N" TO WS-FIM-CONFIRMACAO.
           PERFORM UNTIL WS-FIM-CONFIRMACAO = "S"
               PERFORM LER-MATRICULA-LIVRE
               DISPLAY "Nome: "
               ACCEPT WS-NOME
               MOVE WS-NOME TO PRO-NOME
               DISPLAY "--------------------------------"
               DISPLAY "Confirmar cadastro?"
               DISPLAY "Matricula: " PRO-MATRICULA
               DISPLAY "Nome: " PRO-NOME
               PERFORM CONFIRMAR-ACAO
               IF WS-CONFIRMACAO = "S"
                   WRITE REG-PROFESSOR
                       INVALID KEY
                           DISPLAY "Erro ao gravar professor."
                       NOT INVALID KEY
                           DISPLAY
                               "Professor cadastrado com sucesso!"
                   END-WRITE
                   MOVE "S" TO WS-FIM-CONFIRMACAO
               ELSE
                   IF WS-CONFIRMACAO = "N"
                       DISPLAY "Cadastro cancelado."
                       MOVE "S" TO WS-FIM-CONFIRMACAO
                   ELSE
                       DISPLAY "Informe os dados novamente."
                   END-IF
               END-IF
           END-PERFORM.
           PERFORM FECHAR-ARQUIVO.
           PERFORM INICIO.

       LER-MATRICULA-LIVRE.
           MOVE "N" TO WS-MATRICULA-LIVRE.
           PERFORM UNTIL WS-MATRICULA-LIVRE = "S"
               PERFORM LER-MATRICULA
               MOVE WS-MATRICULA TO PRO-MATRICULA
               READ ARQ-PROFESSOR KEY IS PRO-MATRICULA
                   INVALID KEY
                       MOVE "S" TO WS-MATRICULA-LIVRE
                   NOT INVALID KEY
                       DISPLAY "Matricula "
                           PRO-MATRICULA " ja cadastrada!"
                       DISPLAY "Informe outra matricula."
               END-READ
           END-PERFORM.

       LER-MATRICULA.
           MOVE "N" TO WS-VALIDO.
           PERFORM UNTIL WS-VALIDO = "S"
               DISPLAY "Matricula: PROF" WITH NO ADVANCING
               ACCEPT WS-SUFIXO
               IF WS-SUFIXO IS NUMERIC
                   MOVE "S" TO WS-VALIDO
               ELSE
                   DISPLAY "Informe 3 digitos numericos."
               END-IF
           END-PERFORM.
           MOVE SPACES TO WS-MATRICULA.
           STRING "PROF" DELIMITED BY SIZE
                  WS-SUFIXO DELIMITED BY SIZE
                  INTO WS-MATRICULA
           END-STRING.

       CONFIRMAR-ACAO.
           MOVE "N" TO WS-VALIDO.
           PERFORM UNTIL WS-VALIDO = "S"
               DISPLAY "S - Sim | N - Nao | C - Corrigir"
               MOVE SPACE TO WS-CONFIRMACAO
               ACCEPT WS-CONFIRMACAO
               MOVE FUNCTION UPPER-CASE(WS-CONFIRMACAO)
                   TO WS-CONFIRMACAO
               IF WS-CONFIRMACAO = "S"
                   OR WS-CONFIRMACAO = "N"
                   OR WS-CONFIRMACAO = "C"
                   MOVE "S" TO WS-VALIDO
               ELSE
                   DISPLAY "Digite S, N ou C."
               END-IF
           END-PERFORM.

       MOSTRAR.
           DISPLAY "--------------------------------".
           DISPLAY "Listar Professor".
           DISPLAY "--------------------------------".
           PERFORM ABRIR-ARQUIVO.
           MOVE 0 TO WS-QTD-REGISTROS.
           MOVE "N" TO WS-FIM-LEITURA.
           MOVE "N" TO WS-LIMITE-ATINGIDO.
           PERFORM UNTIL WS-FIM-LEITURA = "S"
               READ ARQ-PROFESSOR NEXT RECORD
                   AT END
                       MOVE "S" TO WS-FIM-LEITURA
                   NOT AT END
                       IF WS-QTD-REGISTROS < 1000
                           ADD 1 TO WS-QTD-REGISTROS
                           MOVE PRO-MATRICULA TO
                               WS-ITEM-MATRICULA(WS-QTD-REGISTROS)
                           MOVE PRO-NOME TO
                               WS-ITEM-NOME(WS-QTD-REGISTROS)
                       ELSE
                           MOVE "S" TO WS-LIMITE-ATINGIDO
                           MOVE "S" TO WS-FIM-LEITURA
                       END-IF
               END-READ
           END-PERFORM.

           IF WS-LIMITE-ATINGIDO = "S"
               DISPLAY "Aviso: limite de 1000 registros."
               DISPLAY "A lista foi limitada."
           END-IF.
           IF WS-QTD-REGISTROS = 0
               DISPLAY "Nenhum professor cadastrado."
           ELSE
               MOVE 1 TO WS-PAGINA-INICIO
               MOVE SPACE TO WS-NAVEGACAO
               PERFORM UNTIL WS-NAVEGACAO = "S"
                   PERFORM MOSTRAR-PAGINA
                   DISPLAY "D - Baixo | C - Cima"
                   DISPLAY "T - Topo | F - Final | S - Sair"
                   ACCEPT WS-NAVEGACAO
                   MOVE FUNCTION UPPER-CASE(WS-NAVEGACAO)
                       TO WS-NAVEGACAO
                   EVALUATE WS-NAVEGACAO
                       WHEN "D"
                           IF WS-PAGINA-INICIO + WS-TAM-PAGINA
                               <= WS-QTD-REGISTROS
                               ADD WS-TAM-PAGINA
                                   TO WS-PAGINA-INICIO
                           ELSE
                               DISPLAY "Voce ja esta no final."
                           END-IF
                       WHEN "C"
                           IF WS-PAGINA-INICIO > WS-TAM-PAGINA
                               SUBTRACT WS-TAM-PAGINA
                                   FROM WS-PAGINA-INICIO
                           ELSE
                               MOVE 1 TO WS-PAGINA-INICIO
                               DISPLAY "Voce ja esta no topo."
                           END-IF
                       WHEN "T"
                           MOVE 1 TO WS-PAGINA-INICIO
                       WHEN "F"
                           COMPUTE WS-PAGINA-INICIO =
                               ((WS-QTD-REGISTROS - 1)
                               / WS-TAM-PAGINA)
                               * WS-TAM-PAGINA + 1
                       WHEN "S"
                           CONTINUE
                       WHEN OTHER
                           DISPLAY "Opcao invalida."
                   END-EVALUATE
               END-PERFORM
           END-IF.

           PERFORM FECHAR-ARQUIVO.
           PERFORM INICIO.

       MOSTRAR-PAGINA.
           DISPLAY "--------------------------------".
           PERFORM VARYING WS-INDICE FROM WS-PAGINA-INICIO BY 1
               UNTIL WS-INDICE > WS-QTD-REGISTROS
                   OR WS-INDICE >=
                       WS-PAGINA-INICIO + WS-TAM-PAGINA
               DISPLAY "Matricula: "
                   WS-ITEM-MATRICULA(WS-INDICE)
               DISPLAY "Nome: "
                   WS-ITEM-NOME(WS-INDICE)
               DISPLAY "--------------------------------"
           END-PERFORM.
           
       MODIFICAR.
           DISPLAY "--------------------------------".
           DISPLAY "Alterar Professor".
           DISPLAY "--------------------------------".
           PERFORM ABRIR-ARQUIVO.
           PERFORM LER-MATRICULA.
           MOVE WS-MATRICULA TO PRO-MATRICULA.
           READ ARQ-PROFESSOR KEY IS PRO-MATRICULA
               INVALID KEY
                   DISPLAY "Matricula nao encontrada!"
               NOT INVALID KEY
                   MOVE "C" TO WS-CONFIRMACAO
                   MOVE "N" TO WS-FIM-CONFIRMACAO
                   PERFORM UNTIL WS-FIM-CONFIRMACAO = "S"
                       DISPLAY "--------------------------------"
                       DISPLAY "Dados atuais:"
                       DISPLAY "Matricula: " PRO-MATRICULA
                       DISPLAY "Nome: " PRO-NOME
                       DISPLAY "Novo nome: "
                       ACCEPT WS-NOME
                       MOVE WS-NOME TO PRO-NOME
                       DISPLAY "Confirmar alteracao?"
                       DISPLAY "Matricula: " PRO-MATRICULA
                       DISPLAY "Nome: " PRO-NOME
                       PERFORM CONFIRMAR-ACAO
                       IF WS-CONFIRMACAO = "S"
                           REWRITE REG-PROFESSOR
                               INVALID KEY
                                   DISPLAY "Erro ao atualizar!"
                               NOT INVALID KEY
                                   DISPLAY "Registro atualizado!"
                           END-REWRITE
                           MOVE "S" TO WS-FIM-CONFIRMACAO
                       ELSE
                           IF WS-CONFIRMACAO = "N"
                               DISPLAY "Alteracao cancelada."
                               MOVE "S" TO WS-FIM-CONFIRMACAO
                           ELSE
                               DISPLAY "Digite o nome novamente."
                           END-IF
                       END-IF
                   END-PERFORM
           END-READ.

           PERFORM FECHAR-ARQUIVO.
           PERFORM INICIO.

       EXCLUIR.
           DISPLAY "--------------------------------".
           DISPLAY "Excluir Professor".
           DISPLAY "--------------------------------".
           PERFORM ABRIR-ARQUIVO.
           MOVE "N" TO WS-ENCONTRADO.
           PERFORM UNTIL WS-ENCONTRADO = "S"
               PERFORM LER-MATRICULA
               MOVE WS-MATRICULA TO PRO-MATRICULA
               READ ARQ-PROFESSOR KEY IS PRO-MATRICULA
                   INVALID KEY
                       DISPLAY "Matricula nao encontrada!"
                   NOT INVALID KEY
                       DISPLAY "--------------------------------"
                       DISPLAY "Confirmar exclusao?"
                       DISPLAY "Matricula: " PRO-MATRICULA
                       DISPLAY "Nome: " PRO-NOME
                       PERFORM CONFIRMAR-ACAO
                       IF WS-CONFIRMACAO = "S"
                           DELETE ARQ-PROFESSOR
                               INVALID KEY
                                   DISPLAY "Erro ao excluir!"
                               NOT INVALID KEY
                                   DISPLAY "Registro excluido!"
                           END-DELETE
                           MOVE "S" TO WS-ENCONTRADO
                       ELSE
                           IF WS-CONFIRMACAO = "N"
                               DISPLAY "Exclusao cancelada."
                               MOVE "S" TO WS-ENCONTRADO
                           ELSE
                               DISPLAY "Informe a matricula"
                                   " do registro correto."
                           END-IF
                       END-IF
               END-READ
           END-PERFORM.

           PERFORM FECHAR-ARQUIVO.
           PERFORM INICIO.   
                   