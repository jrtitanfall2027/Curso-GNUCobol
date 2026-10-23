       IDENTIFICATION DIVISION.
           PROGRAM-ID. ALUNO-RELATIVO.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQUIVO-ALUNO ASSIGN TO "alunos.dat"
               ORGANIZATION IS RELATIVE
               ACCESS MODE IS DYNAMIC
               RELATIVE KEY IS WS-CHAVE-RELATIVA.

       DATA DIVISION.
       FILE SECTION.
       FD ARQUIVO-ALUNO.
       01 REGISTRO-ALUNO.
           05 WS-MATRICULA        PIC 9(3).
           05 WS-NOME             PIC X(30).
           05 WS-IDADE            PIC 9(2).

       WORKING-STORAGE SECTION.
       01 OPCAO                   PIC X.
       01 WS-OPCAO-BUSCA          PIC X.
       01 WS-CHAVE-RELATIVA       PIC 9(3).
       01 WS-CHAVE-BUSCA          PIC 9(3).
       01 WS-INDICE-BUSCA         PIC 9(4).
       01 WS-ENTRADA-NUMERO       PIC X(256).
       01 WS-NOME-ENTRADA         PIC X(256).
       01 WS-NOME-BUSCA           PIC X(30).
       01 WS-ALTERAR              PIC X.
       01 WS-VALIDO               PIC X.
       01 WS-ENCONTROU            PIC X.
       01 WS-MATRICULA-LIVRE      PIC X.
       01 WS-ESPACO-VISTO         PIC X.
       01 WS-TEM-LETRA            PIC X.
       01 WS-CARACTERE            PIC X.
       01 WS-POSICAO              PIC 999.
       01 WS-QTD-DIGITOS          PIC 999.
       01 WS-MAX-DIGITOS          PIC 99.
       01 WS-LIMITE-NUMERO        PIC 9(3).
       01 WS-VALOR-NUMERO         PIC 9(10).

       PROCEDURE DIVISION.
       INICIO.
           DISPLAY "---------------------".
           DISPLAY "Sistema de Alunos".
           DISPLAY "---------------------".
           DISPLAY "1 - Inserir Aluno".
           DISPLAY "2 - Buscar Aluno".
           DISPLAY "3 - Sair".
           ACCEPT OPCAO.

           EVALUATE OPCAO
               WHEN "1"
                   PERFORM INSERIR-ALUNO
               WHEN "2"
                   PERFORM BUSCAR-ALUNO
               WHEN "3"
                   STOP RUN
               WHEN OTHER
                   DISPLAY "Opcao Invalida!"
                   PERFORM INICIO
           END-EVALUATE.

       INSERIR-ALUNO.
           DISPLAY "---------------------".
           DISPLAY "Cadastrar Aluno".
           DISPLAY "---------------------".
           OPEN I-O ARQUIVO-ALUNO.
           MOVE "N" TO WS-MATRICULA-LIVRE.
           PERFORM UNTIL WS-MATRICULA-LIVRE = "S"
               DISPLAY "Matricula do Aluno:"
               MOVE 999 TO WS-LIMITE-NUMERO
               MOVE 3 TO WS-MAX-DIGITOS
               PERFORM LER-NUMERO
               MOVE WS-VALOR-NUMERO TO WS-CHAVE-RELATIVA
               MOVE WS-CHAVE-RELATIVA TO WS-MATRICULA
               READ ARQUIVO-ALUNO INVALID KEY
                   MOVE "S" TO WS-MATRICULA-LIVRE
               NOT INVALID KEY
                   DISPLAY "Matricula ja cadastrada."
                   DISPLAY "Digite outra matricula."
               END-READ
           END-PERFORM.
           PERFORM LER-NOME.
           PERFORM LER-IDADE.

           WRITE REGISTRO-ALUNO INVALID KEY
               DISPLAY "Erro ao gravar registro!"
           NOT INVALID KEY
               DISPLAY "Aluno gravado com sucesso!"
           END-WRITE.
           CLOSE ARQUIVO-ALUNO.

           PERFORM INICIO.

       BUSCAR-ALUNO.
           DISPLAY "---------------------".
           DISPLAY " Buscar Aluno".
           DISPLAY "---------------------".
           DISPLAY "1 - Buscar por matricula".
           DISPLAY "2 - Buscar por nome".
           ACCEPT WS-OPCAO-BUSCA.

           OPEN I-O ARQUIVO-ALUNO.
           MOVE "N" TO WS-ENCONTROU.
           EVALUATE WS-OPCAO-BUSCA
               WHEN "1"
                   PERFORM BUSCAR-POR-MATRICULA
               WHEN "2"
                   PERFORM BUSCAR-POR-NOME
               WHEN OTHER
                   DISPLAY "Opcao de busca invalida!"
           END-EVALUATE.

           IF WS-ENCONTROU = "S"
               PERFORM MOSTRAR-REGISTRO
               PERFORM PERGUNTAR-ALTERACAO
           ELSE
               IF WS-OPCAO-BUSCA = "1" OR WS-OPCAO-BUSCA = "2"
                   DISPLAY "Registro nao encontrado!"
               END-IF
           END-IF.
           CLOSE ARQUIVO-ALUNO.

           PERFORM INICIO.

       BUSCAR-POR-MATRICULA.
           DISPLAY "Qual matricula deseja obter?".
           MOVE 999 TO WS-LIMITE-NUMERO.
           MOVE 3 TO WS-MAX-DIGITOS.
           PERFORM LER-NUMERO.
           MOVE WS-VALOR-NUMERO TO WS-CHAVE-BUSCA.
           MOVE WS-CHAVE-BUSCA TO WS-CHAVE-RELATIVA.
           READ ARQUIVO-ALUNO INVALID KEY
               CONTINUE
           NOT INVALID KEY
               MOVE "S" TO WS-ENCONTROU
           END-READ.

       BUSCAR-POR-NOME.
           DISPLAY "Digite o nome completo do aluno:".
           PERFORM LER-NOME.
           MOVE WS-NOME TO WS-NOME-BUSCA.
           PERFORM VARYING WS-INDICE-BUSCA FROM 1 BY 1
               UNTIL WS-INDICE-BUSCA > 999
                   OR WS-ENCONTROU = "S"
               MOVE WS-INDICE-BUSCA TO WS-CHAVE-RELATIVA
               READ ARQUIVO-ALUNO INVALID KEY
                   CONTINUE
               NOT INVALID KEY
                   IF FUNCTION UPPER-CASE(FUNCTION TRIM(WS-NOME))
                       = FUNCTION UPPER-CASE(
                           FUNCTION TRIM(WS-NOME-BUSCA))
                       MOVE "S" TO WS-ENCONTROU
                   END-IF
               END-READ
           END-PERFORM.

       MOSTRAR-REGISTRO.
           DISPLAY "---------------------".
           DISPLAY "Aluno Encontrado:".
           DISPLAY "Matricula: " WS-MATRICULA.
           DISPLAY "Nome: " WS-NOME.
           DISPLAY "Idade: " WS-IDADE.

       PERGUNTAR-ALTERACAO.
           DISPLAY "Deseja alterar este registro? (S/N)".
           MOVE SPACE TO WS-ALTERAR.
           PERFORM UNTIL WS-ALTERAR = "S" OR WS-ALTERAR = "N"
               ACCEPT WS-ALTERAR
               MOVE FUNCTION UPPER-CASE(WS-ALTERAR)
                   TO WS-ALTERAR
               IF WS-ALTERAR NOT = "S" AND WS-ALTERAR NOT = "N"
                   DISPLAY "Digite S para sim ou N para nao."
               END-IF
           END-PERFORM.
           IF FUNCTION UPPER-CASE(WS-ALTERAR) = "S"
               PERFORM ALTERAR-REGISTRO
           END-IF.

       ALTERAR-REGISTRO.
           DISPLAY "Informe os novos dados."
           DISPLAY "Pressione Enter para manter o valor atual."
           DISPLAY "Nome atual: " WS-NOME.
           PERFORM LER-NOME-ALTERACAO.
           DISPLAY "Idade atual: " WS-IDADE.
           PERFORM LER-IDADE-ALTERACAO.
           DISPLAY "---------------------".
           DISPLAY "Dados que serao salvos:".
           PERFORM MOSTRAR-REGISTRO.
           DISPLAY "Confirma a gravacao? (S/N)".
           MOVE SPACE TO WS-ALTERAR.
           PERFORM UNTIL WS-ALTERAR = "S" OR WS-ALTERAR = "N"
               ACCEPT WS-ALTERAR
               MOVE FUNCTION UPPER-CASE(WS-ALTERAR)
                   TO WS-ALTERAR
               IF WS-ALTERAR NOT = "S" AND WS-ALTERAR NOT = "N"
                   DISPLAY "Digite S para sim ou N para nao."
               END-IF
           END-PERFORM.
           IF WS-ALTERAR = "S"
               REWRITE REGISTRO-ALUNO INVALID KEY
                   DISPLAY "Erro ao alterar registro!"
               NOT INVALID KEY
                   DISPLAY "Registro alterado com sucesso!"
               END-REWRITE
           ELSE
               DISPLAY "Alteracao cancelada."
           END-IF.

       LER-NOME-ALTERACAO.
           MOVE "N" TO WS-VALIDO.
           PERFORM UNTIL WS-VALIDO = "S"
               DISPLAY "Novo nome (Enter para manter):"
               ACCEPT WS-NOME-ENTRADA
               IF WS-NOME-ENTRADA = SPACES
                   MOVE "S" TO WS-VALIDO
               ELSE
                   PERFORM VALIDAR-NOME
                   IF WS-VALIDO = "S"
                       MOVE WS-NOME-ENTRADA(1:30) TO WS-NOME
                   ELSE
                       DISPLAY "Use apenas letras e espacos,"
                           " ate 30 caracteres."
                   END-IF
               END-IF
           END-PERFORM.

       LER-IDADE-ALTERACAO.
           MOVE 99 TO WS-LIMITE-NUMERO.
           MOVE 2 TO WS-MAX-DIGITOS.
           MOVE "N" TO WS-VALIDO.
           PERFORM UNTIL WS-VALIDO = "S"
               DISPLAY "Nova idade (Enter para manter):"
               ACCEPT WS-ENTRADA-NUMERO
               IF WS-ENTRADA-NUMERO = SPACES
                   MOVE "S" TO WS-VALIDO
               ELSE
                   PERFORM VALIDAR-NUMERO
                   IF WS-VALIDO = "S"
                       MOVE WS-VALOR-NUMERO TO WS-IDADE
                   ELSE
                       DISPLAY "Digite uma idade valida de 1 a 99."
                   END-IF
               END-IF
           END-PERFORM.

       LER-NOME.
           MOVE "N" TO WS-VALIDO.
           PERFORM UNTIL WS-VALIDO = "S"
               DISPLAY "Nome do Aluno (somente letras e espacos):"
               ACCEPT WS-NOME-ENTRADA
               PERFORM VALIDAR-NOME
               IF WS-VALIDO NOT = "S"
                   DISPLAY "Use apenas letras e espacos,"
                       " ate 30 caracteres."
               END-IF
           END-PERFORM
           MOVE WS-NOME-ENTRADA(1:30) TO WS-NOME.

       VALIDAR-NOME.
               MOVE "S" TO WS-VALIDO
               MOVE "N" TO WS-TEM-LETRA
               PERFORM VARYING WS-POSICAO FROM 1 BY 1
                   UNTIL WS-POSICAO > 256
                   MOVE WS-NOME-ENTRADA(WS-POSICAO:1)
                       TO WS-CARACTERE
                   IF WS-CARACTERE NOT = SPACE
                       IF WS-POSICAO > 30
                           MOVE "N" TO WS-VALIDO
                       ELSE
                           IF WS-CARACTERE IS ALPHABETIC
                           MOVE "S" TO WS-TEM-LETRA
                           ELSE
                               MOVE "N" TO WS-VALIDO
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
               IF WS-TEM-LETRA NOT = "S"
                   MOVE "N" TO WS-VALIDO
               END-IF
           .

       LER-IDADE.
           DISPLAY "Idade do Aluno:".
           MOVE 99 TO WS-LIMITE-NUMERO.
           MOVE 2 TO WS-MAX-DIGITOS.
           PERFORM LER-NUMERO.
           MOVE WS-VALOR-NUMERO TO WS-IDADE.

       LER-NUMERO.
           MOVE "N" TO WS-VALIDO.
           PERFORM UNTIL WS-VALIDO = "S"
               ACCEPT WS-ENTRADA-NUMERO
               PERFORM VALIDAR-NUMERO
               IF WS-VALIDO NOT = "S"
                   DISPLAY "Digite um numero valido."
               END-IF
           END-PERFORM.

       VALIDAR-NUMERO.
               MOVE "S" TO WS-VALIDO
               MOVE "N" TO WS-ESPACO-VISTO
               MOVE 0 TO WS-QTD-DIGITOS
               PERFORM VARYING WS-POSICAO FROM 1 BY 1
                   UNTIL WS-POSICAO > 256
                   MOVE WS-ENTRADA-NUMERO(WS-POSICAO:1)
                       TO WS-CARACTERE
                   IF WS-CARACTERE = SPACE
                       MOVE "S" TO WS-ESPACO-VISTO
                   ELSE
                       IF WS-ESPACO-VISTO = "S"
                           MOVE "N" TO WS-VALIDO
                       END-IF
                       IF WS-CARACTERE IS NUMERIC
                           ADD 1 TO WS-QTD-DIGITOS
                       ELSE
                           MOVE "N" TO WS-VALIDO
                       END-IF
                   END-IF
               END-PERFORM
               IF WS-QTD-DIGITOS = 0
                   MOVE "N" TO WS-VALIDO
               END-IF
               IF WS-QTD-DIGITOS > WS-MAX-DIGITOS
                   MOVE "N" TO WS-VALIDO
               END-IF
               IF WS-VALIDO = "S"
                   COMPUTE WS-VALOR-NUMERO =
                       FUNCTION NUMVAL(WS-ENTRADA-NUMERO)
                   IF WS-VALOR-NUMERO = 0
                       MOVE "N" TO WS-VALIDO
                   END-IF
                   IF WS-VALOR-NUMERO > WS-LIMITE-NUMERO
                       MOVE "N" TO WS-VALIDO
                   END-IF
               END-IF
           .
