      ******************************************************************
      * Author: SAMIR ESTELA
      * Date: 
      * Purpose: * Purpose: Procesar y validar movimientos bancarios 
      *            desde un archivo de entrada.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MOVIMIENTOS-BANCARIOS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
           SELECT MOV-BANCARIO-INPUT
               ASSIGN TO "MOVIMIENTOS.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT MOV-REPORTE-OUTPUT
               ASSIGN TO "MOVIMIENTOS-REPORTE-OUTPUT.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD MOV-BANCARIO-INPUT.
       01 MOV-INPUT.
           05 NUM-CUENTA         PIC 9(10).
           05 FECHA              PIC 9(8).
           05 TIPO-MOV           PIC X(3).
           05 MONTO              PIC 9(11).

       FD MOV-REPORTE-OUTPUT.
       01 REG-REPORTE.
           05 NUM-CUENTA-SAL     PIC 9(10).
           05 FILLER             PIC X.
           05 FECHA-SAL          PIC 9(8).
           05 FILLER             PIC X.
           05 TIPO-MOV-SAL       PIC X(3).
           05 FILLER             PIC X.
           05 MONTO-SAL          PIC 9(11).

       WORKING-STORAGE SECTION.

       01 WS-FIN-ARCHIVO        PIC X VALUE "N".
       01 WS-REG-PROCESADOS     PIC 9(6) VALUE 0.
       01 WS-REG-VALIDO         PIC X VALUE "N".

       01 SUBRAYADO-1           PIC X(50) VALUE ALL '-'.
       01 TITULOS-GENERAL       PIC X(50) VALUE
            'CUENTA      FECHA     TIPO MOVIENTO  MONTO-'.
       01 SUBRAYADO-2           PIC X(50) VALUE ALL '-'.

       01 LINEA-TEXTO           PIC X(50).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           OPEN INPUT  MOV-BANCARIO-INPUT
                OUTPUT MOV-REPORTE-OUTPUT

           *> ENCABEZADO
           MOVE SUBRAYADO-1     TO LINEA-TEXTO
           WRITE REG-REPORTE FROM LINEA-TEXTO

           MOVE TITULOS-GENERAL TO LINEA-TEXTO
           WRITE REG-REPORTE FROM LINEA-TEXTO

           MOVE SUBRAYADO-2     TO LINEA-TEXTO
           WRITE REG-REPORTE FROM LINEA-TEXTO

           READ MOV-BANCARIO-INPUT
               AT END MOVE "S" TO WS-FIN-ARCHIVO
           END-READ

           PERFORM UNTIL WS-FIN-ARCHIVO = "S"

              PERFORM VALIDACION

              IF WS-REG-VALIDO = "S"
                 PERFORM PROCESAR-REGISTRO
              ELSE
                 DISPLAY "REGISTRO INVALIDO: "
                         NUM-CUENTA " " FECHA " " TIPO-MOV " " MONTO
              END-IF

              READ MOV-BANCARIO-INPUT
                  AT END MOVE "S" TO WS-FIN-ARCHIVO
              END-READ

           END-PERFORM

           DISPLAY "TOTAL REGISTROS PROCESADOS : " WS-REG-PROCESADOS

           CLOSE MOV-BANCARIO-INPUT
                 MOV-REPORTE-OUTPUT

           STOP RUN.

       VALIDACION.
           MOVE "N" TO WS-REG-VALIDO

           IF NUM-CUENTA IS NUMERIC
              AND FECHA IS NUMERIC
              AND TIPO-MOV IS ALPHABETIC
              AND MONTO IS NUMERIC
                 MOVE "S" TO WS-REG-VALIDO
           END-IF.

       PROCESAR-REGISTRO.

           ADD 1 TO WS-REG-PROCESADOS

           MOVE NUM-CUENTA TO NUM-CUENTA-SAL
           MOVE FECHA      TO FECHA-SAL
           MOVE TIPO-MOV   TO TIPO-MOV-SAL
           MOVE MONTO      TO MONTO-SAL

           WRITE REG-REPORTE.

       END PROGRAM MOVIMIENTOS-BANCARIOS.
