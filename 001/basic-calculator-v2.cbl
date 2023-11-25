       IDENTIFICATION DIVISION.
       PROGRAM-ID. BASIC-CALCULATOR-V2.
       AUTHOR. LUISA EUSTAQUIO.

       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-RUN PIC 9(3) VALUE 1.
       01  WS-FIRST PIC 9(3).
       01  WS-SECOND PIC 9(3).
       01  WS-INPUT-CHECK PIC 9(9).
       01  WS-RESULT PIC Z(9).
       01  WS-OPERATOR PIC 9(1).

       PROCEDURE DIVISION.
           PERFORM BASIC-CALCULATOR

           PERFORM UNTIL WS-RUN = 0
               DISPLAY 'CONTINUE? ' WITH NO ADVANCING
               ACCEPT WS-RUN
               
               IF WS-RUN = 1
                   PERFORM BASIC-CALCULATOR
               ELSE IF WS-RUN = 0
                   DISPLAY 'PROGRAM ENDS...'
               ELSE
                   DISPLAY '0 OR 1 ONLY'
           END-PERFORM.

           STOP RUN.

           BASIC-CALCULATOR.
               DISPLAY '=================================='.
               DISPLAY '         BASIC CALCULATOR         '.
               DISPLAY '=================================='.
               
               DISPLAY 'FIRST OPERAND: ' WITH NO ADVANCING.
               ACCEPT WS-INPUT-CHECK.
               IF WS-INPUT-CHECK > 999 THEN
                   DISPLAY 'NO GREATER THAN 999'
                   PERFORM STOP-PROGRAM
               ELSE 
                   MOVE WS-INPUT-CHECK TO WS-FIRST 
               END-IF.
                           
               DISPLAY 'SECOND OPERAND: ' WITH NO ADVANCING.
               ACCEPT WS-INPUT-CHECK.
               IF WS-INPUT-CHECK > 999 THEN
                   DISPLAY 'NO GREATER THAN 999'
                   PERFORM STOP-PROGRAM
               ELSE 
                   MOVE WS-INPUT-CHECK TO WS-SECOND 
               END-IF.

               DISPLAY '| [0] + | [1] - | [2] * | [3] / |'.
               DISPLAY 'CHOOSE OPERATOR: ' WITH NO ADVANCING.
               ACCEPT WS-INPUT-CHECK.
               IF WS-INPUT-CHECK > 3 OR WS-INPUT-CHECK < 0 THEN
                   DISPLAY 'MUST BE 0-4 INPUT'
                   PERFORM STOP-PROGRAM
               ELSE IF WS-INPUT-CHECK = 3 AND WS-FIRST = 0
                   OR WS-SECOND = 0 THEN
                   DISPLAY '0 VALUE OF OPERAND/S'
                   PERFORM STOP-PROGRAM
               ELSE
                   MOVE WS-INPUT-CHECK TO WS-OPERATOR
               END-IF.

               EVALUATE WS-OPERATOR
                   WHEN 0
                       ADD WS-FIRST TO WS-SECOND GIVING WS-RESULT
                   WHEN 1
                       SUBTRACT WS-FIRST FROM WS-SECOND GIVING WS-RESULT
                   WHEN 2
                       MULTIPLY WS-FIRST BY WS-SECOND GIVING WS-RESULT
                   WHEN 3
                       DIVIDE WS-FIRST BY WS-SECOND GIVING WS-RESULT
               END-EVALUATE.

               DISPLAY 'Result: ' WS-RESULT.

               STOP-PROGRAM.
                   DISPLAY 'PROGRAM ENDS...'
                   STOP RUN.
                   