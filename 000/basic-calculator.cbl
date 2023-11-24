       IDENTIFICATION DIVISION.
       PROGRAM-ID. BASIC-CALCULATOR.
       AUTHOR. LUISA EUSTAQUIO.

       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-FIRST PIC 9(3).
       01  WS-SECOND PIC 9(3).
       01  WS-RESULT PIC Z(9).
       01  WS-OPERATOR PIC 9(1).

       PROCEDURE DIVISION.
           DISPLAY 'Input first number: '.
               ACCEPT WS-FIRST.
           DISPLAY 'Input second number: '.
               ACCEPT WS-SECOND.

           DISPLAY '| [0] + | [1] - | [2] * | [3] / |'.
           DISPLAY 'Choose operator: '.
               ACCEPT WS-OPERATOR.

           EVALUATE WS-OPERATOR
               WHEN 0
                   ADD WS-FIRST TO WS-SECOND GIVING WS-RESULT
               WHEN 1
                   SUBTRACT WS-FIRST FROM WS-SECOND GIVING WS-RESULT
               WHEN 2
                   MULTIPLY WS-FIRST BY WS-SECOND GIVING WS-RESULT
               WHEN 3
                   DIVIDE WS-FIRST BY WS-SECOND GIVING WS-RESULT
           END-EVALUATE

           DISPLAY 'Result: ' WS-RESULT

           STOP RUN.
           