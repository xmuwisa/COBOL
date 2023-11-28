       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACTIVITY.
       AUTHOR. LUISA EUSTAQUIO.
       DATE-WRITTEN. 29/11/23.

       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-INPUT-1      PIC 9(3).
       01  WS-INPUT-2      PIC 9(3).
       01  WS-RESULT.
           05 DISPLAY-S    PIC X(5) VALUE "SUM: ".
           05 WS-SUM       PIC 9(3).
           05 DIVIDER      PIC X(3) VALUE " | ".
           05 DISPLAY-D    PIC X(5) VALUE "DIF: ".
           05 WS-DIF       PIC 9(3).
           05 DIVIDER      PIC X(3) VALUE " | ".
           05 DISPLAY-P    PIC X(5) VALUE "PRO: ".
           05 WS-PRO       PIC 9(6).
           05 DIVIDER      PIC X(3) VALUE " | ".
           05 DISPLAY-Q    PIC X(5) VALUE "QUO: ".
           05 WS-QUO       PIC 9(3).
           05 FILL         PIC X(1) VALUE SPACE.
           05 DISPLAY-R    PIC X(5) VALUE "REM: ".
           05 WS-REM       PIC 9(3).
       01 WS-END           PIC 9(1).

       PROCEDURE DIVISION.
           PERFORM UNTIL WS-END = 1
               DISPLAY "INPUT 1: " WITH NO ADVANCING
               ACCEPT WS-INPUT-1
               DISPLAY "INPUT 2: " WITH NO ADVANCING
               ACCEPT WS-INPUT-2
               PERFORM RSLT-CALCULATION
               DISPLAY "END? 0:NO | 1:YES => " WITH NO ADVANCING
               ACCEPT WS-END
           END-PERFORM.

           RSLT-CALCULATION.
               ADD WS-INPUT-1 TO WS-INPUT-2
                   GIVING WS-SUM
               SUBTRACT WS-INPUT-1 FROM WS-INPUT-2
                   GIVING WS-DIF
               MULTIPLY WS-INPUT-1 BY WS-INPUT-2
                   GIVING WS-PRO
               DIVIDE WS-INPUT-1 BY WS-INPUT-2
                   GIVING WS-QUO REMAINDER WS-REM
               DISPLAY WS-RESULT.

           STOP RUN.
                   