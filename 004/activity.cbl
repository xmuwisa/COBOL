       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACTIVITY.
       AUTHOR. LUISA EUSTAQUIO.
       DATE-WRITTEN. 29/11/23.

       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-LENGTH       PIC 9(3).
       01  WS-WIDTH        PIC 9(3).
       01  WS-HEIGHT       PIC 9(3).
       01  WS-RESULT.
           05 DISPLAY-V    PIC X(8) VALUE "VOLUME: ".
           05 WS-VOLUME    PIC 9(3).
           05 DIVIDER      PIC X(3) VALUE " | ".
           05 DISPLAY-A    PIC X(14) VALUE "SURFACE AREA: ".
           05 WS-S-AREA    PIC 9(3).
       01 WS-END           PIC 9(1) VALUE 0.

       PROCEDURE DIVISION.
           PERFORM UNTIL WS-END = 1
               DISPLAY "INPUT LENGTH: " WITH NO ADVANCING
               ACCEPT WS-LENGTH
               DISPLAY "INPUT WIDTH: " WITH NO ADVANCING
               ACCEPT WS-WIDTH
               DISPLAY "INPUT HEIGHT: " WITH NO ADVANCING
               ACCEPT WS-HEIGHT
               PERFORM RSLT-CALCULATION
               DISPLAY "END? 0:NO | 1:YES => " WITH NO ADVANCING
               ACCEPT WS-END
               IF WS-END = 1
                   DISPLAY "Program ends..."
               END-IF
           END-PERFORM.

           STOP RUN.

           RSLT-CALCULATION.
               COMPUTE WS-VOLUME = WS-LENGTH * WS-WIDTH * WS-HEIGHT
               COMPUTE WS-S-AREA = 2 * (
                   WS-LENGTH * WS-WIDTH + 
                   WS-WIDTH * WS-HEIGHT +
                   WS-HEIGHT * WS-LENGTH
               )
               DISPLAY WS-RESULT.
