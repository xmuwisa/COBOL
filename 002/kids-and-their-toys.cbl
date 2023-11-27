       IDENTIFICATION DIVISION.
       PROGRAM-ID. SARI-SARI-STORE.
       AUTHOR. LUISA EUSTAQUIO.
       DATE-WRITTEN. 27/11/23.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT KID-TOYS-RECORD
           ASSIGN TO 'input.dat'
           ORGANIZATION IS LINE SEQUENTIAL.
       
       SELECT KID-TOYS-REPORT
           ASSIGN TO 'output.txt'
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  KID-TOYS-RECORD.
       01  KIDS-RECORD.
           05 R-ID         PIC 9(2).
           05 R-TOYTYPE    PIC A(1).
           05 R-TOYS       PIC 9(2).
       FD KID-TOYS-REPORT.
       01  PRNT-OUT        PIC X(80).
       
       WORKING-STORAGE SECTION.
       01  DIVIDER-1       PIC X(21) VALUE "=====================".
       01  DIVIDER-3       PIC X(21) VALUE "---------------------".
       01  HEADER-1.
           05 FILL         PIC X(2) VALUE SPACES.
           05 H1-TITLE     PIC X(17) VALUE "KID'S TOYS REPORT".
       01  HEADER-2.
           05 H2-ID        PIC A(2) VALUE "ID".
           05 DIVIDER-2    PIC X(3) VALUE " | ".
           05 H2-TOYTP     PIC A(8) VALUE "TOY TYPE".
           05 DIVIDER-2    PIC X(3) VALUE " | ".
           05 H2-TTL       PIC A(5) VALUE "TOTAL".

       01  KIDS-REPORT.
           05 O-ID         PIC 9(2).
           05 DIVIDER-2    PIC X(3) VALUE " | ".
           05 FILL         PIC X(2) VALUE SPACES.
           05 O-TOYTYPE    PIC A(4).
           05 FILL         PIC X(2) VALUE SPACES.
           05 DIVIDER-2    PIC X(3) VALUE " | ".
           05 FILL         PIC X(1) VALUE SPACES.
           05 O-TOYS       PIC 9(2).

       01  FOOTER-1.
           05 FILL         PIC X(11) VALUE SPACES.
           05 F1-TTL        PIC X(7) VALUE "TOTAL: ".
           05 F1-TTLTOYS    PIC 9(3) VALUE 0.

       01  FOOTER-2.
           05 F2-TTL        PIC X(12) VALUE "TOTAL KIDS: ".
           05 F2-TTLKIDS    PIC 9(3). 

       01  WS-PREV-ID      PIC 9(2).
       01  WS-EOF          PIC A(1) VALUE 'N'.
       01  WS-LN      PIC 9(3).
      
       PROCEDURE DIVISION.
           OPEN INPUT KID-TOYS-RECORD
                OUTPUT KID-TOYS-REPORT.

           WRITE PRNT-OUT FROM DIVIDER-1.
           WRITE PRNT-OUT FROM HEADER-1.
           WRITE PRNT-OUT FROM DIVIDER-1.
           WRITE PRNT-OUT FROM HEADER-2.
           WRITE PRNT-OUT FROM DIVIDER-3.

           PERFORM UNTIL WS-EOF = 'Y'
               READ KID-TOYS-RECORD
                   AT END
                       PERFORM PRNT-LN-TTTL
                       PERFORM PRNT-LN-KTTL
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       COMPUTE WS-LN = WS-LN + 1
                       IF WS-LN = 1 THEN
                           MOVE R-ID TO WS-PREV-ID
                       END-IF
                       PERFORM PRNT-LN
               END-READ
           END-PERFORM.

           CLOSE KID-TOYS-RECORD
                 KID-TOYS-REPORT.

           STOP RUN.
      
           PRNT-LN.
               MOVE FUNCTION NUMVAL(R-ID) TO O-ID
               EVALUATE R-TOYTYPE
                   WHEN 'A'
                       MOVE "SOFT" TO O-TOYTYPE
                   WHEN 'B'
                       MOVE "HARD" TO O-TOYTYPE
               END-EVALUATE
               MOVE FUNCTION TRIM(R-TOYS) TO O-TOYS
               IF WS-LN = 1 THEN
                   ADD 1 TO F2-TTLKIDS
                   WRITE PRNT-OUT FROM KIDS-REPORT
                   ADD O-TOYS TO F1-TTLTOYS
               ELSE IF WS-LN >= 2 THEN
                   PERFORM ID-CHECK
                   WRITE PRNT-OUT FROM KIDS-REPORT
               END-IF.
               

           PRNT-LN-TTTL.
               MOVE FUNCTION NUMVAL(F1-TTLTOYS) TO F1-TTLTOYS
               WRITE PRNT-OUT FROM FOOTER-1.

           ID-CHECK.
               IF R-ID = WS-PREV-ID THEN
                   ADD O-TOYS TO F1-TTLTOYS
                   MOVE R-ID TO WS-PREV-ID
               ELSE
                   ADD 1 TO F2-TTLKIDS
                   PERFORM PRNT-LN-TTTL
                   MOVE 0 TO F1-TTLTOYS
                   ADD O-TOYS TO F1-TTLTOYS
                   MOVE R-ID TO WS-PREV-ID
               END-IF.

           PRNT-LN-KTTL.
               WRITE PRNT-OUT FROM FOOTER-2.
                       