       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAVE-APPLICATION.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
          
           SELECT OPTIONAL APP-FILE ASSIGN TO 
           "./files/T_APPLICATION.CSV"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD APP-FILE.
       01 APP-RECORD               PIC X(400).

       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS           PIC XX.
       01 WS-OUT-LINE              PIC X(400).
       01 WS-STATUS-PENDING        PIC X(10) VALUE "PENDING".
       
       01 WS-TEMP-RECORD           PIC X(400).
       01 WS-LAST-ID               PIC 9(10) VALUE ZERO.
       01 WS-NEXT-ID               PIC 9(10) VALUE 1.
       01 WS-UNSTRING-ID           PIC X(10).


       LINKAGE SECTION.
       01 LK-COMM-AREA.
           05 LK-CONTINUE           PIC X(10).
           05 LK-DEVICE-DATA.
              10 LK-IMEI            PIC X(15).
              10 LK-DEVICE-TYPE     PIC X(10).
              10 LK-DEVICE-MODEL    PIC X(20).
              10 LK-PURCHASE-DATE   PIC X(10).
              10 LK-PRICE           PIC 9(6).
           05 LK-CALC-RESULTS.
              10 LK-PRICE-CATEGORY  PIC X(6).
              10 LK-PERIOD          PIC X(3).
              10 LK-PERIOD-FACTOR   PIC 9V99.
              10 LK-PERIOD-MONTHS   PIC 99.
              10 LK-EST-PREMIUM     PIC 9(9)V99.
           05 LK-PLAN-DATA.
              10 LK-PLAN-CODE       PIC X(5).
              10 LK-PLAN-NAME       PIC X(20).
              10 LK-PLAN-BASE-RATE   PIC 99999.
              10 LK-PLAN-MAX-PAYOUT PIC 9(8).
              10 LK-CURRENT-PREMIUM PIC 9(9)V99.
              10 LK-FINAL-PREMIUM   PIC 9(9)V99.
           05 LK-SYSTEM-DATE-STR    PIC X(19).
           05 LK-USER-DATA.
              10 LK-USER-NAME        PIC X(50).
              10 LK-USER-EMAIL       PIC X(50).
              10 LK-USER-PHONE       PIC X(15).
              10 LK-USER-POSTAL-CODE PIC X(7).
              10 LK-USER-ADDRESS     PIC X(100).
              10 LK-USER-DOB         PIC X(8).

       PROCEDURE DIVISION USING LK-COMM-AREA.

       MAIN-PROCEDURE.
           PERFORM DETERMINE-NEXT-APP-ID
           PERFORM WRITE-APPLICATION-RECORD
           EXIT PROGRAM.

       DETERMINE-NEXT-APP-ID.
           MOVE 0 TO WS-LAST-ID
           OPEN INPUT APP-FILE
           IF WS-FILE-STATUS = '00'
               PERFORM UNTIL WS-FILE-STATUS = '10'
                   READ APP-FILE INTO WS-TEMP-RECORD
                       AT END
                           MOVE '10' TO WS-FILE-STATUS
                       NOT AT END
                           IF WS-TEMP-RECORD NOT = SPACES
                               MOVE SPACES TO WS-UNSTRING-ID
                               UNSTRING WS-TEMP-RECORD DELIMITED BY ","
                                   INTO WS-UNSTRING-ID
                               COMPUTE WS-LAST-ID = 
                                   FUNCTION NUMVAL(WS-UNSTRING-ID)
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE APP-FILE
               COMPUTE WS-NEXT-ID = WS-LAST-ID + 1
           ELSE
               CLOSE APP-FILE
               MOVE 1 TO WS-NEXT-ID
           END-IF.

           MOVE WS-NEXT-ID TO LK-CONTINUE.

       WRITE-APPLICATION-RECORD.
           OPEN EXTEND APP-FILE
           IF WS-FILE-STATUS NOT = '00' AND WS-FILE-STATUS NOT = '02'
               CLOSE APP-FILE
               OPEN OUTPUT APP-FILE
           END-IF.

           MOVE SPACES TO WS-OUT-LINE
           
           STRING 
               FUNCTION TRIM(LK-CONTINUE) DELIMITED BY SIZE ","
               FUNCTION TRIM(LK-USER-NAME) DELIMITED BY SIZE ","
               FUNCTION TRIM(LK-USER-EMAIL) DELIMITED BY SIZE ","
               FUNCTION TRIM(LK-USER-ADDRESS) DELIMITED BY SIZE ","
               FUNCTION TRIM(LK-DEVICE-TYPE) DELIMITED BY SIZE ","
               FUNCTION TRIM(LK-PLAN-CODE) DELIMITED BY SIZE ","
               FUNCTION TRIM(WS-STATUS-PENDING) DELIMITED BY SIZE ","
               'Y'                                DELIMITED BY SIZE ","
               FUNCTION TRIM(LK-SYSTEM-DATE-STR) DELIMITED BY SIZE
               INTO WS-OUT-LINE
           END-STRING

           MOVE WS-OUT-LINE TO APP-RECORD
           WRITE APP-RECORD
           CLOSE APP-FILE.


       END PROGRAM SAVE-APPLICATION.
       




       