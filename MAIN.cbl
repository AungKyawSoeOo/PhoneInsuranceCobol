       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN-PROGRAM.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-COMM-AREA.

           05 WS-DEVICE-DATA.
              10 WS-IMEI            PIC X(15)   VALUE SPACES.
              10 WS-DEVICE-TYPE     PIC X(10)   VALUE SPACES.
              10 WS-DEVICE-MODEL    PIC X(20)   VALUE SPACES.
              10 WS-PURCHASE-DATE   PIC X(10)   VALUE SPACES.
              10 WS-PRICE           PIC 9(6)    VALUE ZERO.
           05 WS-CALC-RESULTS.
              10 WS-PRICE-CATEGORY  PIC X(6)    VALUE SPACES.
              10 WS-PERIOD          PIC X(3)    VALUE SPACES.
              10 WS-PERIOD-FACTOR   PIC 9V99    VALUE ZERO.
              10 WS-PERIOD-MONTHS   PIC 99      VALUE ZERO.
              10 WS-EST-PREMIUM     PIC 9(9)V99 VALUE ZERO.
           05 WS-PLAN-DATA.
              10 WS-PLAN-CODE       PIC X(5)    VALUE SPACES.
              10 WS-PLAN-NAME       PIC X(20)   VALUE SPACES.
              10 WS-PLAN-BASE-RATE  PIC 99999   VALUE ZERO.
              10 WS-PLAN-MAX-PAYOUT PIC 9(8)    VALUE ZERO.
              10 WS-CURRENT-PREMIUM PIC 9(9)V99 VALUE ZERO.
              10 WS-FINAL-PREMIUM   PIC 9(9)V99 VALUE ZERO.
           05 WS-SYSTEM-DATE-STR    PIC X(19)   VALUE SPACES.
           05 WS-USER-DATA.
              10 WS-USER-NAME        PIC X(50)   VALUE SPACES.
              10 WS-USER-EMAIL       PIC X(50)   VALUE SPACES.
              10 WS-USER-PHONE       PIC X(15)   VALUE SPACES.
              10 WS-USER-POSTAL-CODE PIC X(7)    VALUE SPACES.
              10 WS-USER-ADDRESS     PIC X(100)  VALUE SPACES.
              10 WS-USER-DOB         PIC X(8)    VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-ORCHESTRATOR.
               CALL 'QUOTATION' USING WS-COMM-AREA
      *    Delete display after project finished
               DISPLAY "User Name from USER-INFO: " WS-USER-NAME
               DISPLAY "PRICE FROM MAIN" WS-PRICE
               DISPLAY "purchase date FROM MAIN" WS-PURCHASE-DATE
               DISPLAY "DEVICE TYPE"WS-DEVICE-TYPE.
           STOP RUN.

       END PROGRAM MAIN-PROGRAM.


