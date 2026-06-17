       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN-PROGRAM.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-COMM-AREA.
           05 WS-CONTINUE           PIC X       VALUE 'Y'.
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
           05 WS-SYSTEM-DATE-STR    PIC X(19)   VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-ORCHESTRATOR.
           
           PERFORM UNTIL WS-CONTINUE = 'N' OR WS-CONTINUE = 'n'
               
               CALL 'QUOTATION' USING WS-COMM-AREA
               
               DISPLAY ' '
               DISPLAY 'Do you want to add another quotation? (Y/N): '
               ACCEPT WS-CONTINUE
           END-PERFORM
           
           DISPLAY ' '
           DISPLAY '========================================='
           DISPLAY '    Thank you for using our service!    '
           DISPLAY '========================================='
           STOP RUN.

       END PROGRAM MAIN-PROGRAM.
