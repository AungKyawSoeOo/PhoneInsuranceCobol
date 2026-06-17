       IDENTIFICATION DIVISION.
       PROGRAM-ID. PLAN-CSV-APPEND.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL PLAN-FILE ASSIGN TO "../files/M_PLAN.CSV"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT OPTIONAL COVERAGE-FILE ASSIGN TO
            "../files/M_PLAN_COVERAGE.CSV"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD PLAN-FILE.
       01 PLAN-LINE            PIC X(200).

       FD COVERAGE-FILE.
       01 COVERAGE-LINE        PIC X(200).

       WORKING-STORAGE SECTION.
       01 PLAN-CODE            PIC X(5).
       01 PLAN-NAME            PIC X(20).

       01 BASE-RATE            PIC 99999.
       01 MAX-PAYOUT           PIC 9(8).
       01 ACTIVE-FLAG          PIC X(1).

       01 COVERAGE-TYPE        PIC X(20).
       01 ENABLED-FLAG         PIC X(1).

       01 WS-NUM-INPUT         PIC X(15).

       01 WS-BASE-RATE-OUT     PIC ZZZ99.
       01 WS-MAX-PAYOUT-OUT    PIC ZZZZZZZ9.

       01 WS-END               PIC X VALUE 'Y'.
       01 WS-CHOICE            PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "STARTING PLAN ENTRY SYSTEM"

           OPEN EXTEND PLAN-FILE
           OPEN EXTEND COVERAGE-FILE

           PERFORM UNTIL WS-END = 'N' OR WS-END = 'n'

               DISPLAY "============================"
               DISPLAY "ENTER PLAN DETAILS"
               DISPLAY "============================"

               DISPLAY "PLAN CODE (e.g. PLN-L):"
               ACCEPT PLAN-CODE
               DISPLAY "PLAN NAME:"
               ACCEPT PLAN-NAME

               DISPLAY "BASE RATE (eg. 10.3):"
               ACCEPT WS-NUM-INPUT
               COMPUTE BASE-RATE = FUNCTION NUMVAL(WS-NUM-INPUT)

               DISPLAY "MAX PAYOUT (e.g. 200):"
               ACCEPT WS-NUM-INPUT
               COMPUTE MAX-PAYOUT = FUNCTION NUMVAL(WS-NUM-INPUT)

               DISPLAY "ACTIVE FLAG (Y/N):"
               ACCEPT ACTIVE-FLAG

               MOVE SPACES TO PLAN-LINE
               MOVE BASE-RATE TO WS-BASE-RATE-OUT
               MOVE MAX-PAYOUT TO WS-MAX-PAYOUT-OUT

               STRING
                   PLAN-CODE DELIMITED BY "  " ","
                   PLAN-NAME DELIMITED BY "  " ","
                   WS-BASE-RATE-OUT  DELIMITED BY SIZE ","
                   FUNCTION TRIM(WS-MAX-PAYOUT-OUT)
                   DELIMITED BY SIZE ","
                   ACTIVE-FLAG  DELIMITED BY SIZE
                   INTO PLAN-LINE
               END-STRING

               WRITE PLAN-LINE

               DISPLAY "ADD COVERAGE FOR THIS PLAN? (Y/N)"
               ACCEPT WS-CHOICE

               PERFORM UNTIL WS-CHOICE = 'N' OR WS-CHOICE = 'n'

                   IF WS-CHOICE = 'Y' OR WS-CHOICE = 'y'
                     DISPLAY "COVERAGE TYPE (Screen Damage/Water Damage"
                     DISPLAY "               /Theft/Full Failure):"
                       ACCEPT COVERAGE-TYPE
                       DISPLAY "ENABLED FLAG (Y/N):"
                       ACCEPT ENABLED-FLAG

                       MOVE SPACES TO COVERAGE-LINE

                  STRING
                  FUNCTION TRIM(PLAN-CODE)     DELIMITED BY SIZE ","
                  FUNCTION TRIM(COVERAGE-TYPE) DELIMITED BY SIZE ","
                  ENABLED-FLAG                 DELIMITED BY SIZE
                  INTO COVERAGE-LINE
                  END-STRING

                       WRITE COVERAGE-LINE
                   END-IF

                   DISPLAY "ADD ANOTHER COVERAGE? (Y/N)"
                   ACCEPT WS-CHOICE
               END-PERFORM

               DISPLAY "ADD ANOTHER PLAN? (Y/N)"
               ACCEPT WS-END
           END-PERFORM

           CLOSE PLAN-FILE
           CLOSE COVERAGE-FILE

           DISPLAY "CSV FILES UPDATED SUCCESSFULLY"
           STOP RUN.
