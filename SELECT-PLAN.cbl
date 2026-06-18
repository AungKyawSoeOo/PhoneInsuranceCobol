       IDENTIFICATION DIVISION.
       PROGRAM-ID. SELECT-PLAN.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PLAN-FILE ASSIGN TO "./files/M_PLAN.CSV"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

           SELECT OPTIONAL COVERAGE-FILE ASSIGN TO
               "./files/M_PLAN_COVERAGE.CSV"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-COV-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD PLAN-FILE.
       01 PLAN-LINE                 PIC X(200).

       FD COVERAGE-FILE.
       01 COVERAGE-LINE             PIC X(200).

       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS            PIC XX.
       01 WS-COV-FILE-STATUS        PIC XX.
       01 WS-EOF                    PIC X VALUE 'N'.
       01 WS-COV-EOF                PIC X VALUE 'N'.
       01 WS-IDX                    PIC 99 VALUE ZERO.
       01 WS-IDX-DISP               PIC Z9.
       01 WS-CHOICE                 PIC 99 VALUE ZERO.
       01 WS-PLAN-COUNT             PIC 99 VALUE ZERO.
       01 WS-VALID-CHOICE           PIC X VALUE 'N'.
       01 WS-COVERAGE-FOUND         PIC X VALUE 'N'.
       01 WS-CURRENT-PLAN-CODE      PIC X(5).
       01 WS-YES-NO                 PIC X(3).

       01 WS-CSV-PLAN-CODE          PIC X(5).
       01 WS-CSV-PLAN-NAME          PIC X(20).
       01 WS-CSV-BASE-RATE-TXT      PIC X(10).
       01 WS-CSV-MAX-PAYOUT-TXT     PIC X(10).
       01 WS-CSV-ACTIVE-FLAG        PIC X(1).

       01 WS-COV-PLAN-CODE          PIC X(5).
       01 WS-COV-TYPE               PIC X(20).
       01 WS-COV-ENABLED-FLAG       PIC X(1).

       01 WS-PLAN-TABLE.
          05 WS-PLAN-ITEM OCCURS 20 TIMES.
             10 TB-PLAN-CODE        PIC X(5).
             10 TB-PLAN-NAME        PIC X(20).
             10 TB-BASE-RATE        PIC 99999.
             10 TB-MAX-PAYOUT       PIC 9(8).
       01 BASE-RATE-OUT             PIC ZZZ99.       
       01 MAX-PAYOUT-OUT            PIC ZZZZZZZ9.
       LINKAGE SECTION.
       01 LK-PLAN-CODE              PIC X(5).
       01 LK-PLAN-NAME              PIC X(20).
       01 LK-PLAN-BASE-RATE         PIC 99999.
       01 LK-PLAN-MAX-PAYOUT        PIC 9(8).
       01 LK-USER-DATA.
           05 LK-USER-NAME        PIC X(50).
           05 LK-USER-EMAIL       PIC X(50).
           05 LK-USER-PHONE       PIC X(15).
           05 LK-USER-POSTAL-CODE PIC X(7).
           05 LK-USER-ADDRESS     PIC X(100).
           05 LK-USER-DOB         PIC X(8).

       PROCEDURE DIVISION USING LK-PLAN-CODE,
                                LK-PLAN-NAME,
                                LK-PLAN-BASE-RATE,
                                LK-PLAN-MAX-PAYOUT,
                                LK-USER-DATA.

       MAIN-PROCEDURE.
           PERFORM LOAD-ACTIVE-PLANS

           IF WS-PLAN-COUNT = 0
               DISPLAY "No active insurance plans found."
               EXIT PROGRAM
           END-IF

           PERFORM SHOW-PLAN-MENU
           PERFORM GET-PLAN-CHOICE
           PERFORM SET-SELECTED-PLAN

           CALL 'USER-INFO' USING LK-USER-DATA

           EXIT PROGRAM.

       LOAD-ACTIVE-PLANS.
           MOVE 0 TO WS-PLAN-COUNT
           MOVE 'N' TO WS-EOF

           OPEN INPUT PLAN-FILE

           IF WS-FILE-STATUS NOT = '00'
               DISPLAY "Cannot open ./files/M_PLAN.CSV"
               EXIT PROGRAM
           END-IF

           PERFORM UNTIL WS-EOF = 'Y'
               READ PLAN-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       PERFORM PARSE-PLAN-LINE
                       IF WS-CSV-ACTIVE-FLAG = 'Y'
                           ADD 1 TO WS-PLAN-COUNT
                           MOVE WS-CSV-PLAN-CODE TO
                               TB-PLAN-CODE(WS-PLAN-COUNT)
                           MOVE WS-CSV-PLAN-NAME TO
                               TB-PLAN-NAME(WS-PLAN-COUNT)
                           COMPUTE TB-BASE-RATE(WS-PLAN-COUNT) =
                               FUNCTION NUMVAL(WS-CSV-BASE-RATE-TXT)
                           COMPUTE TB-MAX-PAYOUT(WS-PLAN-COUNT) =
                               FUNCTION NUMVAL(WS-CSV-MAX-PAYOUT-TXT)
                       END-IF
               END-READ
           END-PERFORM

           CLOSE PLAN-FILE.

       PARSE-PLAN-LINE.
           MOVE SPACES TO WS-CSV-PLAN-CODE
           MOVE SPACES TO WS-CSV-PLAN-NAME
           MOVE SPACES TO WS-CSV-BASE-RATE-TXT
           MOVE SPACES TO WS-CSV-MAX-PAYOUT-TXT
           MOVE SPACES TO WS-CSV-ACTIVE-FLAG

           UNSTRING PLAN-LINE DELIMITED BY ","
               INTO WS-CSV-PLAN-CODE
                    WS-CSV-PLAN-NAME
                    WS-CSV-BASE-RATE-TXT
                    WS-CSV-MAX-PAYOUT-TXT
                    WS-CSV-ACTIVE-FLAG
           END-UNSTRING.

       SHOW-PLAN-MENU.
           DISPLAY " "
           DISPLAY "========================================="
           DISPLAY "       SELECT INSURANCE PLAN             "
           DISPLAY "========================================="

           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > WS-PLAN-COUNT
               MOVE WS-IDX TO WS-IDX-DISP
               MOVE TB-BASE-RATE(WS-IDX) TO BASE-RATE-OUT
               MOVE TB-MAX-PAYOUT(WS-IDX) TO MAX-PAYOUT-OUT
               DISPLAY "[" FUNCTION TRIM(WS-IDX-DISP) "] "
                       TB-PLAN-CODE(WS-IDX) " "
                       TB-PLAN-NAME(WS-IDX)
                       " RATE: " FUNCTION TRIM(BASE-RATE-OUT)
                       " MAX: " FUNCTION TRIM(MAX-PAYOUT-OUT)
               MOVE TB-PLAN-CODE(WS-IDX) TO WS-CURRENT-PLAN-CODE
               PERFORM SHOW-COVERAGE-FOR-PLAN
           END-PERFORM

           DISPLAY "-----------------------------------------".

       SHOW-COVERAGE-FOR-PLAN.
           MOVE 'N' TO WS-COVERAGE-FOUND
           MOVE 'N' TO WS-COV-EOF

           OPEN INPUT COVERAGE-FILE

           IF WS-COV-FILE-STATUS NOT = '00'
               DISPLAY "    Coverage: not configured"
               EXIT PARAGRAPH
           END-IF

           DISPLAY "    Coverage Items:"

           PERFORM UNTIL WS-COV-EOF = 'Y'
               READ COVERAGE-FILE
                   AT END
                       MOVE 'Y' TO WS-COV-EOF
                   NOT AT END
                       PERFORM PARSE-COVERAGE-LINE
                       IF WS-COV-PLAN-CODE = WS-CURRENT-PLAN-CODE
                           MOVE 'Y' TO WS-COVERAGE-FOUND
                           IF WS-COV-ENABLED-FLAG = 'Y'
                               MOVE 'YES' TO WS-YES-NO
                           ELSE
                               MOVE 'NO' TO WS-YES-NO
                           END-IF
                           DISPLAY "      - " WS-COV-TYPE ": " WS-YES-NO
                       END-IF
               END-READ
           END-PERFORM

           CLOSE COVERAGE-FILE

           IF WS-COVERAGE-FOUND NOT = 'Y'
               DISPLAY "      - No coverage items found"
           END-IF.

       PARSE-COVERAGE-LINE.
           MOVE SPACES TO WS-COV-PLAN-CODE
           MOVE SPACES TO WS-COV-TYPE
           MOVE SPACES TO WS-COV-ENABLED-FLAG

           UNSTRING COVERAGE-LINE DELIMITED BY ","
               INTO WS-COV-PLAN-CODE
                    WS-COV-TYPE
                    WS-COV-ENABLED-FLAG
           END-UNSTRING.

       GET-PLAN-CHOICE.
           MOVE 'N' TO WS-VALID-CHOICE

           PERFORM UNTIL WS-VALID-CHOICE = 'Y'
               DISPLAY "Enter plan choice: "
               ACCEPT WS-CHOICE

               IF WS-CHOICE >= 1 AND WS-CHOICE <= WS-PLAN-COUNT
                   MOVE 'Y' TO WS-VALID-CHOICE
               ELSE
                   DISPLAY "Invalid plan choice. Please try again."
               END-IF
           END-PERFORM.

       SET-SELECTED-PLAN.
           MOVE TB-PLAN-CODE(WS-CHOICE) TO LK-PLAN-CODE
           MOVE TB-PLAN-NAME(WS-CHOICE) TO LK-PLAN-NAME
           MOVE TB-BASE-RATE(WS-CHOICE) TO LK-PLAN-BASE-RATE
           MOVE TB-MAX-PAYOUT(WS-CHOICE) TO LK-PLAN-MAX-PAYOUT.

       END PROGRAM SELECT-PLAN.

