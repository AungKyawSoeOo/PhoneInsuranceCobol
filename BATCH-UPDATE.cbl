       IDENTIFICATION DIVISION.
       PROGRAM-ID. BATCH-UPDATE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL APP-FILE ASSIGN TO
               "./files/T_APPLICATION.CSV"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

           SELECT OPTIONAL TEMP-FILE ASSIGN TO
               "./files/TEMP_APPLICATION.CSV"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-TEMP-STATUS.

           SELECT OPTIONAL DECL-DETAIL ASSIGN TO
               "./files/T_APPLICATION_DECL.CSV"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-DETAIL-STATUS.

           SELECT OPTIONAL DECL-MASTER ASSIGN TO
               "./files/M_DECLARATION.CSV"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-MASTER-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD APP-FILE.
       01 APP-RECORD             PIC X(400).

       FD TEMP-FILE.
       01 TEMP-RECORD            PIC X(400).

       FD DECL-DETAIL.
       01 DETAIL-LINE            PIC X(200).

       FD DECL-MASTER.
       01 MASTER-LINE            PIC X(100).

       WORKING-STORAGE SECTION.
      *> File Status
       01 WS-FILE-STATUS         PIC XX.
       01 WS-TEMP-STATUS         PIC XX.
       01 WS-DETAIL-STATUS       PIC XX.
       01 WS-MASTER-STATUS       PIC XX.
       01 WS-EOF                 PIC X VALUE 'N'.
           88 WS-END-OF-FILE     VALUE 'Y'.

      *> Application Variables
       01 WS-CSV-APP-ID          PIC X(10).
       01 WS-CSV-USER-NAME       PIC X(50).
       01 WS-CSV-USER-EMAIL      PIC X(50).
       01 WS-CSV-USER-ADDRESS    PIC X(100).
       01 WS-CSV-DEVICE-TYPE     PIC X(10).
       01 WS-CSV-DEVICE-MODEL    PIC X(20).
       01 WS-CSV-IMEI-NUMBER     PIC X(15).
       01 WS-CSV-PREMIUM-PRICE   PIC 9(6).
       01 WS-CSV-PLAN-CODE       PIC X(5).
       01 WS-CSV-STATUS          PIC X(15).
       01 WS-CSV-ACTIVE-FLAG     PIC X(1).
       01 WS-CSV-DATE            PIC X(19).

      *> Detail Variables
       01 WS-DET-APP-ID          PIC X(10).
       01 WS-ANSWERS-TABLE.
           05 WS-ANS OCCURS 10 TIMES PIC X(1) VALUE 'N'.

       01 WS-MST-CODE            PIC X(5).
       01 WS-MST-QUESTION        PIC X(100).
       01 WS-MST-WEIGHT          PIC 99.
       01 WS-QUESTION-TABLE.
           05 WS-QUE-ENTRY OCCURS 10 TIMES.
               10 WS-Q-CODE      PIC X(5).
               10 WS-Q-TEXT      PIC X(100).
               10 WS-Q-WEIGHT    PIC 99.
       01 WS-TOTAL-QUESTIONS     PIC 99 VALUE 0.
       01 WS-IDX                 PIC 99.
       01 WS-Q-WEIGHT-ALPHA      PIC X(5).

      *> Working Variables
       01 WS-TOTAL-SCORE         PIC 9(3) VALUE 0.
       01 WS-FINAL-STATUS        PIC X(15) VALUE SPACES.
       01 WS-NEW-LINE            PIC X(400).
       01 WS-DETAIL-EOF          PIC X VALUE 'N'.
           88 WS-END-OF-DETAIL   VALUE 'Y'.
       01 WS-RECORD-COUNT        PIC 99 VALUE 0.
       
       01 WS-PROCESS-COUNT       PIC 99 VALUE 0.
       01 WS-DISP-SCORE          PIC Z(3)9. 
             
           
       01 WS-DETAIL-FOUND        PIC X VALUE 'N'.
           88 WS-FOUND-DETAIL    VALUE 'Y'.
       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           DISPLAY "========================================="
           DISPLAY "     BATCH STATUS UPDATE PROGRAM        "
           DISPLAY "========================================="

           PERFORM LOAD-MASTER-QUESTIONS
           PERFORM PROCESS-ALL-APPLICATIONS
           DISPLAY "Batch update completed successfully."
           DISPLAY "Total records processed: " WS-PROCESS-COUNT
           EXIT PROGRAM.

      
       PROCESS-ALL-APPLICATIONS.
           OPEN INPUT APP-FILE
           IF WS-FILE-STATUS NOT = '00'
               DISPLAY "ERROR: Cannot open T_APPLICATION.CSV"
               EXIT PROGRAM
           END-IF

           OPEN OUTPUT TEMP-FILE

           MOVE 0 TO WS-RECORD-COUNT
           MOVE 'N' TO WS-EOF

           PERFORM UNTIL WS-END-OF-FILE
               READ APP-FILE INTO APP-RECORD
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                       ADD 1 TO WS-RECORD-COUNT
                       PERFORM PARSE-AND-PROCESS-RECORD
               END-READ
           END-PERFORM

           CLOSE APP-FILE
           CLOSE TEMP-FILE

      *> Deleting old file and creating new Temp file 
           OPEN INPUT TEMP-FILE
           IF WS-TEMP-STATUS NOT = '00'
               DISPLAY "ERROR: Cannot open TEMP_APPLICATION.CSV"
               EXIT PROGRAM
           END-IF

           OPEN OUTPUT APP-FILE
           IF WS-FILE-STATUS NOT = '00'
               DISPLAY "ERROR: Cannot open T_APPLICATION.CSV for write"
               EXIT PROGRAM
           END-IF

           MOVE 'N' TO WS-EOF
           PERFORM UNTIL WS-END-OF-FILE
               READ TEMP-FILE INTO TEMP-RECORD
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                       MOVE TEMP-RECORD TO APP-RECORD
                       WRITE APP-RECORD
               END-READ
           END-PERFORM

           CLOSE TEMP-FILE
           CLOSE APP-FILE.
       PARSE-AND-PROCESS-RECORD.
           UNSTRING APP-RECORD DELIMITED BY ","
               INTO WS-CSV-APP-ID
                    WS-CSV-USER-NAME
                    WS-CSV-USER-EMAIL
                    WS-CSV-USER-ADDRESS
                    WS-CSV-DEVICE-TYPE
                    WS-CSV-DEVICE-MODEL
                    WS-CSV-IMEI-NUMBER
                    WS-CSV-PREMIUM-PRICE
                    WS-CSV-PLAN-CODE
                    WS-CSV-STATUS
                    WS-CSV-ACTIVE-FLAG
                    WS-CSV-DATE
           END-UNSTRING.

      *> ==========================================
      *> ACTIVE_FLAG = 'Y' and STATUS = 'PENDING' check
      *> ==========================================
           IF WS-CSV-ACTIVE-FLAG = 'Y' AND
              WS-CSV-STATUS = 'PENDING'

               ADD 1 TO WS-PROCESS-COUNT
               PERFORM CALCULATE-SCORE-FROM-DETAIL
               PERFORM DETERMINE-FINAL-STATUS

               MOVE WS-FINAL-STATUS TO WS-CSV-STATUS
               MOVE WS-TOTAL-SCORE TO WS-DISP-SCORE

               DISPLAY "Processed Application: " WS-CSV-APP-ID
                       " Score: " FUNCTION TRIM(WS-DISP-SCORE)
                       " Status: " WS-FINAL-STATUS
           END-IF.

           MOVE SPACES TO WS-NEW-LINE
           STRING
               FUNCTION TRIM(WS-CSV-APP-ID)     DELIMITED BY SIZE ","
               FUNCTION TRIM(WS-CSV-USER-NAME)  DELIMITED BY SIZE ","
               FUNCTION TRIM(WS-CSV-USER-EMAIL) DELIMITED BY SIZE ","
               FUNCTION TRIM(WS-CSV-USER-ADDRESS) DELIMITED BY SIZE ","
               FUNCTION TRIM(WS-CSV-DEVICE-TYPE) DELIMITED BY SIZE ","
               FUNCTION TRIM(WS-CSV-DEVICE-MODEL) DELIMITED BY SIZE ","
               FUNCTION TRIM(WS-CSV-IMEI-NUMBER) DELIMITED BY SIZE ","
               FUNCTION TRIM(WS-CSV-PREMIUM-PRICE) DELIMITED BY SIZE ","
               FUNCTION TRIM(WS-CSV-PLAN-CODE)  DELIMITED BY SIZE ","
               FUNCTION TRIM(WS-CSV-STATUS)     DELIMITED BY SIZE ","
               FUNCTION TRIM(WS-CSV-ACTIVE-FLAG) DELIMITED BY SIZE ","
               FUNCTION TRIM(WS-CSV-DATE)       DELIMITED BY SIZE
               INTO WS-NEW-LINE
           END-STRING.

           MOVE WS-NEW-LINE TO TEMP-RECORD
           WRITE TEMP-RECORD.

       LOAD-MASTER-QUESTIONS.
           OPEN INPUT DECL-MASTER
           MOVE 0 TO WS-TOTAL-QUESTIONS
           PERFORM UNTIL WS-END-OF-FILE
               READ DECL-MASTER INTO MASTER-LINE
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                       ADD 1 TO WS-TOTAL-QUESTIONS
                       UNSTRING MASTER-LINE DELIMITED BY ","
                           INTO WS-Q-CODE(WS-TOTAL-QUESTIONS)
                                WS-Q-TEXT(WS-TOTAL-QUESTIONS)
                                WS-Q-WEIGHT-ALPHA
                       END-UNSTRING
                       COMPUTE WS-Q-WEIGHT(WS-TOTAL-QUESTIONS) = 
                           FUNCTION NUMVAL(
                             FUNCTION TRIM(WS-Q-WEIGHT-ALPHA)
                           )
               END-READ
           END-PERFORM
           CLOSE DECL-MASTER
           MOVE 'N' TO WS-EOF.

      *> ==========================================
      *> Calculate score from details
      *> ==========================================
       CALCULATE-SCORE-FROM-DETAIL.
           MOVE 0 TO WS-TOTAL-SCORE
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 10
               MOVE 'N' TO WS-ANS(WS-IDX)
           END-PERFORM
           MOVE 'N' TO WS-DETAIL-EOF
           MOVE 'N' TO WS-DETAIL-FOUND  

           OPEN INPUT DECL-DETAIL
           IF WS-DETAIL-STATUS NOT = '00'
               CLOSE DECL-DETAIL
               EXIT PARAGRAPH
           END-IF

           PERFORM UNTIL WS-END-OF-DETAIL
               READ DECL-DETAIL INTO DETAIL-LINE
                   AT END MOVE 'Y' TO WS-DETAIL-EOF
                   NOT AT END
                       PERFORM PARSE-DETAIL-LINE
               END-READ
           END-PERFORM

           CLOSE DECL-DETAIL.
       PARSE-DETAIL-LINE.
           UNSTRING DETAIL-LINE DELIMITED BY ","
               INTO WS-DET-APP-ID
                    WS-ANS(1)
                    WS-ANS(2)
                    WS-ANS(3)
                    WS-ANS(4)
                    WS-ANS(5)
                    WS-ANS(6)
                    WS-ANS(7)
                    WS-ANS(8)
                    WS-ANS(9)
                    WS-ANS(10)
           END-UNSTRING.

            IF FUNCTION TRIM(WS-DET-APP-ID) =
               FUNCTION TRIM(WS-CSV-APP-ID)
                PERFORM VARYING WS-IDX FROM 1 BY 1 
                        UNTIL WS-IDX > WS-TOTAL-QUESTIONS
                    IF WS-ANS(WS-IDX) = 'Y'
                        ADD WS-Q-WEIGHT(WS-IDX) TO WS-TOTAL-SCORE
                    END-IF
                END-PERFORM
                MOVE 'Y' TO WS-DETAIL-FOUND
            END-IF.


      *> ==========================================
      *> Assigning Final Status based on scores
      *> ==========================================
       DETERMINE-FINAL-STATUS.
           EVALUATE TRUE
               WHEN WS-TOTAL-SCORE = 0
                   MOVE 'APPROVED' TO WS-FINAL-STATUS
               WHEN WS-TOTAL-SCORE <= 30
                   MOVE 'APPROVED' TO WS-FINAL-STATUS
               WHEN WS-TOTAL-SCORE <= 70
                   MOVE 'CONDITIONAL' TO WS-FINAL-STATUS
               WHEN WS-TOTAL-SCORE > 70
                   MOVE 'REJECTED' TO WS-FINAL-STATUS
           END-EVALUATE.

       END PROGRAM BATCH-UPDATE.
