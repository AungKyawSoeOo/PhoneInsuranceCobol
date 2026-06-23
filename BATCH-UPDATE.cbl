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

       DATA DIVISION.
       FILE SECTION.
       FD APP-FILE.
       01 APP-RECORD             PIC X(400).

       FD TEMP-FILE.
       01 TEMP-RECORD            PIC X(400).

       FD DECL-DETAIL.
       01 DETAIL-LINE            PIC X(200).

       WORKING-STORAGE SECTION.
      *> File Status
       01 WS-FILE-STATUS         PIC XX.
       01 WS-TEMP-STATUS         PIC XX.
       01 WS-DETAIL-STATUS       PIC XX.
       01 WS-EOF                 PIC X VALUE 'N'.
           88 WS-END-OF-FILE     VALUE 'Y'.

      *> Application Variables
       01 WS-CSV-APP-ID          PIC X(10).
       01 WS-CSV-USER-NAME       PIC X(50).
       01 WS-CSV-USER-EMAIL      PIC X(50).
       01 WS-CSV-USER-ADDRESS    PIC X(100).
       01 WS-CSV-DEVICE-TYPE     PIC X(10).
       01 WS-CSV-PLAN-CODE       PIC X(5).
       01 WS-CSV-STATUS          PIC X(15).
       01 WS-CSV-ACTIVE-FLAG     PIC X(1).
       01 WS-CSV-DATE            PIC X(19).

      *> Detail Variables
       01 WS-DET-APP-ID          PIC X(10).
       01 WS-DET-DAMAGE          PIC X(1).
       01 WS-DET-SCREEN          PIC X(1).
       01 WS-DET-WATER           PIC X(1).
       01 WS-DET-OLD             PIC X(1).

      *> Flag Variables 
       01 WS-DAMAGE-FLG          PIC X(1).
       01 WS-SCREEN-FLG          PIC X(1).
       01 WS-WATER-FLG           PIC X(1).
       01 WS-OLD-FLG             PIC X(1).

      *> Working Variables
       01 WS-TOTAL-SCORE         PIC 9(3) VALUE 0.
       01 WS-FINAL-STATUS        PIC X(15) VALUE SPACES.
       01 WS-NEW-LINE            PIC X(400).
       01 WS-DETAIL-EOF          PIC X VALUE 'N'.
           88 WS-END-OF-DETAIL   VALUE 'Y'.
       01 WS-RECORD-COUNT        PIC 99 VALUE 0.

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           DISPLAY "========================================="
           DISPLAY "     BATCH STATUS UPDATE PROGRAM        "
           DISPLAY "========================================="

           PERFORM PROCESS-ALL-APPLICATIONS
           DISPLAY "Batch update completed successfully."
           DISPLAY "Total records processed: " WS-RECORD-COUNT
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

               PERFORM CALCULATE-SCORE-FROM-DETAIL
               PERFORM DETERMINE-FINAL-STATUS

      *> ==========================================
      *> Plan Coverage testing
      *> ==========================================
               EVALUATE WS-CSV-PLAN-CODE
                   WHEN 'PLN-L'
                       IF WS-WATER-FLG = 'Y' OR WS-DAMAGE-FLG = 'Y'
                           MOVE 'REJECTED' TO WS-FINAL-STATUS
                       END-IF

                   WHEN 'PLN-S'
                       IF WS-DAMAGE-FLG = 'Y' OR 
                          WS-WATER-FLG = 'Y' OR
                          WS-OLD-FLG    = 'Y'
                           MOVE 'REJECTED' TO WS-FINAL-STATUS
                       END-IF

                   WHEN 'PLN-P'
                       CONTINUE

                   WHEN OTHER
                       CONTINUE

               END-EVALUATE

               MOVE WS-FINAL-STATUS TO WS-CSV-STATUS
              

               DISPLAY "Processed Application: " WS-CSV-APP-ID
                       " Score: " WS-TOTAL-SCORE
                       " Status: " WS-FINAL-STATUS
           END-IF.

           MOVE SPACES TO WS-NEW-LINE
           STRING
               FUNCTION TRIM(WS-CSV-APP-ID)     DELIMITED BY SIZE ","
               FUNCTION TRIM(WS-CSV-USER-NAME)  DELIMITED BY SIZE ","
               FUNCTION TRIM(WS-CSV-USER-EMAIL) DELIMITED BY SIZE ","
               FUNCTION TRIM(WS-CSV-USER-ADDRESS) DELIMITED BY SIZE ","
               FUNCTION TRIM(WS-CSV-DEVICE-TYPE) DELIMITED BY SIZE ","
               FUNCTION TRIM(WS-CSV-PLAN-CODE)  DELIMITED BY SIZE ","
               FUNCTION TRIM(WS-CSV-STATUS)     DELIMITED BY SIZE ","
               FUNCTION TRIM(WS-CSV-ACTIVE-FLAG) DELIMITED BY SIZE ","
               FUNCTION TRIM(WS-CSV-DATE)       DELIMITED BY SIZE
               INTO WS-NEW-LINE
           END-STRING.

           MOVE WS-NEW-LINE TO TEMP-RECORD
           WRITE TEMP-RECORD.

      *> ==========================================
      *> Calculate score from details
      *> ==========================================
       CALCULATE-SCORE-FROM-DETAIL.
           MOVE 0 TO WS-TOTAL-SCORE
           MOVE 'N' TO WS-DAMAGE-FLG
           MOVE 'N' TO WS-SCREEN-FLG
           MOVE 'N' TO WS-WATER-FLG
           MOVE 'N' TO WS-OLD-FLG
           MOVE 'N' TO WS-DETAIL-EOF

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
                    WS-DET-DAMAGE
                    WS-DET-SCREEN
                    WS-DET-WATER
                    WS-DET-OLD
           END-UNSTRING.

           IF FUNCTION TRIM(WS-DET-APP-ID) =
              FUNCTION TRIM(WS-CSV-APP-ID)
               MOVE WS-DET-DAMAGE TO WS-DAMAGE-FLG
               MOVE WS-DET-SCREEN TO WS-SCREEN-FLG
               MOVE WS-DET-WATER  TO WS-WATER-FLG
               MOVE WS-DET-OLD    TO WS-OLD-FLG

      *> Hard-coded Score 
               IF WS-DAMAGE-FLG = 'Y' ADD 50 TO WS-TOTAL-SCORE END-IF
               IF WS-SCREEN-FLG = 'Y' ADD 30 TO WS-TOTAL-SCORE END-IF
               IF WS-WATER-FLG  = 'Y' ADD 40 TO WS-TOTAL-SCORE END-IF
               IF WS-OLD-FLG    = 'Y' ADD 20 TO WS-TOTAL-SCORE END-IF
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
