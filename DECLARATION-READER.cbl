       IDENTIFICATION DIVISION.
       PROGRAM-ID. DECLARATION-READER.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL DECL-MASTER ASSIGN TO
               "./files/M_DECLARATION.CSV"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-MASTER-STATUS.

           SELECT OPTIONAL DECL-DETAIL ASSIGN TO
               "./files/T_APPLICATION_DECL.CSV"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-DETAIL-STATUS.

           SELECT OPTIONAL APP-FILE ASSIGN TO
               "./files/T_APPLICATION.CSV"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

           SELECT OPTIONAL TEMP-FILE ASSIGN TO
               "./files/TEMP_APPLICATION.CSV"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-TEMP-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD DECL-MASTER.
       01 MASTER-LINE            PIC X(100).

       FD DECL-DETAIL.
       01 DETAIL-LINE            PIC X(200).

       FD APP-FILE.
       01 APP-RECORD             PIC X(400).

       FD TEMP-FILE.
       01 TEMP-RECORD            PIC X(400).

       WORKING-STORAGE SECTION.
      *> File Status
       01 WS-MASTER-STATUS       PIC XX.
       01 WS-DETAIL-STATUS       PIC XX.
       01 WS-FILE-STATUS         PIC XX.
       01 WS-TEMP-STATUS         PIC XX.
       01 WS-EOF                 PIC X VALUE 'N'.
           88 WS-END-OF-FILE     VALUE 'Y'.

      *> Master Data
       01 WS-MST-CODE            PIC X(5).
       01 WS-MST-QUESTION        PIC X(100).
       01 WS-MST-WEIGHT          PIC 99.

      *> Question Table (Max 10)
       01 WS-QUESTION-TABLE.
           05 WS-QUE-ENTRY OCCURS 10 TIMES.
               10 WS-Q-CODE      PIC X(5).
               10 WS-Q-TEXT      PIC X(100).
               10 WS-Q-WEIGHT    PIC 99.

      *> Working Variables
       01 WS-IDX                 PIC 99.
       01 WS-ANSWER-CHAR         PIC X(1).
       01 WS-ANSWER-YN           PIC X(1).
       01 WS-TOTAL-SCORE         PIC 9(3) VALUE 0.
       01 WS-FINAL-STATUS        PIC X(15) VALUE SPACES.
       01 WS-VALID               PIC X VALUE 'N'.
       01 WS-DAMAGE-FLG          PIC X(1) VALUE 'N'.
       01 WS-SCREEN-FLG          PIC X(1) VALUE 'N'.
       01 WS-WATER-FLG           PIC X(1) VALUE 'N'.
       01 WS-OLD-FLG             PIC X(1) VALUE 'N'.

      *> CSV Update Variables
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
       01 WS-NEW-LINE            PIC X(400).
       01 WS-CUR-PLAN-CODE       PIC X(5).

      *> Detail Variables (CALCULATE-SCORE-FROM-DETAI)
       01 WS-DET-APP-ID          PIC X(10).
       01 WS-DET-DAMAGE          PIC X(1).
       01 WS-DET-SCREEN          PIC X(1).
       01 WS-DET-WATER           PIC X(1).
       01 WS-DET-OLD             PIC X(1).

      *> Detail EOF
       01 WS-DETAIL-EOF          PIC X VALUE 'N'.
           88 WS-END-OF-DETAIL   VALUE 'Y'.

      *> ==========================================
      *> LINKAGE SECTION
      *> ==========================================
       LINKAGE SECTION.
       01 LK-APP-ID              PIC 9(10).
       01 LK-FINAL-STATUS        PIC X(15).
       01 LK-TOTAL-SCORE         PIC 9(3).

       PROCEDURE DIVISION USING LK-APP-ID
                                LK-FINAL-STATUS
                                LK-TOTAL-SCORE.

           MAIN-PROCEDURE.
           PERFORM LOAD-MASTER-QUESTIONS
           PERFORM ASK-QUESTIONS-AND-CALC
           PERFORM DETERMINE-FINAL-STATUS      
           PERFORM UPDATE-APPLICATION-STATUS
           MOVE WS-FINAL-STATUS TO LK-FINAL-STATUS
           MOVE WS-TOTAL-SCORE TO LK-TOTAL-SCORE
           EXIT PROGRAM.

       LOAD-MASTER-QUESTIONS.
           MOVE 0 TO WS-IDX
           MOVE 'N' TO WS-EOF

           OPEN INPUT DECL-MASTER
           IF WS-MASTER-STATUS NOT = '00'
               DISPLAY "ERROR: Cannot open M_DECLARATION.CSV"
               EXIT PROGRAM
           END-IF

           PERFORM UNTIL WS-END-OF-FILE
               READ DECL-MASTER INTO MASTER-LINE
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                       PERFORM PARSE-MASTER-LINE
               END-READ
           END-PERFORM

           CLOSE DECL-MASTER.

       PARSE-MASTER-LINE.
           UNSTRING MASTER-LINE DELIMITED BY ","
               INTO WS-MST-CODE
                    WS-MST-QUESTION
                    WS-MST-WEIGHT
           END-UNSTRING.

           ADD 1 TO WS-IDX
           MOVE WS-MST-CODE   TO WS-Q-CODE(WS-IDX)
           MOVE WS-MST-QUESTION TO WS-Q-TEXT(WS-IDX)
           MOVE WS-MST-WEIGHT TO WS-Q-WEIGHT(WS-IDX).

            ASK-QUESTIONS-AND-CALC.
           MOVE 0 TO WS-TOTAL-SCORE
           MOVE SPACES TO WS-FINAL-STATUS
           MOVE 'N' TO WS-DAMAGE-FLG
           MOVE 'N' TO WS-SCREEN-FLG
           MOVE 'N' TO WS-WATER-FLG
           MOVE 'N' TO WS-OLD-FLG

           DISPLAY " "
           DISPLAY "========================================="
           DISPLAY "      DECLARATION QUESTIONS              "
           DISPLAY "========================================="
           DISPLAY "Please answer the following questions:"
           DISPLAY " "

           PERFORM VARYING WS-IDX FROM 1 BY 1
                     UNTIL WS-IDX > 4

               MOVE 'N' TO WS-VALID
               PERFORM UNTIL WS-VALID = 'Y'
                   DISPLAY WS-Q-TEXT(WS-IDX) " (Y/N): "
                   ACCEPT WS-ANSWER-CHAR

                       IF WS-ANSWER-CHAR = 'Y' OR 'y'
                       MOVE 'Y' TO WS-ANSWER-YN
                       MOVE 'Y' TO WS-VALID
                       ADD WS-Q-WEIGHT(WS-IDX) TO WS-TOTAL-SCORE
                       EVALUATE WS-IDX
                           WHEN 1 MOVE 'Y' TO WS-DAMAGE-FLG
                           WHEN 2 MOVE 'Y' TO WS-SCREEN-FLG
                           WHEN 3 MOVE 'Y' TO WS-WATER-FLG
                           WHEN 4 MOVE 'Y' TO WS-OLD-FLG
                       END-EVALUATE
                   ELSE
                       IF WS-ANSWER-CHAR = 'N' OR 'n'
                           MOVE 'N' TO WS-ANSWER-YN
                           MOVE 'Y' TO WS-VALID
                       ELSE
                           DISPLAY "Invalid input! Please enter Y or N."
                       END-IF
                   END-IF
               END-PERFORM

               
               MOVE 'N' TO WS-VALID
           END-PERFORM.
           PERFORM WRITE-DECLARATION-DETAIL.
      *> ==========================================
      *> DETAIL CSV (T_APPLICATION_DECL) 
      *> ==========================================
              WRITE-DECLARATION-DETAIL.
           OPEN EXTEND DECL-DETAIL

           IF WS-DETAIL-STATUS NOT = '00' AND
              WS-DETAIL-STATUS NOT = '02'
               CLOSE DECL-DETAIL
               OPEN OUTPUT DECL-DETAIL
           END-IF

           MOVE SPACES TO DETAIL-LINE

           STRING
               FUNCTION TRIM(LK-APP-ID) DELIMITED BY SIZE ","
               WS-DAMAGE-FLG DELIMITED BY SIZE ","
               WS-SCREEN-FLG DELIMITED BY SIZE ","
               WS-WATER-FLG  DELIMITED BY SIZE ","
               WS-OLD-FLG    DELIMITED BY SIZE
               INTO DETAIL-LINE
           END-STRING

           WRITE DETAIL-LINE
           CLOSE DECL-DETAIL.

      
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

               IF WS-DAMAGE-FLG = 'Y' ADD 50 TO WS-TOTAL-SCORE END-IF
               IF WS-SCREEN-FLG = 'Y' ADD 30 TO WS-TOTAL-SCORE END-IF
               IF WS-WATER-FLG  = 'Y' ADD 40 TO WS-TOTAL-SCORE END-IF
               IF WS-OLD-FLG    = 'Y' ADD 20 TO WS-TOTAL-SCORE END-IF
           END-IF.

      
       DETERMINE-FINAL-STATUS.
           EVALUATE TRUE
               WHEN WS-TOTAL-SCORE = 0
                   MOVE 'APPROVED' TO WS-FINAL-STATUS
               WHEN WS-TOTAL-SCORE = 100
                   MOVE 'REJECTED' TO WS-FINAL-STATUS
               WHEN WS-TOTAL-SCORE <= 30
                   MOVE 'APPROVED' TO WS-FINAL-STATUS
               WHEN WS-TOTAL-SCORE <= 70
                   MOVE 'CONDITIONAL' TO WS-FINAL-STATUS
               WHEN WS-TOTAL-SCORE > 70
                   MOVE 'REJECTED' TO WS-FINAL-STATUS
           END-EVALUATE.

      
       UPDATE-APPLICATION-STATUS.
           OPEN INPUT APP-FILE

           IF WS-FILE-STATUS NOT = '00'
               DISPLAY "ERROR: Cannot open T_APPLICATION.CSV"
               EXIT PARAGRAPH
           END-IF

           OPEN OUTPUT TEMP-FILE

           MOVE 'N' TO WS-EOF

           PERFORM UNTIL WS-END-OF-FILE
               READ APP-FILE INTO APP-RECORD
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                       PERFORM PARSE-AND-UPDATE-RECORD
               END-READ
           END-PERFORM

           CLOSE APP-FILE
           CLOSE TEMP-FILE

           OPEN INPUT TEMP-FILE
           IF WS-TEMP-STATUS NOT = '00'
               DISPLAY "ERROR: Cannot open TEMP_APPLICATION.CSV"
               EXIT PARAGRAPH
           END-IF

           OPEN OUTPUT APP-FILE
           IF WS-FILE-STATUS NOT = '00'
               DISPLAY "ERROR: Cannot open T_APPLICATION.CSV for write"
               EXIT PARAGRAPH
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

       PARSE-AND-UPDATE-RECORD.
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

           MOVE WS-CSV-PLAN-CODE TO WS-CUR-PLAN-CODE.
                          

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

       END PROGRAM DECLARATION-READER.
