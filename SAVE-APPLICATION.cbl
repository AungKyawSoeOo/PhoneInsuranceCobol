       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAVE-APPLICATION.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
          
           SELECT OPTIONAL APP-FILE ASSIGN TO 
           "./files/T_APPLICATION.CSV"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

           SELECT TEMP-APP-FILE ASSIGN TO "./files/T_APPLICATION.TMP"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-TEMP-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD APP-FILE.
       01 APP-RECORD               PIC X(400).

       FD  TEMP-APP-FILE.
       01  TEMP-APP-RECORD         PIC X(400).

       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS           PIC XX.
       01 WS-OUT-LINE              PIC X(400).
       01 WS-STATUS-PENDING        PIC X(10) VALUE "PENDING".
       
       01 WS-TEMP-RECORD           PIC X(400).
       01 WS-LAST-ID               PIC 9(10) VALUE ZERO.
       01 WS-NEXT-ID               PIC 9(10) VALUE 1.
       01 WS-UNSTRING-ID           PIC X(10).
       01 WS-APP-ACTIVATE-FLAG     PIC X(1).
       
       01 WS-DUPLICATE-FLAG        PIC X VALUE 'N'.
       01 WS-FILE-IMEI             PIC X(15).
       01 WS-FILE-PLAN             PIC X(5).
       01  WS-FILE-STATUS-VAL      PIC X(10).
       01 WS-FILE-DUMMY            PIC X(100).

       01 WS-CHG-CHOICE            PIC X VALUE SPACES.
       01 WS-OLD-RECORD-TO-UPD     PIC X(400) VALUE SPACES.
       01 WS-MATCH-FOUND-FLAG      PIC X VALUE 'N'.
       
       *> Temporary file tracking items for Sequential Rewrite emulation
       01 WS-TEMP-FILE-STATUS      PIC XX.


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
           EVALUATE WS-DUPLICATE-FLAG        
               WHEN 'E'
                   MOVE "DUP" TO LK-CONTINUE
               WHEN 'P'
                   MOVE "BLK" TO LK-CONTINUE
                   DISPLAY ' '
                   DISPLAY 
                   'Plan change is not allowed because the existing'
                   DISPLAY 'application is still in PENDING status.'
               WHEN 'C'
                   PERFORM HANDLE-PLAN-CHANGE-PROMPT
               WHEN 'N'
                   PERFORM WRITE-APPLICATION-RECORD
           END-EVALUATE
           EXIT PROGRAM.

       DETERMINE-NEXT-APP-ID.
           MOVE 0 TO WS-LAST-ID
           MOVE 'N' TO WS-DUPLICATE-FLAG
           OPEN INPUT APP-FILE
           IF WS-FILE-STATUS = '00'
               PERFORM UNTIL WS-FILE-STATUS = '10'
                   READ APP-FILE INTO WS-TEMP-RECORD
                       AT END
                           MOVE '10' TO WS-FILE-STATUS
                       NOT AT END
                           IF WS-TEMP-RECORD NOT = SPACES
                               UNSTRING WS-TEMP-RECORD DELIMITED BY ","
                                   INTO WS-UNSTRING-ID
                                        WS-FILE-DUMMY   *> USER_NAME
                                        WS-FILE-DUMMY   *> EMAIL
                                        WS-FILE-DUMMY   *> ADDRESS
                                        WS-FILE-DUMMY   *> DEVICE_TYPE
                                        WS-FILE-DUMMY   *> DEVICE_MODEL
                                        WS-FILE-IMEI    *> IMEI_NUMBER
                                        WS-FILE-DUMMY   *> PREMIUM_PRICE
                                        WS-FILE-PLAN    *> PLAN_CODE
                                        WS-FILE-STATUS-VAL   *> STATUS
                                        WS-APP-ACTIVATE-FLAG
                                        WS-FILE-DUMMY   *> CREATED_AT
                               
                               COMPUTE WS-LAST-ID = 
                                   FUNCTION NUMVAL(WS-UNSTRING-ID)
                               
            *> Check for any Active records matching current IMEI
                               IF FUNCTION TRIM(WS-FILE-IMEI) = 
                                  FUNCTION TRIM(LK-IMEI) AND
                                  WS-APP-ACTIVATE-FLAG = 'Y'
                                   
                                   IF FUNCTION TRIM(WS-FILE-PLAN) = 
                                      FUNCTION TRIM(LK-PLAN-CODE)
                                       MOVE 'E' TO WS-DUPLICATE-FLAG 
                                   ELSE
                                       IF FUNCTION 
                                   TRIM(WS-FILE-STATUS-VAL) = "PENDING"
                                           MOVE 'P' TO WS-DUPLICATE-FLAG
                                       ELSE
                                        MOVE 'C' TO WS-DUPLICATE-FLAG
                                        MOVE WS-TEMP-RECORD
                                          TO WS-OLD-RECORD-TO-UPD
                                    END-IF
                               END-IF
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE APP-FILE
               COMPUTE WS-NEXT-ID = WS-LAST-ID + 1
           ELSE
               CLOSE APP-FILE
               MOVE 1 TO WS-NEXT-ID
           END-IF.

           IF WS-DUPLICATE-FLAG = 'N'
               MOVE WS-NEXT-ID TO LK-CONTINUE
           END-IF.

           MOVE WS-NEXT-ID TO LK-CONTINUE.

       WRITE-APPLICATION-RECORD.
           OPEN EXTEND APP-FILE
           IF WS-FILE-STATUS NOT = '00' AND WS-FILE-STATUS NOT = '02'
               CLOSE APP-FILE
               OPEN OUTPUT APP-FILE
           END-IF.

           MOVE SPACES TO WS-OUT-LINE
           MOVE 'Y' TO WS-APP-ACTIVATE-FLAG
           
           STRING 
               FUNCTION TRIM(LK-CONTINUE)          DELIMITED BY SIZE ","
               FUNCTION TRIM(LK-USER-NAME)         DELIMITED BY SIZE ","
               FUNCTION TRIM(LK-USER-EMAIL)        DELIMITED BY SIZE ","
               FUNCTION TRIM(LK-USER-ADDRESS)      DELIMITED BY SIZE ","
               FUNCTION TRIM(LK-DEVICE-TYPE)       DELIMITED BY SIZE ","
               FUNCTION TRIM(LK-DEVICE-MODEL)      DELIMITED BY SIZE ","
               FUNCTION TRIM(LK-IMEI)              DELIMITED BY SIZE ","
               FUNCTION TRIM(LK-PRICE)             DELIMITED BY SIZE ","
               FUNCTION TRIM(LK-PLAN-CODE)         DELIMITED BY SIZE ","
               FUNCTION TRIM(WS-STATUS-PENDING)    DELIMITED BY SIZE ","
               WS-APP-ACTIVATE-FLAG                DELIMITED BY SIZE ","
               FUNCTION TRIM(LK-SYSTEM-DATE-STR)   DELIMITED BY SIZE
               INTO WS-OUT-LINE
           END-STRING

           MOVE WS-OUT-LINE TO APP-RECORD
           WRITE APP-RECORD
           CLOSE APP-FILE.

           HANDLE-PLAN-CHANGE-PROMPT.
           DISPLAY ' '
           DISPLAY 'This IMEI already has an active application with'
           DISPLAY 
           'another plan. Do you want to change to the new plan? (Y/N)'
           ACCEPT WS-CHG-CHOICE.
           
           IF WS-CHG-CHOICE = 'Y' OR WS-CHG-CHOICE = 'y'
               PERFORM DEACTIVATE-OLD-RECORD
               PERFORM WRITE-APPLICATION-RECORD
           ELSE
               MOVE "REJ" TO LK-CONTINUE
           END-IF.

       DEACTIVATE-OLD-RECORD.
           OPEN INPUT APP-FILE
           OPEN OUTPUT TEMP-APP-FILE
           
           MOVE '00' TO WS-FILE-STATUS
           PERFORM UNTIL WS-FILE-STATUS = '10'
               READ APP-FILE INTO WS-TEMP-RECORD
                   AT END
                       MOVE '10' TO WS-FILE-STATUS
                   NOT AT END
                       IF WS-TEMP-RECORD = WS-OLD-RECORD-TO-UPD
                           PERFORM FLIP-FLAG-STRING
                           WRITE TEMP-APP-RECORD FROM WS-TEMP-RECORD
                       ELSE
                           WRITE TEMP-APP-RECORD FROM WS-TEMP-RECORD
                       END-IF
               END-READ
           END-PERFORM
           
           CLOSE APP-FILE
           CLOSE TEMP-APP-FILE
           
           OPEN INPUT TEMP-APP-FILE
           OPEN OUTPUT APP-FILE
           MOVE '00' TO WS-TEMP-FILE-STATUS
           PERFORM UNTIL WS-TEMP-FILE-STATUS = '10'
               READ TEMP-APP-FILE INTO WS-TEMP-RECORD
                   AT END
                       MOVE '10' TO WS-TEMP-FILE-STATUS
                   NOT AT END
                       WRITE APP-RECORD FROM WS-TEMP-RECORD
               END-READ
           END-PERFORM
           CLOSE TEMP-APP-FILE
           CLOSE APP-FILE.

       FLIP-FLAG-STRING.
           INSPECT WS-TEMP-RECORD REPLACING FIRST ",Y," BY ",N,".


       END PROGRAM SAVE-APPLICATION.




       