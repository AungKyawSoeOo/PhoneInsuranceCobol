       IDENTIFICATION DIVISION.
       PROGRAM-ID. SEARCH-STATUS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT APP-FILE ASSIGN TO
               "./files/T_APPLICATION.CSV"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD APP-FILE.
       01 APP-RECORD             PIC X(400).

       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS         PIC XX.
       01 WS-EOF                 PIC X VALUE 'N'.
           88 WS-END-OF-FILE     VALUE 'Y'.

       01 WS-IMEI-INPUT          PIC X(15).
       01 WS-IMEI-LEN            PIC 99.
       01 WS-FOUND               PIC X VALUE 'N'.
           88 WS-FOUND-YES       VALUE 'Y'.
           88 WS-FOUND-NO        VALUE 'N'.

      *> Application Record Fields
       01 WS-APP-ID              PIC X(10).
       01 WS-USER-NAME           PIC X(50).
       01 WS-USER-EMAIL          PIC X(50).
       01 WS-USER-ADDRESS        PIC X(100).
       01 WS-DEVICE-TYPE         PIC X(10).
       01 WS-DEVICE-MODEL        PIC X(20).
       01 WS-IMEI                PIC X(15).
       01 WS-PREMIUM             PIC X(10).
       01 WS-PLAN-CODE           PIC X(5).
       01 WS-STATUS              PIC X(15).
       01 WS-ACTIVE-FLAG         PIC X(1).
       01 WS-SYSTEM-DATE         PIC X(19).

      *> Display Fields
       01 WS-PREMIUM-DISP        PIC ZZZ,ZZZ,ZZ9.

      *> Search Flag
       01 WS-IMEI-FOUND          PIC 99 VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM DISPLAY-HEADER
           PERFORM GET-IMEI
           PERFORM SEARCH-IMEI
           PERFORM DISPLAY-RESULT
           STOP RUN.

       DISPLAY-HEADER.
           DISPLAY "========================================="
           DISPLAY "     APPLICATION STATUS SEARCH           "
           DISPLAY "=========================================".

       GET-IMEI.
           PERFORM UNTIL WS-IMEI-LEN >= 10 AND WS-IMEI-LEN <= 15
               DISPLAY " "
               DISPLAY "Enter IMEI Number (10-15 digits): "
               ACCEPT WS-IMEI-INPUT
               
               MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-IMEI-INPUT))
                 TO WS-IMEI-LEN
               
               IF WS-IMEI-LEN < 10 OR WS-IMEI-LEN > 15
                   DISPLAY "Error: IMEI must be 10-15 digits!"
               END-IF
           END-PERFORM.

       SEARCH-IMEI.
           MOVE 'N' TO WS-FOUND
           MOVE 'N' TO WS-EOF
           MOVE 0 TO WS-IMEI-FOUND
           
           OPEN INPUT APP-FILE
           
           IF WS-FILE-STATUS NOT = '00'
               DISPLAY "ERROR: Cannot open T_APPLICATION.CSV"
               EXIT PARAGRAPH
           END-IF

           PERFORM UNTIL WS-END-OF-FILE
               READ APP-FILE INTO APP-RECORD
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                       PERFORM CHECK-IMEI-IN-RECORD
               END-READ
           END-PERFORM

           CLOSE APP-FILE.

       CHECK-IMEI-IN-RECORD.
           MOVE 0 TO WS-IMEI-FOUND
           
           INSPECT APP-RECORD 
               TALLYING WS-IMEI-FOUND 
               FOR ALL FUNCTION TRIM(WS-IMEI-INPUT)
           
           IF WS-IMEI-FOUND > 0
               PERFORM PARSE-AND-CHECK-RECORD
      *> Stop searching after finding the record
               MOVE 'Y' TO WS-EOF
           END-IF.

       PARSE-AND-CHECK-RECORD.
           UNSTRING APP-RECORD DELIMITED BY ","
               INTO WS-APP-ID
                    WS-USER-NAME
                    WS-USER-EMAIL
                    WS-USER-ADDRESS
                    WS-DEVICE-TYPE
                    WS-DEVICE-MODEL
                    WS-IMEI
                    WS-PREMIUM
                    WS-PLAN-CODE
                    WS-STATUS
                    WS-ACTIVE-FLAG
                    WS-SYSTEM-DATE
           END-UNSTRING.

      *> Check if IMEI matches and ACTIVE_FLAG = 'Y'
           IF FUNCTION TRIM(WS-IMEI) = FUNCTION TRIM(WS-IMEI-INPUT)
              AND WS-ACTIVE-FLAG = 'Y'
               SET WS-FOUND-YES TO TRUE
           END-IF.

       DISPLAY-RESULT.
           DISPLAY " "
           
           IF WS-FOUND-YES
               COMPUTE WS-PREMIUM-DISP = 
                   FUNCTION NUMVAL(FUNCTION TRIM(WS-PREMIUM))
               
               DISPLAY "========================================="
               DISPLAY "           APPLICATION STATUS            "
               DISPLAY "========================================="
               DISPLAY "PHONE MODEL    : " 
                       FUNCTION TRIM(WS-DEVICE-MODEL)
               DISPLAY "APPLICANT NAME : " 
                       FUNCTION TRIM(WS-USER-NAME)
               DISPLAY "PLAN NAME      : " 
                       FUNCTION TRIM(WS-PLAN-CODE)
               DISPLAY "IMEI           : " 
                       FUNCTION TRIM(WS-IMEI)
               DISPLAY "ADDRESS        : " 
                       FUNCTION TRIM(WS-USER-ADDRESS)
               DISPLAY "EMAIL          : " 
                       FUNCTION TRIM(WS-USER-EMAIL)
               DISPLAY "COVERAGE PERIOD: 12 MONTHS"
               DISPLAY "PREMIUM        : " 
                       FUNCTION TRIM(WS-PREMIUM-DISP) " JPY"
               DISPLAY "APPLICATION DATE: " WS-SYSTEM-DATE
               DISPLAY "STATUS          : " 
                       FUNCTION TRIM(WS-STATUS)
               DISPLAY "========================================="
           ELSE
               DISPLAY "========================================="
               DISPLAY "         IMEI NOT FOUND!                "
               DISPLAY "========================================="
               DISPLAY " "
               DISPLAY "The IMEI " FUNCTION TRIM(WS-IMEI-INPUT)
               DISPLAY "was not found in the system."
               DISPLAY "Please check and try again."
               DISPLAY "========================================="
           END-IF.
       END PROGRAM SEARCH-STATUS.
       