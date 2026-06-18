       IDENTIFICATION DIVISION.
       PROGRAM-ID. USER-INFO.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-VALID-FLAG        PIC X VALUE 'N'.
       01  WS-AT-POS            PIC 99 VALUE 0.
       01  WS-DOT-POS           PIC 99 VALUE 0.
       01  WS-I                 PIC 999 VALUE 0.
       01  WS-CHAR              PIC X.
       01  WS-PHONE-VALID       PIC X VALUE 'Y'.
       01  WS-POSTAL-VALID      PIC X VALUE 'Y'.
       01  WS-LEN               PIC 99 VALUE 0.
       01  WS-YEAR              PIC 9(4) VALUE 0.
       01  WS-MONTH             PIC 9(2) VALUE 0.
       01  WS-DAY               PIC 9(2) VALUE 0.
       01  WS-LEAP-YEAR         PIC X VALUE 'N'.
       01  WS-CURRENT-DATE      PIC X(8).
       01  WS-CURRENT-YEAR      PIC 9(4).
       01  WS-AGE               PIC 9(3) VALUE 0.
       01  WS-INPUT-NAME        PIC X(500) VALUE SPACES.
       01  WS-INPUT-PHONE       PIC X(100) VALUE SPACES.
       01  WS-INPUT-POSTAL      PIC X(100) VALUE SPACES.
       01  WS-NAME-LEN          PIC 999 VALUE 0.
       01  WS-INPUT-DOB         PIC X(10) VALUE SPACES.

       LINKAGE SECTION.
       01  LS-USER-DATA.
           05 LS-USER-NAME        PIC X(50).
           05 LS-USER-EMAIL       PIC X(50).
           05 LS-USER-PHONE       PIC X(15).
           05 LS-USER-POSTAL-CODE PIC X(7).
           05 LS-USER-ADDRESS     PIC X(100).
           05 LS-USER-DOB         PIC X(8).

       PROCEDURE DIVISION USING LS-USER-DATA.
       MAIN-PARA.
           PERFORM GET-NAME
           PERFORM GET-EMAIL
           PERFORM GET-PHONE
           PERFORM GET-POSTAL-CODE
           PERFORM GET-ADDRESS
           PERFORM GET-DOB
           GOBACK.

       GET-NAME.
           MOVE 'N' TO WS-VALID-FLAG
           PERFORM UNTIL WS-VALID-FLAG = 'Y'
               DISPLAY "Enter Name (Required, max 50 chars): "
               ACCEPT WS-INPUT-NAME
               IF WS-INPUT-NAME = SPACES
                   DISPLAY "Error: Name is required."
               ELSE
                   PERFORM VARYING WS-I FROM 500 BY -1 UNTIL WS-I = 0
                    OR WS-INPUT-NAME(WS-I:1) NOT = SPACE
                       CONTINUE
                   END-PERFORM
                   MOVE WS-I TO WS-NAME-LEN
                   IF WS-NAME-LEN > 50
                       DISPLAY "Error: Name must be max 50 characters."
                   ELSE
                       MOVE WS-INPUT-NAME(1:50) TO LS-USER-NAME
                       MOVE 'Y' TO WS-VALID-FLAG
                   END-IF
               END-IF
           END-PERFORM.

       GET-EMAIL.
           MOVE 'N' TO WS-VALID-FLAG
           PERFORM UNTIL WS-VALID-FLAG = 'Y'
               DISPLAY "Enter Email (e.g., user@domain.com): "
               ACCEPT LS-USER-EMAIL
               MOVE 0 TO WS-AT-POS
               MOVE 0 TO WS-DOT-POS
               IF LS-USER-EMAIL NOT = SPACES
                   INSPECT LS-USER-EMAIL TALLYING WS-AT-POS FOR ALL "@"
                   INSPECT LS-USER-EMAIL TALLYING WS-DOT-POS FOR ALL "."
                   IF WS-AT-POS > 0 AND WS-DOT-POS > 0
                       MOVE 'Y' TO WS-VALID-FLAG
                   ELSE
                       DISPLAY "Error: Invalid Email format."
                   END-IF
               ELSE
                   DISPLAY "Error: Email is required."
               END-IF
           END-PERFORM.

       GET-PHONE.
           MOVE 'N' TO WS-VALID-FLAG
           PERFORM UNTIL WS-VALID-FLAG = 'Y'
               DISPLAY "Enter Phone (10-11 digits): "
               ACCEPT WS-INPUT-PHONE
               MOVE 'Y' TO WS-PHONE-VALID
               PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 100
                 OR WS-INPUT-PHONE(WS-I:1) = SPACE
                   MOVE WS-INPUT-PHONE(WS-I:1) TO WS-CHAR
                   IF WS-CHAR < '0' OR WS-CHAR > '9'
                       MOVE 'N' TO WS-PHONE-VALID
                   END-IF
               END-PERFORM
               COMPUTE WS-LEN = WS-I - 1
               IF WS-PHONE-VALID = 'Y' AND WS-LEN >= 10 AND WS-LEN <= 11
                   MOVE WS-INPUT-PHONE(1:15) TO LS-USER-PHONE
                   MOVE 'Y' TO WS-VALID-FLAG
               ELSE IF WS-PHONE-VALID = 'N'
                   DISPLAY "Error: Phone must contain digits only."
               ELSE
                   DISPLAY "Error: Phone must be 10 to 11 digits."
               END-IF
           END-PERFORM.

       GET-POSTAL-CODE.
           MOVE 'N' TO WS-VALID-FLAG
           PERFORM UNTIL WS-VALID-FLAG = 'Y'
               DISPLAY "Enter Postal Code (5-7 digits): "
               ACCEPT WS-INPUT-POSTAL
               MOVE 'Y' TO WS-POSTAL-VALID
               PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 100 
                 OR WS-INPUT-POSTAL(WS-I:1) = SPACE
                   MOVE WS-INPUT-POSTAL(WS-I:1) TO WS-CHAR
                   IF WS-CHAR < '0' OR WS-CHAR > '9'
                       MOVE 'N' TO WS-POSTAL-VALID
                   END-IF
               END-PERFORM
               COMPUTE WS-LEN = WS-I - 1
               IF WS-POSTAL-VALID = 'Y' AND WS-LEN >= 5 AND WS-LEN <= 7
                   MOVE WS-INPUT-POSTAL(1:7) TO LS-USER-POSTAL-CODE
                   MOVE 'Y' TO WS-VALID-FLAG
               ELSE IF WS-POSTAL-VALID = 'N'
                   DISPLAY "Error: Postal Code must be numeric."
               ELSE
                   DISPLAY "Error: Postal Code must be 5 to 7 digits."
               END-IF
           END-PERFORM.

       GET-ADDRESS.
           MOVE 'N' TO WS-VALID-FLAG
           PERFORM UNTIL WS-VALID-FLAG = 'Y'
               DISPLAY "Enter Address (Required): "
               ACCEPT LS-USER-ADDRESS
               IF LS-USER-ADDRESS NOT = SPACES
                   MOVE 'Y' TO WS-VALID-FLAG
               ELSE
                   DISPLAY "Error: Address is required."
               END-IF
           END-PERFORM.

       GET-DOB.
           MOVE 'N' TO WS-VALID-FLAG
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           MOVE WS-CURRENT-DATE(1:4) TO WS-CURRENT-YEAR
           PERFORM UNTIL WS-VALID-FLAG = 'Y'
               DISPLAY "Enter Date of Birth (YYYY-MM-DD): "
               ACCEPT WS-INPUT-DOB
               IF WS-INPUT-DOB(5:1) NOT = '-' OR
                WS-INPUT-DOB(8:1) NOT = '-'
                   DISPLAY "Error: Date format must be YYYY-MM-DD."
               ELSE
                   IF WS-INPUT-DOB(1:4) IS NOT NUMERIC OR 
                      WS-INPUT-DOB(6:2) IS NOT NUMERIC OR 
                      WS-INPUT-DOB(9:2) IS NOT NUMERIC
                       DISPLAY "Error: Date parts must be numeric."
                   ELSE
                       MOVE WS-INPUT-DOB(1:4) TO WS-YEAR
                       MOVE WS-INPUT-DOB(6:2) TO WS-MONTH
                       MOVE WS-INPUT-DOB(9:2) TO WS-DAY

                       IF WS-MONTH < 1 OR WS-MONTH > 12
                           DISPLAY "Error: Invalid month!Must be 01-12."
                       ELSE
                           MOVE 'Y' TO WS-VALID-FLAG
                           EVALUATE WS-MONTH
                               WHEN 1  WHEN 3  WHEN 5  WHEN 7
                               WHEN 8  WHEN 10 WHEN 12
                                   IF WS-DAY < 1 OR WS-DAY > 31
                                    DISPLAY "Error:Month has 31 days."
                                    MOVE 'N' TO WS-VALID-FLAG
                                   END-IF
                               WHEN 4  WHEN 6  WHEN 9  WHEN 11
                                   IF WS-DAY < 1 OR WS-DAY > 30
                                    DISPLAY "Error: Month has 30 days."
                                    MOVE 'N' TO WS-VALID-FLAG
                                   END-IF
                               WHEN 2
                                   IF WS-DAY < 1 OR WS-DAY > 29
                                    DISPLAY "February has 28 or 29 day."
                                    MOVE 'N' TO WS-VALID-FLAG
                                   ELSE
                                       IF WS-DAY = 29
                                           PERFORM IS-LEAP-YEAR-CHECK
                                           IF WS-LEAP-YEAR = 'N'
                                            DISPLAY "Not a leap year."
                                            MOVE 'N' TO WS-VALID-FLAG
                                           END-IF
                                       END-IF
                                   END-IF
                           END-EVALUATE

                           IF WS-VALID-FLAG = 'Y'
                            COMPUTE WS-AGE = WS-CURRENT-YEAR - WS-YEAR
                               IF WS-AGE < 10 OR WS-AGE > 100
                                   DISPLAY "Error: not in valid age"
                                   MOVE 'N' TO WS-VALID-FLAG
                               ELSE
                                   MOVE WS-YEAR TO LS-USER-DOB(1:4)
                                   MOVE WS-MONTH TO LS-USER-DOB(5:2)
                                   MOVE WS-DAY TO LS-USER-DOB(7:2)
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM.

       IS-LEAP-YEAR-CHECK.
           MOVE 'N' TO WS-LEAP-YEAR
           IF FUNCTION MOD(WS-YEAR, 400) = 0
               MOVE 'Y' TO WS-LEAP-YEAR
           ELSE
               IF FUNCTION MOD(WS-YEAR, 100) = 0
                   MOVE 'N' TO WS-LEAP-YEAR
               ELSE
                   IF FUNCTION MOD(WS-YEAR, 4) = 0
                       MOVE 'Y' TO WS-LEAP-YEAR
                   ELSE
                       MOVE 'N' TO WS-LEAP-YEAR
                   END-IF
               END-IF
           END-IF.
