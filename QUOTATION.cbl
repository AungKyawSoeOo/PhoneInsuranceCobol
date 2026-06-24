       IDENTIFICATION DIVISION.
       PROGRAM-ID. QUOTATION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *> ==========================================
      *> LEVEL 77 CONSTANTS
      *> ==========================================
       77 WS-FACTOR-12       PIC 9V99 VALUE 1.0.
       77 WS-FACTOR-24       PIC 9V99 VALUE 1.8.
       77 WS-FACTOR-36       PIC 9V99 VALUE 2.5.

       77 WS-BASE-RATE       PIC 9V99 VALUE 0.02.

      *> ==========================================
      *> VARIABLES
      *> ==========================================
       01 WS-LEN             PIC 99.
       01 WS-IMEI            PIC X(15).
       01 WS-IMEI-TEMP       PIC X(15).
       01 WS-DEVICE-TYPE     PIC X(10).
       01 WS-DEVICE-MODEL    PIC X(20).
       01 WS-PURCHASE-DATE   PIC X(10).
       01 WS-PRICE           PIC 9(6).
       01 WS-PRICE-CATEGORY  PIC X(6).
       01 WS-PERIOD          PIC X(3).
       01 WS-PERIOD-FACTOR   PIC 9V99.
       01 WS-PERIOD-MONTHS   PIC 99.
       01 WS-EST-PREMIUM     PIC 9(9)V99.
       01 WS-PLAN-CODE       PIC X(5).
       01 WS-PLAN-NAME       PIC X(20).
       01 WS-PLAN-BASE-RATE  PIC 99999.
       01 WS-PLAN-MAX-PAYOUT PIC 9(8).
       01 WS-CURRENT-PREMIUM PIC 9(9)V99.
       01 WS-FINAL-PREMIUM   PIC 9(9)V99.
       01 WS-ERROR           PIC X(60).

      *> ==========================================
      *> CHOICE FIELDS
      *> ==========================================
       01 WS-CHOICE          PIC X.
       01 WS-CHOICE-NUM      PIC 9.

      *> ==========================================
      *> DISPLAY FIELDS (For removing leading zeros)
      *> ==========================================
       01 WS-DISP-COUNT     PIC Z9.
       01 WS-DECL-SCORE-DISP    PIC Z(3)9.

      *> ==========================================
      *> DEVICE MASTER VARIABLES
      *> ==========================================
       01 WS-DEVICE-TYPE-CODE PIC 9.
       01 WS-DEVICE-TYPE-NAME PIC X(10).
       01 WS-MODEL-FOUND      PIC X VALUE 'N'.
       01 WS-LOAD-FLAG        PIC X VALUE 'N'.
           88 WS-LOAD-DONE    VALUE 'Y'.

       01 WS-EMPTY-NUM        PIC 9 VALUE 0.
       01 WS-EMPTY-CHAR       PIC X VALUE SPACES.

       01 WS-TYPE-LIST.
           05 WS-TYPE-COUNT   PIC 99.
           05 WS-TYPE-ENTRY OCCURS 10 TIMES.
               10 WS-TYPE-CODE PIC 9.
               10 WS-TYPE-NAME PIC X(10).

       01 WS-MODEL-LIST.
           05 WS-MODEL-COUNT  PIC 99.
           05 WS-MODEL-ENTRY OCCURS 20 TIMES.
               10 WS-MODEL-NAME PIC X(20).

       01 WS-I                PIC 99.
       01 WS-J                PIC 99.
       01 WS-FOUND            PIC X VALUE 'N'.
       01 WS-VALID-FLAG       PIC X VALUE 'Y'.

      *> ==========================================
      *> CURRENCY FORMAT FIELDS
      *> ==========================================
       01 WS-PRICE-DISP      PIC ZZZ,ZZ9.
       01 WS-PREMIUM-DISP      PIC ZZZ,ZZ9.
       01 WS-CURRENT-PREM-DISP PIC ZZZ,ZZZ,ZZ9.
       01 WS-FINAL-PREM-DISP   PIC ZZZ,ZZZ,ZZ9.

      *> ==========================================
      *> DATE FIELDS
      *> ==========================================
       01 WS-YEAR            PIC 9(4).
       01 WS-MONTH           PIC 9(2).
       01 WS-DAY             PIC 9(2).
       01 WS-AGE             PIC 9(3).
       01 WS-AGE-OUT         PIC ZZZ.
       01 WS-USER-DOB-YEAR   PIC 9(4).

       01 WS-SYSTEM-DATE.
           05 WS-SYS-YEAR    PIC 9(4).
           05 WS-SYS-MONTH   PIC 9(2).
           05 WS-SYS-DAY     PIC 9(2).
           05 WS-SYS-HOUR    PIC 9(2).
           05 WS-SYS-MINUTE  PIC 9(2).
           05 WS-SYS-SECOND  PIC 9(2).

       01 WS-SYSTEM-DATE-STR PIC X(19).
       01 WS-PURCHASE-NUM    PIC 9(8).
       01 WS-SYSTEM-NUM      PIC 9(8).

      *> ==========================================
      *> LEAP YEAR FIELDS
      *> ==========================================
       01 WS-LEAP-YEAR       PIC X VALUE 'N'.
           88 IS-LEAP-YEAR   VALUE 'Y'.
           88 NOT-LEAP-YEAR  VALUE 'N'.
      *> ==========================================
      *> PLAN PREMIUM CALC FIELDS
      *> ==========================================
       01 PLAN-BASE-RATE     PIC ZZZ99.
       01 PLAN-MAX-RATE      PIC ZZZZZZZ9.
       01 WS-DECL-APP-ID         PIC 9(10).
       01 WS-DECL-FINAL-STATUS   PIC X(15).
       01 WS-DECL-TOTAL-SCORE    PIC 9(3).
      *> ==========================================
      *> Displaying chosen plan coverages
      *> ==========================================
       01 WS-COVERAGE-TABLE.
           05 WS-COV-COUNT        PIC 99.
           05 WS-COV-ENTRY OCCURS 10 TIMES.
               10 WS-COV-TYPE     PIC X(20).
               10 WS-COV-FLAG     PIC X(1).
       01 WS-COV-YES-NO           PIC X(3).
       01 WS-K                    PIC 99.

      *> ==========================================
      *> LINKAGE SECTION
      *> ==========================================
       LINKAGE SECTION.
       01  LK-COMM-AREA.
           05 WS-CONTINUE           PIC X(10).
           05 WS-DEVICE-DATA.
              10 WS-IMEI-LK         PIC X(15).
              10 WS-DEVICE-TYPE-LK  PIC X(10).
              10 WS-DEVICE-MODEL-LK PIC X(20).
              10 WS-PURCHASE-DATE-LK PIC X(10).
              10 WS-PRICE-LK        PIC 9(6).
           05 WS-CALC-RESULTS.
              10 WS-PRICE-CATEGORY-LK PIC X(6).
              10 WS-PERIOD-LK       PIC X(3).
              10 WS-PERIOD-FACTOR-LK PIC 9V99.
              10 WS-PERIOD-MONTHS-LK PIC 99.
              10 WS-EST-PREMIUM-LK  PIC 9(9)V99.
           05 WS-PLAN-DATA.
              10 WS-PLAN-CODE-LK   PIC X(5).
              10 WS-PLAN-NAME-LK   PIC X(20).
              10 WS-PLAN-BASE-RATE-LK PIC 99999.
              10 WS-PLAN-MAX-PAYOUT-LK PIC 9(8).
              10 WS-CURRENT-PREMIUM-LK PIC 9(9)V99.
              10 WS-FINAL-PREMIUM-LK PIC 9(9)V99.
           05 WS-SYSTEM-DATE-STR-LK PIC X(19).
           05 WS-USER-DATA-LK.
              10 WS-USER-NAME-LK        PIC X(50).
              10 WS-USER-EMAIL-LK       PIC X(50).
              10 WS-USER-PHONE-LK       PIC X(15).
              10 WS-USER-POSTAL-CODE-LK PIC X(7).
              10 WS-USER-ADDRESS-LK     PIC X(100).
              10 WS-USER-DOB-LK         PIC X(8).
       
       PROCEDURE DIVISION USING LK-COMM-AREA.

       MAIN-PROCEDURE.
           PERFORM LOAD-DEVICE-MASTER
           PERFORM GET-ONE-QUOTATION
      
           EXIT PROGRAM.

       LOAD-DEVICE-MASTER.
           IF NOT WS-LOAD-DONE
               MOVE 0 TO WS-EMPTY-NUM
               MOVE SPACES TO WS-EMPTY-CHAR
               CALL 'DEVICE-READER' USING 'L', WS-EMPTY-NUM,
                    WS-EMPTY-CHAR, WS-DEVICE-TYPE-NAME,
                    WS-MODEL-FOUND, WS-TYPE-LIST, WS-MODEL-LIST
               SET WS-LOAD-DONE TO TRUE
           END-IF.

       GET-ONE-QUOTATION.
           PERFORM DISPLAY-WELCOME
           PERFORM GET-IMEI
           PERFORM GET-DEVICE-TYPE
           PERFORM GET-DEVICE-MODEL
           PERFORM GET-PURCHASE-DATE
           PERFORM GET-PURCHASE-PRICE
           PERFORM GET-COVERAGE-PERIOD
           PERFORM SET-PRICE-CATEGORY
           PERFORM CALC-PREMIUM
           PERFORM SELECT-INSURANCE-PLAN
           PERFORM CALC-SELECTED-PLAN-PREMIUM
           PERFORM GEN-SYS-DATE
           PERFORM MOVE-TO-LINKAGE
           CALL 'SAVE-APPLICATION' USING LK-COMM-AREA
           MOVE WS-CONTINUE TO WS-DECL-APP-ID
           CALL 'DECLARATION-READER' USING WS-DECL-APP-ID,
                                           WS-DECL-FINAL-STATUS,
                                           WS-DECL-TOTAL-SCORE

           
           PERFORM DISPLAY-RESULT.

       MOVE-TO-LINKAGE.
           MOVE WS-IMEI           TO WS-IMEI-LK
           MOVE WS-DEVICE-TYPE    TO WS-DEVICE-TYPE-LK
           MOVE WS-DEVICE-MODEL   TO WS-DEVICE-MODEL-LK
           MOVE WS-PURCHASE-DATE  TO WS-PURCHASE-DATE-LK
           MOVE WS-PRICE          TO WS-PRICE-LK
           MOVE WS-PRICE-CATEGORY TO WS-PRICE-CATEGORY-LK
           MOVE WS-PERIOD         TO WS-PERIOD-LK
           MOVE WS-PERIOD-FACTOR  TO WS-PERIOD-FACTOR-LK
           MOVE WS-PERIOD-MONTHS  TO WS-PERIOD-MONTHS-LK
           MOVE WS-EST-PREMIUM    TO WS-EST-PREMIUM-LK
           MOVE WS-PLAN-CODE      TO WS-PLAN-CODE-LK
           MOVE WS-PLAN-NAME      TO WS-PLAN-NAME-LK
           MOVE WS-PLAN-BASE-RATE TO WS-PLAN-BASE-RATE-LK
           MOVE WS-PLAN-MAX-PAYOUT TO WS-PLAN-MAX-PAYOUT-LK
           MOVE WS-CURRENT-PREMIUM TO WS-CURRENT-PREMIUM-LK
           MOVE WS-FINAL-PREMIUM TO WS-FINAL-PREMIUM-LK
           MOVE WS-SYSTEM-DATE-STR TO WS-SYSTEM-DATE-STR-LK.

       DISPLAY-WELCOME.
           DISPLAY '========================================='
           DISPLAY '    MOBILE INSURANCE QUOTATION SYSTEM    '
           DISPLAY '========================================='
           DISPLAY ' '
           DISPLAY 'Please enter your device information:'.

      *> ==========================================
      *> GET IMEI
      *> ==========================================
       GET-IMEI.
           DISPLAY ' '
           DISPLAY 'Enter Phone IMEI (10-15 digits only): '
           DISPLAY '(Example: 123456789012345)'
           ACCEPT WS-IMEI.

           MOVE SPACES TO WS-IMEI-TEMP
           MOVE 0 TO WS-LEN

           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > 15
               IF WS-IMEI(WS-I:1) >= '0' AND
                  WS-IMEI(WS-I:1) <= '9'
                   ADD 1 TO WS-LEN
                   MOVE WS-IMEI(WS-I:1)
                     TO WS-IMEI-TEMP(WS-LEN:1)
               END-IF
           END-PERFORM

           MOVE WS-IMEI-TEMP TO WS-IMEI

           IF WS-LEN < 10 OR WS-LEN > 15
               DISPLAY 'IMEI must be 10-15 digits!'
               PERFORM GET-IMEI
               EXIT PARAGRAPH
           END-IF.

      *> ==========================================
      *> GET DEVICE TYPE (FIXED DISPLAY - NO LEADING ZERO)
      *> ==========================================
       GET-DEVICE-TYPE.
           PERFORM GET-TYPE-LIST-FROM-FILE.

           DISPLAY ' '
           DISPLAY 'SELECT DEVICE TYPE:'
           DISPLAY '-----------------------------------------'

           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-TYPE-COUNT
               DISPLAY '[' WS-TYPE-CODE(WS-I) '] '
                       FUNCTION TRIM(WS-TYPE-NAME(WS-I))
           END-PERFORM

           DISPLAY '-----------------------------------------'
      *> FIX: Use WS-DISP-COUNT to remove leading zero
           MOVE WS-TYPE-COUNT TO WS-DISP-COUNT
           DISPLAY 'Enter choice (1-'
                   FUNCTION TRIM(WS-DISP-COUNT) '): '
           ACCEPT WS-CHOICE

           IF WS-CHOICE NOT NUMERIC
               DISPLAY 'Invalid choice! Please enter a number.'
               PERFORM GET-DEVICE-TYPE
               EXIT PARAGRAPH
           END-IF

           MOVE FUNCTION NUMVAL(WS-CHOICE) TO WS-CHOICE-NUM

           MOVE 0 TO WS-DEVICE-TYPE-CODE.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-TYPE-COUNT
               IF WS-CHOICE-NUM = WS-TYPE-CODE(WS-I)
                   MOVE WS-TYPE-CODE(WS-I)
                     TO WS-DEVICE-TYPE-CODE
                   MOVE WS-TYPE-NAME(WS-I)
                     TO WS-DEVICE-TYPE
               END-IF
           END-PERFORM

           IF WS-DEVICE-TYPE-CODE = 0
               DISPLAY 'Invalid device type!'
               PERFORM GET-DEVICE-TYPE
           END-IF.

       GET-TYPE-LIST-FROM-FILE.
           MOVE 0 TO WS-EMPTY-NUM
           MOVE SPACES TO WS-EMPTY-CHAR
           CALL 'DEVICE-READER' USING 'T', WS-EMPTY-NUM,
                WS-EMPTY-CHAR, WS-DEVICE-TYPE-NAME,
                WS-MODEL-FOUND, WS-TYPE-LIST, WS-MODEL-LIST.

      *> ==========================================
      *> GET DEVICE MODEL (FIXED DISPLAY - NO LEADING ZERO)
      *> ==========================================
       GET-DEVICE-MODEL.
           PERFORM GET-MODELS-FROM-FILE.
           PERFORM DISPLAY-MODELS-DYNAMIC.

           DISPLAY '-----------------------------------------'
      *> FIX: Use WS-DISP-COUNT to remove leading zero
           MOVE WS-MODEL-COUNT TO WS-DISP-COUNT
           DISPLAY 'Enter choice (1-'
                   FUNCTION TRIM(WS-DISP-COUNT) '): '
           ACCEPT WS-CHOICE

           IF WS-CHOICE NOT NUMERIC
               DISPLAY 'Invalid choice! Please enter a number.'
               PERFORM GET-DEVICE-MODEL
               EXIT PARAGRAPH
           END-IF

           MOVE FUNCTION NUMVAL(WS-CHOICE) TO WS-CHOICE-NUM

           IF WS-CHOICE-NUM >= 1 AND WS-CHOICE-NUM <= WS-MODEL-COUNT
               MOVE WS-MODEL-NAME(WS-CHOICE-NUM)
                 TO WS-DEVICE-MODEL
               PERFORM VALIDATE-MODEL-FROM-FILE
           ELSE
               DISPLAY 'Invalid model choice!'
               PERFORM GET-DEVICE-MODEL
           END-IF.

       GET-MODELS-FROM-FILE.
           MOVE SPACES TO WS-EMPTY-CHAR
           CALL 'DEVICE-READER' USING 'M', WS-DEVICE-TYPE-CODE,
                WS-EMPTY-CHAR, WS-DEVICE-TYPE-NAME,
                WS-MODEL-FOUND, WS-TYPE-LIST, WS-MODEL-LIST.

      *> ==========================================
      *> DISPLAY MODELS DYNAMIC (FIXED - NO LEADING ZERO)
      *> ==========================================
       DISPLAY-MODELS-DYNAMIC.
           DISPLAY ' '
           DISPLAY 'SELECT ' FUNCTION TRIM(WS-DEVICE-TYPE)
                   ' MODEL:'
           DISPLAY '-----------------------------------------'

           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-MODEL-COUNT
      *> FIX: Use WS-DISP-COUNT to remove leading zero
               MOVE WS-I TO WS-DISP-COUNT
               DISPLAY '[' FUNCTION TRIM(WS-DISP-COUNT) '] '
                       FUNCTION TRIM(WS-MODEL-NAME(WS-I))
           END-PERFORM.

       VALIDATE-MODEL-FROM-FILE.
           CALL 'DEVICE-READER' USING 'S', WS-DEVICE-TYPE-CODE,
                WS-DEVICE-MODEL, WS-DEVICE-TYPE-NAME,
                WS-MODEL-FOUND, WS-TYPE-LIST, WS-MODEL-LIST

           IF WS-MODEL-FOUND = 'N'
               DISPLAY 'Invalid model! Please try again.'
               PERFORM GET-DEVICE-MODEL
           END-IF.

      *> ==========================================
      *> GET PURCHASE DATE
      *> ==========================================
       GET-PURCHASE-DATE.
           DISPLAY ' '
           DISPLAY 'Enter Purchase Date (YYYY-MM-DD): '
           DISPLAY 'Example: 2026-06-16'
           ACCEPT WS-PURCHASE-DATE.

           PERFORM VALIDATE-DATE.

      *> ==========================================
      *> VALIDATE DATE
      *> ==========================================
       VALIDATE-DATE.
           IF WS-PURCHASE-DATE(5:1) NOT = '-' OR
              WS-PURCHASE-DATE(8:1) NOT = '-'
               DISPLAY 'Date format must be YYYY-MM-DD!'
               PERFORM GET-PURCHASE-DATE
               EXIT PARAGRAPH
           END-IF.

           MOVE WS-PURCHASE-DATE(1:4) TO WS-YEAR.
           MOVE WS-PURCHASE-DATE(6:2) TO WS-MONTH.
           MOVE WS-PURCHASE-DATE(9:2) TO WS-DAY.

           IF WS-MONTH < 1 OR WS-MONTH > 12
               DISPLAY 'Invalid month! Must be 01-12.'
               PERFORM GET-PURCHASE-DATE
               EXIT PARAGRAPH
           END-IF.

           PERFORM CHECK-FUTURE-DATE.

           EVALUATE WS-MONTH
               WHEN 1  WHEN 3  WHEN 5  WHEN 7
               WHEN 8  WHEN 10 WHEN 12
                   IF WS-DAY < 1 OR WS-DAY > 31
                       DISPLAY 'Invalid day! Month has 31 days.'
                       PERFORM GET-PURCHASE-DATE
                   END-IF

               WHEN 4  WHEN 6  WHEN 9  WHEN 11
                   IF WS-DAY < 1 OR WS-DAY > 30
                       DISPLAY 'Invalid day! Month has 30 days.'
                       PERFORM GET-PURCHASE-DATE
                   END-IF

               WHEN 2
                   PERFORM CHECK-LEAP-YEAR
           END-EVALUATE.

      *> ==========================================
      *> CHECK LEAP YEAR
      *> ==========================================
       CHECK-LEAP-YEAR.
           IF WS-DAY < 1 OR WS-DAY > 29
               DISPLAY 'Invalid day! February has 28 or 29 days.'
               PERFORM GET-PURCHASE-DATE
               EXIT PARAGRAPH
           END-IF.

           IF WS-DAY = 29
               PERFORM IS-LEAP-YEAR-CHECK
               IF WS-LEAP-YEAR = 'N'
                   DISPLAY 'Invalid date! ' WS-YEAR
                           ' is not a leap year.'
                   PERFORM GET-PURCHASE-DATE
               END-IF
           END-IF.

       IS-LEAP-YEAR-CHECK.
           MOVE 'N' TO WS-LEAP-YEAR.

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

      *> ==========================================
      *> CHECK FUTURE DATE
      *> ==========================================
       CHECK-FUTURE-DATE.
           MOVE FUNCTION CURRENT-DATE TO WS-SYSTEM-DATE.

           IF WS-YEAR > WS-SYS-YEAR
           DISPLAY 'Invalid date!Purchase date cannot be in the future.'
               PERFORM GET-PURCHASE-DATE
               EXIT PARAGRAPH
           END-IF.

           IF WS-YEAR = WS-SYS-YEAR
               IF WS-MONTH > WS-SYS-MONTH
           DISPLAY 'Invalid date!Purchase date cannot be in the future.'
                   PERFORM GET-PURCHASE-DATE
                   EXIT PARAGRAPH
               END-IF
           END-IF.

           IF WS-YEAR = WS-SYS-YEAR
               IF WS-MONTH = WS-SYS-MONTH
                   IF WS-DAY > WS-SYS-DAY
           DISPLAY 'Invalid date!Purchase date cannot be in the future.'
                       PERFORM GET-PURCHASE-DATE
                       EXIT PARAGRAPH
                   END-IF
               END-IF
           END-IF.

      *> ==========================================
      *> GET PURCHASE PRICE
      *> ==========================================
       GET-PURCHASE-PRICE.
           DISPLAY ' '
           DISPLAY 'Enter Purchase Price (JPY): '
           DISPLAY '-----------------------------------------'
           DISPLAY 'Allowed Range: 10,000 ~ 200,000 JPY'
           DISPLAY 'Price Categories:'
           DISPLAY '  LOW    : 10,000 - 50,000 JPY'
           DISPLAY '  MEDIUM : 50,001 - 100,000 JPY'
           DISPLAY '  HIGH   : 100,001 - 200,000 JPY'
           DISPLAY '-----------------------------------------'
           DISPLAY 'Enter price: '
           ACCEPT WS-PRICE.

           IF WS-PRICE < 10000
               DISPLAY 'Price must be at least 10,000 JPY!'
               PERFORM GET-PURCHASE-PRICE
           END-IF.

           IF WS-PRICE > 200000
               DISPLAY 'Price cannot exceed 200,000 JPY!'
               PERFORM GET-PURCHASE-PRICE
           END-IF.

      *> ==========================================
      *> GET COVERAGE PERIOD
      *> ==========================================
       GET-COVERAGE-PERIOD.
           DISPLAY ' '
           DISPLAY 'SELECT COVERAGE PERIOD:'
           DISPLAY '-----------------------------------------'
           DISPLAY '[1] 12 months  (Multiplier: ' WS-FACTOR-12 ')'
           DISPLAY '[2] 24 months  (Multiplier: ' WS-FACTOR-24 ')'
           DISPLAY '[3] 36 months  (Multiplier: ' WS-FACTOR-36 ')'
           DISPLAY '-----------------------------------------'
           DISPLAY 'Enter choice (1-3): '
           ACCEPT WS-CHOICE

           IF WS-CHOICE NOT NUMERIC
               DISPLAY 'Invalid choice! Please enter 1, 2, or 3.'
               PERFORM GET-COVERAGE-PERIOD
               EXIT PARAGRAPH
           END-IF

           MOVE FUNCTION NUMVAL(WS-CHOICE) TO WS-CHOICE-NUM

           EVALUATE WS-CHOICE-NUM
               WHEN 1
                   MOVE 12 TO WS-PERIOD-MONTHS
                   MOVE WS-FACTOR-12 TO WS-PERIOD-FACTOR
               WHEN 2
                   MOVE 24 TO WS-PERIOD-MONTHS
                   MOVE WS-FACTOR-24 TO WS-PERIOD-FACTOR
               WHEN 3
                   MOVE 36 TO WS-PERIOD-MONTHS
                   MOVE WS-FACTOR-36 TO WS-PERIOD-FACTOR
               WHEN OTHER
                   DISPLAY 'Invalid period! (1, 2, or 3 only)'
                   PERFORM GET-COVERAGE-PERIOD
           END-EVALUATE.

      *> ==========================================
      *> PRICE CATEGORY
      *> ==========================================
       SET-PRICE-CATEGORY.
           IF WS-PRICE <= 50000
               MOVE 'LOW' TO WS-PRICE-CATEGORY
           ELSE
               IF WS-PRICE <= 100000
                   MOVE 'MEDIUM' TO WS-PRICE-CATEGORY
               ELSE
                   MOVE 'HIGH' TO WS-PRICE-CATEGORY
               END-IF
           END-IF.

      *> ==========================================
      *> CALCULATE PREMIUM
      *> ==========================================
       CALC-PREMIUM.
           CALL 'PremiumCalculation' USING
               WS-PRICE, WS-PERIOD-FACTOR, WS-BASE-RATE, WS-EST-PREMIUM.


      *> ==========================================
      *> SELECT INSURANCE PLAN (Step 3-1, 3-2)
      *> ==========================================
       SELECT-INSURANCE-PLAN.
           CALL 'SELECT-PLAN' USING
               WS-PLAN-CODE,
               WS-PLAN-NAME,
               WS-PLAN-BASE-RATE,
               WS-PLAN-MAX-PAYOUT,
               WS-USER-DATA-LK,
               WS-COVERAGE-TABLE.
      *> ==========================================
      *> CALCULATE SELECTED PLAN PREMIUM (Step 3-3)
      *> ==========================================
       CALC-SELECTED-PLAN-PREMIUM.
           CALL 'PLAN-PREMIUM-CALC' USING
               WS-PRICE,
               WS-PERIOD-FACTOR,
               WS-EST-PREMIUM,
               WS-PLAN-BASE-RATE,
               WS-CURRENT-PREMIUM,
               WS-FINAL-PREMIUM.
      *> ==========================================
      *> GENERATE SYSTEM DATE
      *> ==========================================
       GEN-SYS-DATE.
           MOVE FUNCTION CURRENT-DATE TO WS-SYSTEM-DATE.
           STRING WS-SYS-YEAR '-' WS-SYS-MONTH '-' WS-SYS-DAY
                  ' ' WS-SYS-HOUR ':' WS-SYS-MINUTE ':' WS-SYS-SECOND
               INTO WS-SYSTEM-DATE-STR.

      *> ==========================================
      *> DISPLAY RESULT
      *> ==========================================
       DISPLAY-RESULT.
           MOVE WS-PRICE TO WS-PRICE-DISP.
           MOVE WS-EST-PREMIUM TO WS-PREMIUM-DISP.
           MOVE WS-CURRENT-PREMIUM TO WS-CURRENT-PREM-DISP.
           MOVE WS-FINAL-PREMIUM TO WS-FINAL-PREM-DISP.

           DISPLAY ' '
           DISPLAY '========================================='
           DISPLAY '            QUOTATION RESULT             '
           DISPLAY '========================================='
           DISPLAY 'IMEI            : ' WS-IMEI
           DISPLAY 'Device Type     : ' WS-DEVICE-TYPE
           DISPLAY 'Device Model    : ' WS-DEVICE-MODEL
           DISPLAY 'Purchase Date   : ' WS-PURCHASE-DATE
           DISPLAY 'Purchase Price  : ' WS-PRICE-DISP ' JPY'
           DISPLAY 'Price Category  : ' WS-PRICE-CATEGORY
           DISPLAY 'Coverage Period : ' WS-PERIOD-MONTHS ' months'
           DISPLAY 'Period Multiplier: ' WS-PERIOD-FACTOR
           DISPLAY 'Base Rate       : ' WS-BASE-RATE
           DISPLAY '-----------------------------------------'
           DISPLAY 'Estimated Premium: '
                   FUNCTION TRIM(WS-PREMIUM-DISP) ' JPY'
           DISPLAY 'Selected Plan    : ' WS-PLAN-CODE ' '
                   WS-PLAN-NAME
           MOVE WS-PLAN-BASE-RATE TO PLAN-BASE-RATE
           DISPLAY 'Plan Base Rate   : ' FUNCTION TRIM(PLAN-BASE-RATE)
           MOVE WS-PLAN-MAX-PAYOUT TO PLAN-MAX-RATE
           DISPLAY 'Plan Max Payout  : ' FUNCTION TRIM(PLAN-MAX-RATE) 
                                      ' JPY'
           DISPLAY 'Current Premium  : '
                   FUNCTION TRIM(WS-CURRENT-PREM-DISP) ' JPY'
           DISPLAY 'Final Premium    : '
                   FUNCTION TRIM(WS-FINAL-PREM-DISP) ' JPY'
           DISPLAY 'Applied Date     : ' WS-SYSTEM-DATE-STR
           DISPLAY 'Status           : PENDING'
           DISPLAY '-----------------------------------------'
           MOVE WS-DECL-TOTAL-SCORE TO WS-DECL-SCORE-DISP
           DISPLAY 'Declaration Score  : ' 
                   FUNCTION TRIM(WS-DECL-SCORE-DISP)

           MOVE WS-USER-DOB-LK(1:4) TO WS-USER-DOB-YEAR
           COMPUTE WS-AGE = WS-SYS-YEAR - WS-USER-DOB-YEAR
           MOVE WS-AGE TO WS-AGE-OUT
           DISPLAY ' '
           DISPLAY '========================================='
           DISPLAY '            USER INFORMATION             '
           DISPLAY '========================================='
           DISPLAY 'Name            : ' FUNCTION TRIM(WS-USER-NAME-LK)
           DISPLAY 'Email           : ' FUNCTION TRIM(WS-USER-EMAIL-LK)
           DISPLAY 'Phone           : ' FUNCTION TRIM(WS-USER-PHONE-LK)
           DISPLAY 'Postal Code     : ' 
                   FUNCTION TRIM(WS-USER-POSTAL-CODE-LK)
           DISPLAY 'Address         : 'FUNCTION TRIM(WS-USER-ADDRESS-LK)
           DISPLAY 'Date of Birth   : ' WS-USER-DOB-LK(1:4) '-'
                   WS-USER-DOB-LK(5:2) '-' WS-USER-DOB-LK(7:2)
           DISPLAY 'Age             : ' FUNCTION TRIM(WS-AGE-OUT)
                                      ' years'

           PERFORM DISPLAY-COMPLETED-SCREEN.
           DISPLAY-COMPLETED-SCREEN.
           MOVE WS-FINAL-PREMIUM TO WS-FINAL-PREM-DISP.

           DISPLAY "========================================="
           DISPLAY "      APPLICATION SUBMISSION SUCCESS     "
           DISPLAY "========================================="
           DISPLAY "Fields          :"
           DISPLAY "Application ID  : " 
           FUNCTION TRIM(WS-CONTINUE)
           DISPLAY "Applicant Name  : " 
           FUNCTION TRIM(WS-USER-NAME-LK)
           DISPLAY "Plan Code       : " 
           FUNCTION TRIM(WS-PLAN-CODE)
           DISPLAY FUNCTION TRIM(WS-PLAN-CODE) "'s Coverage details:"
           PERFORM VARYING WS-K FROM 1 BY 1
               UNTIL WS-K > WS-COV-COUNT
               IF WS-COV-FLAG(WS-K) = 'Y'
                   MOVE 'YES' TO WS-COV-YES-NO
               ELSE
                   MOVE 'NO'  TO WS-COV-YES-NO
               END-IF
               DISPLAY "  - " FUNCTION TRIM(WS-COV-TYPE(WS-K))
                              ": " WS-COV-YES-NO
           END-PERFORM
           DISPLAY "Final Premium   : " 
           FUNCTION TRIM(WS-FINAL-PREM-DISP) " JPY"
           DISPLAY "Application Status: PENDING"
           DISPLAY " "
           DISPLAY "Message:"
           DISPLAY "       'Your application has been successful."
           DISPLAY "        The underwriting result will be processed"
           DISPLAY "        by the nightly batch.'"
           DISPLAY "--------------------------------------------------".

       END PROGRAM QUOTATION.
       


       



