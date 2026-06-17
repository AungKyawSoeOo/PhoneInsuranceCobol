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
       01 WS-CHOICE          PIC 9(2).
       01 WS-ERROR           PIC X(60).

      *> ==========================================
      *> CURRENCY FORMAT FIELDS
      *> ==========================================
       01 WS-PRICE-DISP      PIC ZZZ,ZZ9.
       01 WS-PREMIUM-DISP    PIC ZZZ,ZZ9.99.
       01 WS-CURRENT-PREM-DISP PIC ZZZ,ZZZ,ZZ9.99.
       01 WS-FINAL-PREM-DISP   PIC ZZZ,ZZZ,ZZ9.99.

      *> ==========================================
      *> DATE FIELDS
      *> ==========================================
       01 WS-YEAR            PIC 9(4).
       01 WS-MONTH           PIC 9(2).
       01 WS-DAY             PIC 9(2).

       01 WS-SYSTEM-DATE.
           05 WS-SYS-YEAR    PIC 9(4).
           05 WS-SYS-MONTH   PIC 9(2).
           05 WS-SYS-DAY     PIC 9(2).
           05 WS-SYS-HOUR    PIC 9(2).
           05 WS-SYS-MINUTE  PIC 9(2).
           05 WS-SYS-SECOND  PIC 9(2).

       01 WS-SYSTEM-DATE-STR PIC X(19).

      *> ==========================================
      *> LEAP YEAR FIELDS
      *> ==========================================
       01 WS-LEAP-YEAR       PIC X VALUE 'N'.
           88 IS-LEAP-YEAR   VALUE 'Y'.
           88 NOT-LEAP-YEAR  VALUE 'N'.

      *> ==========================================
      *> LINKAGE SECTION (accept data from main)
      *> ==========================================
       LINKAGE SECTION.
       01  LK-COMM-AREA.
           05 WS-CONTINUE           PIC X.
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

       PROCEDURE DIVISION USING LK-COMM-AREA.

       MAIN-PROCEDURE.
           PERFORM GET-ONE-QUOTATION
           EXIT PROGRAM.

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
           PERFORM DISPLAY-RESULT
           PERFORM MOVE-TO-LINKAGE.

      *> ==========================================
      *> MOVE DATA TO LINKAGE (Return to Main)
      *> ==========================================
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

      *> ==========================================
      *> DISPLAY WELCOME
      *> ==========================================
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
           DISPLAY 'Enter Phone IMEI (10-15 chars): '
           DISPLAY 'Example: 123456789012345'
           ACCEPT WS-IMEI.

           MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-IMEI)) TO WS-LEN.
           IF WS-LEN < 10 OR WS-LEN > 15
               DISPLAY 'IMEI must be 10-15 characters!'
               PERFORM GET-IMEI
           END-IF.

      *> ==========================================
      *> GET DEVICE TYPE
      *> ==========================================
       GET-DEVICE-TYPE.
           DISPLAY ' '
           DISPLAY 'SELECT DEVICE TYPE:'
           DISPLAY '-----------------------------------------'
           DISPLAY '[1] iPhone'
           DISPLAY '[2] Android'
           DISPLAY '-----------------------------------------'
           DISPLAY 'Enter choice (1-2): '
           ACCEPT WS-CHOICE

           IF WS-CHOICE = 1
               MOVE 'iPhone' TO WS-DEVICE-TYPE
           ELSE
               IF WS-CHOICE = 2
                   MOVE 'Android' TO WS-DEVICE-TYPE
               ELSE
                   DISPLAY 'Invalid device type! (1 or 2 only)'
                   PERFORM GET-DEVICE-TYPE
               END-IF
           END-IF.

      *> ==========================================
      *> GET DEVICE MODEL
      *> ==========================================
       GET-DEVICE-MODEL.
           IF WS-DEVICE-TYPE = 'iPhone'
               PERFORM GET-IPHONE-MODEL
           ELSE
               PERFORM GET-ANDROID-MODEL
           END-IF.

       GET-IPHONE-MODEL.
           DISPLAY ' '
           DISPLAY 'SELECT IPHONE MODEL:'
           DISPLAY '-----------------------------------------'
           DISPLAY '[1] iPhone 13'
           DISPLAY '[2] iPhone 14'
           DISPLAY '[3] iPhone 15'
           DISPLAY '[4] iPhone 15 Pro'
           DISPLAY '[5] iPhone 16 Pro'
           DISPLAY '-----------------------------------------'
           DISPLAY 'Enter choice (1-5): '
           ACCEPT WS-CHOICE

           IF WS-CHOICE = 1
               MOVE 'iPhone 13' TO WS-DEVICE-MODEL
           ELSE
               IF WS-CHOICE = 2
                   MOVE 'iPhone 14' TO WS-DEVICE-MODEL
               ELSE
                   IF WS-CHOICE = 3
                       MOVE 'iPhone 15' TO WS-DEVICE-MODEL
                   ELSE
                       IF WS-CHOICE = 4
                           MOVE 'iPhone 15 Pro' TO WS-DEVICE-MODEL
                       ELSE
                           IF WS-CHOICE = 5
                               MOVE 'iPhone 16 Pro' TO WS-DEVICE-MODEL
                           ELSE
                             DISPLAY 'Invalid iPhone model! (1-5 only)'
                               PERFORM GET-IPHONE-MODEL
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF.

       GET-ANDROID-MODEL.
           DISPLAY ' '
           DISPLAY 'SELECT ANDROID MODEL:'
           DISPLAY '-----------------------------------------'
           DISPLAY '[1] Galaxy S24'
           DISPLAY '[2] Xperia 1 VI'
           DISPLAY '[3] Pixel 8'
           DISPLAY '[4] AQUOS sense'
           DISPLAY '[5] Android Low-end'
           DISPLAY '-----------------------------------------'
           DISPLAY 'Enter choice (1-5): '
           ACCEPT WS-CHOICE

           IF WS-CHOICE = 1
               MOVE 'Galaxy S24' TO WS-DEVICE-MODEL
           ELSE
               IF WS-CHOICE = 2
                   MOVE 'Xperia 1 VI' TO WS-DEVICE-MODEL
               ELSE
                   IF WS-CHOICE = 3
                       MOVE 'Pixel 8' TO WS-DEVICE-MODEL
                   ELSE
                       IF WS-CHOICE = 4
                           MOVE 'AQUOS sense' TO WS-DEVICE-MODEL
                       ELSE
                           IF WS-CHOICE = 5
                               MOVE 'Android Low-end' TO WS-DEVICE-MODEL
                           ELSE
                             DISPLAY 'Invalid Android model! (1-5 only)'
                               PERFORM GET-ANDROID-MODEL
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF.

      *> ==========================================
      *> GET PURCHASE DATE WITH VALIDATION
      *> ==========================================
       GET-PURCHASE-DATE.
           DISPLAY ' '
           DISPLAY 'Enter Purchase Date (YYYY-MM-DD): '
           DISPLAY 'Example: 2026-06-16'
           ACCEPT WS-PURCHASE-DATE.

           PERFORM VALIDATE-DATE.

      *> ==========================================
      *> VALIDATE DATE (Format + Month + Day + Leap Year)
      *> ==========================================
       VALIDATE-DATE.
      *> Check format (YYYY-MM-DD)
           IF WS-PURCHASE-DATE(5:1) NOT = '-' OR
              WS-PURCHASE-DATE(8:1) NOT = '-'
               DISPLAY 'Date format must be YYYY-MM-DD!'
               PERFORM GET-PURCHASE-DATE
               EXIT PARAGRAPH
           END-IF.

      *> Get Year, Month, Day
           MOVE WS-PURCHASE-DATE(1:4) TO WS-YEAR.
           MOVE WS-PURCHASE-DATE(6:2) TO WS-MONTH.
           MOVE WS-PURCHASE-DATE(9:2) TO WS-DAY.

      *> Validate Month (01-12)
           IF WS-MONTH < 1 OR WS-MONTH > 12
               DISPLAY 'Invalid month! Must be 01-12.'
               PERFORM GET-PURCHASE-DATE
               EXIT PARAGRAPH
           END-IF.

      *> Validate Day based on Month
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

      *> ==========================================
      *> CHECK IF YEAR IS LEAP YEAR
      *> ==========================================
       IS-LEAP-YEAR-CHECK.
           MOVE 'N' TO WS-LEAP-YEAR.

      *> Leap year rules:
      *> 1. Year divisible by 400 -> Leap year
      *> 2. Year divisible by 100 -> Not leap year
      *> 3. Year divisible by 4 -> Leap year

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

           EVALUATE WS-CHOICE
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
      *> CALCULATE PREMIUM (CALL SUB-PROGRAM)
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
               WS-PLAN-MAX-PAYOUT.

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
      *> DISPLAY RESULT (With Currency Format)
      *> ==========================================
       DISPLAY-RESULT.
      *> Format numbers with commas
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
           DISPLAY 'Selected Plan   : ' WS-PLAN-CODE ' '
                   WS-PLAN-NAME
           DISPLAY 'Plan Base Rate  : ' WS-PLAN-BASE-RATE
           DISPLAY 'Plan Max Payout : ' WS-PLAN-MAX-PAYOUT ' JPY'
           DISPLAY 'Current Premium: '
                   FUNCTION TRIM(WS-CURRENT-PREM-DISP) ' JPY'
           DISPLAY 'Final Premium  : '
                   FUNCTION TRIM(WS-FINAL-PREM-DISP) ' JPY'
           DISPLAY 'System Date     : ' WS-SYSTEM-DATE-STR
           DISPLAY 'Status          : PENDING'
           DISPLAY '========================================='.

       END PROGRAM QUOTATION.



