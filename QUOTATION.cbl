       IDENTIFICATION DIVISION.
       PROGRAM-ID. QUOTATION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-LEN           PIC 99.
       01 WS-FOUND         PIC X VALUE 'N'.

       01 WS-SYSTEM-DATE.
           05 WS-SYS-YEAR     PIC 9(4).
           05 WS-SYS-MONTH    PIC 9(2).
           05 WS-SYS-DAY      PIC 9(2).
           05 WS-SYS-HOUR     PIC 9(2).
           05 WS-SYS-MINUTE   PIC 9(2).
           05 WS-SYS-SECOND   PIC 9(2).

       01 WS-ERROR            PIC X(60).
       01 WS-CHOICE           PIC 9(2).

       LINKAGE SECTION.
       01  LK-COMM-AREA.
           05 WS-CONTINUE           PIC X.
           05 WS-DEVICE-DATA.
              10 WS-IMEI            PIC X(15).
              10 WS-DEVICE-TYPE     PIC X(10).
              10 WS-DEVICE-MODEL    PIC X(20).
              10 WS-PURCHASE-DATE   PIC X(10).
              10 WS-PRICE           PIC 9(6).
           05 WS-CALC-RESULTS.
              10 WS-PRICE-CATEGORY  PIC X(6).
              10 WS-PERIOD          PIC X(3).
              10 WS-PERIOD-FACTOR   PIC 9V99.
              10 WS-PERIOD-MONTHS   PIC 99.
              10 WS-EST-PREMIUM     PIC 9(9)V99.
           05 WS-SYSTEM-DATE-STR    PIC X(19).

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
           PERFORM GEN-SYS-DATE
           PERFORM DISPLAY-RESULT.

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
      *> GET PURCHASE DATE
      *> ==========================================
       GET-PURCHASE-DATE.
           DISPLAY ' '
           DISPLAY 'Enter Purchase Date (YYYY-MM-DD): '
           DISPLAY 'Example: 2026-06-16'
           ACCEPT WS-PURCHASE-DATE.

           IF WS-PURCHASE-DATE(5:1) NOT = '-' OR
              WS-PURCHASE-DATE(8:1) NOT = '-'
               DISPLAY 'Date format must be YYYY-MM-DD!'
               PERFORM GET-PURCHASE-DATE
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
           DISPLAY '[1] 12 months'
           DISPLAY '[2] 24 months'
           DISPLAY '[3] 36 months'
           DISPLAY '-----------------------------------------'
           DISPLAY 'Enter choice (1-3): '
           ACCEPT WS-CHOICE

           IF WS-CHOICE = 1
               MOVE 'P12' TO WS-PERIOD
               MOVE 12 TO WS-PERIOD-MONTHS
               MOVE 1.0 TO WS-PERIOD-FACTOR
           ELSE
               IF WS-CHOICE = 2
                   MOVE 'P24' TO WS-PERIOD
                   MOVE 24 TO WS-PERIOD-MONTHS
                   MOVE 1.8 TO WS-PERIOD-FACTOR
               ELSE
                   IF WS-CHOICE = 3
                       MOVE 'P36' TO WS-PERIOD
                       MOVE 36 TO WS-PERIOD-MONTHS
                       MOVE 2.5 TO WS-PERIOD-FACTOR
                   ELSE
                       DISPLAY 'Invalid period! (1, 2, or 3 only)'
                       PERFORM GET-COVERAGE-PERIOD
                   END-IF
               END-IF
           END-IF.

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
           COMPUTE WS-EST-PREMIUM = WS-PRICE * 0.02 * WS-PERIOD-FACTOR.

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
           DISPLAY ' '
           DISPLAY '========================================='
           DISPLAY '            QUOTATION RESULT             '
           DISPLAY '========================================='
           DISPLAY 'IMEI            : ' WS-IMEI
           DISPLAY 'Device Type     : ' WS-DEVICE-TYPE
           DISPLAY 'Device Model    : ' WS-DEVICE-MODEL
           DISPLAY 'Purchase Date   : ' WS-PURCHASE-DATE
           DISPLAY 'Purchase Price  : ' WS-PRICE ' JPY'
           DISPLAY 'Price Category  : ' WS-PRICE-CATEGORY
           DISPLAY 'Coverage Period : ' WS-PERIOD-MONTHS ' months'
           DISPLAY 'Period Multiplier: ' WS-PERIOD-FACTOR
           DISPLAY '-----------------------------------------'
           DISPLAY 'Estimated Premium: ' WS-EST-PREMIUM ' JPY'
           DISPLAY 'System Date     : ' WS-SYSTEM-DATE-STR
           DISPLAY 'Status          : PENDING'
           DISPLAY '=========================================='.

       END PROGRAM QUOTATION.
