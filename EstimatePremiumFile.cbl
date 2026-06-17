       IDENTIFICATION DIVISION.
       PROGRAM-ID. EstimatePremiumFile.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT QUOTE-FILE ASSIGN TO "QUOTATION_HISTORY.TXT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD QUOTE-FILE.
       01 QUOTE-REC.
           05 QR-APP-ID          PIC X(10).
           05 QR-IMEI            PIC X(15).
           05 QR-DEVICE-TYPE     PIC X(10).
           05 QR-DEVICE-MODEL    PIC X(20).
           05 QR-PRICE           PIC 9(6).
           05 QR-PRICE-CATEGORY  PIC X(6).
           05 QR-PERIOD          PIC X(3).
           05 QR-PERIOD-MONTHS   PIC 99.
           05 QR-PERIOD-FACTOR   PIC 9V99.
           05 QR-PURCHASE-DATE   PIC X(10).
           05 QR-SYSTEM-DATE     PIC X(19).
           05 QR-EST-PREMIUM     PIC 9(9)V99.
           05 QR-STATUS          PIC X(10).

       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS   PIC XX.
       01 WS-EOF           PIC X VALUE 'N'.
       01 WS-COUNT         PIC 9(4) VALUE 0.
       01 WS-CONTINUE      PIC X.
       01 WS-FOUND         PIC X VALUE 'N'.
       01 WS-I             PIC 99.
       01 WS-LEN           PIC 99.

       01 WS-IMEI             PIC X(15).
       01 WS-DEVICE-TYPE      PIC X(10).
       01 WS-DEVICE-MODEL     PIC X(20).
       01 WS-PURCHASE-DATE    PIC X(10).
       01 WS-PRICE            PIC 9(6).
       01 WS-PRICE-CATEGORY   PIC X(6).
       01 WS-PERIOD           PIC X(3).
       01 WS-PERIOD-FACTOR    PIC 9V99.
       01 WS-PERIOD-MONTHS    PIC 99.
       01 WS-BASE-RATE        PIC 9V99 VALUE 0.02.
       01 WS-EST-PREMIUM      PIC 9(9)V99.

       01 WS-SYSTEM-DATE.
           05 WS-SYS-YEAR     PIC 9(4).
           05 WS-SYS-MONTH    PIC 9(2).
           05 WS-SYS-DAY      PIC 9(2).
           05 WS-SYS-HOUR     PIC 9(2).
           05 WS-SYS-MINUTE   PIC 9(2).
           05 WS-SYS-SECOND   PIC 9(2).

       01 WS-SYSTEM-DATE-STR  PIC X(19).
       01 WS-APP-ID           PIC X(10).
       01 WS-APP-NUM          PIC 9(4).
       01 WS-VALID            PIC X VALUE 'Y'.
       01 WS-ERROR            PIC X(50).
       01 WS-CHOICE           PIC 9(2).

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           MOVE 'Y' TO WS-CONTINUE.
           PERFORM UNTIL WS-CONTINUE = 'N' OR WS-CONTINUE = 'n'
               PERFORM GET-ONE-QUOTATION
           END-PERFORM
           DISPLAY ' '
           DISPLAY '========================================='
           DISPLAY '    Thank you for using our service!    '
           DISPLAY '========================================='
           STOP RUN.

       ASK-CONTINUE.
           DISPLAY ' '
           DISPLAY 'Do you want to add another quotation? (Y/N): '.
           ACCEPT WS-CONTINUE.

       GET-ONE-QUOTATION.
           PERFORM GENERATE-APP-ID
           PERFORM DISPLAY-WELCOME
           PERFORM GET-IMEI
           PERFORM VALIDATE-IMEI
           IF WS-VALID = 'N'
               PERFORM DISPLAY-ERROR
               PERFORM GET-IMEI
               PERFORM VALIDATE-IMEI
           END-IF

           IF WS-VALID = 'Y'
               PERFORM GET-DEVICE-TYPE
               PERFORM GET-DEVICE-MODEL
               PERFORM GET-PURCHASE-DATE
               PERFORM GET-PURCHASE-PRICE
               PERFORM GET-COVERAGE-PERIOD
               PERFORM VALIDATE-INPUT
           END-IF

           IF WS-VALID = 'Y'
               PERFORM CALCULATE-PREMIUM
               PERFORM GENERATE-SYSTEM-DATE
               PERFORM SAVE-TO-FILE
               PERFORM DISPLAY-RESULT
               PERFORM ASK-CONTINUE
           ELSE
               PERFORM DISPLAY-ERROR
           END-IF.

      *> ==========================================
      *> GENERATE APPLICATION ID
      *> ==========================================
       GENERATE-APP-ID.
           MOVE 0 TO WS-COUNT.

           OPEN INPUT QUOTE-FILE.

           IF WS-FILE-STATUS = '35'
               OPEN OUTPUT QUOTE-FILE
               CLOSE QUOTE-FILE
               MOVE 0 TO WS-COUNT
           ELSE
               IF WS-FILE-STATUS = '00'
                   MOVE 'N' TO WS-EOF
                   PERFORM UNTIL WS-EOF = 'Y'
                       READ QUOTE-FILE INTO QUOTE-REC
                           AT END MOVE 'Y' TO WS-EOF
                           NOT AT END ADD 1 TO WS-COUNT
                       END-READ
                   END-PERFORM
                   CLOSE QUOTE-FILE
               END-IF
           END-IF.

           COMPUTE WS-APP-NUM = WS-COUNT + 1.
           STRING 'Q' WS-APP-NUM INTO WS-APP-ID.

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
           DISPLAY 'Enter Phone IMEI (10-15 chars, letters & numbers): '
           DISPLAY 'Example: 123456789012345'
           ACCEPT WS-IMEI.

      *> ==========================================
      *> VALIDATE IMEI (Length 10-15, No Duplicate)
      *> ==========================================
       VALIDATE-IMEI.
           MOVE 'Y' TO WS-VALID.

      *> Check length
           MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-IMEI)) TO WS-LEN.
           IF WS-LEN < 10 OR WS-LEN > 15
               MOVE 'N' TO WS-VALID
               MOVE 'IMEI must be 10-15 characters!' TO WS-ERROR
               EXIT PARAGRAPH
           END-IF.

      *> Check duplicate
           MOVE 'N' TO WS-FOUND.
           OPEN INPUT QUOTE-FILE.

           IF WS-FILE-STATUS = '00'
               MOVE 'N' TO WS-EOF
               PERFORM UNTIL WS-EOF = 'Y'
                   READ QUOTE-FILE INTO QUOTE-REC
                       AT END MOVE 'Y' TO WS-EOF
                       NOT AT END
                           IF QR-IMEI = WS-IMEI
                               MOVE 'Y' TO WS-FOUND
                               MOVE 'N' TO WS-VALID
                               MOVE 'IMEI already exists!' TO WS-ERROR
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE QUOTE-FILE
           END-IF.

      *> ==========================================
      *> 1. GET DEVICE TYPE
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

           EVALUATE WS-CHOICE
               WHEN 1
                   MOVE 'iPhone' TO WS-DEVICE-TYPE
               WHEN 2
                   MOVE 'Android' TO WS-DEVICE-TYPE
               WHEN OTHER
                   MOVE 'N' TO WS-VALID
                   MOVE 'Invalid device type!' TO WS-ERROR
           END-EVALUATE.

      *> ==========================================
      *> 2. GET DEVICE MODEL
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

           EVALUATE WS-CHOICE
               WHEN 1
                   MOVE 'iPhone 13' TO WS-DEVICE-MODEL
               WHEN 2
                   MOVE 'iPhone 14' TO WS-DEVICE-MODEL
               WHEN 3
                   MOVE 'iPhone 15' TO WS-DEVICE-MODEL
               WHEN 4
                   MOVE 'iPhone 15 Pro' TO WS-DEVICE-MODEL
               WHEN 5
                   MOVE 'iPhone 16 Pro' TO WS-DEVICE-MODEL
               WHEN OTHER
                   MOVE 'N' TO WS-VALID
                   MOVE 'Invalid iPhone model!' TO WS-ERROR
           END-EVALUATE.

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

           EVALUATE WS-CHOICE
               WHEN 1
                   MOVE 'Galaxy S24' TO WS-DEVICE-MODEL
               WHEN 2
                   MOVE 'Xperia 1 VI' TO WS-DEVICE-MODEL
               WHEN 3
                   MOVE 'Pixel 8' TO WS-DEVICE-MODEL
               WHEN 4
                   MOVE 'AQUOS sense' TO WS-DEVICE-MODEL
               WHEN 5
                   MOVE 'Android Low-end' TO WS-DEVICE-MODEL
               WHEN OTHER
                   MOVE 'N' TO WS-VALID
                   MOVE 'Invalid Android model!' TO WS-ERROR
           END-EVALUATE.

      *> ==========================================
      *> 3. GET PURCHASE DATE
      *> ==========================================
       GET-PURCHASE-DATE.
           DISPLAY ' '
           DISPLAY 'Enter Purchase Date (YYYY-MM-DD): '
           DISPLAY 'Example: 2026-06-16'
           ACCEPT WS-PURCHASE-DATE.

      *> ==========================================
      *> 4. GET PURCHASE PRICE
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

      *> ==========================================
      *> 5. GET COVERAGE PERIOD (12/24/36 only)
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

           EVALUATE WS-CHOICE
               WHEN 1
                   MOVE 'P12' TO WS-PERIOD
                   MOVE 12 TO WS-PERIOD-MONTHS
                   MOVE 1.0 TO WS-PERIOD-FACTOR
               WHEN 2
                   MOVE 'P24' TO WS-PERIOD
                   MOVE 24 TO WS-PERIOD-MONTHS
                   MOVE 1.8 TO WS-PERIOD-FACTOR
               WHEN 3
                   MOVE 'P36' TO WS-PERIOD
                   MOVE 36 TO WS-PERIOD-MONTHS
                   MOVE 2.5 TO WS-PERIOD-FACTOR
               WHEN OTHER
                   MOVE 'N' TO WS-VALID
              MOVE 'Invalid period choice! (12/24/36 only)' TO WS-ERROR
           END-EVALUATE.

      *> ==========================================
      *> 6. VALIDATE INPUT
      *> ==========================================
       VALIDATE-INPUT.
           IF WS-PRICE = 0
               MOVE 'N' TO WS-VALID
               MOVE 'Price is required!' TO WS-ERROR
               EXIT PARAGRAPH
           END-IF.

           IF WS-PRICE < 10000 OR WS-PRICE > 200000
               MOVE 'N' TO WS-VALID
               MOVE 'Price must be between 10,000 and 200,000 JPY'
                   TO WS-ERROR
               EXIT PARAGRAPH
           END-IF.

           IF WS-PURCHASE-DATE = SPACES
               MOVE 'N' TO WS-VALID
               MOVE 'Purchase Date is required!' TO WS-ERROR
               EXIT PARAGRAPH
           END-IF.

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
      *> 7. CALCULATE PREMIUM
      *> ==========================================
       CALCULATE-PREMIUM.
           COMPUTE WS-EST-PREMIUM =
               WS-PRICE * WS-BASE-RATE * WS-PERIOD-FACTOR.

      *> ==========================================
      *> 8. GENERATE SYSTEM DATE
      *> ==========================================
       GENERATE-SYSTEM-DATE.
           MOVE FUNCTION CURRENT-DATE TO WS-SYSTEM-DATE.
           STRING WS-SYS-YEAR '-' WS-SYS-MONTH '-' WS-SYS-DAY
                  ' ' WS-SYS-HOUR ':' WS-SYS-MINUTE ':' WS-SYS-SECOND
               INTO WS-SYSTEM-DATE-STR.

      *> ==========================================
      *> 9. SAVE TO FILE
      *> ==========================================
       SAVE-TO-FILE.
           OPEN EXTEND QUOTE-FILE.

           MOVE WS-APP-ID TO QR-APP-ID
           MOVE WS-IMEI TO QR-IMEI
           MOVE WS-DEVICE-TYPE TO QR-DEVICE-TYPE
           MOVE WS-DEVICE-MODEL TO QR-DEVICE-MODEL
           MOVE WS-PRICE TO QR-PRICE
           MOVE WS-PRICE-CATEGORY TO QR-PRICE-CATEGORY
           MOVE WS-PERIOD TO QR-PERIOD
           MOVE WS-PERIOD-MONTHS TO QR-PERIOD-MONTHS
           MOVE WS-PERIOD-FACTOR TO QR-PERIOD-FACTOR
           MOVE WS-PURCHASE-DATE TO QR-PURCHASE-DATE
           MOVE WS-SYSTEM-DATE-STR TO QR-SYSTEM-DATE
           MOVE WS-EST-PREMIUM TO QR-EST-PREMIUM
           MOVE 'PENDING' TO QR-STATUS

           WRITE QUOTE-REC
           CLOSE QUOTE-FILE.

      *> ==========================================
      *> 10. DISPLAY RESULT
      *> ==========================================
       DISPLAY-RESULT.
           DISPLAY ' '
           DISPLAY '========================================='
           DISPLAY '            QUOTATION RESULT             '
           DISPLAY '========================================='
           DISPLAY 'Application ID  : ' WS-APP-ID
           DISPLAY 'IMEI            : ' WS-IMEI
           DISPLAY 'Device Type     : ' WS-DEVICE-TYPE
           DISPLAY 'Device Model    : ' WS-DEVICE-MODEL
           DISPLAY 'Purchase Date   : ' WS-PURCHASE-DATE
           DISPLAY 'Purchase Price  : ' WS-PRICE ' JPY'
           DISPLAY 'Price Category  : ' WS-PRICE-CATEGORY
           DISPLAY 'Coverage Period : ' WS-PERIOD-MONTHS ' months'
           DISPLAY 'Period Multiplier: ' WS-PERIOD-FACTOR
           DISPLAY '-----------------------------------------'
           DISPLAY 'Base Rate       : ' WS-BASE-RATE
           DISPLAY 'Estimated Premium: ' WS-EST-PREMIUM ' JPY'
           DISPLAY 'System Date     : ' WS-SYSTEM-DATE-STR
           DISPLAY 'Status          : PENDING'
           DISPLAY '========================================='
           DISPLAY 'Data saved to: QUOTATION_HISTORY.TXT'.

       DISPLAY-ERROR.
           DISPLAY ' '
           DISPLAY '========================================='
           DISPLAY 'ERROR: ' WS-ERROR
           DISPLAY 'Please try again.'
           DISPLAY '========================================='.

       END PROGRAM EstimatePremiumFile.
