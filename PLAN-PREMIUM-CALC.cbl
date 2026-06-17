       IDENTIFICATION DIVISION.
       PROGRAM-ID. PLAN-PREMIUM-CALC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-PLAN-RATE-CALC         PIC 9V999.

       LINKAGE SECTION.
       01 LK-PRICE                  PIC 9(6).
       01 LK-PERIOD-FACTOR          PIC 9V99.
       01 LK-EST-PREMIUM            PIC 9(9)V99.
       01 LK-PLAN-BASE-RATE         PIC 99999.
       01 LK-CURRENT-PREMIUM        PIC 9(9)V99.
       01 LK-FINAL-PREMIUM          PIC 9(9)V99.

       PROCEDURE DIVISION USING LK-PRICE,
                                LK-PERIOD-FACTOR,
                                LK-EST-PREMIUM,
                                LK-PLAN-BASE-RATE,
                                LK-CURRENT-PREMIUM,
                                LK-FINAL-PREMIUM.

       MAIN-PROCEDURE.
           COMPUTE WS-PLAN-RATE-CALC = LK-PLAN-BASE-RATE / 10000

           COMPUTE LK-CURRENT-PREMIUM =
               LK-PRICE * WS-PLAN-RATE-CALC * LK-PERIOD-FACTOR

           COMPUTE LK-FINAL-PREMIUM =
               LK-EST-PREMIUM + LK-CURRENT-PREMIUM

           EXIT PROGRAM.

       END PROGRAM PLAN-PREMIUM-CALC.
