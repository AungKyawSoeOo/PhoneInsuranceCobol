       IDENTIFICATION DIVISION.
       PROGRAM-ID. DEVICE-READER.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DEVICE-FILE ASSIGN TO
               './files/device-master.txt'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD DEVICE-FILE.
       01 WS-FILE-RECORD      PIC X(80).

       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS      PIC XX.
           88 WS-FILE-OK      VALUE '00'.
           88 WS-FILE-EOF     VALUE '10'.

       01 WS-RECORD-COUNT     PIC 99 VALUE 0.
       01 WS-TYPE-COUNT       PIC 99 VALUE 0.
       01 WS-MODEL-COUNT      PIC 99 VALUE 0.

       01 WS-FOUND            PIC X VALUE 'N'.
           88 WS-FOUND-YES    VALUE 'Y'.
           88 WS-FOUND-NO     VALUE 'N'.

       01 WS-RECORD-FIELDS.
           05 WS-DEVICE-TYPE-CODE   PIC 9.
           05 WS-DEVICE-TYPE-NAME   PIC X(10).
           05 WS-DEVICE-MODEL-NAME  PIC X(20).

       01 WS-DEVICE-LIST.
           05 WS-DEVICE-ENTRY OCCURS 50 TIMES.
               10 WS-ENTRY-TYPE-CODE  PIC 9.
               10 WS-ENTRY-TYPE-NAME  PIC X(10).
               10 WS-ENTRY-MODEL-NAME PIC X(20).

       01 WS-TYPE-LIST.
           05 WS-TYPE-ENTRY OCCURS 10 TIMES.
               10 WS-TYPE-CODE        PIC 9.
               10 WS-TYPE-NAME        PIC X(10).

       01 WS-MODEL-LIST.
           05 WS-MODEL-ENTRY OCCURS 20 TIMES.
               10 WS-MODEL-NAME       PIC X(20).

       01 WS-I                PIC 99.
       01 WS-J                PIC 99.

       LINKAGE SECTION.
       01 LK-ACTION           PIC X.
           88 LK-LOAD         VALUE 'L'.
           88 LK-SEARCH       VALUE 'S'.
           88 LK-GET-TYPES    VALUE 'T'.
           88 LK-GET-MODELS   VALUE 'M'.

       01 LK-DEVICE-TYPE-CODE PIC 9.
       01 LK-DEVICE-MODEL     PIC X(20).

       01 LK-DEVICE-TYPE-NAME PIC X(10).
       01 LK-MODEL-FOUND      PIC X VALUE 'N'.
           88 LK-MODEL-FOUND-YES VALUE 'Y'.

       01 LK-TYPE-LIST.
           05 LK-TYPE-COUNT   PIC 99.
           05 LK-TYPE-ENTRY OCCURS 10 TIMES.
               10 LK-TYPE-CODE PIC 9.
               10 LK-TYPE-NAME PIC X(10).

       01 LK-MODEL-LIST.
           05 LK-MODEL-COUNT  PIC 99.
           05 LK-MODEL-ENTRY OCCURS 20 TIMES.
               10 LK-MODEL-NAME PIC X(20).

       PROCEDURE DIVISION USING LK-ACTION, LK-DEVICE-TYPE-CODE,
                                LK-DEVICE-MODEL, LK-DEVICE-TYPE-NAME,
                                LK-MODEL-FOUND, LK-TYPE-LIST,
                                LK-MODEL-LIST.

       MAIN-PROCEDURE.
           IF LK-LOAD
               PERFORM LOAD-DEVICE-FILE
           END-IF.

           IF LK-GET-TYPES
               PERFORM GET-TYPE-LIST
           END-IF.

           IF LK-GET-MODELS
               PERFORM GET-MODEL-LIST
           END-IF.

           IF LK-SEARCH
               PERFORM SEARCH-DEVICE
           END-IF.

           EXIT PROGRAM.

       LOAD-DEVICE-FILE.
           MOVE 0 TO WS-RECORD-COUNT.
           MOVE SPACES TO WS-DEVICE-LIST.

           OPEN INPUT DEVICE-FILE

           IF NOT WS-FILE-OK
               DISPLAY 'Error opening device-master.txt!'
               EXIT PARAGRAPH
           END-IF.

           PERFORM UNTIL WS-FILE-EOF
               READ DEVICE-FILE INTO WS-FILE-RECORD
               AT END
                   SET WS-FILE-EOF TO TRUE
                   EXIT PERFORM
               NOT AT END
                   PERFORM PARSE-RECORD
               END-READ
           END-PERFORM.

           CLOSE DEVICE-FILE.

       PARSE-RECORD.
           UNSTRING WS-FILE-RECORD DELIMITED BY ','
               INTO WS-DEVICE-TYPE-CODE,
                    WS-DEVICE-TYPE-NAME,
                    WS-DEVICE-MODEL-NAME
           .

           ADD 1 TO WS-RECORD-COUNT.
           MOVE WS-DEVICE-TYPE-CODE TO
               WS-ENTRY-TYPE-CODE(WS-RECORD-COUNT)
           MOVE WS-DEVICE-TYPE-NAME TO
               WS-ENTRY-TYPE-NAME(WS-RECORD-COUNT)
           MOVE WS-DEVICE-MODEL-NAME TO
               WS-ENTRY-MODEL-NAME(WS-RECORD-COUNT).

       GET-TYPE-LIST.
           MOVE 0 TO WS-TYPE-COUNT.
           MOVE 0 TO LK-TYPE-COUNT.

           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-RECORD-COUNT

               MOVE 'N' TO WS-FOUND
               PERFORM VARYING WS-J FROM 1 BY 1
                   UNTIL WS-J > WS-TYPE-COUNT
                   IF WS-TYPE-CODE(WS-J) =
                      WS-ENTRY-TYPE-CODE(WS-I)
                       MOVE 'Y' TO WS-FOUND
                       EXIT PERFORM
                   END-IF
               END-PERFORM

               IF WS-FOUND = 'N'
                   ADD 1 TO WS-TYPE-COUNT
                   MOVE WS-ENTRY-TYPE-CODE(WS-I)
                     TO WS-TYPE-CODE(WS-TYPE-COUNT)
                   MOVE WS-ENTRY-TYPE-NAME(WS-I)
                     TO WS-TYPE-NAME(WS-TYPE-COUNT)
               END-IF
           END-PERFORM.

           MOVE WS-TYPE-COUNT TO LK-TYPE-COUNT.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-TYPE-COUNT
               MOVE WS-TYPE-CODE(WS-I)
                 TO LK-TYPE-CODE(WS-I)
               MOVE WS-TYPE-NAME(WS-I)
                 TO LK-TYPE-NAME(WS-I)
           END-PERFORM.

       GET-MODEL-LIST.
           MOVE 0 TO WS-MODEL-COUNT.
           MOVE 0 TO LK-MODEL-COUNT.

           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-RECORD-COUNT
               IF WS-ENTRY-TYPE-CODE(WS-I) =
                  LK-DEVICE-TYPE-CODE
                   ADD 1 TO WS-MODEL-COUNT
                   MOVE WS-ENTRY-MODEL-NAME(WS-I)
                     TO WS-MODEL-NAME(WS-MODEL-COUNT)
               END-IF
           END-PERFORM.

           MOVE WS-MODEL-COUNT TO LK-MODEL-COUNT.
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-MODEL-COUNT
               MOVE WS-MODEL-NAME(WS-I)
                 TO LK-MODEL-NAME(WS-I)
           END-PERFORM.

       SEARCH-DEVICE.
           MOVE 'N' TO LK-MODEL-FOUND.

           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-RECORD-COUNT
               OR LK-MODEL-FOUND-YES

               IF WS-ENTRY-TYPE-CODE(WS-I) =
                  LK-DEVICE-TYPE-CODE
                   IF WS-ENTRY-MODEL-NAME(WS-I) =
                      LK-DEVICE-MODEL
                       MOVE WS-ENTRY-TYPE-NAME(WS-I)
                         TO LK-DEVICE-TYPE-NAME
                       SET LK-MODEL-FOUND-YES TO TRUE
                   END-IF
               END-IF
           END-PERFORM.

       END PROGRAM DEVICE-READER.
