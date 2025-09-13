      ******************************************************************
      * Author: 6621600461, 6621604548
      * Date: 08-09-2025
      * Purpose: Library Management System (Final Mini Project)
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LIBRARY-PROJECT.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BOOK-FILE        ASSIGN TO "books.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT MEMBER-FILE      ASSIGN TO "members.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT STAFF-FILE       ASSIGN TO "staff.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TRANSACTION-FILE ASSIGN TO "transactions.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT BOOK-TEMP-FILE   ASSIGN TO "book-temp.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TR-TEMP-FILE     ASSIGN TO "tr-temp.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  BOOK-FILE.
       01  BOOK-RECORD.
           05  ISBN                        PIC 9(13).
           05  CONGRESS                    PIC X(35).
           05  TITLE                       PIC X(100).
           05  COPY-NO                     PIC 9(2).
           05  BOOK-STATUS                 PIC X(13).
       FD  MEMBER-FILE.
       01  MEMBER-RECORD.
           05  MEMBER-ID                   PIC 9(14).
           05  MEMBER-F-NAME               PIC X(10).
           05  MEMBER-L-NAME               PIC X(15).
       FD  STAFF-FILE.
       01  STAFF-RECORD.
           05  STAFF-ID                    PIC 9(14).
           05  STAFF-F-NAME                PIC X(10).
           05  STAFF-L-NAME                PIC X(15).
       FD  TRANSACTION-FILE.
       01  TRANSACTION-RECORD.
           05  TR-ID                       PIC 999.
           05  TR-ISBN                     PIC 9(13).
           05  TR-MEMBER-ID                PIC 9(14).
           05  TR-BORROW-DATE.
               07 TR-BORROW-Y              PIC 9999.
               07 TR-BORROW-M              PIC 99.
               07 TR-BORROW-D              PIC 99.
           05  TR-DUE-DATE.
               07 TR-DUE-Y                 PIC 9999.
               07 TR-DUE-M                 PIC 99.
               07 TR-DUE-D                 PIC 99.
           05  TR-RETURN-DATE.
               07 TR-RETURN-Y              PIC 9999.
               07 TR-RETURN-M              PIC 99.
               07 TR-RETURN-D              PIC 99.
           05  TR-RETURN-STATUS            PIC X(7).
           05  TR-FINE                     PIC 9(5).
       FD  BOOK-TEMP-FILE.
       01  BOOK-TEMP-RECORD.
           05  ISBN-TEMP-R                 PIC 9(13).
           05  CONGRESS-TEMP-R             PIC X(35).
           05  TITLE-TEMP-R                PIC X(100).
           05  COPY-NO-TEMP-R              PIC 9(2).
           05  BOOK-STATUS-TEMP-R          PIC X(13).
       FD  TR-TEMP-FILE.
       01  TR-TEMP-RECORD.
           05  TR-ID-TEMP-R                PIC 999.
           05  TR-ISBN-TEMP-R              PIC 9(13).
           05  TR-MEMBER-ID-TEMP-R         PIC 9(14).
           05  TR-BORROW-DATE-TEMP-R.
               07 TR-BORROW-TEMP-Y         PIC 9999.
               07 TR-BORROW-TEMP-M         PIC 99.
               07 TR-BORROW-TEMP-D         PIC 99.
           05  TR-DUE-DATE-TEMP-R.
               07 TR-DUE-TEMP-Y            PIC 9999.
               07 TR-DUE-TEMP-M            PIC 99.
               07 TR-DUE-TEMP-D            PIC 99.
           05  TR-RETURN-DATE-TEMP-R.
               07 TR-RETURN-TEMP-Y         PIC 9999.
               07 TR-RETURN-TEMP-M         PIC 99.
               07 TR-RETURN-TEMP-D         PIC 99.
           05  TR-RETURN-STATUS-TEMP-R     PIC X(7).
           05  TR-FINE-TEMP-R              PIC 9(5).
       WORKING-STORAGE SECTION.
       01  WS-FLAGS.
           05  WS-EOF-FLAG                 PIC A(1)    VALUE 'N'.
               88  WS-END-OF-FILE                      VALUE 'Y'.
               88  WS-NOT-END-OF-FILE                  VALUE 'N'.
       01  INPUT-VARS.
           05  ROLE                        PIC X(6).
           05  CHOICE                      PIC 9(1)    VALUE ZERO.
           05  INPUT-MEM-ID                PIC 9(14).
           05  INPUT-STAFF-ID              PIC 9(14).
           05  INPUT-ISBN                  PIC 9(13).
           05  INPUT-ACCEPT                PIC X(1).
           05  INPUT-TR-ID                 PIC 999.
           05  BORROW-DATE.
               07  BORROW-Y                PIC 9999.
               07  BORROW-M                PIC 99.
               07  BORROW-D                PIC 99.
           05  RETURN-DATE.
               07  RETURN-Y                PIC 9999.
               07  RETURN-M                PIC 99.
               07  RETURN-D                PIC 99.
       01  TEMP-VARS.
           05  FOUND-COUNT                 PIC 9(1).
           05  WS-UPDATE                   PIC A      VALUE 'N'.
           05  TOTAL-DAYS                  PIC 9(4).
           05  DIFF-MONTH                  PIC 9(4)   VALUE ZERO.
           05  TOTAL-MONTHS                PIC 9(4)   VALUE ZERO.
           05  DAYS-OF-THE-MONTH           PIC 99.
           05  DUE-DATE-TEMP-D             PIC 99.
           05  DIFF-DATE                   PIC 99.
       01  EDT-VARS.
           05  EDT-COPY-NO                 PIC Z9.
           05  EDT-ST                      PIC X(25).
           05  EDT-ST-DATE                 PIC X(8).
           05  EDT-FINE                    PIC Z(4)9.
           05  EDT-TR-ID                   PIC Z(2)9.
           05  EDT-TOTAL-DAYS              PIC Z(3)9.
       01  WRITE-BOOK-TEMP-RECORD.
           05  ISBN-TEMP                   PIC 9(13).
           05  CONGRESS-TEMP               PIC X(35).
           05  TITLE-TEMP                  PIC X(100).
           05  COPY-NO-TEMP                PIC 9(2).
           05  BOOK-STATUS-TEMP            PIC X(13).
       01  WRITE-TR-TEMP-RECORD.
           05  TR-ID-TEMP                  PIC 999.
           05  TR-ISBN-TEMP                PIC 9(13).
           05  TR-MEMBER-ID-TEMP           PIC 9(14).
           05  TR-BORROW-DATE-TEMP.
               07 TR-BORROW-TEMP-Y         PIC 9999.
               07 TR-BORROW-TEMP-M         PIC 99.
               07 TR-BORROW-TEMP-D         PIC 99.
           05  TR-DUE-DATE-TEMP.
               07 TR-DUE-TEMP-Y            PIC 9999.
               07 TR-DUE-TEMP-M            PIC 99.
               07 TR-DUE-TEMP-D            PIC 99.
           05  TR-RETURN-DATE-TEMP.
               07 TR-RETURN-TEMP-Y         PIC 9999.
               07 TR-RETURN-TEMP-M         PIC 99.
               07 TR-RETURN-TEMP-D         PIC 99.
           05  TR-RETURN-STATUS-TEMP       PIC X(7).
           05  TR-FINE-TEMP                PIC 9(5).
       01  NEW-WRITE-TR-RECORD.
           05  NEW-TR-ID                   PIC 999.
           05  NEW-TR-ISBN                 PIC 9(13).
           05  NEW-TR-MEMBER-ID            PIC 9(14).
           05  NEW-TR-BORROW-DATE.
               07 NEW-TR-BORROW-Y          PIC 9999.
               07 NEW-TR-BORROW-M          PIC 99.
               07 NEW-TR-BORROW-D          PIC 99.
           05  NEW-TR-DUE-DATE.
               07 NEW-TR-DUE-Y             PIC 9999.
               07 NEW-TR-DUE-M             PIC 99.
               07 NEW-TR-DUE-D             PIC 99.
           05  NEW-TR-RETURN-DATE.
               07 NEW-TR-RETURN-Y          PIC 9999.
               07 NEW-TR-RETURN-M          PIC 99.
               07 NEW-TR-RETURN-D          PIC 99.
           05  NEW-TR-RETURN-STATUS        PIC X(7).
           05  NEW-TR-FINE                 PIC 9(5).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
      *>       PERFORM 0000MAIN-LOGIN.
      *>       PERFORM 1000MAIN-MENU.
            PERFORM 500TRANSACTION-REPORT.
            STOP RUN.
       0000MAIN-LOGIN.
           PERFORM TEST AFTER UNTIL FOUND-COUNT = 1
                DISPLAY "Please enter role (MEMBER/STAFF): "
                ACCEPT ROLE
                EVALUATE ROLE
                   WHEN 'MEMBER' PERFORM 001MEMBER-LOGIN
                   WHEN 'STAFF' PERFORM 002STAFF-LOGIN
                   WHEN OTHER
                       DISPLAY "Please enter MEMBER or STAFF only."
                END-EVALUATE
           END-PERFORM.

       1000MAIN-MENU.
               EVALUATE ROLE
                   WHEN 'MEMBER' PERFORM 011MEMBER-MENU
                   WHEN 'STAFF' PERFORM 022STAFF-MENU
                END-EVALUATE.
       001MEMBER-LOGIN.
           DISPLAY "===== Member Login =====".
           PERFORM 010FIND-MEMBER.
           DISPLAY "Login Successful."
               STRING "Welcome, ", DELIMITED BY SIZE,
               MEMBER-F-NAME, DELIMITED BY SPACE
               " ", DELIMITED BY SIZE,
               MEMBER-L-NAME, DELIMITED BY SPACE
               INTO EDT-ST
           DISPLAY EDT-ST.
       010FIND-MEMBER.
           PERFORM TEST AFTER UNTIL FOUND-COUNT = 1
           SET WS-NOT-END-OF-FILE TO TRUE
           MOVE 0 TO FOUND-COUNT
           OPEN INPUT MEMBER-FILE
               DISPLAY "Enter Member ID: "
               ACCEPT INPUT-MEM-ID
               PERFORM UNTIL WS-END-OF-FILE
                   READ MEMBER-FILE
                   AT END SET WS-END-OF-FILE TO TRUE
                   NOT AT END
                       IF INPUT-MEM-ID = MEMBER-ID
                           ADD 1 TO FOUND-COUNT
                           EXIT PERFORM
                       END-IF
                   END-READ
               END-PERFORM
           CLOSE MEMBER-FILE
           IF FOUND-COUNT = 0
               DISPLAY "User Not Found"
           END-IF
           END-PERFORM.
       011MEMBER-MENU.
           PERFORM WITH TEST AFTER UNTIL CHOICE = 5
               DISPLAY "===== Member Menu ====="
               DISPLAY "[1] Show Books"
               DISPLAY "[2] Search Book"
               DISPLAY "[3] Borrow Book"
               DISPLAY "[4] Return Book"
               DISPLAY "[5] Exit"
               DISPLAY "Please select menu (1-5):"
               ACCEPT CHOICE
               EVALUATE CHOICE
                   WHEN 1 PERFORM 100SHOW-BOOKS
                   WHEN 2 PERFORM 200SEARCH-BOOK
                   WHEN 3 PERFORM 300LENDING-MENU
                   WHEN 4 PERFORM 400RETURN-MENU
                   WHEN 5 DISPLAY "Exit program..."
                   WHEN OTHER
                       DISPLAY "Please select only 1-5"
               END-EVALUATE
           END-PERFORM.

       002STAFF-LOGIN.
           DISPLAY "===== Staff Login =====".
           PERFORM TEST AFTER UNTIL FOUND-COUNT = 1
           SET WS-NOT-END-OF-FILE TO TRUE
           MOVE 0 TO FOUND-COUNT
           OPEN INPUT STAFF-FILE
               DISPLAY "Enter Staff ID: "
               ACCEPT INPUT-STAFF-ID
               PERFORM UNTIL WS-END-OF-FILE
                   READ STAFF-FILE
                   AT END SET WS-END-OF-FILE TO TRUE
                   NOT AT END
                       IF INPUT-STAFF-ID = STAFF-ID
                           ADD 1 TO FOUND-COUNT
                           DISPLAY "Login Successful."
                           STRING "Welcome, ", DELIMITED BY SIZE,
                                   STAFF-F-NAME, DELIMITED BY SPACE
                                   " ",
                                   STAFF-L-NAME INTO EDT-ST
                           DISPLAY EDT-ST
                           EXIT PERFORM
                       END-IF
                   END-READ
               END-PERFORM
           CLOSE STAFF-FILE
           IF FOUND-COUNT = 0
               DISPLAY "User Not Found"
           END-IF
           END-PERFORM.
       022STAFF-MENU.
           PERFORM WITH TEST AFTER UNTIL CHOICE = 6
               DISPLAY "===== Staff Menu ====="
               DISPLAY "[1] Show Books"
               DISPLAY "[2] Search Book"
               DISPLAY "[3] Borrow Book"
               DISPLAY "[4] Return Book"
               DISPLAY "[5] Show Transactions Report"
               DISPLAY "[6] Exit"
               DISPLAY "Please select menu (1-6):"
               ACCEPT CHOICE
               EVALUATE CHOICE
                   WHEN 1 PERFORM 100SHOW-BOOKS
                   WHEN 2 PERFORM 200SEARCH-BOOK
                   WHEN 3 PERFORM 300LENDING-MENU
                   WHEN 4 PERFORM 400RETURN-MENU
                   WHEN 5 PERFORM 500TRANSACTION-REPORT
                   WHEN 6 DISPLAY "Exit program..."
                   WHEN OTHER
                       DISPLAY "Please select only 1-6:"
               END-EVALUATE
           END-PERFORM.
       100SHOW-BOOKS.
           SET WS-NOT-END-OF-FILE TO TRUE.
           DISPLAY "===== Show Books =====".
           OPEN INPUT BOOK-FILE.
            PERFORM UNTIL WS-END-OF-FILE
               READ BOOK-FILE
                   AT END
                       SET WS-END-OF-FILE TO TRUE
                   NOT AT END
                       MOVE COPY-NO TO EDT-COPY-NO
                       DISPLAY "ISBN: " ISBN
                       DISPLAY "Title: " TITLE
                       DISPLAY "Copies: " EDT-COPY-NO
                       DISPLAY "Status: " BOOK-STATUS
                       DISPLAY SPACE
               END-READ
            END-PERFORM.
           CLOSE BOOK-FILE.
           DISPLAY "Press Enter to return to the main menu...".
           ACCEPT CHOICE.
       200SEARCH-BOOK.
           SET WS-NOT-END-OF-FILE TO TRUE.
           OPEN INPUT BOOK-FILE
               MOVE 0 TO FOUND-COUNT
               DISPLAY "===== Search Book ====="
               DISPLAY "Enter Book's ISBN: "
               ACCEPT INPUT-ISBN
               DISPLAY "======================="
               PERFORM UNTIL WS-END-OF-FILE
                   READ BOOK-FILE
                   AT END SET WS-END-OF-FILE TO TRUE
                   NOT AT END
                       IF INPUT-ISBN = ISBN
                           MOVE 1 TO FOUND-COUNT
                           MOVE COPY-NO TO EDT-COPY-NO
                           DISPLAY "ISBN: " ISBN
                           DISPLAY "Title: " TITLE
                           DISPLAY "Copies: " EDT-COPY-NO
                           DISPLAY "Status: " BOOK-STATUS
                       END-IF
                   END-READ
               END-PERFORM
               IF FOUND-COUNT = 0
                   DISPLAY "Book Not Found"
               END-IF
           CLOSE BOOK-FILE
           .
           DISPLAY "Press Enter to return to the main menu...".
           ACCEPT CHOICE.
       300LENDING-MENU.
           DISPLAY "===== Borrow Book ====="
           IF ROLE = "STAFF"
               PERFORM 010FIND-MEMBER
           END-IF.
           SET WS-NOT-END-OF-FILE TO TRUE.
           MOVE 'N' TO WS-UPDATE.
           OPEN INPUT BOOK-FILE
               MOVE 0 TO FOUND-COUNT
               OPEN OUTPUT BOOK-TEMP-FILE
               DISPLAY "Enter book's ISBN: "
               ACCEPT INPUT-ISBN
               DISPLAY "======================="
                   PERFORM UNTIL WS-END-OF-FILE
                   READ BOOK-FILE
                   AT END SET WS-END-OF-FILE TO TRUE
                   NOT AT END
                       IF INPUT-ISBN = ISBN
                           MOVE 1 TO FOUND-COUNT
                           MOVE COPY-NO TO EDT-COPY-NO
                           DISPLAY "Found: " TITLE
                           DISPLAY "Copy left: " EDT-COPY-NO
                           DISPLAY "Status: " BOOK-STATUS
                           IF BOOK-STATUS = "AVAILABLE"
                               DISPLAY "Enter 'Y' to accept: "
                               ACCEPT INPUT-ACCEPT
                           DISPLAY "INPUT-ACCEPT: ", INPUT-ACCEPT
                           DISPLAY "Processing..."
                           END-IF
                           IF INPUT-ACCEPT = 'Y' AND COPY-NO > 0
                                  DISPLAY
                                       "Enter borrow date (YYYYMMDD): "
                                  ACCEPT BORROW-DATE
                                  SUBTRACT 1 FROM COPY-NO
                                  IF COPY-NO = 0
                                   MOVE "NOT AVAILABLE" TO BOOK-STATUS
                                  END-IF
                                  PERFORM 003GET-LATEST-TR-ID
                                  ADD 1 TO TR-ID
                                  MOVE TR-ID TO NEW-TR-ID
                                  MOVE INPUT-ISBN TO NEW-TR-ISBN
                                  MOVE INPUT-MEM-ID TO NEW-TR-MEMBER-ID
                                  PERFORM 00544CALCULATE-DUE-DATE
                                  MOVE BORROW-DATE TO NEW-TR-BORROW-DATE
                                  MOVE TR-DUE-DATE TO NEW-TR-DUE-DATE
                                  MOVE "-" TO NEW-TR-RETURN-DATE
                                  MOVE "-" TO NEW-TR-RETURN-STATUS
                                  MOVE 0 TO NEW-TR-FINE
                                  MOVE 'Y' TO WS-UPDATE
                                  MOVE TR-ID TO EDT-TR-ID
                                  DISPLAY
                                   "Borrowed. Transaction ID = ",
                                                              EDT-TR-ID

                           END-IF
                   END-IF
                   PERFORM 0341WRITE-BOOK-TEMP-FILE
                   END-READ
               END-PERFORM
               IF FOUND-COUNT = 0
                   DISPLAY "Book Not Found"
               END-IF
           CLOSE BOOK-FILE, BOOK-TEMP-FILE
           .
           IF WS-UPDATE = 'Y'
               PERFORM 034UPDATE-BOOK-FILE
               PERFORM 00511UPDATE-TR-NEW-FILE
           END-IF.
           DISPLAY "Press Enter to return to the main menu...".
           ACCEPT CHOICE.
       003GET-LATEST-TR-ID.
           SET WS-NOT-END-OF-FILE TO TRUE.
           OPEN INPUT TRANSACTION-FILE.
            PERFORM UNTIL WS-END-OF-FILE
               READ TRANSACTION-FILE
                   AT END
                       SET WS-END-OF-FILE TO TRUE
               END-READ
            END-PERFORM.
           CLOSE TRANSACTION-FILE
           .
       400RETURN-MENU.
           DISPLAY "===== Return Book ====="
           SET WS-NOT-END-OF-FILE TO TRUE.
           IF ROLE = "STAFF"
               PERFORM 010FIND-MEMBER
           END-IF.
           MOVE 'N' TO WS-UPDATE.
           OPEN INPUT BOOK-FILE
               OPEN OUTPUT BOOK-TEMP-FILE
                   MOVE 0 TO FOUND-COUNT
                   DISPLAY "Enter book's ISBN: "
                   ACCEPT INPUT-ISBN
                   DISPLAY "======================="
                   PERFORM UNTIL WS-END-OF-FILE
                   READ BOOK-FILE
                   AT END SET WS-END-OF-FILE TO TRUE
                   NOT AT END
                       IF INPUT-ISBN = ISBN
                           MOVE 1 TO FOUND-COUNT
                           DISPLAY "Found: " TITLE
                           DISPLAY "Enter 'Y' to accept: "
                           ACCEPT INPUT-ACCEPT
                           DISPLAY "INPUT-ACCEPT: ", INPUT-ACCEPT
                           DISPLAY "Processing..."
                           IF INPUT-ACCEPT = 'Y'
                                  DISPLAY
                                       "Enter return date (YYYYMMDD): "
                                  ACCEPT RETURN-DATE
                                  DISPLAY
                                       "Enter transaction ID: "
                                  ACCEPT INPUT-TR-ID
                                  PERFORM 0034CHECK-TR-ID
                                  IF FOUND-COUNT = 0
                                      EXIT PERFORM
                                  END-IF
                                  ADD 1 TO COPY-NO
                                  MOVE "AVAILABLE" TO BOOK-STATUS
                                  DISPLAY "Processing..."
                                  DISPLAY "Returned."
                                  MOVE 'Y' TO WS-UPDATE
                           END-IF
                   END-IF
                   PERFORM 0341WRITE-BOOK-TEMP-FILE
                   END-READ
               END-PERFORM
               IF FOUND-COUNT = 0
                   DISPLAY "Book Not Found"
               END-IF
           CLOSE BOOK-FILE, BOOK-TEMP-FILE
           .
           IF WS-UPDATE = 'Y'
               PERFORM 034UPDATE-BOOK-FILE
               PERFORM 0041UPDATE-TR
           END-IF.
           DISPLAY "Press Enter to return to the main menu...".
           ACCEPT CHOICE.
       0034CHECK-TR-ID.
           MOVE 0 TO FOUND-COUNT.
           SET WS-NOT-END-OF-FILE TO TRUE.
           OPEN INPUT TRANSACTION-FILE.
            PERFORM UNTIL WS-END-OF-FILE
               READ TRANSACTION-FILE
                   AT END
                       SET WS-END-OF-FILE TO TRUE
                   NOT AT END
                       IF INPUT-ISBN = TR-ISBN AND INPUT-TR-ID = TR-ID
                           ADD 1 TO FOUND-COUNT
                           MOVE TR-ID TO EDT-TR-ID
                           DISPLAY "Found Transaction ID: ", EDT-TR-ID
                       END-IF
               END-READ
            END-PERFORM.
                IF FOUND-COUNT = 0
                   DISPLAY "Transaction Not Found"
               END-IF
           CLOSE TRANSACTION-FILE
           .
       0041UPDATE-TR.
           SET WS-NOT-END-OF-FILE TO TRUE.
           OPEN INPUT TRANSACTION-FILE.
            OPEN OUTPUT TR-TEMP-FILE
            PERFORM UNTIL WS-END-OF-FILE
               READ TRANSACTION-FILE
                   AT END
                       SET WS-END-OF-FILE TO TRUE
                   NOT AT END
                       IF INPUT-TR-ID = TR-ID
                       MOVE RETURN-DATE TO TR-RETURN-DATE
                       PERFORM 00555CALCULATE-FINE
                       IF TR-FINE = 0
                           MOVE "IN TIME" TO TR-RETURN-STATUS
                       ELSE
                           MOVE "LATE" TO TR-RETURN-STATUS
                       END-IF
                       END-IF
                       PERFORM 00522WRITE-TR-TEMP-FILE
               END-READ
            END-PERFORM.
           CLOSE TRANSACTION-FILE, TR-TEMP-FILE
           .

           SET WS-NOT-END-OF-FILE TO TRUE.
           OPEN INPUT TR-TEMP-FILE.
            OPEN OUTPUT TRANSACTION-FILE
            PERFORM UNTIL WS-END-OF-FILE
               READ TR-TEMP-FILE
                   AT END
                       SET WS-END-OF-FILE TO TRUE
                   NOT AT END
                       PERFORM 00533WRITE-TR-FILE
               END-READ
            END-PERFORM.
           CLOSE TRANSACTION-FILE, TR-TEMP-FILE
           .
       500TRANSACTION-REPORT.
           SET WS-NOT-END-OF-FILE TO TRUE.
           DISPLAY "=====Transactions Report =====".
           DISPLAY "Enter book's ISBN: ".
           ACCEPT INPUT-ISBN.
           DISPLAY "=======================".
           OPEN INPUT TRANSACTION-FILE.
            PERFORM UNTIL WS-END-OF-FILE
               READ TRANSACTION-FILE
                   AT END
                       SET WS-END-OF-FILE TO TRUE
                   NOT AT END
                       IF INPUT-ISBN = TR-ISBN
                           PERFORM 005GET-BOOK
                           SET WS-NOT-END-OF-FILE TO TRUE
                           ADD 1 TO FOUND-COUNT
                           MOVE TR-ID TO EDT-TR-ID
                           DISPLAY "Transaction ID: " EDT-TR-ID
                           DISPLAY "ISBN: " TR-ISBN
                           DISPLAY "Title: " TITLE-TEMP
                           DISPLAY "Borrower: " TR-MEMBER-ID
                           DISPLAY "Borrow Date: " TR-BORROW-DATE
                           DISPLAY "Due Date: " TR-DUE-DATE
                           IF TR-RETURN-DATE = "00000000"
                               MOVE "-" TO TR-RETURN-DATE
                           END-IF
                           DISPLAY "Return Date: " TR-RETURN-DATE
                           DISPLAY "Return Status: " TR-RETURN-STATUS
                           DIVIDE TR-FINE BY 10 GIVING TOTAL-DAYS
                           MOVE TOTAL-DAYS TO EDT-TOTAL-DAYS
                           DISPLAY "Overdue: " EDT-TOTAL-DAYS " day(s)"
                           MOVE TR-FINE TO EDT-FINE
                           DISPLAY "FINE: " EDT-FINE
                           DISPLAY SPACE
                       END-IF
               END-READ
            END-PERFORM.
                IF FOUND-COUNT = 0
                   DISPLAY "Transaction Not Found"
               END-IF
           CLOSE TRANSACTION-FILE
           .
           DISPLAY "Press Enter to return to the main menu...".
           ACCEPT CHOICE.
       034UPDATE-BOOK-FILE.
           SET WS-NOT-END-OF-FILE TO TRUE.
           OPEN INPUT BOOK-TEMP-FILE
               OPEN OUTPUT BOOK-FILE

                   PERFORM UNTIL WS-END-OF-FILE
                       READ BOOK-TEMP-FILE
                       AT END SET WS-END-OF-FILE TO TRUE
                       NOT AT END
                               PERFORM 0342WRITE-BOOK-FILE
                       END-READ
                   END-PERFORM
           CLOSE BOOK-FILE, BOOK-TEMP-FILE
           .
       0341WRITE-BOOK-TEMP-FILE.
           MOVE ISBN TO ISBN-TEMP.
           MOVE CONGRESS TO CONGRESS-TEMP.
           MOVE TITLE TO TITLE-TEMP.
           MOVE COPY-NO TO COPY-NO-TEMP.
           MOVE BOOK-STATUS TO BOOK-STATUS-TEMP.
           WRITE BOOK-TEMP-RECORD FROM WRITE-BOOK-TEMP-RECORD.
       0342WRITE-BOOK-FILE.
           MOVE ISBN-TEMP-R TO ISBN-TEMP.
           MOVE CONGRESS-TEMP-R TO CONGRESS-TEMP.
           MOVE TITLE-TEMP-R TO TITLE-TEMP.
           MOVE COPY-NO-TEMP-R TO COPY-NO-TEMP.
           MOVE BOOK-STATUS-TEMP-R TO BOOK-STATUS-TEMP.
           WRITE BOOK-RECORD FROM WRITE-BOOK-TEMP-RECORD.
       00511UPDATE-TR-NEW-FILE.
           SET WS-NOT-END-OF-FILE TO TRUE.
           OPEN INPUT TRANSACTION-FILE.
            OPEN OUTPUT TR-TEMP-FILE
            PERFORM UNTIL WS-END-OF-FILE
               READ TRANSACTION-FILE
                   AT END
                       SET WS-END-OF-FILE TO TRUE
                   NOT AT END
                       PERFORM 00522WRITE-TR-TEMP-FILE
               END-READ
            END-PERFORM.
               MOVE NEW-TR-ID TO TR-ID
               MOVE NEW-TR-ISBN TO TR-ISBN
               MOVE NEW-TR-MEMBER-ID TO TR-MEMBER-ID
               MOVE NEW-TR-DUE-DATE TO TR-DUE-DATE
               MOVE NEW-TR-RETURN-DATE TO TR-RETURN-DATE
               MOVE NEW-TR-RETURN-STATUS TO TR-RETURN-STATUS
               MOVE NEW-TR-FINE TO TR-FINE
               PERFORM 00522WRITE-TR-TEMP-FILE
           CLOSE TRANSACTION-FILE, TR-TEMP-FILE
           .

           SET WS-NOT-END-OF-FILE TO TRUE.
           OPEN INPUT TR-TEMP-FILE.
            OPEN OUTPUT TRANSACTION-FILE
            PERFORM UNTIL WS-END-OF-FILE
               READ TR-TEMP-FILE
                   AT END
                       SET WS-END-OF-FILE TO TRUE
                   NOT AT END
                       PERFORM 00533WRITE-TR-FILE
               END-READ
            END-PERFORM.
           CLOSE TRANSACTION-FILE, TR-TEMP-FILE
           .
       00522WRITE-TR-TEMP-FILE.
           MOVE TR-ID TO TR-ID-TEMP.
           MOVE TR-ISBN TO TR-ISBN-TEMP.
           MOVE TR-MEMBER-ID TO TR-MEMBER-ID-TEMP.
           MOVE TR-BORROW-DATE TO TR-BORROW-DATE-TEMP.
           MOVE TR-DUE-DATE TO TR-DUE-DATE-TEMP.
           MOVE TR-RETURN-DATE TO TR-RETURN-DATE-TEMP.
           MOVE TR-RETURN-STATUS TO TR-RETURN-STATUS-TEMP.
           MOVE TR-FINE TO TR-FINE-TEMP.
           WRITE TR-TEMP-RECORD FROM WRITE-TR-TEMP-RECORD.
       00533WRITE-TR-FILE.
           MOVE TR-ID-TEMP-R TO TR-ID-TEMP.
           MOVE TR-ISBN-TEMP-R TO TR-ISBN-TEMP.
           MOVE TR-MEMBER-ID-TEMP-R TO TR-MEMBER-ID-TEMP.
           MOVE TR-BORROW-DATE-TEMP-R TO TR-BORROW-DATE-TEMP.
           MOVE TR-DUE-DATE-TEMP-R TO TR-DUE-DATE-TEMP.
           MOVE TR-RETURN-DATE-TEMP-R TO TR-RETURN-DATE-TEMP.
           MOVE TR-RETURN-STATUS-TEMP-R TO TR-RETURN-STATUS-TEMP.
           MOVE TR-FINE-TEMP-R TO TR-FINE-TEMP.
           WRITE TRANSACTION-RECORD FROM WRITE-TR-TEMP-RECORD.
       00544CALCULATE-DUE-DATE.
      *>      DUE = BORROW-DATE + 7 DAYS
           ADD 7 TO BORROW-D GIVING DUE-DATE-TEMP-D.
           PERFORM GET-DAYS-OF-THE-MONTH.

      *>      CASE 1: WITHIN MONTH, WITHIN YEAR
           IF DUE-DATE-TEMP-D < DAYS-OF-THE-MONTH
               AND BORROW-M NOT = 12
               MOVE DUE-DATE-TEMP-D TO TR-DUE-D
               EXIT PARAGRAPH
           END-IF.

           IF BORROW-M NOT = 12
      *>          CASE 2: NEW MONTH, WITHIN YEAR
               SUBTRACT DUE-DATE-TEMP-D FROM DAYS-OF-THE-MONTH
                   GIVING DIFF-DATE
               ADD 1 TO BORROW-M GIVING TR-DUE-M
               MOVE DIFF-DATE TO TR-DUE-D
           ELSE
      *>          CASE 3: NEW MONTH, NEW YEAR
               SUBTRACT DUE-DATE-TEMP-D FROM DAYS-OF-THE-MONTH
                   GIVING DIFF-DATE
               ADD 1 TO BORROW-Y GIVING TR-DUE-Y
               MOVE 01 TO TR-DUE-M
               MOVE DIFF-DATE TO TR-DUE-D
           END-IF.

      *>      MOVE "20250916" TO TR-DUE-DATE.
           DISPLAY "Due Date : " TR-DUE-DATE.
       00555CALCULATE-FINE.
           PERFORM CAL_DAYS_OVERDUE.
      *>      IF RETURN-DATE - BORROW-DATE > 7 THEN FINE = DAYS*10
           MOVE TOTAL-DAYS TO EDT-TOTAL-DAYS
           DISPLAY "Overdue: " EDT-TOTAL-DAYS " day(s)".
           IF TOTAL-DAYS = 0
               MOVE 0 TO TR-FINE
           ELSE
               MULTIPLY TOTAL-DAYS BY 10 GIVING TR-FINE
           END-IF.
           MOVE TR-FINE TO EDT-FINE.
           DISPLAY "Fine: " EDT-FINE.
       005GET-BOOK.
           SET WS-NOT-END-OF-FILE TO TRUE.
           OPEN INPUT BOOK-FILE
               MOVE 0 TO FOUND-COUNT
               PERFORM UNTIL WS-END-OF-FILE
                   READ BOOK-FILE
                   AT END SET WS-END-OF-FILE TO TRUE
                   NOT AT END
                       IF INPUT-ISBN = ISBN
                           MOVE 1 TO FOUND-COUNT
                           MOVE TITLE TO TITLE-TEMP
                       END-IF
                   END-READ
               END-PERFORM
               IF FOUND-COUNT = 0
                   DISPLAY "Book Not Found"
               END-IF
           CLOSE BOOK-FILE
           .
       CAL_DAYS_OVERDUE.
           MOVE ZERO TO TOTAL-DAYS.
           MOVE ZERO TO TOTAL-MONTHS.
           IF (TR-DUE-D = RETURN-D AND TR-DUE-M = RETURN-M
               AND TR-DUE-Y = RETURN-Y)
               OR (TR-DUE-D > RETURN-D AND TR-DUE-M = RETURN-M
               AND TR-DUE-Y = RETURN-Y)
               OR ((TR-DUE-M > RETURN-M)
               AND TR-DUE-Y >= RETURN-Y)
                   EXIT PARAGRAPH
           END-IF
           IF (TR-DUE-D = RETURN-D AND TR-DUE-M = RETURN-M
               AND TR-DUE-Y < RETURN-Y)
               PERFORM GET-TOTAL-MONTHS
               MOVE TOTAL-MONTHS TO TOTAL-DAYS
               EXIT PARAGRAPH
           END-IF
           IF TR-DUE-D < RETURN-D AND TR-DUE-M = RETURN-M
               AND TR-DUE-Y = RETURN-Y
                   SUBTRACT TR-DUE-D FROM RETURN-D
                       GIVING TOTAL-DAYS
                   EXIT PARAGRAPH
               END-IF
           IF (TR-DUE-D < RETURN-D AND TR-DUE-M < RETURN-M)
               OR (TR-DUE-M < RETURN-M)
               PERFORM GET-DAYS-OF-THE-MONTH
               SUBTRACT TR-DUE-D FROM DAYS-OF-THE-MONTH
                   GIVING TOTAL-DAYS
               ADD 1 TO TR-DUE-M
           END-IF.
           IF TR-DUE-Y < RETURN-Y
               PERFORM UNTIL TR-DUE-Y = RETURN-Y
                   PERFORM UNTIL TR-DUE-M > 12
                       PERFORM GET-DAYS-OF-THE-MONTH
                           ADD DAYS-OF-THE-MONTH TO TOTAL-DAYS
                           ADD 1 TO TR-DUE-M
                       END-PERFORM
                   ADD 1 TO TR-DUE-Y
               END-PERFORM
               SET TR-DUE-M TO 1
               PERFORM UNTIL TR-DUE-M = RETURN-M
               PERFORM GET-DAYS-OF-THE-MONTH
               ADD DAYS-OF-THE-MONTH TO TOTAL-DAYS
               ADD 1 TO TR-DUE-M
               END-PERFORM
               ADD RETURN-D TO TOTAL-DAYS
               EXIT PARAGRAPH
           END-IF.
           PERFORM UNTIL TR-DUE-M = RETURN-M
               PERFORM GET-DAYS-OF-THE-MONTH
               ADD DAYS-OF-THE-MONTH TO TOTAL-DAYS
               ADD 1 TO TR-DUE-M
           END-PERFORM.
           ADD RETURN-D TO TOTAL-DAYS.
       GET-DAYS-OF-THE-MONTH.
           EVALUATE TR-DUE-M
               WHEN "01" MOVE 31 TO DAYS-OF-THE-MONTH
               WHEN "02" MOVE 28 TO DAYS-OF-THE-MONTH
               WHEN "03" MOVE 31 TO DAYS-OF-THE-MONTH
               WHEN "04" MOVE 30 TO DAYS-OF-THE-MONTH
               WHEN "05" MOVE 31 TO DAYS-OF-THE-MONTH
               WHEN "06" MOVE 30 TO DAYS-OF-THE-MONTH
               WHEN "07" MOVE 31 TO DAYS-OF-THE-MONTH
               WHEN "08" MOVE 31 TO DAYS-OF-THE-MONTH
               WHEN "09" MOVE 30 TO DAYS-OF-THE-MONTH
               WHEN "10" MOVE 31 TO DAYS-OF-THE-MONTH
               WHEN "11" MOVE 30 TO DAYS-OF-THE-MONTH
               WHEN "12" MOVE 31 TO DAYS-OF-THE-MONTH
           END-EVALUATE.
       GET-TOTAL-MONTHS.
               IF (TR-DUE-M = RETURN-M)
                   SUBTRACT TR-DUE-Y FROM RETURN-Y
                       GIVING DIFF-MONTH
               END-IF.
               MULTIPLY 365 BY DIFF-MONTH GIVING TOTAL-MONTHS.
       END PROGRAM LIBRARY-PROJECT.
