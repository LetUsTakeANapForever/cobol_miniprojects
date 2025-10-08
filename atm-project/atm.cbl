      ******************************************************************
      * Author: 6621604548
      * Date: 29 SEP 2025
      * Purpose: ATM COBOL PROJECT
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATM-MACHINE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNT-FILE ASSIGN TO 'accounts.dat'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TEMP-FILE ASSIGN TO 'tmp-acc-file.dat'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SLIP-FILE ASSIGN TO 'slip.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TRANSACTION-FILE ASSIGN TO 'translog.dat'
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  ACCOUNT-FILE.
       01  ACCOUNT-RECORD.
           05 ACC-NO   PIC X(10).
           05 PIN      PIC X(4).
           05 ACC-NAME PIC X(20).
           05 BALANCE  PIC 9(8)V99.
       FD  TEMP-FILE.
       01  TEMP-ACCOUT-RECORD.
           05 TEMP-ACC-NO   PIC X(10).
           05 TEMP-PIN      PIC X(4).
           05 TEMP-ACC-NAME PIC X(20).
           05 TEMP-BALANCE  PIC 9(8)V99.
       FD  SLIP-FILE.
       01  SLIP-TEXT       PIC X(1000).
       FD  TRANSACTION-FILE.
       01  TRANSACTION-FILE-RECORD.
           05 ACC-NO-T   PIC X(10).
           05 DATE-T     PIC X(8).
           05 TIME-T     PIC X(6).
           05 TYPE-T     PIC X(1).
           05 AMOUNT-T   PIC 9(8)V99.
       WORKING-STORAGE SECTION.
       01  WS-EOF-FLAG          PIC X(1)    VALUE 'N'.
       01  WS-INPUT-VAR.
           05 ACC-NUM-INPUT    PIC X(10).
           05 PIN-INPUT        PIC X(4).
           05 AMOUNT-INPUT     PIC 9(8)V99.
           05 ACC-NUM-TO-TRANSFER-INPUT PIC X(10).
       01  WS-TEMP-VAR.
           05 ISLOGGEDIN       PIC X(1)    VALUE 'N'.
           05 WITHDRAWAL-STATUS PIC X(1)   VALUE 'N'.
           05 DEPOSIT-STATUS   PIC X(1)    VALUE 'N'.
           05 TRANSFER-STATUS-1  PIC X(1)    VALUE 'N'.
           05 TRANSFER-STATUS-2  PIC X(1)    VALUE 'N'.
           05 BALANCE-UPDATE   PIC 9(8)V99.
           05 RECIEVER-BALANCE   PIC 9(8)V99.
           05 RECIEVER-BALANCE-UPDATE   PIC 9(8)V99.
           05 CHOICE           PIC 9.
       01  WS-DSP-VAR.
           05 DSP-BALANCE      PIC ZZ,ZZZ,ZZZ.99.
           05 DSP-AMOUNT      PIC ZZ,ZZZ,ZZZ.99.
       01  TEMP-ACCOUT-R.
           05 TEMP-ACC-NO-R   PIC X(10).
           05 TEMP-PIN-R      PIC X(4).
           05 TEMP-ACC-NAME-R PIC X(20).
           05 TEMP-BALANCE-R  PIC 9(8)V99.
           05 EXIT-STATUS      PIC X(1) VALUE 'N'.
           05 WRITE-FIRST     PIC X VALUE 'N'.
           05 COUNT-LINE      PIC 999.
       01  ACCOUT-R.
           05 ACC-NO-R   PIC X(10).
           05 PIN-R      PIC X(4).
           05 ACC-NAME-R PIC X(20).
           05 BALANCE-R  PIC 9(8)V99.
       01  SLIP-LINE.
           05 FILLER     PIC X(50) VALUE ALL "-".
       01  SLIP-HEADER.
           05 FILLER     PIC X(50)
               VALUE "          *** GEMIMI BANK TRANSACTION SLIP ***".
       01  SLIP-DETAIL.
           05 SLIP-DETAIL-1.
               08 ACC-SLIP   PIC X(50)
                   VALUE "ACCOUNT NO: ".
           05 SLIP-DETAIL-2.
               08 DATE-TIME-SLIP PIC X(50)
               VALUE "DATE:  TIME: ".
           05 SLIP-DETAIL-3.
               08 TRANSACTION-SLIP PIC X(50)
                   VALUE "TRANSACTION: ".
           05 SLIP-DETAIL-4.
               08 AMOUNT-SLIP PIC X(50)
                   VALUE "AMOUNT: ".
           05 SLIP-DETAIL-5.
               08 BALANCE-SLIP PIC X(50)
                   VALUE "REMAINING BALANCE: ".
       01  SLIP-ENDING.
           05 TY-MSG PIC X(50)
               VALUE "            *** THANK YOU ***".
       01  CURR-DT.
           05  WS-YEAR PIC X(4).
           05  WS-MONTH PIC X(2).
           05  WS-DAY PIC X(2).
           05  WS-HOUR   PIC X(2).
           05  WS-MINUTE PIC X(2).
           05  WS-SECOND PIC X(2).
       01  TRANSACTION-FILE-R.
           05 ACC-NO-T-R   PIC X(10).
           05 DATE-T-R    PIC X(8).
           05 TIME-T-R   PIC X(6).
           05 TYPE-T-R   PIC X(1).
           05 AMOUNT-T-R  PIC 9(8)V99.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM AUTHENTICATION.
            STOP RUN.
       TEST-READ-FILE.
           OPEN INPUT ACCOUNT-FILE
               PERFORM UNTIL WS-EOF-FLAG = 'Y'
               READ ACCOUNT-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF-FLAG
                   NOT AT END
                       DISPLAY ACC-NO
               END-READ
               END-PERFORM
             CLOSE ACCOUNT-FILE.
       AUTHENTICATION.
           DISPLAY "---AUTHENTICATION---".
           DISPLAY "Enter account Number: ".
           ACCEPT ACC-NUM-INPUT.
           DISPLAY "Enter PIN: ".
           ACCEPT PIN-INPUT.

           MOVE 'N' TO WS-EOF-FLAG.
           MOVE 'N' TO ISLOGGEDIN.

               OPEN INPUT ACCOUNT-FILE
                   PERFORM UNTIL WS-EOF-FLAG = 'Y'
                   READ ACCOUNT-FILE
                       AT END
                           MOVE 'Y' TO WS-EOF-FLAG
                       NOT AT END
                           IF ACC-NUM-INPUT = ACC-NO
                               AND PIN-INPUT = PIN
                               MOVE 'Y' TO ISLOGGEDIN
                               MOVE 'Y' TO WS-EOF-FLAG
                           END-IF
                   END-READ
                   END-PERFORM
                CLOSE ACCOUNT-FILE
           IF ISLOGGEDIN = 'Y'
               DISPLAY "Logged in successfully."
               DISPLAY "-----------------------"
               PERFORM MAIN-MENU
           ELSE
               DISPLAY "Login failed."
               DISPLAY "-----------------------"
           END-IF.
       MAIN-MENU.
           PERFORM UNTIL EXIT-STATUS = 'Y'
               DISPLAY "===== Menu ====="
               DISPLAY "[1] Check Balance"
               DISPLAY "[2] Withdrawal"
               DISPLAY "[3] Deposite"
               DISPLAY "[4] Transfer"
               DISPLAY "[5] Exit/Logout"
               DISPLAY "Enter choice: "
               ACCEPT CHOICE
               EVALUATE CHOICE
                   WHEN 1 PERFORM CHECK-BALANCE
                   WHEN 2
                       PERFORM WITHDRAWAL
                       MOVE 'Y' TO EXIT-STATUS
                   WHEN 3
                       PERFORM DEPOSIT
                       MOVE 'Y' TO EXIT-STATUS
                   WHEN 4
                       PERFORM TRANSFER
                       MOVE 'Y' TO EXIT-STATUS
                   WHEN 5
                       DISPLAY "Exit program..."
                       MOVE 'Y' TO EXIT-STATUS
                   WHEN OTHER DISPLAY "Please select 1-5"
           END-PERFORM.
           PERFORM PRINT-SLIP.
       CHECK-BALANCE.
           DISPLAY "---CHECK BALANCE---".
           MOVE 'N' TO WS-EOF-FLAG.
           OPEN INPUT ACCOUNT-FILE
               PERFORM UNTIL WS-EOF-FLAG = 'Y'
               READ ACCOUNT-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF-FLAG
                   NOT AT END
                       IF ISLOGGEDIN = 'Y' AND ACC-NUM-INPUT = ACC-NO
                               AND PIN-INPUT = PIN
                           MOVE BALANCE TO DSP-BALANCE
                           DISPLAY "BALANCE : ", DSP-BALANCE
                       END-IF
               END-READ
               END-PERFORM
             CLOSE ACCOUNT-FILE.
           DISPLAY "Press Enter to return to the main menu...".
           ACCEPT CHOICE.
           DISPLAY "-----------------------".
       WITHDRAWAL.
           MOVE 0 TO AMOUNT-INPUT.
           DISPLAY "---WITHDRAW---".
           MOVE 'N' TO WS-EOF-FLAG.
           MOVE 'N' TO WITHDRAWAL-STATUS.
           DISPLAY "Enter amount to withdraw: ".
           ACCEPT AMOUNT-INPUT.
           OPEN INPUT ACCOUNT-FILE
               PERFORM UNTIL WS-EOF-FLAG = 'Y'
               READ ACCOUNT-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF-FLAG
                   NOT AT END
                       IF ISLOGGEDIN = 'Y' AND ACC-NUM-INPUT = ACC-NO
                               AND PIN-INPUT = PIN
                               AND BALANCE >= AMOUNT-INPUT
                       SUBTRACT AMOUNT-INPUT FROM BALANCE
                       MOVE BALANCE TO BALANCE-UPDATE
                       MOVE 'Y' TO WITHDRAWAL-STATUS
                       END-IF
               END-READ
               END-PERFORM
             CLOSE ACCOUNT-FILE.

           IF WITHDRAWAL-STATUS = 'Y'
               PERFORM WRITE-DATA-PERSISTENCE
               MOVE 'W' TO TYPE-T-R
               PERFORM GENERATE-TRANSLOG
               DISPLAY "Withdraw Status : Success."
           ELSE
               DISPLAY "Withdraw Status : Fail."
           END-IF.
           DISPLAY "-----------------------".
       DEPOSIT.
           MOVE 0 TO AMOUNT-INPUT.
           DISPLAY "---DEPOSIT---".
           MOVE 'N' TO WS-EOF-FLAG.
           MOVE 'N' TO DEPOSIT-STATUS.
           DISPLAY "Enter amount to deposit: ".
           ACCEPT AMOUNT-INPUT.
           OPEN INPUT ACCOUNT-FILE
               PERFORM UNTIL WS-EOF-FLAG = 'Y'
               READ ACCOUNT-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF-FLAG
                   NOT AT END
                       IF ISLOGGEDIN = 'Y' AND ACC-NUM-INPUT = ACC-NO
                               AND PIN-INPUT = PIN
                               AND AMOUNT-INPUT > 0
                       ADD AMOUNT-INPUT TO BALANCE
                       MOVE BALANCE TO BALANCE-UPDATE
                       MOVE 'Y' TO DEPOSIT-STATUS
                       END-IF
               END-READ
               END-PERFORM
             CLOSE ACCOUNT-FILE.

           IF DEPOSIT-STATUS = 'Y'
               PERFORM WRITE-DATA-PERSISTENCE
               MOVE 'D' TO TYPE-T-R
               PERFORM GENERATE-TRANSLOG
               DISPLAY "Deposit Status : Success."
           ELSE
               DISPLAY "Deposit Status : Fail."
           END-IF.
           DISPLAY "-----------------------".
       TRANSFER.
           MOVE SPACE TO ACC-NUM-TO-TRANSFER-INPUT.
           MOVE 0 TO AMOUNT-INPUT.
           DISPLAY "---TRANSFER---".
           MOVE 'N' TO WS-EOF-FLAG.
           MOVE 'N' TO TRANSFER-STATUS-1.
           MOVE 'N' TO TRANSFER-STATUS-2.
           DISPLAY "Enter account no. to transfer to: ".
           ACCEPT ACC-NUM-TO-TRANSFER-INPUT.
           DISPLAY "Enter amount to TRANSFER: ".
           ACCEPT AMOUNT-INPUT.
           OPEN INPUT ACCOUNT-FILE
               PERFORM UNTIL WS-EOF-FLAG = 'Y'
               READ ACCOUNT-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF-FLAG
                   NOT AT END
                       IF ISLOGGEDIN = 'Y' AND ACC-NUM-INPUT = ACC-NO
                               AND PIN-INPUT = PIN
                               AND BALANCE >= AMOUNT-INPUT
                           SUBTRACT AMOUNT-INPUT FROM BALANCE
                           MOVE BALANCE TO BALANCE-UPDATE
                           MOVE 'Y' TO TRANSFER-STATUS-1
                       END-IF
                       IF ACC-NUM-TO-TRANSFER-INPUT = ACC-NO
                           ADD AMOUNT-INPUT TO RECIEVER-BALANCE
                           MOVE 'Y' TO TRANSFER-STATUS-2
                       END-IF
               END-READ
               END-PERFORM
             CLOSE ACCOUNT-FILE.

           IF TRANSFER-STATUS-1 = 'Y' AND TRANSFER-STATUS-2 = 'Y'
               PERFORM WRITE-DATA-PERSISTENCE
               MOVE 'T' TO TYPE-T-R
               PERFORM GENERATE-TRANSLOG
               DISPLAY "Transfer Status : Success."
           ELSE
               DISPLAY "Transfer Status : Fail."
           END-IF.
           DISPLAY "-----------------------".
       WRITE-DATA-PERSISTENCE.
           MOVE 'N' TO WS-EOF-FLAG.
           OPEN INPUT ACCOUNT-FILE
           OPEN OUTPUT TEMP-FILE
               PERFORM UNTIL WS-EOF-FLAG = 'Y'
               READ ACCOUNT-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF-FLAG
                   NOT AT END
                       IF ISLOGGEDIN = 'Y' AND ACC-NUM-INPUT = ACC-NO
                               AND PIN-INPUT = PIN
                               AND (WITHDRAWAL-STATUS = 'Y'
                               OR DEPOSIT-STATUS = 'Y')
                               MOVE BALANCE-UPDATE TO BALANCE
                       END-IF
                       IF (TRANSFER-STATUS-1 = 'Y'
                           AND TRANSFER-STATUS-2)
                           IF ACC-NUM-INPUT = ACC-NO
                               MOVE BALANCE-UPDATE TO BALANCE
                           END-IF
                           IF ACC-NUM-TO-TRANSFER-INPUT = ACC-NO
                               ADD RECIEVER-BALANCE TO BALANCE
                           END-IF
                       END-IF
                       PERFORM WRITE-TEMP-FILE
               END-READ
               END-PERFORM
           CLOSE ACCOUNT-FILE, TEMP-FILE.

           MOVE 'N' TO WS-EOF-FLAG.
           OPEN INPUT TEMP-FILE
           OPEN OUTPUT ACCOUNT-FILE
               PERFORM UNTIL WS-EOF-FLAG = 'Y'
               READ TEMP-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF-FLAG
                   NOT AT END
                       PERFORM WRITE-ACC-FILE
               END-READ
               END-PERFORM
           CLOSE ACCOUNT-FILE, TEMP-FILE.
       WRITE-TEMP-FILE.
           MOVE ACC-NO TO TEMP-ACC-NO-R.
           MOVE PIN TO TEMP-PIN-R.
           MOVE ACC-NAME TO TEMP-ACC-NAME-R.
           MOVE BALANCE TO TEMP-BALANCE-R.

           WRITE TEMP-ACCOUT-RECORD FROM TEMP-ACCOUT-R.
       WRITE-ACC-FILE.
           MOVE TEMP-ACC-NO TO ACC-NO-R.
           MOVE TEMP-PIN TO PIN-R.
           MOVE TEMP-ACC-NAME TO ACC-NAME-R.
           MOVE TEMP-BALANCE TO BALANCE-R.

           WRITE ACCOUNT-RECORD FROM ACCOUT-R.
       PRINT-SLIP.
           MOVE 'N' TO WS-EOF-FLAG.
           OPEN INPUT ACCOUNT-FILE
           OPEN OUTPUT SLIP-FILE
               PERFORM UNTIL WS-EOF-FLAG = 'Y'
               READ ACCOUNT-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF-FLAG
                   NOT AT END
                       IF ISLOGGEDIN = 'Y' AND ACC-NUM-INPUT = ACC-NO
                               AND PIN-INPUT = PIN
                           PERFORM WRITE-SLIP
                       END-IF
               END-READ
               END-PERFORM
             CLOSE ACCOUNT-FILE, SLIP-FILE.
           DISPLAY "Printing slip...".
           DISPLAY "-----------------------".
       WRITE-SLIP.
           DISPLAY "Wrting slip...".
           STRING "ACCOUNT NO: ", ACC-NO
               INTO ACC-SLIP.
           PERFORM CURRENT-DATE-TIME.
           MOVE AMOUNT-INPUT TO DSP-AMOUNT.
           IF WITHDRAWAL-STATUS = 'Y'
               MOVE "TRANSACTION: WITHDRAWAL" TO TRANSACTION-SLIP
               STRING "AMOUNT: - ", DSP-AMOUNT
                   INTO AMOUNT-SLIP
               END-STRING
           END-IF
           IF DEPOSIT-STATUS = 'Y'
              MOVE "TRANSACTION: DEPOSIT" TO TRANSACTION-SLIP
              STRING "AMOUNT: + ", DSP-AMOUNT
                   INTO AMOUNT-SLIP
               END-STRING
           END-IF.
           IF TRANSFER-STATUS-1 = 'Y' AND TRANSFER-STATUS-2 = 'Y'
              MOVE "TRANSACTION: TRANSFER" TO TRANSACTION-SLIP
              STRING "AMOUNT: - ", DSP-AMOUNT
                   INTO AMOUNT-SLIP
               END-STRING
           END-IF.

           MOVE BALANCE TO DSP-BALANCE.
           STRING "REMAINING BALANCE: ", DSP-BALANCE, " BAHT"
           INTO BALANCE-SLIP.

           WRITE SLIP-TEXT FROM SLIP-LINE.
           WRITE SLIP-TEXT FROM SLIP-HEADER.
           WRITE SLIP-TEXT FROM SLIP-LINE.
           WRITE SLIP-TEXT FROM SLIP-DETAIL-1.
           WRITE SLIP-TEXT FROM SLIP-DETAIL-2.
           WRITE SLIP-TEXT FROM SLIP-DETAIL-3.
           WRITE SLIP-TEXT FROM SLIP-DETAIL-4.
           WRITE SLIP-TEXT FROM SLIP-DETAIL-5.
           WRITE SLIP-TEXT FROM SLIP-ENDING.
           WRITE SLIP-TEXT FROM SLIP-LINE.
           DISPLAY "-----------------------".
       CURRENT-DATE-TIME.
           MOVE FUNCTION CURRENT-DATE TO CURR-DT
           STRING "DATE: " WS-DAY "/" WS-MONTH "/" WS-YEAR " ",
           "TIME: " WS-HOUR ":" WS-MINUTE ":" WS-SECOND
           INTO DATE-TIME-SLIP.
       GENERATE-TRANSLOG.
             DISPLAY "Writing transaction log..."
             MOVE 0 TO COUNT-LINE.
             MOVE 'N' TO WS-EOF-FLAG.
             MOVE ACC-NO TO ACC-NO-T-R.
             MOVE FUNCTION CURRENT-DATE TO CURR-DT
             STRING WS-YEAR, WS-MONTH, WS-DAY
             INTO DATE-T-R.
             STRING WS-HOUR, WS-MINUTE, WS-SECOND
             INTO TIME-T-R.
             MOVE AMOUNT-INPUT TO AMOUNT-T-R.

             OPEN INPUT TRANSACTION-FILE
               PERFORM UNTIL WS-EOF-FLAG = 'Y'
               READ TRANSACTION-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF-FLAG
                   NOT AT END
                       ADD 1 TO COUNT-LINE
               END-READ
               END-PERFORM
             CLOSE TRANSACTION-FILE.


             IF COUNT-LINE = 0
                 OPEN OUTPUT TRANSACTION-FILE
                   WRITE TRANSACTION-FILE-RECORD FROM TRANSACTION-FILE-R
                 CLOSE TRANSACTION-FILE
             ELSE
             OPEN EXTEND TRANSACTION-FILE
               WRITE TRANSACTION-FILE-RECORD FROM TRANSACTION-FILE-R
             CLOSE TRANSACTION-FILE
             END-IF.

       END PROGRAM ATM-MACHINE.
