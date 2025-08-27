      ******************************************************************
      * Author:6621600461, 6621604548
      * Date:04/08/2025
      * Purpose:Mini Stock Management System Project
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STORE-SIMULATION.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  PRODUCTS.
           05 FILLER           PIC X(7) VALUE 'BK-1001'.
           05 FILLER           PIC X(7) VALUE 'BK-1002'.
           05 FILLER           PIC X(7) VALUE 'FD-2001'.
           05 FILLER           PIC X(7) VALUE 'FD-2002'.
           05 FILLER           PIC X(7) VALUE 'EL-3001'.
           05 FILLER           PIC X(7) VALUE 'EL-3002'.
           05 FILLER           PIC X(7) VALUE 'ST-4001'.
           05 FILLER           PIC X(7) VALUE 'FD-2003'.
           05 FILLER           PIC X(7) VALUE 'ST-4002'.
           05 FILLER           PIC X(7) VALUE 'BK-1003'.

           05 FILLER           PIC X(30) VALUE 'COBOL Programming'.
           05 FILLER           PIC X(30) VALUE 'JCL for Mainframes'.
           05 FILLER           PIC X(30) VALUE 'Instant Noodles'.
           05 FILLER           PIC X(30) VALUE 'Canned Tuna'.
           05 FILLER           PIC X(30) VALUE 'USB-C Cable'.
           05 FILLER           PIC X(30) VALUE 'Wireless Mouse'.
           05 FILLER           PIC X(30) VALUE 'A4 Paper Ream'.
           05 FILLER           PIC X(30) VALUE 'Potato Chips'.
           05 FILLER           PIC X(30) VALUE 'Ballpoint Pen Box'.
           05 FILLER           PIC X(30) VALUE 'Database Design'.

           05 FILLER           PIC 999 VALUE 15.
           05 FILLER           PIC 999 VALUE 8.
           05 FILLER           PIC 999 VALUE 150.
           05 FILLER           PIC 999 VALUE 80.
           05 FILLER           PIC 999 VALUE 45.
           05 FILLER           PIC 999 VALUE 22.
           05 FILLER           PIC 999 VALUE 30.
           05 FILLER           PIC 999 VALUE 120.
           05 FILLER           PIC 999 VALUE 50.
           05 FILLER           PIC 999 VALUE 12.


           05 FILLER           PIC 9(4)V99 VALUE 1250.50.
           05 FILLER           PIC 9(4)V99 VALUE 1100.00.
           05 FILLER           PIC 9(4)V99 VALUE 6.00.
           05 FILLER           PIC 9(4)V99 VALUE 35.50.
           05 FILLER           PIC 9(4)V99 VALUE 150.00.
           05 FILLER           PIC 9(4)V99 VALUE 499.00.
           05 FILLER           PIC 9(4)V99 VALUE 120.00.
           05 FILLER           PIC 9(4)V99 VALUE 25.00.
           05 FILLER           PIC 9(4)V99 VALUE 85.75.
           05 FILLER           PIC 9(4)V99 VALUE 1800.25.

       01  PRODUCT-TABLE REDEFINES PRODUCTS.
           05 PRODUCT-IDS OCCURS 10 TIMES.
               10 CATEGORY     PIC XX.
               10 SERIAL-NO    PIC X(5).
           05 PRODUCT-NAMES     PIC X(30) OCCURS       10 TIMES.
           05 PRODUCT-QTYS      PIC 999 OCCURS         10 TIMES.
           05 PRODUCT-PRICES    PIC 9(4)V99 OCCURS     10 TIMES.

       01  INPUT-VAR.
           05 ID-INPUT                 PIC X(7).
           05 CHOICE                   PIC 9.
           05 QTY-INPUT                PIC 999.
           05 TOTAL-PRICE              PIC 9(7)V9(2).
           05 TOTAL-INVENTORY-PRICE    PIC 9(7)V9(2).
           05 VAL-INDEX                PIC 99            VALUE 1.
           05 ST-DIS                   PIC X(150).
           05 ST-PRODUCT-ID            PIC X(8).
           05 QTY-NUM                  PIC 999.
           05 DISP-QTY                 PIC ZZ9.
           05 DISP-PRICE               PIC Z,ZZZ,ZZ9.99.
           05 PRICE-NUM                PIC 9(4)V9(2).
           05 PRODUCT-VALUE            PIC 9(7)V9(2).
           05 USER-NAME-INPUT          PIC X(4).
           05 PASS-WORD-INPUT          PIC X(4).

       01  EDT-VAR.
           05 EDT-TOTAL-INVENTORY-PRICE    PIC Z,ZZZ,ZZZ.ZZ.
           05 EDT-TOTAL-PRICE              PIC Z,ZZZ,ZZZ.ZZ.
           05 EDT-PRODUCT-VALUE            PIC Z,ZZZ,ZZZ.ZZ.

       01  WS-INPUT-AREA.
           05 WS-INPUT-DATE        PIC X(8).

       01  WS-DATE-PARSED REDEFINES WS-INPUT-AREA.
           05 WS-YEAR              PIC X(4).
           05 WS-MONTH             PIC X(2).
           05 WS-DAY               PIC X(2).

       01  WS-DISPLAY-DATE.
           05 DISP-DAY            PIC X(2).
           05 FILLER               PIC X       VALUE "-".
           05 DISP-MONTH          PIC X(3).
           05 FILLER               PIC X       VALUE "-".
           05 DISP-YEAR           PIC X(4).

       PROCEDURE DIVISION.

           PERFORM 0000LOGIN.

           PERFORM WITH TEST AFTER UNTIL CHOICE = 4
               DISPLAY SPACE
               PERFORM 0001MAIN-MENU

               EVALUATE CHOICE
                   WHEN 1
                       PERFORM 1000SELL-ITEM-MENU
                   WHEN 2
                       PERFORM 2000RESTOCK-ITEM
                   WHEN 3
                       PERFORM 3000REPORT-STOCK
                   WHEN 4
                       DISPLAY "Exiting program. Goodbye!"
                   WHEN OTHER
                        DISPLAY "Please input only choice 1 - 4"

           END-PERFORM.
           STOP RUN.

       0000LOGIN.
           PERFORM TEST AFTER UNTIL
               USER-NAME-INPUT = 'user'
               AND PASS-WORD-INPUT = '1234'

               DISPLAY "Enter username: "
               ACCEPT USER-NAME-INPUT

               DISPLAY "Enter password: "
               ACCEPT PASS-WORD-INPUT
           END-PERFORM.

       0001MAIN-MENU.
            DISPLAY "===================================="
            DISPLAY "=== Mini Stock Management System ==="
            DISPLAY "===================================="

            DISPLAY "[1] Sell Item"
            DISPLAY "[2] Retock Item"
            DISPLAY "[3] Print Inventory Report"
            DISPLAY "[4] Exit Program"
            DISPLAY "====================================".
            DISPLAY "Please enter your choice: "
            ACCEPT CHOICE.

       1000SELL-ITEM-MENU.
            DISPLAY "Enter Product ID to sell:"
            ACCEPT ID-INPUT.

            PERFORM 1001GET-PRODUCT.

            IF VAL-INDEX > 10 THEN
                DISPLAY "Error : ID was not found."
                DISPLAY "Press enter to return to the main menu..."
                ACCEPT CHOICE
                EXIT PARAGRAPH
            END-IF.

            DISPLAY "Enter quantity to sell:"
            ACCEPT QTY-INPUT.

            MOVE PRODUCT-QTYS (VAL-INDEX) TO QTY-NUM.
               MOVE PRODUCT-PRICES (VAL-INDEX) TO PRICE-NUM.

               IF QTY-INPUT <= QTY-NUM THEN
                   MULTIPLY PRICE-NUM BY QTY-INPUT
                   GIVING TOTAL-PRICE
                   MOVE TOTAL-PRICE TO EDT-TOTAL-PRICE

                   DISPLAY "Sale successful. Total price: ",
                   EDT-TOTAL-PRICE

                   SUBTRACT QTY-INPUT FROM QTY-NUM GIVING QTY-NUM
                   MOVE QTY-NUM TO PRODUCT-QTYS (VAL-INDEX)

               ELSE
                   DISPLAY "ERROR: Not enough stock. Available: ",
                   PRODUCT-QTYS (VAL-INDEX)
               END-IF.

               DISPLAY "Press Enter to return to the main menu...".
               ACCEPT CHOICE.

       2000RESTOCK-ITEM.
            DISPLAY "Enter Product ID to restock:"
            ACCEPT ID-INPUT.

            PERFORM 1001GET-PRODUCT.

            IF VAL-INDEX > 10 THEN
                DISPLAY "Error : ID was not found."
                DISPLAY "Press enter to return to the main menu..."
                ACCEPT CHOICE
                EXIT PARAGRAPH
            END-IF.

            DISPLAY "Enter quantity to add:"
            ACCEPT QTY-INPUT.

            PERFORM VARYING VAL-INDEX FROM 1 BY 1
            UNTIL VAL-INDEX > 10
               IF ID-INPUT = PRODUCT-IDS (VAL-INDEX)
                   ADD QTY-INPUT TO PRODUCT-QTYS (VAL-INDEX)
                   MOVE PRODUCT-QTYS (VAL-INDEX) TO DISP-QTY
                   DISPLAY "Restock successful."
                   DISPLAY "New quantity: "  DISP-QTY
               END-IF
            END-PERFORM.
            DISPLAY "Press enter to return to the main menu..."
            ACCEPT CHOICE.

       1001GET-PRODUCT.
           PERFORM VARYING VAL-INDEX
           FROM 1 BY 1 UNTIL VAL-INDEX > 10
               IF PRODUCT-IDS (VAL-INDEX) = ID-INPUT
                   THEN EXIT PARAGRAPH
               END-IF
           END-PERFORM.

       3000REPORT-STOCK.
            DISPLAY "Enter Date (YYYYMMDD): ".
            ACCEPT WS-INPUT-DATE.
            EVALUATE WS-MONTH
               WHEN "01" MOVE "JAN" TO DISP-MONTH
               WHEN "02" MOVE "FEB" TO DISP-MONTH
               WHEN "03" MOVE "MAR" TO DISP-MONTH
               WHEN "04" MOVE "APR" TO DISP-MONTH
               WHEN "05" MOVE "MAY" TO DISP-MONTH
               WHEN "06" MOVE "JUN" TO DISP-MONTH
               WHEN "07" MOVE "JUL" TO DISP-MONTH
               WHEN "08" MOVE "AUG" TO DISP-MONTH
               WHEN "09" MOVE "SEO" TO DISP-MONTH
               WHEN "10" MOVE "OCT" TO DISP-MONTH
               WHEN "11" MOVE "NOV" TO DISP-MONTH
               WHEN "12" MOVE "DEC" TO DISP-MONTH
               WHEN OTHER MOVE "???" TO DISP-MONTH
            END-EVALUATE.
            MOVE WS-DAY TO DISP-DAY
            MOVE WS-YEAR TO DISP-YEAR
            DISPLAY " ".
            DISPLAY "Report Date: " WS-DISPLAY-DATE

            DISPLAY "*** Inventory Report ***".
               STRING
               "ID        " DELIMITED BY SIZE,
               "           " DELIMITED BY SIZE,
               "PRODUCT NAME" DELIMITED BY SIZE,
               "                             " DELIMITED BY SIZE,
               "CATEGORY" DELIMITED BY SIZE,
               "    " DELIMITED BY SIZE,
               "QTY      " DELIMITED BY SIZE,
               "     " DELIMITED BY SIZE,
               "PRICE     " DELIMITED BY SIZE,
               "            " DELIMITED BY SIZE,
               "VALUE" DELIMITED BY SIZE
               INTO ST-DIS.

               DISPLAY ST-DIS.

               MOVE ZERO TO TOTAL-INVENTORY-PRICE.

               PERFORM VARYING VAL-INDEX FROM 1 BY 1
               UNTIL VAL-INDEX > 10

                   MOVE PRODUCT-QTYS (VAL-INDEX) TO QTY-NUM
                   MOVE PRODUCT-PRICES (VAL-INDEX) TO PRICE-NUM

                   MULTIPLY QTY-NUM
                   BY PRICE-NUM
                   GIVING PRODUCT-VALUE

                   MOVE PRODUCT-VALUE TO EDT-PRODUCT-VALUE

                   ADD PRODUCT-VALUE TO TOTAL-INVENTORY-PRICE

                   IF QTY-NUM < 10 THEN
                       STRING "*", PRODUCT-IDS (VAL-INDEX)
                       DELIMITED BY SIZE, INTO ST-PRODUCT-ID
                   ELSE
                       MOVE PRODUCT-IDS (VAL-INDEX) TO
                       ST-PRODUCT-ID
                   END-IF

                   MOVE PRODUCT-QTYS (VAL-INDEX) TO DISP-QTY
                   MOVE PRODUCT-PRICES (VAL-INDEX) TO DISP-PRICE

                   STRING ST-PRODUCT-ID DELIMITED BY SIZE,
                   "             " DELIMITED BY SIZE,
                   PRODUCT-NAMES (VAL-INDEX) DELIMITED BY SIZE,
                   "           " DELIMITED BY SIZE,
                   CATEGORY (VAL-INDEX) DELIMITED BY SIZE,
                   "          " DELIMITED BY SIZE,
                   DISP-QTY DELIMITED BY SIZE,
                   "        " DELIMITED BY SIZE,
                   DISP-PRICE DELIMITED BY SIZE,
                   "          " DELIMITED BY SIZE,
                   EDT-PRODUCT-VALUE DELIMITED BY SIZE
                   INTO ST-DIS

                   DISPLAY ST-DIS

               END-PERFORM.

               DISPLAY SPACE.

               MOVE TOTAL-INVENTORY-PRICE TO EDT-TOTAL-INVENTORY-PRICE.

               DISPLAY "TOTAL INVENTORY Value:",
               EDT-TOTAL-INVENTORY-PRICE.
               DISPLAY "*** End of Report ***".

               DISPLAY "Press Enter to return to the main menu...".
               ACCEPT CHOICE.
       END PROGRAM STORE-SIMULATION.
