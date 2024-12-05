       IDENTIFICATION DIVISION.
       PROGRAM-ID. BATCH3.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REPORT-FILE ASSIGN TO "report.csv"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  REPORT-FILE.
       01  REPORT-RECORD.
           05  RE-CUSTOMER-ID        PIC 9(9).
           05  RE-CUSTOMER-NAME      PIC X(50).
           05  RE-TOTAL-TRANSACTIONS PIC 9(9).
           05  RE-TOTAL-REVENUE      PIC 9(7)V99.
           05  RE-NO-REFUNDS         PIC 9(9).
       
       WORKING-STORAGE SECTION.
       01  WS-CUSTOMER-ID           PIC 9(9).
       01  WS-CUSTOMER-NAME         PIC X(50).
       01  WS-TOTAL-TRANSACTIONS    PIC 9(9) VALUE 0.
       01  WS-TOTAL-REVENUE         PIC 9(7)V99 VALUE 0.
       01  WS-NO-REFUNDS            PIC 9(9) VALUE 0.
       01  FILE-STATUS              PIC XX.
           88  FILE-OK              VALUE "00".
           88  FILE-ERROR           VALUE "10".
       01  SQLCODE                  PIC S9(9) COMP.
       01  SQLSTATE                 PIC X(5).

       EXEC SQL
           INCLUDE SQLCA
       END-EXEC.

       PROCEDURE DIVISION.
           PERFORM 100-CONNECT-TO-DB
           PERFORM 200-PROCESS-CUSTOMERS
           CLOSE REPORT-FILE
           STOP RUN.

       100-CONNECT-TO-DB SECTION.
           EXEC SQL
               CONNECT TO DB (DB CONNECTION DETAILS HERE)
           END-EXEC.

           IF SQLCODE NOT = 0
               DISPLAY 'ERROR: Unable to connect to 
       -        'DB. SQLCODE: ' SQLCODE
               STOP RUN
           END-IF

       100-CONNECT-TO-DB EXIT.
           EXIT.

       200-PROCESS-CUSTOMERS SECTION.
           OPEN OUTPUT REPORT-FILE
           IF FILE-STATUS NOT = '00'
               DISPLAY 'ERROR: Unable to open report file. 
               FILE-STATUS: ' FILE-STATUS
               STOP RUN
           END-IF

           EXEC SQL
               DECLARE CUST_TRANS_CURSOR CURSOR FOR
                   SELECT c.CUSTOMERID, c.NAME,
                          COUNT(t.TRANSACTIONID) AS TOTAL_TRANSACTIONS,
                          SUM(CASE WHEN t.TRANSACTIONTYPE = 'Purchase' 
                          THEN t.AMOUNT ELSE 0 END) 
                          AS TOTAL_REVENUE,
                          COUNT(CASE WHEN t.TRANSACTIONTYPE = 'Refund' 
                          THEN 1 ELSE NULL END) AS NUMBER_REFUNDS
                   FROM CUSTOMER c
                   LEFT JOIN TRANSACTIONS t ON 
                   c.CUSTOMERID = t.CUSTOMERID
                   GROUP BY c.CUSTOMERID, c.NAME
           END-EXEC

           EXEC SQL
               OPEN CUST_TRANS_CURSOR
           END-EXEC

           IF SQLCODE NOT = 0
               DISPLAY 'ERROR: Unable to open cursor. SQLCODE: ' SQLCODE
               CLOSE REPORT-FILE
               STOP RUN
           END-IF

           PERFORM 210-FETCH-CUSTOMERS
           EXEC SQL
               CLOSE CUST_TRANS_CURSOR
           END-EXEC
           IF SQLCODE NOT = 0
               DISPLAY 'ERROR: Unable to close cursor. SQLCODE:'SQLCODE
               CLOSE REPORT-FILE
               STOP RUN
           END-IF

       200-PROCESS-CUSTOMERS EXIT.
           EXIT.

       210-FETCH-CUSTOMERS SECTION.
           PERFORM UNTIL SQLCODE NOT = 0
               EXEC SQL
                   FETCH CUST_TRANS_CURSOR INTO :WS-CUSTOMER-ID, 
                   :WS-CUSTOMER-NAME, 
                        :WS-TOTAL-TRANSACTIONS, :WS-TOTAL-REVENUE, 
                        :WS-NO-REFUNDS
               END-EXEC.

               IF SQLCODE = 100
                   DISPLAY 'End of cursor result set reached. 
           -         'No more rows to fetch.'
                   EXIT
               ELSE IF SQLCODE = 0
                   PERFORM 220-WRITE-REPORT
               ELSE
                   DISPLAY 'ERROR: FETCH failed. SQLCODE: ' SQLCODE
                   STOP RUN
               END-IF
           END-PERFORM

       210-FETCH-CUSTOMERS-EXIT.
           EXIT.

       220-WRITE-REPORT SECTION.
           MOVE WS-CUSTOMER-ID TO RE-CUSTOMER-ID
           MOVE WS-CUSTOMER-NAME TO RE-CUSTOMER-NAME
           MOVE WS-TOTAL-TRANSACTIONS TO RE-TOTAL-TRANSACTIONS
           MOVE WS-TOTAL-REVENUE TO RE-TOTAL-REVENUE
           MOVE WS-NO-REFUNDS TO RE-NO-REFUNDS
           
           WRITE REPORT-RECORD
           IF FILE-STATUS NOT = '00'
               DISPLAY 'ERROR: Unable to write to file. 
           -    'FILE-STATUS: ' FILE-STATUS
               STOP RUN
           END-IF

       220-WRITE-REPORT EXIT.
           EXIT.
