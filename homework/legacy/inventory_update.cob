       IDENTIFICATION DIVISION.
       PROGRAM-ID. INVENTORY-UPDATE.
       AUTHOR. LEGACY-INVENTORY-TEAM.
       DATE-WRITTEN. 1997-08-12.
      ******************************************************************
      * PURPOSE: COMPLEX INVENTORY RECONCILIATION AND UPDATE PROGRAM  *
      * PROCESSES: Sales, Returns, Adjustments from multiple sources  *
      * COMPLEXITY: Multi-file matching, nested loops, error handling *
      * LAST MODIFIED: 2006-03-15 - ADDED RETURN PROCESSING          *
      ******************************************************************
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INVENTORY-MASTER ASSIGN TO "INVMAST.DAT"
               ORGANIZATION IS INDEXED
               ACCESS IS DYNAMIC
               RECORD KEY IS INV-KEY
               ALTERNATE RECORD KEY IS INV-SKU WITH DUPLICATES
               FILE STATUS IS WS-INV-STATUS.
           SELECT SALES-TRANS ASSIGN TO "SALESTRAN.DAT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-SALES-STATUS.
           SELECT RETURN-TRANS ASSIGN TO "RETTRAN.DAT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-RET-STATUS.
           SELECT ADJ-TRANS ASSIGN TO "ADJTRAN.DAT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ADJ-STATUS.
           SELECT ERROR-LOG ASSIGN TO "ERRLOG.DAT"
               ORGANIZATION IS SEQUENTIAL.
           SELECT UPDATE-REPORT ASSIGN TO "UPDRPT.DAT"
               ORGANIZATION IS SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  INVENTORY-MASTER.
       01  INVENTORY-RECORD.
           05  INV-KEY.
               10  INV-LOCATION        PIC X(4).
               10  INV-SKU             PIC X(10).
           05  INV-DESCRIPTION         PIC X(50).
           05  INV-CATEGORY            PIC X(20).
           05  INV-QTY-ONHAND          PIC S9(7) COMP-3.
           05  INV-QTY-ALLOCATED       PIC S9(7) COMP-3.
           05  INV-QTY-AVAILABLE       PIC S9(7) COMP-3.
           05  INV-REORDER-POINT       PIC 9(5).
           05  INV-REORDER-QTY         PIC 9(5).
           05  INV-UNIT-COST           PIC S9(7)V99 COMP-3.
           05  INV-LAST-UPDATED        PIC X(26).
           05  INV-STATUS-CODE         PIC X(2).
               88  INV-ACTIVE          VALUE "AC".
               88  INV-DISCONTINUED    VALUE "DC".
               88  INV-ONHOLD          VALUE "OH".
       
       FD  SALES-TRANS.
       01  SALES-RECORD.
           05  SAL-TRANS-ID            PIC X(10).
           05  SAL-DATE                PIC 9(8).
           05  SAL-LOCATION            PIC X(4).
           05  SAL-SKU                 PIC X(10).
           05  SAL-QUANTITY            PIC S9(5) COMP-3.
           05  SAL-UNIT-PRICE          PIC S9(7)V99 COMP-3.
           05  SAL-CUSTOMER-ID         PIC X(10).
       
       FD  RETURN-TRANS.
       01  RETURN-RECORD.
           05  RET-TRANS-ID            PIC X(10).
           05  RET-DATE                PIC 9(8).
           05  RET-LOCATION            PIC X(4).
           05  RET-SKU                 PIC X(10).
           05  RET-QUANTITY            PIC S9(5) COMP-3.
           05  RET-REASON-CODE         PIC X(2).
           05  RET-ORIG-SALE-ID        PIC X(10).
       
       FD  ADJ-TRANS.
       01  ADJ-RECORD.
           05  ADJ-TRANS-ID            PIC X(10).
           05  ADJ-DATE                PIC 9(8).
           05  ADJ-LOCATION            PIC X(4).
           05  ADJ-SKU                 PIC X(10).
           05  ADJ-QUANTITY            PIC S9(5) COMP-3.
           05  ADJ-REASON-CODE         PIC X(2).
               88  ADJ-DAMAGE          VALUE "DM".
               88  ADJ-THEFT           VALUE "TH".
               88  ADJ-RECOUNT         VALUE "RC".
               88  ADJ-TRANSFER        VALUE "TR".
           05  ADJ-APPROVED-BY         PIC X(8).
       
       FD  ERROR-LOG.
       01  ERROR-RECORD.
           05  ERR-TIMESTAMP           PIC X(26).
           05  ERR-TRANS-TYPE          PIC X(10).
           05  ERR-TRANS-ID            PIC X(10).
           05  ERR-MESSAGE             PIC X(100).
       
       FD  UPDATE-REPORT.
       01  REPORT-LINE                 PIC X(132).
       
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUSES.
           05  WS-INV-STATUS           PIC XX.
               88  INV-OK              VALUE "00".
               88  INV-NOT-FOUND       VALUE "23".
           05  WS-SALES-STATUS         PIC XX.
               88  SALES-OK            VALUE "00".
               88  SALES-EOF           VALUE "10".
           05  WS-RET-STATUS           PIC XX.
               88  RET-OK              VALUE "00".
               88  RET-EOF             VALUE "10".
           05  WS-ADJ-STATUS           PIC XX.
               88  ADJ-OK              VALUE "00".
               88  ADJ-EOF             VALUE "10".
       
       01  WS-COUNTERS.
           05  WS-SALES-PROCESSED      PIC 9(6) VALUE ZERO.
           05  WS-RETURNS-PROCESSED    PIC 9(6) VALUE ZERO.
           05  WS-ADJ-PROCESSED        PIC 9(6) VALUE ZERO.
           05  WS-ERRORS-LOGGED        PIC 9(6) VALUE ZERO.
           05  WS-INV-UPDATED          PIC 9(6) VALUE ZERO.
       
       01  WS-CALCULATIONS.
           05  WS-NEW-QTY-ONHAND       PIC S9(7) COMP-3.
           05  WS-NEW-QTY-AVAIL        PIC S9(7) COMP-3.
       
       01  WS-FLAGS.
           05  WS-SALES-EOF-FLAG       PIC X VALUE 'N'.
           05  WS-RET-EOF-FLAG         PIC X VALUE 'N'.
           05  WS-ADJ-EOF-FLAG         PIC X VALUE 'N'.
       
       PROCEDURE DIVISION.
       MAIN-PROCESS.
           PERFORM INIT-PROCESS
           PERFORM PROCESS-SALES-TRANSACTIONS
           PERFORM PROCESS-RETURN-TRANSACTIONS
           PERFORM PROCESS-ADJUSTMENT-TRANSACTIONS
           PERFORM CLOSE-PROCESS
           STOP RUN.
       
       INIT-PROCESS.
           OPEN I-O INVENTORY-MASTER
           OPEN INPUT SALES-TRANS
           OPEN INPUT RETURN-TRANS
           OPEN INPUT ADJ-TRANS
           OPEN OUTPUT ERROR-LOG
           OPEN OUTPUT UPDATE-REPORT
           DISPLAY "INVENTORY UPDATE PROCESS STARTED".
       
       PROCESS-SALES-TRANSACTIONS.
           READ SALES-TRANS
               AT END MOVE 'Y' TO WS-SALES-EOF-FLAG
           END-READ
           
           PERFORM UNTIL WS-SALES-EOF-FLAG = 'Y'
               PERFORM PROCESS-SINGLE-SALE
               READ SALES-TRANS
                   AT END MOVE 'Y' TO WS-SALES-EOF-FLAG
               END-READ
           END-PERFORM
           CLOSE SALES-TRANS.
       
       PROCESS-SINGLE-SALE.
           MOVE SAL-LOCATION TO INV-LOCATION
           MOVE SAL-SKU TO INV-SKU
           
           READ INVENTORY-MASTER KEY IS INV-KEY
               INVALID KEY PERFORM LOG-INVENTORY-NOT-FOUND
           END-READ
           
           IF INV-OK
               IF INV-ACTIVE
                   COMPUTE WS-NEW-QTY-ONHAND = 
                       INV-QTY-ONHAND - SAL-QUANTITY
                   COMPUTE WS-NEW-QTY-AVAIL = 
                       INV-QTY-AVAILABLE - SAL-QUANTITY
                   
                   IF WS-NEW-QTY-ONHAND >= 0
                       MOVE WS-NEW-QTY-ONHAND TO INV-QTY-ONHAND
                       MOVE WS-NEW-QTY-AVAIL TO INV-QTY-AVAILABLE
                       MOVE FUNCTION CURRENT-DATE TO INV-LAST-UPDATED
                       REWRITE INVENTORY-RECORD
                       ADD 1 TO WS-SALES-PROCESSED
                       ADD 1 TO WS-INV-UPDATED
                   ELSE
                       PERFORM LOG-NEGATIVE-INVENTORY-ERROR
                   END-IF
               ELSE
                   PERFORM LOG-INACTIVE-PRODUCT-ERROR
               END-IF
           END-IF.
       
       PROCESS-RETURN-TRANSACTIONS.
           READ RETURN-TRANS
               AT END MOVE 'Y' TO WS-RET-EOF-FLAG
           END-READ
           
           PERFORM UNTIL WS-RET-EOF-FLAG = 'Y'
               PERFORM PROCESS-SINGLE-RETURN
               READ RETURN-TRANS
                   AT END MOVE 'Y' TO WS-RET-EOF-FLAG
               END-READ
           END-PERFORM
           CLOSE RETURN-TRANS.
       
       PROCESS-SINGLE-RETURN.
           MOVE RET-LOCATION TO INV-LOCATION
           MOVE RET-SKU TO INV-SKU
           
           READ INVENTORY-MASTER KEY IS INV-KEY
               INVALID KEY PERFORM LOG-INVENTORY-NOT-FOUND
           END-READ
           
           IF INV-OK
               COMPUTE WS-NEW-QTY-ONHAND = 
                   INV-QTY-ONHAND + RET-QUANTITY
               COMPUTE WS-NEW-QTY-AVAIL = 
                   INV-QTY-AVAILABLE + RET-QUANTITY
               
               MOVE WS-NEW-QTY-ONHAND TO INV-QTY-ONHAND
               MOVE WS-NEW-QTY-AVAIL TO INV-QTY-AVAILABLE
               MOVE FUNCTION CURRENT-DATE TO INV-LAST-UPDATED
               REWRITE INVENTORY-RECORD
               ADD 1 TO WS-RETURNS-PROCESSED
               ADD 1 TO WS-INV-UPDATED
           END-IF.
       
       PROCESS-ADJUSTMENT-TRANSACTIONS.
           READ ADJ-TRANS
               AT END MOVE 'Y' TO WS-ADJ-EOF-FLAG
           END-READ
           
           PERFORM UNTIL WS-ADJ-EOF-FLAG = 'Y'
               PERFORM PROCESS-SINGLE-ADJUSTMENT
               READ ADJ-TRANS
                   AT END MOVE 'Y' TO WS-ADJ-EOF-FLAG
               END-READ
           END-PERFORM
           CLOSE ADJ-TRANS.
       
       PROCESS-SINGLE-ADJUSTMENT.
           MOVE ADJ-LOCATION TO INV-LOCATION
           MOVE ADJ-SKU TO INV-SKU
           
           READ INVENTORY-MASTER KEY IS INV-KEY
               INVALID KEY PERFORM LOG-INVENTORY-NOT-FOUND
           END-READ
           
           IF INV-OK
               COMPUTE WS-NEW-QTY-ONHAND = 
                   INV-QTY-ONHAND + ADJ-QUANTITY
               
               IF WS-NEW-QTY-ONHAND >= 0
                   MOVE WS-NEW-QTY-ONHAND TO INV-QTY-ONHAND
                   COMPUTE INV-QTY-AVAILABLE = 
                       INV-QTY-ONHAND - INV-QTY-ALLOCATED
                   MOVE FUNCTION CURRENT-DATE TO INV-LAST-UPDATED
                   REWRITE INVENTORY-RECORD
                   ADD 1 TO WS-ADJ-PROCESSED
                   ADD 1 TO WS-INV-UPDATED
               ELSE
                   PERFORM LOG-NEGATIVE-INVENTORY-ERROR
               END-IF
           END-IF.
       
       LOG-INVENTORY-NOT-FOUND.
           MOVE FUNCTION CURRENT-DATE TO ERR-TIMESTAMP
           MOVE "SALES" TO ERR-TRANS-TYPE
           MOVE SAL-TRANS-ID TO ERR-TRANS-ID
           MOVE "INVENTORY RECORD NOT FOUND" TO ERR-MESSAGE
           WRITE ERROR-RECORD
           ADD 1 TO WS-ERRORS-LOGGED.
       
       LOG-NEGATIVE-INVENTORY-ERROR.
           MOVE FUNCTION CURRENT-DATE TO ERR-TIMESTAMP
           MOVE "SALES" TO ERR-TRANS-TYPE
           MOVE SAL-TRANS-ID TO ERR-TRANS-ID
           MOVE "WOULD CAUSE NEGATIVE INVENTORY" TO ERR-MESSAGE
           WRITE ERROR-RECORD
           ADD 1 TO WS-ERRORS-LOGGED.
       
       LOG-INACTIVE-PRODUCT-ERROR.
           MOVE FUNCTION CURRENT-DATE TO ERR-TIMESTAMP
           MOVE "SALES" TO ERR-TRANS-TYPE
           MOVE SAL-TRANS-ID TO ERR-TRANS-ID
           MOVE "PRODUCT IS INACTIVE OR DISCONTINUED" TO ERR-MESSAGE
           WRITE ERROR-RECORD
           ADD 1 TO WS-ERRORS-LOGGED.
       
       CLOSE-PROCESS.
           DISPLAY "=================================="
           DISPLAY "INVENTORY UPDATE COMPLETE"
           DISPLAY "=================================="
           DISPLAY "SALES PROCESSED: " WS-SALES-PROCESSED
           DISPLAY "RETURNS PROCESSED: " WS-RETURNS-PROCESSED
           DISPLAY "ADJUSTMENTS PROCESSED: " WS-ADJ-PROCESSED
           DISPLAY "INVENTORY RECORDS UPDATED: " WS-INV-UPDATED
           DISPLAY "ERRORS LOGGED: " WS-ERRORS-LOGGED
           DISPLAY "=================================="
           
           CLOSE INVENTORY-MASTER
           CLOSE ERROR-LOG
           CLOSE UPDATE-REPORT.
