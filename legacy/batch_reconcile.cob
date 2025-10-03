       IDENTIFICATION DIVISION.
       PROGRAM-ID. BATCH-RECONCILE.
       AUTHOR. FINANCE-SYSTEMS.
       DATE-WRITTEN. 1992-06-10.
      ******************************************************************
      * PURPOSE: END-OF-DAY BATCH RECONCILIATION SYSTEM               *
      *          Matches transactions between payment processor and   *
      *          internal ledger, identifies discrepancies, and       *
      *          generates variance reports                           *
      * COMPLEXITY: Multi-file matching with complex tolerances,      *
      *             nested perform loops, and multiple sort keys      *
      * CRITICAL: Must match to penny - affects GL close             *
      * LAST MODIFIED: 2005-09-30 - ENHANCED TOLERANCE LOGIC         *
      ******************************************************************
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INTERNAL-TRANS ASSIGN TO "INTTRAN.DAT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-INT-STATUS.
           SELECT EXTERNAL-TRANS ASSIGN TO "EXTTRAN.DAT"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-EXT-STATUS.
           SELECT MATCHED-FILE ASSIGN TO "MATCHED.DAT"
               ORGANIZATION IS SEQUENTIAL.
           SELECT VARIANCE-FILE ASSIGN TO "VARIANCE.DAT"
               ORGANIZATION IS SEQUENTIAL.
           SELECT RECON-REPORT ASSIGN TO "RECONRPT.DAT"
               ORGANIZATION IS SEQUENTIAL.
           SELECT TOLERANCE-TABLE ASSIGN TO "TOLERANCE.DAT"
               ORGANIZATION IS SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  INTERNAL-TRANS.
       01  INT-TRANS-RECORD.
           05  INT-RECORD-TYPE     PIC X(2).
               88  INT-HEADER      VALUE "HD".
               88  INT-DETAIL      VALUE "DT".
               88  INT-TRAILER     VALUE "TR".
           05  INT-BATCH-NUM       PIC X(8).
           05  INT-TRANS-ID        PIC X(16).
           05  INT-TRANS-DATE      PIC 9(8).
           05  INT-SETTLE-DATE     PIC 9(8).
           05  INT-AMOUNT          PIC S9(11)V99 COMP-3.
           05  INT-TRANS-TYPE      PIC X(4).
           05  INT-ACCT-NUM        PIC X(20).
           05  INT-MERCHANT-ID     PIC X(15).
           05  INT-CURRENCY        PIC X(3).
           05  INT-STATUS          PIC X(2).
               88  INT-APPROVED    VALUE "AP".
               88  INT-SETTLED     VALUE "SE".
               88  INT-CANCELLED   VALUE "CN".
           05  INT-TIMESTAMP       PIC X(26).
           05  FILLER              PIC X(50).
       
       FD  EXTERNAL-TRANS.
       01  EXT-TRANS-RECORD.
           05  EXT-RECORD-TYPE     PIC X(2).
               88  EXT-HEADER      VALUE "HD".
               88  EXT-DETAIL      VALUE "DT".
               88  EXT-TRAILER     VALUE "TR".
           05  EXT-BATCH-NUM       PIC X(8).
           05  EXT-TRANS-ID        PIC X(16).
           05  EXT-TRANS-DATE      PIC 9(8).
           05  EXT-POST-DATE       PIC 9(8).
           05  EXT-AMOUNT          PIC S9(11)V99 COMP-3.
           05  EXT-TRANS-CODE      PIC X(4).
           05  EXT-ACCOUNT         PIC X(20).
           05  EXT-MERCHANT        PIC X(15).
           05  EXT-CURR-CODE       PIC X(3).
           05  EXT-PROC-STATUS     PIC X(2).
               88  EXT-SUCCESS     VALUE "OK".
               88  EXT-POSTED      VALUE "PS".
               88  EXT-VOID        VALUE "VD".
           05  EXT-PROC-TIME       PIC X(26).
           05  EXT-FEES            PIC S9(7)V99 COMP-3.
           05  FILLER              PIC X(35).
       
       FD  MATCHED-FILE.
       01  MATCHED-RECORD.
           05  MT-TRANS-ID         PIC X(16).
           05  MT-INT-AMOUNT       PIC S9(11)V99 COMP-3.
           05  MT-EXT-AMOUNT       PIC S9(11)V99 COMP-3.
           05  MT-VARIANCE         PIC S9(9)V99 COMP-3.
           05  MT-MATCH-TYPE       PIC X(10).
           05  MT-TIMESTAMP        PIC X(26).
       
       FD  VARIANCE-FILE.
       01  VARIANCE-RECORD.
           05  VAR-TYPE            PIC X(15).
               88  VAR-MISSING-INT VALUE "MISSING-INTRNL".
               88  VAR-MISSING-EXT VALUE "MISSING-EXTERN".
               88  VAR-AMOUNT-DIFF VALUE "AMOUNT-DIFF".
               88  VAR-STATUS-DIFF VALUE "STATUS-DIFF".
           05  VAR-TRANS-ID        PIC X(16).
           05  VAR-INT-AMOUNT      PIC S9(11)V99 COMP-3.
           05  VAR-EXT-AMOUNT      PIC S9(11)V99 COMP-3.
           05  VAR-DIFFERENCE      PIC S9(9)V99 COMP-3.
           05  VAR-BATCH-NUM       PIC X(8).
           05  VAR-DETAILS         PIC X(100).
       
       FD  RECON-REPORT.
       01  REPORT-LINE             PIC X(132).
       
       FD  TOLERANCE-TABLE.
       01  TOLERANCE-RECORD.
           05  TOL-TRANS-TYPE      PIC X(4).
           05  TOL-AMOUNT          PIC S9(7)V99 COMP-3.
           05  TOL-PERCENT         PIC 9V9999.
       
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUSES.
           05  WS-INT-STATUS       PIC XX.
               88  INT-OK          VALUE "00".
               88  INT-EOF         VALUE "10".
           05  WS-EXT-STATUS       PIC XX.
               88  EXT-OK          VALUE "00".
               88  EXT-EOF         VALUE "10".
       
       01  WS-CONTROL-TOTALS.
           05  WS-INT-COUNT        PIC 9(8) VALUE ZERO.
           05  WS-INT-TOTAL-AMT    PIC S9(13)V99 COMP-3 VALUE ZERO.
           05  WS-EXT-COUNT        PIC 9(8) VALUE ZERO.
           05  WS-EXT-TOTAL-AMT    PIC S9(13)V99 COMP-3 VALUE ZERO.
           05  WS-MATCHED-COUNT    PIC 9(8) VALUE ZERO.
           05  WS-MATCHED-AMT      PIC S9(13)V99 COMP-3 VALUE ZERO.
           05  WS-VARIANCE-COUNT   PIC 9(6) VALUE ZERO.
           05  WS-VARIANCE-AMT     PIC S9(13)V99 COMP-3 VALUE ZERO.
       
       01  WS-VARIANCE-BREAKDOWN.
           05  WS-MISSING-INT-CNT  PIC 9(6) VALUE ZERO.
           05  WS-MISSING-EXT-CNT  PIC 9(6) VALUE ZERO.
           05  WS-AMOUNT-DIFF-CNT  PIC 9(6) VALUE ZERO.
           05  WS-STATUS-DIFF-CNT  PIC 9(6) VALUE ZERO.
       
       01  WS-BATCH-CONTROLS.
           05  WS-CURR-BATCH       PIC X(8).
           05  WS-BATCH-INT-CNT    PIC 9(6) VALUE ZERO.
           05  WS-BATCH-EXT-CNT    PIC 9(6) VALUE ZERO.
           05  WS-BATCH-INT-AMT    PIC S9(13)V99 COMP-3 VALUE ZERO.
           05  WS-BATCH-EXT-AMT    PIC S9(13)V99 COMP-3 VALUE ZERO.
       
       01  WS-TOLERANCES.
           05  WS-DEFAULT-TOL-AMT  PIC S9(5)V99 COMP-3 VALUE 0.01.
           05  WS-DEFAULT-TOL-PCT  PIC 9V9999 VALUE 0.0001.
           05  WS-APPLIED-TOL-AMT  PIC S9(7)V99 COMP-3.
           05  WS-APPLIED-TOL-PCT  PIC 9V9999.
       
       01  WS-MATCH-TABLE.
           05  WS-MATCH-ENTRY OCCURS 10000 TIMES
               INDEXED BY MT-IDX.
               10  WS-MT-TRANS-ID  PIC X(16).
               10  WS-MT-AMOUNT    PIC S9(11)V99 COMP-3.
               10  WS-MT-MATCHED   PIC X VALUE 'N'.
       
       01  WS-CALCULATIONS.
           05  WS-AMOUNT-DIFF      PIC S9(11)V99 COMP-3.
           05  WS-ABS-DIFF         PIC S9(11)V99 COMP-3.
           05  WS-PERCENT-DIFF     PIC S9(5)V9999 COMP-3.
           05  WS-TOLERANCE-AMT    PIC S9(7)V99 COMP-3.
       
       01  WS-FLAGS.
           05  WS-INT-EOF-FLAG     PIC X VALUE 'N'.
           05  WS-EXT-EOF-FLAG     PIC X VALUE 'N'.
           05  WS-MATCH-FOUND      PIC X VALUE 'N'.
           05  WS-WITHIN-TOL       PIC X VALUE 'N'.
       
       01  WS-COUNTERS.
           05  WS-TABLE-SIZE       PIC 9(5) VALUE ZERO.
           05  WS-SEARCH-IDX       PIC 9(5).
           05  WS-LINE-COUNT       PIC 9(4) VALUE ZERO.
           05  WS-PAGE-COUNT       PIC 9(4) VALUE ZERO.
       
       01  WS-REPORT-HEADERS.
           05  WS-HEADER-1.
               10  FILLER          PIC X(50) VALUE SPACES.
               10  FILLER          PIC X(30) 
                   VALUE "BATCH RECONCILIATION REPORT".
               10  FILLER          PIC X(52) VALUE SPACES.
           05  WS-HEADER-2.
               10  FILLER          PIC X(10) VALUE "DATE: ".
               10  WS-RPT-DATE     PIC X(10).
               10  FILLER          PIC X(50) VALUE SPACES.
               10  FILLER          PIC X(10) VALUE "PAGE: ".
               10  WS-RPT-PAGE     PIC ZZZ9.
               10  FILLER          PIC X(48) VALUE SPACES.
       
       01  WS-DETAIL-LINE.
           05  WS-DL-TRANS-ID      PIC X(16).
           05  FILLER              PIC X(2) VALUE SPACES.
           05  WS-DL-INT-AMT       PIC $$$,$$$,$$9.99-.
           05  FILLER              PIC X(2) VALUE SPACES.
           05  WS-DL-EXT-AMT       PIC $$$,$$$,$$9.99-.
           05  FILLER              PIC X(2) VALUE SPACES.
           05  WS-DL-VARIANCE      PIC $$$,$$$,$$9.99-.
           05  FILLER              PIC X(2) VALUE SPACES.
           05  WS-DL-STATUS        PIC X(15).
           05  FILLER              PIC X(57) VALUE SPACES.
       
       01  WS-SUMMARY-LINE.
           05  WS-SL-LABEL         PIC X(30).
           05  FILLER              PIC X(5) VALUE SPACES.
           05  WS-SL-COUNT         PIC ZZZ,ZZZ,ZZ9.
           05  FILLER              PIC X(5) VALUE SPACES.
           05  WS-SL-AMOUNT        PIC $$$,$$$,$$$,$$9.99-.
           05  FILLER              PIC X(58) VALUE SPACES.
       
       01  WS-CURRENT-DATE.
           05  WS-CURR-YEAR        PIC 9(4).
           05  WS-CURR-MONTH       PIC 9(2).
           05  WS-CURR-DAY         PIC 9(2).
       
       PROCEDURE DIVISION.
       MAIN-PROCESS.
           PERFORM INIT-PROCESS
           PERFORM LOAD-TOLERANCE-TABLE
           PERFORM BUILD-INTERNAL-TABLE
           PERFORM RECONCILE-EXTERNAL-FILE
           PERFORM IDENTIFY-UNMATCHED-INTERNAL
           PERFORM PRINT-SUMMARY-REPORT
           PERFORM CLOSE-PROCESS
           STOP RUN.
       
       INIT-PROCESS.
           OPEN INPUT INTERNAL-TRANS
           OPEN INPUT EXTERNAL-TRANS
           OPEN INPUT TOLERANCE-TABLE
           OPEN OUTPUT MATCHED-FILE
           OPEN OUTPUT VARIANCE-FILE
           OPEN OUTPUT RECON-REPORT
           
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-RPT-DATE
           MOVE 1 TO WS-PAGE-COUNT
           PERFORM PRINT-REPORT-HEADERS.
       
       LOAD-TOLERANCE-TABLE.
           READ TOLERANCE-TABLE
               AT END MOVE 'Y' TO WS-INT-EOF-FLAG
           END-READ
           PERFORM UNTIL WS-INT-EOF-FLAG = 'Y'
               IF TOL-TRANS-TYPE = "DFLT"
                   MOVE TOL-AMOUNT TO WS-DEFAULT-TOL-AMT
                   MOVE TOL-PERCENT TO WS-DEFAULT-TOL-PCT
               END-IF
               READ TOLERANCE-TABLE
                   AT END MOVE 'Y' TO WS-INT-EOF-FLAG
               END-READ
           END-PERFORM
           MOVE 'N' TO WS-INT-EOF-FLAG
           CLOSE TOLERANCE-TABLE.
       
       BUILD-INTERNAL-TABLE.
           SET MT-IDX TO 1
           READ INTERNAL-TRANS
               AT END MOVE 'Y' TO WS-INT-EOF-FLAG
           END-READ
           
           PERFORM UNTIL WS-INT-EOF-FLAG = 'Y' OR MT-IDX > 10000
               IF INT-DETAIL
                   MOVE INT-TRANS-ID TO 
                       WS-MT-TRANS-ID(MT-IDX)
                   MOVE INT-AMOUNT TO 
                       WS-MT-AMOUNT(MT-IDX)
                   MOVE 'N' TO WS-MT-MATCHED(MT-IDX)
                   ADD 1 TO WS-INT-COUNT
                   ADD INT-AMOUNT TO WS-INT-TOTAL-AMT
                   SET MT-IDX UP BY 1
               END-IF
               READ INTERNAL-TRANS
                   AT END MOVE 'Y' TO WS-INT-EOF-FLAG
               END-READ
           END-PERFORM
           
           SUBTRACT 1 FROM MT-IDX GIVING WS-TABLE-SIZE
           CLOSE INTERNAL-TRANS.
       
       RECONCILE-EXTERNAL-FILE.
           READ EXTERNAL-TRANS
               AT END MOVE 'Y' TO WS-EXT-EOF-FLAG
           END-READ
           
           PERFORM UNTIL WS-EXT-EOF-FLAG = 'Y'
               IF EXT-DETAIL
                   PERFORM MATCH-TRANSACTION
                   ADD 1 TO WS-EXT-COUNT
                   ADD EXT-AMOUNT TO WS-EXT-TOTAL-AMT
               END-IF
               READ EXTERNAL-TRANS
                   AT END MOVE 'Y' TO WS-EXT-EOF-FLAG
               END-READ
           END-PERFORM
           
           CLOSE EXTERNAL-TRANS.
       
       MATCH-TRANSACTION.
           MOVE 'N' TO WS-MATCH-FOUND
           PERFORM VARYING WS-SEARCH-IDX FROM 1 BY 1
               UNTIL WS-SEARCH-IDX > WS-TABLE-SIZE
                  OR WS-MATCH-FOUND = 'Y'
               
               IF EXT-TRANS-ID = 
                   WS-MT-TRANS-ID(WS-SEARCH-IDX)
                   PERFORM CHECK-AMOUNT-MATCH
                   IF WS-WITHIN-TOL = 'Y'
                       PERFORM WRITE-MATCHED-RECORD
                       MOVE 'Y' TO WS-MT-MATCHED(WS-SEARCH-IDX)
                       MOVE 'Y' TO WS-MATCH-FOUND
                   ELSE
                       PERFORM WRITE-VARIANCE-RECORD
                       MOVE 'Y' TO WS-MT-MATCHED(WS-SEARCH-IDX)
                       MOVE 'Y' TO WS-MATCH-FOUND
                   END-IF
               END-IF
           END-PERFORM
           
           IF WS-MATCH-FOUND = 'N'
               PERFORM WRITE-MISSING-INTERNAL-VAR
           END-IF.
       
       CHECK-AMOUNT-MATCH.
           COMPUTE WS-AMOUNT-DIFF = 
               WS-MT-AMOUNT(WS-SEARCH-IDX) - EXT-AMOUNT
           COMPUTE WS-ABS-DIFF = FUNCTION ABS(WS-AMOUNT-DIFF)
           
           MOVE WS-DEFAULT-TOL-AMT TO WS-APPLIED-TOL-AMT
           
           IF WS-ABS-DIFF <= WS-APPLIED-TOL-AMT
               MOVE 'Y' TO WS-WITHIN-TOL
           ELSE
               MOVE 'N' TO WS-WITHIN-TOL
           END-IF.
       
       WRITE-MATCHED-RECORD.
           MOVE EXT-TRANS-ID TO MT-TRANS-ID
           MOVE WS-MT-AMOUNT(WS-SEARCH-IDX) TO MT-INT-AMOUNT
           MOVE EXT-AMOUNT TO MT-EXT-AMOUNT
           MOVE WS-AMOUNT-DIFF TO MT-VARIANCE
           MOVE "EXACT" TO MT-MATCH-TYPE
           MOVE FUNCTION CURRENT-DATE TO MT-TIMESTAMP
           WRITE MATCHED-RECORD
           
           ADD 1 TO WS-MATCHED-COUNT
           ADD MT-INT-AMOUNT TO WS-MATCHED-AMT.
       
       WRITE-VARIANCE-RECORD.
           MOVE "AMOUNT-DIFF" TO VAR-TYPE
           MOVE EXT-TRANS-ID TO VAR-TRANS-ID
           MOVE WS-MT-AMOUNT(WS-SEARCH-IDX) TO VAR-INT-AMOUNT
           MOVE EXT-AMOUNT TO VAR-EXT-AMOUNT
           MOVE WS-AMOUNT-DIFF TO VAR-DIFFERENCE
           MOVE EXT-BATCH-NUM TO VAR-BATCH-NUM
           STRING "INTERNAL: " DELIMITED BY SIZE
                  VAR-INT-AMOUNT DELIMITED BY SIZE
                  " EXTERNAL: " DELIMITED BY SIZE
                  VAR-EXT-AMOUNT DELIMITED BY SIZE
                  " DIFF: " DELIMITED BY SIZE
                  VAR-DIFFERENCE DELIMITED BY SIZE
               INTO VAR-DETAILS
           END-STRING
           WRITE VARIANCE-RECORD
           
           ADD 1 TO WS-VARIANCE-COUNT
           ADD 1 TO WS-AMOUNT-DIFF-CNT
           ADD WS-ABS-DIFF TO WS-VARIANCE-AMT.
       
       WRITE-MISSING-INTERNAL-VAR.
           MOVE "MISSING-INTRNL" TO VAR-TYPE
           MOVE EXT-TRANS-ID TO VAR-TRANS-ID
           MOVE ZERO TO VAR-INT-AMOUNT
           MOVE EXT-AMOUNT TO VAR-EXT-AMOUNT
           MOVE EXT-AMOUNT TO VAR-DIFFERENCE
           MOVE EXT-BATCH-NUM TO VAR-BATCH-NUM
           MOVE "TRANSACTION IN EXTERNAL FILE ONLY" 
               TO VAR-DETAILS
           WRITE VARIANCE-RECORD
           
           ADD 1 TO WS-VARIANCE-COUNT
           ADD 1 TO WS-MISSING-INT-CNT
           ADD EXT-AMOUNT TO WS-VARIANCE-AMT.
       
       IDENTIFY-UNMATCHED-INTERNAL.
           PERFORM VARYING MT-IDX FROM 1 BY 1
               UNTIL MT-IDX > WS-TABLE-SIZE
               
               IF WS-MT-MATCHED(MT-IDX) = 'N'
                   PERFORM WRITE-MISSING-EXTERNAL-VAR
               END-IF
           END-PERFORM.
       
       WRITE-MISSING-EXTERNAL-VAR.
           MOVE "MISSING-EXTERN" TO VAR-TYPE
           MOVE WS-MT-TRANS-ID(MT-IDX) TO VAR-TRANS-ID
           MOVE WS-MT-AMOUNT(MT-IDX) TO VAR-INT-AMOUNT
           MOVE ZERO TO VAR-EXT-AMOUNT
           MOVE WS-MT-AMOUNT(MT-IDX) TO VAR-DIFFERENCE
           MOVE SPACES TO VAR-BATCH-NUM
           MOVE "TRANSACTION IN INTERNAL FILE ONLY" 
               TO VAR-DETAILS
           WRITE VARIANCE-RECORD
           
           ADD 1 TO WS-VARIANCE-COUNT
           ADD 1 TO WS-MISSING-EXT-CNT
           ADD WS-MT-AMOUNT(MT-IDX) TO WS-VARIANCE-AMT.
       
       PRINT-REPORT-HEADERS.
           WRITE REPORT-LINE FROM WS-HEADER-1 AFTER ADVANCING PAGE
           MOVE WS-PAGE-COUNT TO WS-RPT-PAGE
           WRITE REPORT-LINE FROM WS-HEADER-2 AFTER ADVANCING 1 LINE
           WRITE REPORT-LINE FROM SPACES AFTER ADVANCING 2 LINES
           MOVE 3 TO WS-LINE-COUNT.
       
       PRINT-SUMMARY-REPORT.
           PERFORM PRINT-SUMMARY-SECTION
           PERFORM PRINT-VARIANCE-BREAKDOWN.
       
       PRINT-SUMMARY-SECTION.
           MOVE "INTERNAL TRANSACTIONS:" TO WS-SL-LABEL
           MOVE WS-INT-COUNT TO WS-SL-COUNT
           MOVE WS-INT-TOTAL-AMT TO WS-SL-AMOUNT
           WRITE REPORT-LINE FROM WS-SUMMARY-LINE 
               AFTER ADVANCING 2 LINES
           
           MOVE "EXTERNAL TRANSACTIONS:" TO WS-SL-LABEL
           MOVE WS-EXT-COUNT TO WS-SL-COUNT
           MOVE WS-EXT-TOTAL-AMT TO WS-SL-AMOUNT
           WRITE REPORT-LINE FROM WS-SUMMARY-LINE 
               AFTER ADVANCING 1 LINE
           
           MOVE "MATCHED TRANSACTIONS:" TO WS-SL-LABEL
           MOVE WS-MATCHED-COUNT TO WS-SL-COUNT
           MOVE WS-MATCHED-AMT TO WS-SL-AMOUNT
           WRITE REPORT-LINE FROM WS-SUMMARY-LINE 
               AFTER ADVANCING 1 LINE
           
           MOVE "VARIANCE TRANSACTIONS:" TO WS-SL-LABEL
           MOVE WS-VARIANCE-COUNT TO WS-SL-COUNT
           MOVE WS-VARIANCE-AMT TO WS-SL-AMOUNT
           WRITE REPORT-LINE FROM WS-SUMMARY-LINE 
               AFTER ADVANCING 1 LINE.
       
       PRINT-VARIANCE-BREAKDOWN.
           WRITE REPORT-LINE FROM SPACES AFTER ADVANCING 2 LINES
           MOVE "VARIANCE BREAKDOWN:" TO WS-SL-LABEL
           MOVE SPACES TO WS-SL-COUNT
           MOVE SPACES TO WS-SL-AMOUNT
           WRITE REPORT-LINE FROM WS-SUMMARY-LINE 
               AFTER ADVANCING 1 LINE
           
           MOVE "  MISSING IN INTERNAL:" TO WS-SL-LABEL
           MOVE WS-MISSING-INT-CNT TO WS-SL-COUNT
           WRITE REPORT-LINE FROM WS-SUMMARY-LINE 
               AFTER ADVANCING 1 LINE
           
           MOVE "  MISSING IN EXTERNAL:" TO WS-SL-LABEL
           MOVE WS-MISSING-EXT-CNT TO WS-SL-COUNT
           WRITE REPORT-LINE FROM WS-SUMMARY-LINE 
               AFTER ADVANCING 1 LINE
           
           MOVE "  AMOUNT DIFFERENCES:" TO WS-SL-LABEL
           MOVE WS-AMOUNT-DIFF-CNT TO WS-SL-COUNT
           WRITE REPORT-LINE FROM WS-SUMMARY-LINE 
               AFTER ADVANCING 1 LINE.
       
       CLOSE-PROCESS.
           CLOSE MATCHED-FILE
           CLOSE VARIANCE-FILE
           CLOSE RECON-REPORT
           
           DISPLAY "========================================="
           DISPLAY "BATCH RECONCILIATION COMPLETE"
           DISPLAY "========================================="
           DISPLAY "INTERNAL RECORDS: " WS-INT-COUNT
           DISPLAY "EXTERNAL RECORDS: " WS-EXT-COUNT
           DISPLAY "MATCHED: " WS-MATCHED-COUNT
           DISPLAY "VARIANCES: " WS-VARIANCE-COUNT
           DISPLAY "  - MISSING INTERNAL: " WS-MISSING-INT-CNT
           DISPLAY "  - MISSING EXTERNAL: " WS-MISSING-EXT-CNT
           DISPLAY "  - AMOUNT DIFFERENCES: " WS-AMOUNT-DIFF-CNT
           DISPLAY "=========================================".
