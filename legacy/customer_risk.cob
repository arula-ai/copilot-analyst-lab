       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTOMER-RISK.
       AUTHOR. RISK-ANALYTICS-TEAM.
       DATE-WRITTEN. 1998-11-20.
      ******************************************************************
      * PURPOSE: CALCULATE CUSTOMER RISK SCORES BASED ON TRANSACTION  *
      *          PATTERNS, VOLUME, AND HISTORICAL BEHAVIOR            *
      * COMPLEXITY: Uses nested conditionals, table lookups, and      *
      *             compound risk calculations                        *
      * LAST MODIFIED: 2007-04-15 - ENHANCED FRAUD DETECTION         *
      ******************************************************************
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUST-TRANS-FILE ASSIGN TO "CUSTTRAN.DAT"
               ORGANIZATION IS INDEXED
               ACCESS IS DYNAMIC
               RECORD KEY IS CT-KEY
               ALTERNATE RECORD KEY IS CT-CUST-ID WITH DUPLICATES
               FILE STATUS IS WS-FILE-STATUS.
           SELECT RISK-OUTPUT ASSIGN TO "RISKOUT.DAT"
               ORGANIZATION IS SEQUENTIAL.
           SELECT RISK-PARAMS ASSIGN TO "RISKPRM.DAT"
               ORGANIZATION IS SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  CUST-TRANS-FILE.
       01  CUST-TRANS-RECORD.
           05  CT-KEY.
               10  CT-CUST-ID      PIC X(10).
               10  CT-TRANS-SEQ    PIC 9(8).
           05  CT-TRANS-DATE       PIC 9(8).
           05  CT-TRANS-TIME       PIC 9(6).
           05  CT-AMOUNT           PIC S9(9)V99 COMP-3.
           05  CT-TRANS-TYPE       PIC X(3).
           05  CT-REGION           PIC X(2).
           05  CT-CHANNEL          PIC X(3).
               88  ONLINE-CHANNEL  VALUE "ONL".
               88  BRANCH-CHANNEL  VALUE "BRN".
               88  ATM-CHANNEL     VALUE "ATM".
               88  MOBILE-CHANNEL  VALUE "MOB".
           05  CT-STATUS           PIC X(2).
               88  APPROVED        VALUE "AP".
               88  DECLINED        VALUE "DC".
               88  PENDING         VALUE "PN".
               88  REVERSED        VALUE "RV".
           05  CT-PRIOR-DECLINES   PIC 9(3).
           05  CT-VELOCITY-FLAG    PIC X(1).
               88  HIGH-VELOCITY   VALUE "H".
               88  NORMAL-VELOCITY VALUE "N".
           05  CT-GEO-MATCH        PIC X(1).
               88  GEO-EXPECTED    VALUE "Y".
               88  GEO-ANOMALY     VALUE "N".
       
       FD  RISK-OUTPUT.
       01  RISK-RECORD.
           05  RR-CUST-ID          PIC X(10).
           05  RR-RISK-SCORE       PIC 9(3).
           05  RR-RISK-CATEGORY    PIC X(10).
           05  RR-TRANS-COUNT      PIC 9(6).
           05  RR-DECLINE-RATE     PIC 9V999.
           05  RR-AVG-AMOUNT       PIC S9(7)V99 COMP-3.
           05  RR-FLAGS            PIC X(10).
           05  RR-RECOMMEND-ACTION PIC X(30).
       
       FD  RISK-PARAMS.
       01  RISK-PARAM-RECORD.
           05  RP-PARAM-TYPE       PIC X(20).
           05  RP-THRESHOLD        PIC S9(7)V99 COMP-3.
           05  RP-WEIGHT           PIC 9V999.
       
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS          PIC XX.
           88  FILE-OK             VALUE "00".
           88  FILE-EOF            VALUE "10".
           88  FILE-ERROR          VALUE "30" THRU "99".
       
       01  WS-COUNTERS.
           05  WS-CUST-PROCESSED   PIC 9(6) VALUE ZERO.
           05  WS-TRANS-COUNT      PIC 9(8) VALUE ZERO.
           05  WS-HIGH-RISK-COUNT  PIC 9(6) VALUE ZERO.
       
       01  WS-CURRENT-CUSTOMER.
           05  WS-CURR-CUST-ID     PIC X(10).
           05  WS-CURR-TRANS-CNT   PIC 9(6) VALUE ZERO.
           05  WS-CURR-DECLINE-CNT PIC 9(4) VALUE ZERO.
           05  WS-CURR-TOTAL-AMT   PIC S9(11)V99 COMP-3.
           05  WS-CURR-AVG-AMT     PIC S9(9)V99 COMP-3.
       
       01  WS-RISK-CALCULATION.
           05  WS-BASE-SCORE       PIC 9(3) VALUE ZERO.
           05  WS-VELOCITY-SCORE   PIC 9(3) VALUE ZERO.
           05  WS-DECLINE-SCORE    PIC 9(3) VALUE ZERO.
           05  WS-GEO-SCORE        PIC 9(3) VALUE ZERO.
           05  WS-AMOUNT-SCORE     PIC 9(3) VALUE ZERO.
           05  WS-CHANNEL-SCORE    PIC 9(3) VALUE ZERO.
           05  WS-FINAL-SCORE      PIC 9(3) VALUE ZERO.
       
       01  WS-RISK-THRESHOLDS.
           05  WS-HIGH-RISK-MIN    PIC 9(3) VALUE 700.
           05  WS-MED-RISK-MIN     PIC 9(3) VALUE 400.
           05  WS-DECLINE-LIMIT    PIC 9V999 VALUE 0.150.
           05  WS-VELOCITY-LIMIT   PIC 9(4) VALUE 0050.
           05  WS-LARGE-TRANS-AMT  PIC S9(7)V99 COMP-3 VALUE 50000.00.
       
       01  WS-FLAGS.
           05  WS-EOF              PIC X VALUE 'N'.
           05  WS-FIRST-RECORD     PIC X VALUE 'Y'.
           05  WS-PARAMS-LOADED    PIC X VALUE 'N'.
       
       01  WS-ANOMALY-FLAGS.
           05  WS-FLAG-VELOCITY    PIC X VALUE SPACE.
           05  WS-FLAG-GEO         PIC X VALUE SPACE.
           05  WS-FLAG-AMOUNT      PIC X VALUE SPACE.
           05  WS-FLAG-DECLINE     PIC X VALUE SPACE.
           05  WS-FLAG-PATTERN     PIC X VALUE SPACE.
           05  FILLER              PIC X(5) VALUE SPACES.
       
       01  WS-CALCULATIONS.
           05  WS-DECLINE-RATE     PIC 9V999.
           05  WS-WORK-AMT         PIC S9(11)V99 COMP-3.
           05  WS-WORK-COUNT       PIC 9(6).
       
       PROCEDURE DIVISION.
       MAIN-PROCESS.
           PERFORM INIT-PROCESS
           PERFORM LOAD-RISK-PARAMETERS
           PERFORM PROCESS-CUSTOMERS UNTIL WS-EOF = 'Y'
           PERFORM CLOSE-PROCESS
           STOP RUN.
       
       INIT-PROCESS.
           OPEN INPUT CUST-TRANS-FILE
           OPEN INPUT RISK-PARAMS
           OPEN OUTPUT RISK-OUTPUT
           PERFORM READ-NEXT-TRANSACTION.
       
       LOAD-RISK-PARAMETERS.
           READ RISK-PARAMS
               AT END MOVE 'Y' TO WS-PARAMS-LOADED
           END-READ
           IF NOT FILE-ERROR
               PERFORM PROCESS-PARAMETERS UNTIL FILE-EOF
               CLOSE RISK-PARAMS
               MOVE 'Y' TO WS-PARAMS-LOADED
           ELSE
               DISPLAY "WARNING: USING DEFAULT RISK PARAMETERS"
               MOVE 'Y' TO WS-PARAMS-LOADED
           END-IF.
       
       PROCESS-PARAMETERS.
           EVALUATE RP-PARAM-TYPE
               WHEN "HIGH_RISK_THRESHOLD"
                   MOVE RP-THRESHOLD TO WS-HIGH-RISK-MIN
               WHEN "MED_RISK_THRESHOLD"
                   MOVE RP-THRESHOLD TO WS-MED-RISK-MIN
               WHEN "DECLINE_RATE_LIMIT"
                   MOVE RP-THRESHOLD TO WS-DECLINE-LIMIT
               WHEN "VELOCITY_THRESHOLD"
                   MOVE RP-THRESHOLD TO WS-VELOCITY-LIMIT
               WHEN "LARGE_TRANS_AMOUNT"
                   MOVE RP-THRESHOLD TO WS-LARGE-TRANS-AMT
           END-EVALUATE
           READ RISK-PARAMS
               AT END CONTINUE
           END-READ.
       
       PROCESS-CUSTOMERS.
           IF WS-FIRST-RECORD = 'Y'
               MOVE CT-CUST-ID TO WS-CURR-CUST-ID
               MOVE 'N' TO WS-FIRST-RECORD
               PERFORM INIT-CUSTOMER-TOTALS
           END-IF
           
           IF CT-CUST-ID NOT = WS-CURR-CUST-ID
               PERFORM CALCULATE-CUSTOMER-RISK
               PERFORM WRITE-RISK-RECORD
               MOVE CT-CUST-ID TO WS-CURR-CUST-ID
               PERFORM INIT-CUSTOMER-TOTALS
           END-IF
           
           PERFORM ACCUMULATE-TRANSACTION-DATA
           PERFORM READ-NEXT-TRANSACTION.
       
       INIT-CUSTOMER-TOTALS.
           MOVE ZERO TO WS-CURR-TRANS-CNT
           MOVE ZERO TO WS-CURR-DECLINE-CNT
           MOVE ZERO TO WS-CURR-TOTAL-AMT
           MOVE ZERO TO WS-CURR-AVG-AMT
           MOVE SPACES TO WS-ANOMALY-FLAGS.
       
       ACCUMULATE-TRANSACTION-DATA.
           ADD 1 TO WS-CURR-TRANS-CNT
           ADD 1 TO WS-TRANS-COUNT
           ADD CT-AMOUNT TO WS-CURR-TOTAL-AMT
           
           IF DECLINED
               ADD 1 TO WS-CURR-DECLINE-CNT
           END-IF
           
           IF HIGH-VELOCITY
               MOVE 'V' TO WS-FLAG-VELOCITY
           END-IF
           
           IF GEO-ANOMALY
               MOVE 'G' TO WS-FLAG-GEO
           END-IF
           
           IF CT-AMOUNT > WS-LARGE-TRANS-AMT
               MOVE 'A' TO WS-FLAG-AMOUNT
           END-IF
           
           IF CT-PRIOR-DECLINES > 5
               MOVE 'D' TO WS-FLAG-DECLINE
           END-IF.
       
       CALCULATE-CUSTOMER-RISK.
           PERFORM CALCULATE-BASE-RISK
           PERFORM CALCULATE-VELOCITY-RISK
           PERFORM CALCULATE-DECLINE-RISK
           PERFORM CALCULATE-GEO-RISK
           PERFORM CALCULATE-AMOUNT-RISK
           PERFORM CALCULATE-CHANNEL-RISK
           PERFORM COMPUTE-FINAL-SCORE
           PERFORM DETERMINE-RISK-CATEGORY.
       
       CALCULATE-BASE-RISK.
           MOVE 100 TO WS-BASE-SCORE.
       
       CALCULATE-VELOCITY-RISK.
           IF WS-FLAG-VELOCITY = 'V'
               COMPUTE WS-VELOCITY-SCORE = 150
           ELSE IF WS-CURR-TRANS-CNT > WS-VELOCITY-LIMIT
               COMPUTE WS-VELOCITY-SCORE = 100
           ELSE
               MOVE ZERO TO WS-VELOCITY-SCORE
           END-IF.
       
       CALCULATE-DECLINE-RISK.
           IF WS-CURR-TRANS-CNT > ZERO
               COMPUTE WS-DECLINE-RATE ROUNDED = 
                   WS-CURR-DECLINE-CNT / WS-CURR-TRANS-CNT
               IF WS-DECLINE-RATE > WS-DECLINE-LIMIT
                   COMPUTE WS-DECLINE-SCORE = 200
               ELSE IF WS-DECLINE-RATE > (WS-DECLINE-LIMIT * 0.5)
                   COMPUTE WS-DECLINE-SCORE = 100
               ELSE
                   MOVE ZERO TO WS-DECLINE-SCORE
               END-IF
           ELSE
               MOVE ZERO TO WS-DECLINE-SCORE
           END-IF.
       
       CALCULATE-GEO-RISK.
           IF WS-FLAG-GEO = 'G'
               MOVE 175 TO WS-GEO-SCORE
           ELSE
               MOVE ZERO TO WS-GEO-SCORE
           END-IF.
       
       CALCULATE-AMOUNT-RISK.
           IF WS-CURR-TRANS-CNT > ZERO
               COMPUTE WS-CURR-AVG-AMT ROUNDED = 
                   WS-CURR-TOTAL-AMT / WS-CURR-TRANS-CNT
               IF WS-CURR-AVG-AMT > WS-LARGE-TRANS-AMT
                   MOVE 125 TO WS-AMOUNT-SCORE
               ELSE IF WS-FLAG-AMOUNT = 'A'
                   MOVE 75 TO WS-AMOUNT-SCORE
               ELSE
                   MOVE ZERO TO WS-AMOUNT-SCORE
               END-IF
           ELSE
               MOVE ZERO TO WS-AMOUNT-SCORE
           END-IF.
       
       CALCULATE-CHANNEL-RISK.
           IF ONLINE-CHANNEL OR MOBILE-CHANNEL
               MOVE 50 TO WS-CHANNEL-SCORE
           ELSE
               MOVE ZERO TO WS-CHANNEL-SCORE
           END-IF.
       
       COMPUTE-FINAL-SCORE.
           COMPUTE WS-FINAL-SCORE = 
               WS-BASE-SCORE + 
               WS-VELOCITY-SCORE + 
               WS-DECLINE-SCORE + 
               WS-GEO-SCORE + 
               WS-AMOUNT-SCORE + 
               WS-CHANNEL-SCORE
           
           IF WS-FINAL-SCORE > 999
               MOVE 999 TO WS-FINAL-SCORE
           END-IF.
       
       DETERMINE-RISK-CATEGORY.
           MOVE WS-CURR-CUST-ID TO RR-CUST-ID
           MOVE WS-FINAL-SCORE TO RR-RISK-SCORE
           MOVE WS-CURR-TRANS-CNT TO RR-TRANS-COUNT
           MOVE WS-DECLINE-RATE TO RR-DECLINE-RATE
           MOVE WS-CURR-AVG-AMT TO RR-AVG-AMOUNT
           MOVE WS-ANOMALY-FLAGS TO RR-FLAGS
           
           IF WS-FINAL-SCORE >= WS-HIGH-RISK-MIN
               MOVE "HIGH-RISK" TO RR-RISK-CATEGORY
               MOVE "ENHANCED DUE DILIGENCE" TO RR-RECOMMEND-ACTION
               ADD 1 TO WS-HIGH-RISK-COUNT
           ELSE IF WS-FINAL-SCORE >= WS-MED-RISK-MIN
               MOVE "MEDIUM" TO RR-RISK-CATEGORY
               MOVE "STANDARD MONITORING" TO RR-RECOMMEND-ACTION
           ELSE
               MOVE "LOW" TO RR-RISK-CATEGORY
               MOVE "NORMAL PROCESSING" TO RR-RECOMMEND-ACTION
           END-IF.
       
       WRITE-RISK-RECORD.
           WRITE RISK-RECORD
           ADD 1 TO WS-CUST-PROCESSED.
       
       READ-NEXT-TRANSACTION.
           READ CUST-TRANS-FILE NEXT RECORD
               AT END MOVE 'Y' TO WS-EOF
           END-READ
           IF FILE-ERROR
               DISPLAY "FILE ERROR: " WS-FILE-STATUS
               MOVE 'Y' TO WS-EOF
           END-IF.
       
       CLOSE-PROCESS.
           IF WS-CURR-TRANS-CNT > ZERO
               PERFORM CALCULATE-CUSTOMER-RISK
               PERFORM WRITE-RISK-RECORD
           END-IF
           
           DISPLAY "======================================"
           DISPLAY "CUSTOMER RISK ANALYSIS COMPLETE"
           DISPLAY "======================================"
           DISPLAY "CUSTOMERS PROCESSED: " WS-CUST-PROCESSED
           DISPLAY "TRANSACTIONS ANALYZED: " WS-TRANS-COUNT
           DISPLAY "HIGH RISK CUSTOMERS: " WS-HIGH-RISK-COUNT
           DISPLAY "======================================"
           
           CLOSE CUST-TRANS-FILE
           CLOSE RISK-OUTPUT.
