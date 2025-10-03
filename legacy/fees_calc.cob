       IDENTIFICATION DIVISION.
       PROGRAM-ID. FEES-CALC.
       AUTHOR. LEGACY-SYSTEM.
       DATE-WRITTEN. 1995-03-15.
      ******************************************************************
      * PURPOSE: CALCULATE TRANSACTION FEES BASED ON COMPLEX RULES    *
      * LAST MODIFIED: 2003-07-22 - ADDED TIER DISCOUNTS            *
      ******************************************************************
      *                                                                *
      * BUSINESS ANALYST NOTES:                                       *
      * =====================                                          *
      * This program calculates transaction fees in 3 steps:          *
      *   1. BASE FEE - Calculated by payment type (lines 74-83)      *
      *   2. TIER DISCOUNT - Applied based on customer tier (85-93)   *
      *   3. VOLUME ADJUSTMENT - Based on transaction size (95-101)   *
      *                                                                *
      * KEY BUSINESS QUESTIONS:                                       *
      *   - Where are the fee rates stored? (See WS-BASE-RATE)        *
      *   - Why is this a maintenance problem? (Hard-coded values!)   *
      *   - What happens when rates change? (Program must be edited)  *
      *   - How do we add new payment types? (Requires code changes)  *
      *                                                                *
      * EXERCISE: Use Copilot to extract these business rules into    *
      * a decision table showing all combinations and resulting fees. *
      ******************************************************************
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANS-FILE ASSIGN TO "TRANS.DAT"
               ORGANIZATION IS SEQUENTIAL.
           SELECT FEES-FILE ASSIGN TO "FEES.DAT"
               ORGANIZATION IS SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  TRANS-FILE.
       01  TRANS-RECORD.
           05  TR-ID           PIC X(10).
           05  TR-DATE         PIC 9(8).
           05  TR-CUST-ID      PIC X(10).
           05  TR-AMOUNT       PIC S9(7)V99 COMP-3.
           05  TR-TYPE         PIC X(2).
               88  CREDIT-TRANS    VALUE "CR".
               88  DEBIT-TRANS     VALUE "DB".
               88  WIRE-TRANS      VALUE "WR".
           05  TR-TIER         PIC X(1).
               88  GOLD-TIER       VALUE "G".
               88  SILVER-TIER     VALUE "S".
               88  BRONZE-TIER     VALUE "B".
       
       FD  FEES-FILE.
       01  FEES-RECORD.
           05  FE-ID           PIC X(10).
           05  FE-BASE-FEE     PIC S9(5)V99 COMP-3.
           05  FE-DISC-FEE     PIC S9(5)V99 COMP-3.
           05  FE-FINAL-FEE    PIC S9(5)V99 COMP-3.
       
       WORKING-STORAGE SECTION.
       01  WS-COUNTERS.
           05  WS-TRANS-COUNT  PIC 9(6) VALUE ZERO.
           05  WS-ERROR-COUNT  PIC 9(4) VALUE ZERO.
       
       01  WS-CALCULATIONS.
           05  WS-BASE-RATE    PIC 9V999 VALUE 0.025.
           05  WS-TIER-DISC    PIC 9V99.
           05  WS-VOLUME-ADJ   PIC 9V99.
       
       01  WS-FLAGS.
           05  WS-EOF          PIC X VALUE 'N'.
       
       PROCEDURE DIVISION.
       MAIN-PROCESS.
           PERFORM INIT-PROCESS
           PERFORM PROCESS-TRANSACTIONS UNTIL WS-EOF = 'Y'
           PERFORM CLOSE-PROCESS
           STOP RUN.
       
       INIT-PROCESS.
           OPEN INPUT TRANS-FILE
           OPEN OUTPUT FEES-FILE
           PERFORM READ-TRANS.
       
       PROCESS-TRANSACTIONS.
           PERFORM CALCULATE-BASE-FEE
           PERFORM APPLY-TIER-DISCOUNT
           PERFORM APPLY-VOLUME-ADJUSTMENT
           PERFORM WRITE-FEE-RECORD
           PERFORM READ-TRANS.
       
       CALCULATE-BASE-FEE.
           IF CREDIT-TRANS
               COMPUTE FE-BASE-FEE = TR-AMOUNT * WS-BASE-RATE
           ELSE IF DEBIT-TRANS
               COMPUTE FE-BASE-FEE = TR-AMOUNT * (WS-BASE-RATE * 0.8)
           ELSE IF WIRE-TRANS
               COMPUTE FE-BASE-FEE = TR-AMOUNT * (WS-BASE-RATE * 1.5)
                   + 25.00
           ELSE
               COMPUTE FE-BASE-FEE = TR-AMOUNT * WS-BASE-RATE.
       
       APPLY-TIER-DISCOUNT.
           IF GOLD-TIER
               MOVE 0.20 TO WS-TIER-DISC
           ELSE IF SILVER-TIER
               MOVE 0.10 TO WS-TIER-DISC
           ELSE
               MOVE 0.00 TO WS-TIER-DISC
           END-IF
           COMPUTE FE-DISC-FEE = FE-BASE-FEE * (1 - WS-TIER-DISC).
       
       APPLY-VOLUME-ADJUSTMENT.
           IF TR-AMOUNT > 10000
               COMPUTE FE-FINAL-FEE = FE-DISC-FEE * 0.95
           ELSE IF TR-AMOUNT > 5000
               COMPUTE FE-FINAL-FEE = FE-DISC-FEE * 0.98
           ELSE
               MOVE FE-DISC-FEE TO FE-FINAL-FEE.
           
           MOVE TR-ID TO FE-ID.
       
       WRITE-FEE-RECORD.
           WRITE FEES-RECORD
           ADD 1 TO WS-TRANS-COUNT.
       
       READ-TRANS.
           READ TRANS-FILE
               AT END MOVE 'Y' TO WS-EOF.
       
       CLOSE-PROCESS.
           DISPLAY "TRANSACTIONS PROCESSED: " WS-TRANS-COUNT
           CLOSE TRANS-FILE
           CLOSE FEES-FILE.
