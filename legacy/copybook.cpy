      ******************************************************************
      * STANDARD TRANSACTION RECORD LAYOUT - COPYBOOK                 *
      * USED BY: FEESCALC, TRANSRPT, MONTHEND, CUSTRISK              *
      * VERSION: 3.2 - UPDATED 2005-09-30                            *
      ******************************************************************
       01  TRANSACTION-RECORD.
           05  TRANS-KEY.
               10  TRANS-ID            PIC X(10).
               10  TRANS-DATE.
                   15  TRANS-YEAR      PIC 9(4).
                   15  TRANS-MONTH     PIC 9(2).
                   15  TRANS-DAY       PIC 9(2).
           05  TRANS-CUSTOMER.
               10  CUST-ID             PIC X(10).
               10  CUST-TIER           PIC X(1).
                   88  TIER-GOLD       VALUE 'G'.
                   88  TIER-SILVER     VALUE 'S'.
                   88  TIER-BRONZE     VALUE 'B'.
               10  CUST-REGION         PIC X(2).
                   88  REGION-NORTH    VALUE 'NO'.
                   88  REGION-SOUTH    VALUE 'SO'.
                   88  REGION-EAST     VALUE 'EA'.
                   88  REGION-WEST     VALUE 'WE'.
                   88  REGION-CENTRAL  VALUE 'CE'.
           05  TRANS-FINANCIAL.
               10  TRANS-AMOUNT        PIC S9(7)V99 COMP-3.
               10  TRANS-FEE           PIC S9(5)V99 COMP-3.
               10  TRANS-NET           PIC S9(7)V99 COMP-3.
           05  TRANS-TYPE-CODES.
               10  PAYMENT-METHOD      PIC X(2).
                   88  PAY-CREDIT      VALUE 'CR'.
                   88  PAY-DEBIT       VALUE 'DB'.
                   88  PAY-WIRE        VALUE 'WR'.
                   88  PAY-CASH        VALUE 'CA'.
               10  STATUS-CODE         PIC X(2).
                   88  STAT-COMPLETE   VALUE 'OK'.
                   88  STAT-FAILED     VALUE 'FL'.
                   88  STAT-PENDING    VALUE 'PD'.
                   88  STAT-REFUND     VALUE 'RF'.
           05  TRANS-AUDIT.
               10  CREATE-TIMESTAMP    PIC X(26).
               10  UPDATE-TIMESTAMP    PIC X(26).
               10  OPERATOR-ID         PIC X(8).
