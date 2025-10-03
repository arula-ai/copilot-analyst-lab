# Legacy System Documentation

## Overview
These COBOL programs and JCL job streams represent a transaction fee calculation system that has been in production since 1995.

## Business Context
- Processes approximately 10,000 transactions daily
- Calculates fees based on transaction type, customer tier, and volume
- Runs as overnight batch job at 2:00 AM
- Critical for revenue recognition and billing

## System Components

### COBOL Programs

#### fees_calc.cob
Basic fee calculation program that processes transactions and applies tiered discounts.
- **Inputs:** TRANS.DAT (transaction file)
- **Outputs:** FEES.DAT (calculated fees)
- **Business Rules:** 
  - Base rate of 2.5% for credit transactions
  - Debit transactions get 20% discount on base rate
  - Wire transfers get 50% markup plus $25 flat fee
  - Gold tier: 20% discount, Silver: 10% discount
  - Volume adjustments based on transaction size

#### customer_risk.cob
Complex risk scoring system using indexed files and multi-factor analysis.
- **Inputs:** CUSTTRAN.DAT (indexed customer transactions), RISKPRM.DAT (risk parameters)
- **Outputs:** RISKOUT.DAT (risk scores and recommendations)
- **Complexity Features:**
  - Indexed file processing with dynamic access
  - Compound risk calculations using velocity, geography, amount, and decline patterns
  - Table lookups for parameterized thresholds
  - Multi-level scoring (base + velocity + decline + geo + amount + channel)
  - Risk categorization (High/Medium/Low) with automated actions

#### batch_reconcile.cob
End-of-day reconciliation matching internal and external transaction files.
- **Inputs:** INTTRAN.DAT (internal), EXTTRAN.DAT (external), TOLERANCE.DAT
- **Outputs:** MATCHED.DAT, VARIANCE.DAT, RECONRPT.DAT
- **Complexity Features:**
  - In-memory table processing (10,000 transactions)
  - Multi-key matching algorithm
  - Tolerance-based variance detection
  - Missing record identification in both directions
  - Detailed variance breakdown and reporting

### JCL Job Streams

#### job_stream.jcl
Daily fee calculation job stream with 6 steps.
- Backup management
- File sorting
- Fee calculation execution
- Report generation
- Operator notification

#### risk_batch.jcl
Weekly risk scoring batch with 9 steps using advanced JCL features.
- Work file cleanup
- Multi-file extraction with INCLUDE conditions
- VSAM cluster definition and REPRO
- Risk program execution
- Sort by risk score (descending)
- High-risk report generation
- FTP export to fraud system
- Conditional cleanup

## Known Issues
1. Hard-coded fee rates in program (maintenance nightmare)
2. Packed decimal arithmetic sometimes causes precision errors
3. No handling for new payment types (mobile, crypto)
4. Date handling assumes YYYYMMDD format
5. Regional codes limited to 2 characters
6. In-memory table size limited to 10,000 in reconciliation
7. Risk parameters file single-threaded - no concurrent updates
8. No real-time alerts - batch only

## Modernization Goals
- Move to real-time processing
- Externalize business rules to database tables
- Add comprehensive audit trails
- Support new payment methods and channels
- Improve error handling and recovery
- Replace indexed files with relational database
- Implement event-driven architecture for risk alerts
- Add API interfaces for system integration

## Critical Business Rules (from stakeholder interviews)
- Wire transfers have minimum $25 fee regardless of amount
- Gold tier customers never pay more than 2% in fees total
- Failed transactions over $500 need immediate alerts to fraud team
- Month-end totals must match GL within $0.01 or reconciliation fails
- Risk scores above 700 require enhanced due diligence before processing
- Geographic anomalies trigger immediate scoring increase
- Decline rates above 15% automatically flag for review

## Data Volumes
- **Daily Transactions:** 8,000 - 12,000
- **Weekly Risk Scoring:** 50,000 - 60,000 transactions analyzed
- **Reconciliation:** Typically 10,000 transactions per day
- **Variance Rate:** Target < 0.1%, actual ~0.05%

## Performance Characteristics
- **Fee Calculation:** ~15 minutes for 10,000 transactions
- **Risk Scoring:** ~25 minutes for full weekly analysis
- **Reconciliation:** ~10 minutes for daily batch
- **Peak Processing:** Runs during 2:00 AM - 4:00 AM maintenance window
