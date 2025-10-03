# Instructor Guide - Expected Outcomes & Answer Key

## Purpose
This guide helps instructors evaluate student work and understand expected outcomes for each lab exercise.

---

## Data Quality Issues Students Should Find

### In transactions.csv (500 rows total)

| Issue Type | Count | Examples | Impact |
|------------|-------|----------|--------|
| Missing/Empty Values | ~25 cells (5%) | Empty product_category, amount, etc. | Data completeness issues |
| Duplicate transaction_ids | 3 duplicates | T101, T205, T367 each appear twice | Primary key violation |
| Negative amounts | 4-5 records | Various negative values | Business rule violation |
| Invalid date formats | 3-4 records | "01/15/2024", "2024-13-45" | Data type/parsing errors |
| Data type inconsistencies | Throughout | Amounts stored as text in some cases | Calculation errors |

**Expected Student Deliverable:**
- Identification of 8-10+ data quality issues
- Impact assessment for each
- Recommended fixes (e.g., "Establish PK constraint", "Add NOT NULL validation", "Implement date format standardization")

---

## Flawed SQL Errors to Identify

### Query 1: Monthly Revenue by Customer Tier
**Error:** Missing `c.tier` in GROUP BY clause  
**Symptom:** SQL will fail or produce incorrect results  
**Fix:** `GROUP BY MONTH(transaction_date), c.tier`

### Query 2: Find High-Value Failed Transactions
**Error 1:** Column name 'date' should be 'transaction_date'  
**Symptom:** Column not found error  
**Fix:** Change `date` to `transaction_date`

**Error 2:** String comparison on numeric field (`amount > '500'`)  
**Symptom:** Incorrect results due to string comparison  
**Fix:** Change to `amount > 500` (numeric)

### Query 3: Customer Ranking
**Error 1:** Can't use alias `total_spent` in OVER clause  
**Symptom:** SQL error - column not found  
**Fix:** `RANK() OVER (ORDER BY SUM(amount) DESC)`

**Error 2:** Can't use alias `total_spent` in HAVING clause (some databases)  
**Symptom:** May fail depending on database  
**Fix:** `HAVING SUM(amount) > 10000`

**Error 3:** Wrong status value - 'Complete' vs 'Completed'  
**Symptom:** No results returned  
**Fix:** Change to `WHERE status = 'Completed'`

**Total Errors:** 6 across 3 queries

---

## Flawed Analysis Document Errors

### flawed_analysis.md

| Error | What's Wrong | Why It's Wrong |
|-------|--------------|----------------|
| "110% of transactions were successful" | Impossible percentage | Cannot exceed 100% |
| "Revenue increased 50% every single month without fail" | Unrealistic pattern | No business has perfectly consistent growth |
| "Absolutely no data quality issues" | Contradicts reality | Real data always has issues |
| "Gold tier customers always spend exactly $5,000" | Incorrect generalization | No variance is statistically impossible |
| "North 25%, South 25%, East 25%, West 25%, Central 30%" | Math error | Percentages sum to 125% |
| "No verification or testing was performed" | Missing validation | Invalidates all findings |
| "Analyzed data from 2023 to 2025" | Impossible date range | 2025 is in the future (assuming 2024 lab) |
| "10 million transaction dataset" | Wrong count | Actual dataset is 500 rows |

**Expected Student Deliverable:**
- Identification of 8+ logical errors
- Explanation of why each is wrong
- Recommendation for proper analysis approach

---

## Flawed Python Code Errors

### flawed_join_logic.py

| Issue # | Location | Error | Impact | Fix |
|---------|----------|-------|--------|-----|
| 1 | Line 11 | Not handling null values before merge | Failed joins for null customer_ids | Add `.dropna(subset=['customer_id'])` |
| 2 | Line 13 | Including all transactions in revenue calc | Incorrect revenue (includes failed/pending) | Filter: `merged[merged['status'] == 'Completed']` |
| 3 | Line 16 | Date comparison with string | Type error or wrong results | Convert date column to datetime first |
| 4 | Line 19 | Division by zero not handled | Runtime error if customer has 0 transactions | Add try/except or check for zero |
| 5 | Line 23-24 | Discount calculation error | Gold tier: 0.8 = 20% off, should be 0.9 = 10% off | Fix discount multipliers |
| 6 | Line 31 | Applying function to wrong dataframe | Using `transactions_df` instead of `merged` | Change to `merged.apply(...)` |
| 7 | Throughout | No error handling | Code will crash on unexpected data | Add try/except blocks |

**Expected Student Deliverable:**
- Identification of 5-7 errors
- Corrected code with explanations
- Test cases showing before/after behavior

---

## COBOL Business Rules Extraction

### From fees_calc.cob

Students should extract these business rules:

#### Base Fee Calculation (lines 74-83)
| Payment Type | Base Rate | Calculation |
|--------------|-----------|-------------|
| Credit (CR) | 2.5% | amount × 0.025 |
| Debit (DB) | 2.0% | amount × 0.025 × 0.8 |
| Wire (WR) | 3.75% + $25 | amount × 0.025 × 1.5 + 25.00 |
| Other | 2.5% | amount × 0.025 (default) |

#### Tier Discount (lines 85-93)
| Tier | Discount |
|------|----------|
| Gold | 20% off base fee |
| Silver | 10% off base fee |
| Bronze | 0% off base fee |

#### Volume Adjustment (lines 95-101)
| Transaction Amount | Adjustment |
|-------------------|------------|
| > $10,000 | Additional 5% off |
| > $5,000 | Additional 2% off |
| ≤ $5,000 | No adjustment |

#### Decision Table Example
| Payment | Tier | Amount | Base Fee | After Tier | Final Fee |
|---------|------|--------|----------|------------|-----------|
| Credit | Gold | $1,000 | $25.00 | $20.00 | $20.00 |
| Wire | Silver | $15,000 | $587.50 | $528.75 | $502.31 |
| Debit | Bronze | $500 | $10.00 | $10.00 | $10.00 |

---

## Risk Register - Expected Risks

### From fees_calc.cob and customer_risk.cob

| Risk ID | Risk Description | Likelihood | Impact | Mitigation |
|---------|------------------|------------|--------|------------|
| R001 | Hard-coded fee rates make changes difficult | High | High | Externalize to configuration table |
| R002 | Packed decimal (COMP-3) precision errors | Medium | High | Use appropriate rounding, validate totals |
| R003 | No handling for new payment types | High | Medium | Add payment type validation and default handling |
| R004 | Date format assumes YYYYMMDD only | Medium | Medium | Implement flexible date parsing |
| R005 | In-memory table size limited to 10,000 | Low | High | Implement pagination or database processing |
| R006 | No real-time alerts - batch only | High | High | Implement event-driven architecture |
| R007 | Single-threaded risk parameter updates | Medium | Medium | Implement locking or versioning |
| R008 | Minimal error handling in calculations | High | High | Add comprehensive validation and error logging |

---

## Sample Deliverables - What Good Looks Like

### Data Quality Documentation (DATA_NOTES.md)

```markdown
## Data Quality Issues

### Issue 1: Duplicate Transaction IDs
- **Description:** Transaction IDs T101, T205, T367 appear twice
- **Impact:** Violates primary key constraint, causes reporting errors
- **Records Affected:** 3 duplicates (6 total records)
- **Recommendation:** Implement unique constraint, investigate root cause

### Issue 2: Missing Values
- **Description:** ~5% of cells are empty across non-key columns
- **Impact:** Incomplete analysis, calculation errors
- **Examples:** Empty product_category (T007), empty amounts
- **Recommendation:** Add NOT NULL constraints, implement data validation
```

### Business Logic Documentation (REQ_Logic.md)

```markdown
## Fee Calculation Business Rules

### Rule 1: Base Fee by Payment Type
**Input:** Transaction amount, Payment type (CR/DB/WR)
**Logic:** 
- Credit: Amount × 2.5%
- Debit: Amount × 2.0% (20% discount on base rate)
- Wire: Amount × 3.75% + $25 flat fee
**Output:** Base fee amount
**Exceptions:** Unknown payment types default to 2.5%
```

---

## Common Student Mistakes

### Mistake 1: Not Validating Copilot Output
**Symptom:** Accepting SQL without running it  
**Coaching:** "Run this SQL on the sample data. Do the results make sense?"

### Mistake 2: Vague Prompts
**Symptom:** Getting generic or incorrect responses  
**Coaching:** "Use the RIFCC framework. Specify your role, inputs, desired format."

### Mistake 3: Missing Business Context
**Symptom:** Technical documentation without business meaning  
**Coaching:** "Explain this in terms a business stakeholder would understand."

### Mistake 4: Incomplete Error Analysis
**Symptom:** Finding 2-3 errors when there are 6  
**Coaching:** "Review each query carefully. Check GROUP BY, data types, and column names."

---

## Grading Rubric (Optional)

### Data Analysis (25 points)
- Identified 8+ data quality issues (10 pts)
- Provided impact assessment (8 pts)
- Recommended fixes (7 pts)

### Legacy Analysis (35 points)
- Extracted business rules accurately (15 pts)
- Created decision table (10 pts)
- Documented 5+ risks (10 pts)

### Governance (25 points)
- Found all SQL errors (10 pts)
- Identified analysis flaws (10 pts)
- Fixed Python code (5 pts)

### Documentation (15 points)
- Used templates (5 pts)
- Included assumptions (5 pts)
- Professional quality (5 pts)

---

## Timing Checkpoints

| Time | Checkpoint | What to Look For |
|------|------------|------------------|
| 10 min | Setup complete | Students have Copilot open and working |
| 25 min | Data analysis done | At least 5+ data quality issues identified |
| 40 min | COBOL analysis done | Business rules extracted into templates |
| 55 min | Excel to SQL done | SQL queries written and tested |
| 65 min | Governance done | Errors identified and documented |
| 75 min | Wrap-up | Artifacts saved to /outputs/ |

---

## Facilitation Tips

1. **Encourage Iteration:** Show students how to refine prompts based on Copilot's responses
2. **Demonstrate Validation:** Show how to test SQL against sample data
3. **Share Good Examples:** Show strong student work as examples
4. **Address Privacy:** Reinforce the importance of using only synthetic data
5. **Promote Critical Thinking:** Remind students to validate, not just accept, AI output

---

## Extension Activities (For Fast Finishers)

1. Analyze the more complex COBOL programs (customer_risk.cob, batch_reconcile.cob)
2. Create a modernization proposal for the legacy system
3. Generate SQL to load the CSV data and validate all business rules
4. Create a complete data quality assessment report
5. Design test cases for the legacy system business rules
