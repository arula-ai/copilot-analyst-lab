# Sample Data - Business Context

## Business Scenario

You are a Business Analyst at **Global Payments Inc**, a B2B payment processor handling transactions across 5 regions (North, South, East, West, Central).

Your job:
- Spot data issues before reporting
- Use `data/schema.sql` for all table references (no database install required)
- Map patterns by tier + region
- Probe failed transactions + revenue loss
- Draft process improvement recs

---

## Dataset Overview

### transactions.csv (500 records)
Transaction data from January 1, 2024 to October 31, 2024

**Columns:**
- `transaction_id` - Unique identifier (T001-T500)
- `date` - Transaction date
- `customer_id` - Customer reference (C001-C050)
- `product_category` - Electronics, Clothing, Food, Services, Other
- `amount` - Transaction amount in USD
- `status` - Completed, Failed, Pending, Refunded
- `payment_method` - Credit, Debit, Cash, Wire, Other
- `region` - Geographic region

### customers.csv (50 records)
Customer master data

**Columns:**
- `customer_id` - Unique identifier (C001-C050)
- `company_name` - Generic Company 1-50 (synthetic names)
- `tier` - Gold, Silver, or Bronze service level
- `annual_revenue` - Company annual revenue
- `region` - Primary operating region
- `account_manager` - Assigned account manager (AM01-AM05)

---

## Business Questions to Answer

Use GitHub Copilot to help answer these questions:

### Data Quality
1. How many transactions are missing critical data?
2. Are there any duplicate transaction IDs?
3. Do all customer_ids in transactions exist in customers table?
4. Are there any invalid or suspicious amounts?
5. What percentage of transactions have data quality issues?

### Business Analysis
1. Which region has the highest failure rate?
2. Do Gold tier customers have different transaction patterns than Bronze?
3. What percentage of Wire transfers exceed $1,000?
4. What is the average transaction amount by product category?
5. Which payment method has the highest success rate?

**Excel-style baselines you can reference when asking for SQL:**
(See [`reference/formulas/EXCEL_TO_SQL_BASELINES.md`](../reference/formulas/EXCEL_TO_SQL_BASELINES.md) for more examples.)
- Monthly revenue trend: `=SUMIFS(Amount, Status, "Completed", Date, ">=" & start_of_month, Date, "<=" & end_of_month)`
- Customer 80/20 concentration: `=LET(sorted, SORT(Table[Revenue],,-1), running, SCAN(0, sorted, LAMBDA(a,b,a+b)), running / SUM(sorted))`
- Failed transaction alert: `=COUNTIFS(Status, "Failed", Amount, ">=500", Date, ">=" & TODAY()-7)`

### Revenue Analysis
1. What is total revenue by month (Completed transactions only)?
2. Which customers generate the most revenue?
3. How much potential revenue was lost to failed transactions?
4. What is the revenue distribution across regions?

---

## Known Data Quality Issues

**IMPORTANT**: This is **real-world messy data** on purpose!

Do this:
1. **Find** data defects
2. **Document** impact
3. **Recommend** fixes
4. **Validate** SQL against bad data

**Hint to get started:** 
```
Ask Copilot: "How many unique transaction_ids should there be in a 
500-row dataset? How many unique transaction_ids are actually present?"
```
Use `#transactions.csv` or similar tags so Copilot pulls the right file into context.

---

## Sample Analysis Workflow

### Step 1: Initial Data Profiling
```
Copilot Prompt: "Analyze #transactions.csv and provide:
- Row count
- Column data types
- Missing value count per column
- Unique values for categorical fields"
```

### Step 2: Data Quality Assessment
```
Copilot Prompt: "Identify data quality issues in #transactions.csv:
- Check for duplicates
- Find missing values
- Detect invalid dates
- Find negative amounts
- Check referential integrity with customers.csv"
```

### Step 3: Business Analysis
```
Copilot Prompt: "Write SQL to answer:
1. What is the failed transaction rate by region?
2. What is the average transaction amount by customer tier?
3. Which account manager has the most high-value transactions?
Use #schema.sql for column references."
```

### Step 4: Validation
```
Copilot Prompt: "Create validation queries to verify:
- Total revenue calculation is correct
- Count of transactions matches source data
- No transactions are double-counted
- Date ranges are valid
Reference #schema.sql in your checks."
```

---

## Expected Deliverables

Deliverables:
1. **Data Quality Report** (`outputs/DATA_NOTES.md`)
   - Issues list
   - Impact notes
   - Fix recs

2. **SQL Queries** (`/outputs/`)
   - Business answers
   - Logic checks referencing schema
   - Commented logic

3. **Business Insights** (`outputs/REPORT_NOTES.md`)
   - Key findings
   - Trends spotted
   - Action recs

---

## Tips for Success

### Do This:
- Profile data first
- Validate SQL logic against schema
- Log assumptions
- Cross-check customers table
- Calculate rates for context

### Avoid This:
- Avoid skipping validation
- Avoid ignoring NULLs
- Avoid missing status filters
- Avoid assuming consistent spelling
- Avoid skipping issue logs

---

## Data Dictionary

### Transaction Status Values
- **Completed** - Successfully processed and settled
- **Failed** - Transaction declined or rejected
- **Pending** - Awaiting processing or approval
- **Refunded** - Originally completed but reversed

### Customer Tier Definitions
- **Gold** - Premium customers, highest service level
- **Silver** - Standard customers, mid-tier service
- **Bronze** - Basic customers, standard service

### Payment Methods
- **Credit** - Credit card transactions
- **Debit** - Debit card transactions
- **Wire** - Wire transfer/ACH
- **Cash** - Cash payments
- **Other** - Alternative payment methods

---

## Need Help?

- CSV basics -> Ask Copilot: "How do I analyze a CSV file for data quality issues?"
- SQL schema -> `data/schema.sql`
- Question ideas -> list above
- More prompts -> `SESSION_GUIDE.md`

---

## Ready to Analyze?

Next: open `SESSION_GUIDE.md`, run Data Analysis lab (5-25 min)

**Starter Prompt:**
```
"Analyze data/transactions.csv and identify all data quality issues. 
For each issue, provide: description, count, impact, and recommended fix."
```

Good luck! 
