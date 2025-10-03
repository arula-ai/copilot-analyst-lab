# Sample Data - Business Context

## Business Scenario

You are a Business Analyst at **Global Payments Inc**, a B2B payment processor handling transactions across 5 regions (North, South, East, West, Central).

Your task is to analyze recent transaction data to:
- Identify data quality issues before reporting to executives
- Understand transaction patterns by customer tier and region
- Investigate failed transactions and revenue leakage
- Prepare recommendations for process improvements

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

### Revenue Analysis
1. What is total revenue by month (Completed transactions only)?
2. Which customers generate the most revenue?
3. How much potential revenue was lost to failed transactions?
4. What is the revenue distribution across regions?

---

## Known Data Quality Issues

⚠️ **IMPORTANT**: This is **real-world messy data** on purpose!

Part of your job as a Business Analyst is to:
1. **Find** the data quality issues
2. **Document** their impact on analysis
3. **Recommend** fixes to prevent future issues
4. **Validate** your SQL queries account for bad data

**Hint to get started:** 
```
Ask Copilot: "How many unique transaction_ids should there be in a 
500-row dataset? How many unique transaction_ids are actually present?"
```

---

## Sample Analysis Workflow

### Step 1: Initial Data Profiling
```
Copilot Prompt: "Analyze data/transactions.csv and provide:
- Row count
- Column data types
- Missing value count per column
- Unique values for categorical fields"
```

### Step 2: Data Quality Assessment
```
Copilot Prompt: "Identify data quality issues in data/transactions.csv:
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
3. Which account manager has the most high-value transactions?"
```

### Step 4: Validation
```
Copilot Prompt: "Create validation queries to verify:
- Total revenue calculation is correct
- Count of transactions matches source data
- No transactions are double-counted
- Date ranges are valid"
```

---

## Expected Deliverables

After analyzing this data, you should have:

1. **Data Quality Report** (`outputs/DATA_NOTES.md`)
   - List of all data quality issues found
   - Impact assessment for each issue
   - Recommended remediation steps

2. **SQL Queries** (saved in `/outputs/`)
   - Queries to answer business questions
   - Validation queries to check data integrity
   - Comments explaining the logic

3. **Business Insights** (`outputs/REPORT_NOTES.md`)
   - Key findings from the analysis
   - Trends and patterns observed
   - Recommendations based on data

---

## Tips for Success

### ✅ Do This:
- Start with data profiling before diving into analysis
- Validate your SQL queries on small samples first
- Document assumptions (e.g., "Assuming NULL means 0")
- Cross-reference transactions with customers table
- Calculate percentages and rates for context

### ❌ Avoid This:
- Don't trust the data without validation
- Don't ignore missing values in calculations
- Don't forget to filter by status (e.g., only 'Completed' for revenue)
- Don't assume categories are spelled consistently
- Don't skip documenting data quality issues

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

- **First time analyzing CSV data?** Ask Copilot: "How do I analyze a CSV file for data quality issues?"
- **Stuck on SQL?** Reference the schema in `data/schema.sql`
- **Not sure what to look for?** Review the business questions section above
- **Want sample prompts?** See `SESSION_GUIDE.md`

---

## Ready to Analyze?

👉 **Next Step:** Open `SESSION_GUIDE.md` and start the Data Analysis lab (5-25 minute section)

**Starter Prompt:**
```
"Analyze data/transactions.csv and identify all data quality issues. 
For each issue, provide: description, count, impact, and recommended fix."
```

Good luck! 📊
