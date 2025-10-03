# Session Guide: GitHub Copilot for Systems & Business Analysts

## Session Timeline

### 0-5 Minutes: Setup & Safety
- [ ] Open VS Code with Copilot Chat
- [ ] Review VERIFY_BEFORE_SEND.md
- [ ] Understand data privacy rules
- [ ] Check /data/ files are loaded

**First Time?** Read `QUICK_START.md` before continuing!

---

### 5-25 Minutes: Prompt Engineering Practice
- [ ] Review RIFCC_FRAMEWORK.md in /reference/
- [ ] Complete transactions.csv analysis
- [ ] Generate 3 SQL queries for business questions
- [ ] Document validation steps in DATA_NOTES.md

#### 📝 Sample Prompts to Try

**Data Quality Analysis:**
```
"Analyze data/transactions.csv and identify all data quality issues. 
For each issue, provide: description, count, impact, and recommended fix."
```

**SQL Generation:**
```
"Write SQL queries to answer these business questions:
1. Top 10 customers by total revenue (completed transactions only)
2. Failed transaction rate by region
3. Average transaction amount by customer tier
Use the schema in data/schema.sql and include comments."
```

**Validation Query:**
```
"Create a SQL query to validate data quality in transactions.csv. 
Check for: null values, duplicates, negative amounts, and invalid dates."
```

---

### 25-40 Minutes: Legacy Analysis Lab (CRITICAL)
- [ ] Open /legacy/ COBOL files
- [ ] Use Copilot to explain business logic
- [ ] Create REQ_Logic.md with decision tables
- [ ] Generate REQ_DataMap.md
- [ ] Build REQ_Flow.md with Mermaid diagram
- [ ] Document 5+ risks in RISK_Register.md

#### 📝 Sample Prompts to Try

**Understanding COBOL:**
```
"Acting as a business analyst, explain legacy/fees_calc.cob in plain English:
1. What are the inputs and outputs?
2. What business rules are implemented?
3. What are the key calculations?
Format as a business requirements document."
```

**Extract Business Rules:**
```
"Extract all business rules from legacy/fees_calc.cob and create a decision table 
showing: Transaction Type, Customer Tier, Transaction Amount, Resulting Fee %. 
Include all combinations and edge cases."
```

**Create Process Flow:**
```
"Based on legacy/customer_risk.cob, create a Mermaid flowchart showing the 
risk scoring process. Include all decision points and scoring factors."
```

**Risk Identification:**
```
"Review legacy/fees_calc.cob and legacy/batch_reconcile.cob. Identify at least 
5 risks related to: hard-coded values, error handling, scalability, and 
maintainability. For each risk, provide: likelihood, impact, and mitigation."
```

---

### 40-55 Minutes: Excel to SQL Lab
- [ ] Create Excel formulas for KPIs
- [ ] Generate equivalent SQL
- [ ] Verify calculations match
- [ ] Document in REPORT_NOTES.md

#### 📝 Sample Prompts to Try

**Formula to SQL Conversion:**
```
"I have an Excel formula: =SUMIFS(Amount, Status, 'Completed', Tier, 'Gold')
Convert this to SQL using the transactions and customers tables. 
Include the equivalent formula for all three tiers."
```

**KPI Dashboard:**
```
"Create SQL queries for a management dashboard showing:
- Total revenue by month
- Success rate by payment method
- Top 5 products by revenue
- Customer tier distribution
Include CTEs for readability and comments explaining each metric."
```

---

### 55-65 Minutes: Governance Lab
- [ ] Review /exercises/ flawed files
- [ ] Identify all errors using Copilot
- [ ] Document corrections
- [ ] Update VERIFY_BEFORE_SEND.md

#### 📝 Sample Prompts to Try

**Error Detection:**
```
"Review exercises/flawed_sql_example.sql and identify all errors. 
For each error, explain: what's wrong, why it's wrong, and the corrected version."
```

**Analysis Critique:**
```
"Review exercises/flawed_analysis.md as a senior data analyst. 
Identify all logical errors, impossible statistics, and questionable conclusions. 
Explain what's wrong and what the correct approach should be."
```

**Code Review:**
```
"Review exercises/flawed_join_logic.py and identify all bugs and logic errors. 
Focus on: data type issues, null handling, join logic, and function application. 
Provide corrected code with explanations."
```

---

### 65-75 Minutes: Wrap-up
- [ ] Save all artifacts to /outputs/
- [ ] Commit changes with meaningful messages
- [ ] Complete session feedback

---

## Common Pitfalls to Avoid

❌ **Don't**: Copy Copilot's output without verification  
✅ **Do**: Ask "Can you show test cases for this SQL?" or "How can I validate this?"

❌ **Don't**: Use vague prompts like "help me" or "explain this"  
✅ **Do**: Use RIFCC structure (Role, Inputs, Format, Constraints, Checks)

❌ **Don't**: Assume AI-generated code is perfect  
✅ **Do**: Always validate against sample data or known results

❌ **Don't**: Share real customer data with Copilot  
✅ **Do**: Use only the synthetic data provided in this lab

❌ **Don't**: Accept the first answer  
✅ **Do**: Ask follow-up questions like "What are the edge cases?" or "What could go wrong?"

---

## Success Criteria - Did You Accomplish These?

By the end of this lab, you should have:

### Data Analysis
- [ ] Identified at least 8 data quality issues in transactions.csv
- [ ] Written and tested 3+ SQL queries for business questions
- [ ] Created validation queries to check data integrity

### Legacy Analysis  
- [ ] Extracted 5+ business rules from fees_calc.cob in plain English
- [ ] Created a decision table showing fee calculation logic
- [ ] Generated a process flow diagram for risk scoring
- [ ] Documented 5+ risks with mitigation strategies

### Governance
- [ ] Found all 6 errors in flawed_sql_example.sql
- [ ] Identified logical errors in flawed_analysis.md
- [ ] Fixed bugs in flawed_join_logic.py

### Documentation
- [ ] Filled out at least 3 template files in /outputs/
- [ ] All documentation includes assumptions and validation steps
- [ ] Used templates from /templates/ directory

---

## Key Reminders
- NO real data in prompts
- Always validate Copilot outputs
- Document your assumptions
- Save work frequently
- Ask follow-up questions to improve results
