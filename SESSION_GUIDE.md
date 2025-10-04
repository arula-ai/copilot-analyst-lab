# Session Guide: GitHub Copilot for Systems & Business Analysts

## Session Timeline

### 0-5 Minutes: Setup & Safety
- [ ] VS Code + Copilot Chat open
- [ ] Skim VERIFY_BEFORE_SEND.md
- [ ] Recall privacy rules
- [ ] Ensure /data/ files ready
- [ ] Practice using `#file` syntax (e.g., `#fees_calc.cob`) in prompts

**First Time?** Read `QUICK_START.md` before continuing!

---

### 5-25 Minutes: Prompt Engineering Practice
- [ ] Skim reference/RIFCC_FRAMEWORK.md
- [ ] Note table layout from data/schema.sql
- [ ] Finish transactions.csv scan
- [ ] Draft 3 business SQL queries
- [ ] Log validation in DATA_NOTES.md

#### Sample Prompts to Try

**Data Quality Analysis:**
```
"Analyze #transactions.csv and #customers.csv for data quality issues. 
For each issue, provide: description, count, impact, and recommended fix."
```

**SQL Generation:**
```
"Write SQL queries to answer these business questions:
1. Top 10 customers by total revenue (completed transactions only)
2. Failed transaction rate by region
3. Average transaction amount by customer tier
Use #schema.sql for column names and include comments."
```

**Validation Query:**
```
"Create a SQL query to validate data quality in transactions.csv. 
Check for: null values, duplicates, negative amounts, and invalid dates."
```

---

### 25-40 Minutes: Legacy Analysis Lab (CRITICAL)
- [ ] Open legacy COBOL files
- [ ] Ask Copilot for business logic
- [ ] Build REQ_Logic.md decision tables
- [ ] Produce REQ_DataMap.md
- [ ] Build REQ_Flow.md Mermaid flow
- [ ] Note 5+ risks in RISK_Register.md

#### Sample Prompts to Try

**Understanding COBOL:**
```
"Acting as a business analyst, explain #fees_calc.cob in plain English:
1. What are the inputs and outputs?
2. What business rules are implemented?
3. What are the key calculations?
Format as a business requirements document."
```

**Extract Business Rules:**
```
"Extract all business rules from #fees_calc.cob and create a decision table 
showing: Transaction Type, Customer Tier, Transaction Amount, Resulting Fee %. 
Include all combinations and edge cases."
```

**Create Process Flow:**
```
"Based on #customer_risk.cob, create a Mermaid flowchart showing the 
risk scoring process. Include all decision points and scoring factors."
```

**Risk Identification:**
```
"Review #fees_calc.cob and #batch_reconcile.cob. Identify at least 
5 risks related to: hard-coded values, error handling, scalability, and 
maintainability. For each risk, provide: likelihood, impact, and mitigation."
```

---

### 40-55 Minutes: Excel to SQL Lab
- [ ] Draft KPI Excel formulas (see [`reference/formulas/EXCEL_TO_SQL_BASELINES.md`](reference/formulas/EXCEL_TO_SQL_BASELINES.md))
- [ ] Mirror SQL equivalents
- [ ] Reason through calc parity with schema
- [ ] Log in REPORT_NOTES.md

#### Sample Prompts to Try

**Formula to SQL Conversion:**
```
"I have an Excel formula: =SUMIFS(Amount, Status, 'Completed', Tier, 'Gold')
Convert this to SQL using #transactions.csv and #customers.csv. 
Include the equivalent logic for all three tiers."
```

**KPI Dashboard:**
```
"Create SQL queries for a management dashboard showing:
- Total revenue by month
- Success rate by payment method
- Top 5 products by revenue
- Customer tier distribution
Use #schema.sql for column names, include CTEs and explanatory comments."
```

---

### 55-65 Minutes: Governance Lab
- [ ] Review /exercises/ flawed files
- [ ] Ask Copilot to flag errors
- [ ] Document corrections
- [ ] Update VERIFY_BEFORE_SEND.md

#### Sample Prompts to Try

**Error Detection:**
```
"Review #flawed_sql_example.sql and identify all errors. 
For each error, explain: what's wrong, why it's wrong, and the corrected version."
```

**Analysis Critique:**
```
"Review #flawed_analysis.md as a senior data analyst. 
Identify all logical errors, impossible statistics, and questionable conclusions. 
Explain what's wrong and what the correct approach should be."
```

**Code Review:**
```
"Review #flawed_join_logic.py and identify all bugs and logic errors. 
Focus on: data type issues, null handling, join logic, and function application. 
Provide corrected code with explanations."
```

---

### 65-75 Minutes: Wrap-up
- [ ] Save artifacts to /outputs/
- [ ] Commit with meaningful messages
- [ ] Submit session feedback

---

## Common Pitfalls to Avoid

**Don't**: Copy Copilot's output without verification  
**Do**: Ask "Can you show test cases for this SQL?" or "How can I validate this?"

**Don't**: Use vague prompts like "help me" or "explain this"  
**Do**: Use RIFCC structure (Role, Inputs, Format, Constraints, Checks)

**Don't**: Assume AI-generated code is perfect  
**Do**: Validate against schema, business rules, or provided samples

**Don't**: Share real customer data with Copilot  
**Do**: Use only the synthetic data provided in this lab

**Don't**: Accept the first answer  
**Do**: Ask follow-up questions like "What are the edge cases?" or "What could go wrong?"

---

## Success Criteria - Did You Accomplish These?

By the end of this lab, you should have:

### Data Analysis
- [ ] 8+ data issues in transactions.csv
- [ ] 3+ SQL queries written + schema-checked
- [ ] Validation queries built

### Legacy Analysis  
- [ ] 5+ fees_calc.cob rules documented
- [ ] Fee decision table done
- [ ] Risk scoring flow drafted
- [ ] 5+ risks + mitigations logged

### Governance
- [ ] Six errors found in flawed_sql_example.sql
- [ ] flawed_analysis.md logic issues noted
- [ ] flawed_join_logic.py bugs fixed

### Documentation
- [ ] 3+ templates filled in /outputs/
- [ ] Docs list assumptions + validation
- [ ] Used /templates/ files

---

## Key Reminders
- NO real data in prompts
- Always validate Copilot outputs against schema + business rules
- Document your assumptions
- Save work frequently
- Ask follow-up questions to improve results
