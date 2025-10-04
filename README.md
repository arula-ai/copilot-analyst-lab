# GitHub Copilot for Systems & Business Analysts - Lab Repository

## Training Overview
**Duration:** 75 minutes  
**Audience:** Systems Analysts, Business Analysts, Functional Analysts  
**Focus:** Using GitHub Copilot to accelerate analysis tasks, NOT coding

## Prerequisites
- VS Code with GitHub Copilot Chat extension
- Review `data/schema.sql` for table structure (no database install needed)
- Basic SQL knowledge
- Understanding of business requirements documentation
- **No programming experience required!**

## Lab Objectives
1. **Prompt Engineering** - Master the RIFCC framework (Role, Inputs, Format, Constraints, Checks)
2. **Legacy System Analysis** - Understand and document COBOL/JCL systems
3. **Data Analysis Automation** - Convert Excel formulas to SQL queries
4. **Governance & Validation** - Identify and correct AI-generated errors

## Repository Contents
- `/data/` - Sample datasets (synthetic data only)
- `/legacy/` - COBOL/JCL files for analysis
- `/templates/` - Documentation templates
- `/exercises/` - Practice exercises with intentional errors
- `/outputs/` - Your generated artifacts go here
- `/reference/` - Quick reference guides

## Recommended Sequence

### Phase 1: Get Ready (5-10 minutes)
1. Read README now.
2. Skim QUICK_START.md.
3. Skim reference/RIFCC_FRAMEWORK.md.
4. Scan VERIFY_BEFORE_SEND.md.

### Phase 2: Practice Data Analysis (15 minutes)
1. Open data/transactions.csv + data/customers.csv.
2. Ask Copilot for data quality issues.
3. Ask Copilot for business SQL (reference [`reference/formulas/EXCEL_TO_SQL_BASELINES.md`](reference/formulas/EXCEL_TO_SQL_BASELINES.md) when translating Excel-style logic).
4. Log notes in outputs/DATA_NOTES.md.

**Sample Prompt:**
```
"Analyze #transactions.csv and #customers.csv for data quality issues. 
For each issue, provide: description, count, impact, and recommended fix."
```

### Phase 3: Decode Legacy Systems (15 minutes)
1. Open legacy/fees_calc.cob.
2. Ask Copilot for plain-English rules.
3. Draft decision tables for fees.
4. Log risks + modernization ideas.
5. Save outputs/REQ_Logic.md + outputs/RISK_Register.md.

**Sample Prompt:**
```
"Using #fees_calc.cob, explain the business logic in plain English:
1. What are the inputs and outputs?
2. What business rules are implemented?
3. What are the key calculations?"
```

### Phase 4: Validate AI Outputs (15 minutes)
1. Open exercises/flawed_sql_example.sql.
2. Prompt Copilot to list six errors.
3. Review exercises/flawed_analysis.md for logic slips.
4. Fix exercises/flawed_join_logic.py bugs.

**Sample Prompt:**
```
"Review #flawed_sql_example.sql and identify all errors. 
For each error, explain: what's wrong, why it's wrong, and the corrected version."
```

### Phase 5: Create Documentation (20 minutes)
1. Open /templates/.
2. Complete three templates minimum.
3. Save artifacts in /outputs/.
4. Check success criteria before exit.

---

## Getting Started

### Step 1: Launch Copilot Chat
- Shortcut: Ctrl+Shift+I (Win/Linux) or Cmd+Shift+I (Mac).
- Or click the Copilot Chat icon.

### Step 2: Test Setup
Prompt Copilot:
```
"List all files in the legacy folder and explain each purpose"
```

### Step 3: Follow the Guide
Open [SESSION_GUIDE.md](SESSION_GUIDE.md) for detailed instructions and sample prompts.

---

## Key Files to Know

| File | Purpose | When to Use |
|------|---------|-------------|
| `QUICK_START.md` | Copilot basics for beginners | First time using Copilot? Start here! |
| `SESSION_GUIDE.md` | 75-minute session timeline | Your main guide during the lab |
| `VERIFY_BEFORE_SEND.md` | Data privacy checklist | Before sending any prompts |
| `INSTRUCTOR_GUIDE.md` | Expected outcomes & answers | For instructors only |
| `reference/RIFCC_FRAMEWORK.md` | Advanced prompting techniques | To improve prompt quality |
| `reference/COPILOT_COMMANDS.md` | Copilot command reference | Quick command lookup |
| `reference/GLOSSARY.md` | Technical terms explained | When you see unfamiliar terms |

---

## What You'll Learn

By completing this lab, you will be able to:

 Write effective prompts using the RIFCC framework  
 Use Copilot to analyze data quality issues  
 Extract business rules from legacy COBOL code  
 Generate SQL queries from business requirements  
 Create professional documentation (requirements, data maps, risk registers)  
 Identify and correct errors in AI-generated content  
 Apply governance principles to AI-assisted analysis  

---

## Success Criteria

### You're done when you have:
- [ ] 8+ data issues flagged in transactions.csv
- [ ] 5+ COBOL rules captured
- [ ] Fee decision table built
- [ ] Six flawed SQL issues noted
- [ ] 3+ SQL queries validated
- [ ] 3+ templates completed in `/outputs/`
- [ ] Assumptions + validation logged

See [SESSION_GUIDE.md](SESSION_GUIDE.md) for complete checklist.

---

## Important Reminders

- Use Copilot `#filename` in prompts to attach files (e.g., `#fees_calc.cob`).
- Use `data/schema.sql` to validate structure; no database setup required.
- Data privacy: synthetic only.
- Validate every Copilot answer.
- Record assumptions + checks.
- Keep iterating prompts.

---

## Need Help?

- Copilot basics -> QUICK_START.md
- Prompt patterns -> reference/RIFCC_FRAMEWORK.md
- COBOL terms -> reference/GLOSSARY.md
- More prompts -> SESSION_GUIDE.md

---

## Ready to Begin?

**Next Step:** Open [QUICK_START.md](QUICK_START.md) or jump to [SESSION_GUIDE.md](SESSION_GUIDE.md)

Good luck!
