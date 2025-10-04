# Quick Start Guide

## First Time Using GitHub Copilot Chat?

### Opening Copilot Chat
1. Ctrl+Shift+I (Win/Linux) or Cmd+Shift+I (Mac).
2. Or hit Copilot Chat icon.
3. Or pick Copilot Chat from Activity Bar.
4. Type into bottom chat box.

### Your First Prompt
Use this prompt:
```
"List each file in the legacy folder and describe its purpose"
```

### Example Workflow for This Lab

> Use `data/schema.sql` (attach as `#schema.sql`) to check table structure; no SQLite install needed.
> Add `#path/to/file` (e.g., `#fees_calc.cob`) in prompts to pull files into Copilot context.

#### 1. Analyze Data
**Prompt:**
```
"Analyze #transactions.csv and #customers.csv for data quality issues. 
List each issue with impact and recommended fixes."
```
- Tip: Keep [`reference/formulas/EXCEL_TO_SQL_BASELINES.md`](reference/formulas/EXCEL_TO_SQL_BASELINES.md) handy when converting spreadsheet logic.

#### 2. Understand COBOL
**Prompt:**
```
"Using #fees_calc.cob, explain the business logic in plain English. 
What are the inputs, business rules, and outputs?"
```

#### 3. Generate SQL
**Prompt:**
```
"Write SQL to find all failed transactions over $500 using #schema.sql for structure."
```

#### 4. Create Documentation
**Prompt:**
```
"Using #fees_calc.cob, create a decision table showing how 
fees are calculated by transaction type and customer tier"
```

## Pro Tips for Better Results

### Start with Your Role
**Good:** "As a business analyst, help me understand..."
**Better:** "Acting as a senior business analyst reviewing legacy systems, explain..."

### Be Specific
**Vague:** "What does this code do?"
**Specific:** "What business rules are implemented in the CALCULATE-BASE-FEE paragraph of fees_calc.cob?"

### Request Format
**Basic:** "Document this"
**Better:** "Create a markdown table documenting the business rules with columns: Rule Name, Condition, Action, Exception Handling"

### Ask for Validation
**Basic:** "Write SQL for top customers"
**Better:** "Write SQL for top 10 customers by revenue. Include test cases to validate the results."

## Common Copilot Commands

| Command | Purpose | Example |
|---------|---------|---------|
| `/explain` | Understand code/queries | `/explain what does this COBOL paragraph do?` |
| `/fix` | Fix errors | `/fix this SQL query has a GROUP BY error` |
| `/doc` | Generate documentation | `/doc create business requirements for this logic` |
| `/tests` | Create test cases | `/tests create validation queries for this SQL` |

## What to Expect in This Lab

### Phase 1: Data Analysis (20 min)
Use Copilot to spot CSV defects + draft SQL.

### Phase 2: Legacy Code Analysis (15 min)
Use Copilot to translate COBOL into business rules.

### Phase 3: Error Detection (15 min)
Use Copilot to flag planted SQL + analysis mistakes.

### Phase 4: Documentation (25 min)
Use Copilot to assemble polished documentation.

## Need More Help?

- RIFCC patterns -> `reference/RIFCC_FRAMEWORK.md`
- Command reference -> `reference/COPILOT_COMMANDS.md`
- Timeline -> `SESSION_GUIDE.md`

## Ready to Begin?

1. Copilot Chat is open
2. You understand how to write prompts
3. You know the lab structure

**Next Step:** Open `SESSION_GUIDE.md` and start the first exercise!
