# Quick Start Guide

## First Time Using GitHub Copilot Chat?

### Opening Copilot Chat
1. Press `Ctrl+Shift+I` (Windows/Linux) or `Cmd+Shift+I` (Mac)
2. Or click the chat icon in VS Code's left sidebar
3. Or click "Copilot Chat" in the Activity Bar (left side)
4. Type your questions in the chat window at the bottom

### Your First Prompt
Try this to get familiar with the lab:
```
"Explain what files are in the /legacy/ directory and what they do"
```

### Example Workflow for This Lab

#### 1. Analyze Data
**Prompt:**
```
"Analyze data/transactions.csv and identify all data quality issues. 
List them with impact and recommended fixes."
```

#### 2. Understand COBOL
**Prompt:**
```
"Explain the business logic in legacy/fees_calc.cob in plain English. 
What are the inputs, business rules, and outputs?"
```

#### 3. Generate SQL
**Prompt:**
```
"Write SQL to find all failed transactions over $500 using the 
schema in data/schema.sql"
```

#### 4. Create Documentation
**Prompt:**
```
"Using legacy/fees_calc.cob, create a decision table showing how 
fees are calculated based on transaction type and customer tier"
```

## Pro Tips for Better Results

### ✅ Start with Your Role
**Good:** "As a business analyst, help me understand..."
**Better:** "Acting as a senior business analyst reviewing legacy systems, explain..."

### ✅ Be Specific
**Vague:** "What does this code do?"
**Specific:** "What business rules are implemented in the CALCULATE-BASE-FEE paragraph of fees_calc.cob?"

### ✅ Request Format
**Basic:** "Document this"
**Better:** "Create a markdown table documenting the business rules with columns: Rule Name, Condition, Action, Exception Handling"

### ✅ Ask for Validation
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
You'll use Copilot to find data quality issues in CSV files and write SQL queries.

### Phase 2: Legacy Code Analysis (15 min)
You'll use Copilot to decode COBOL programs and extract business rules.

### Phase 3: Error Detection (15 min)
You'll use Copilot to find intentional errors in SQL and analysis documents.

### Phase 4: Documentation (25 min)
You'll use Copilot to create professional business documentation.

## Need More Help?

- **RIFCC Framework:** See `reference/RIFCC_FRAMEWORK.md` for advanced prompting
- **Copilot Commands:** See `reference/COPILOT_COMMANDS.md` for full reference
- **Session Guide:** See `SESSION_GUIDE.md` for detailed timeline

## Ready to Begin?

1. ✅ Copilot Chat is open
2. ✅ You understand how to write prompts
3. ✅ You know the lab structure

**Next Step:** Open `SESSION_GUIDE.md` and start the first exercise!
