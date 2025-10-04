# Post-Workshop Homework Assignments

## Overview

Congrats on finishing the GitHub Copilot for Business Analysts workshop!

Homework goals:
- **Reinforce** workshop skills
- **Practice** using `data/schema.sql` without spinning up databases
- **Practice** tougher scenarios
- **Build confidence** using Copilot solo
- **Develop** production-ready habits

---

## Time Commitment

**Total:** 3-4 hours (spread over 1-2 weeks)

Suggested pacing:
- Week 1: Exercises 1-3 (2-3 hr)
- Week 2: Exercises 4-5 (1 hr)

---

## Exercises Overview

| Exercise | Topic | Time | Difficulty | Points |
|----------|-------|------|------------|--------|
| 1 | Advanced COBOL Analysis | 30-45 min | 3/5 | 20 |
| 2 | Real-World Data Analysis | 45-60 min | 4/5 | 25 |
| 3 | End-to-End Documentation | 60-90 min | 5/5 | 30 |
| 4 | Governance Challenge | 30 min | 3/5 | 15 |
| 5 | Prompt Engineering Mastery | 20-30 min | 2/5 | 10 |

**Total:** 100 points

---

## Exercise Files

- **EXERCISE_1_Advanced_COBOL.md** - Legacy system deep dive
- **EXERCISE_2_Data_Analysis.md** - Messy data cleanup
- **EXERCISE_3_Complete_Documentation.md** - Full documentation set
- **EXERCISE_4_Governance.md** - Error hunt
- **EXERCISE_5_Prompts.md** - Prompt mastery

---

## Resources Available

### Data Files (`/homework/data/`)
- `sales_data.csv` (1000 rows) - Complex sales data with quality issues
- `product_hierarchy.csv` (200 rows) - Product categorization data
- `sales_targets.csv` (100 rows) - Sales targets with missing data

### Legacy Code (`/homework/legacy/`)
- `inventory_update.cob` - Advanced COBOL with multi-file processing
- `month_end_close.jcl` - Complex JCL with 12 steps

### Templates (`/homework/templates/`)
- `MIGRATION_Strategy.md` - For migration planning
- `TEST_Plan.md` - For test documentation
- `MODERNIZATION_Proposal.md` - For architecture proposals
- `CHANGE_Log.md` - For documenting corrections

### Flawed Materials (`/homework/flawed_deliverables/`)
- Intentionally incorrect documents for Exercise 4

---

## Deliverables

Save all your work to `/outputs/homework/`:

```
/outputs/homework/
├── REQ_CustomerRisk.md (Exercise 1)
├── REQ_BatchReconcile.md (Exercise 1)
├── MODERNIZATION_Proposal.md (Exercise 1)
├── DATA_Quality_Report.md (Exercise 2)
├── SQL_Analysis_Queries.sql (Exercise 2)
├── Business_Insights.md (Exercise 2)
├── /migration_docs/ (Exercise 3)
│   ├── Business_Requirements.md
│   ├── Data_Mapping.md
│   ├── Risk_Register.md
│   ├── Test_Plan.md
│   └── Migration_Strategy.md
├── /governance_corrections/ (Exercise 4)
│   ├── Corrected_Requirements.md
│   ├── Corrected_SQL.sql
│   ├── Corrected_Data_Map.md
│   └── CHANGE_Log.md
└── Master_Prompts.md (Exercise 5)
```

---

## Getting Started

### Step 1: Review Workshop Materials
- `SESSION_GUIDE.md` - refresh core flow
- `reference/RIFCC_FRAMEWORK.md` - advanced prompting
- `reference/GLOSSARY.md` - terminology

### Step 2: Set Up Your Environment
1. Copilot Chat on
2. Homework folder open in VS Code
3. Skim homework data files

### Step 3: Start with Exercise 1
Open `EXERCISE_1_Advanced_COBOL.md`, proceed.

---

## Tips for Success

### Do This
- **Use RIFCC** for every prompt (attach files with `#filename`, e.g., `#inventory_update.cob`)
- **Validate** Copilot output against schema + requirements
- **Document** assumptions
- **Iterate** prompts quickly
- **Save** work often

### Avoid This
- Avoid **rushing** deliverables
- Avoid **skipping validation**
- Avoid **blind copying** Copilot output
- Avoid **using real data**
- Avoid **working solo** when stuck

---

## Evaluation Criteria
- **Completeness (30%)**: deliverables uploaded, templates filled, exercises done
- **Quality (40%)**: pro docs, accurate details, clear structure, template fit
- **Correctness (20%)**: SQL aligns with schema, rules correct, errors fixed
- **Critical Thinking (10%)**: analysis depth, assumptions noted, edge cases, actionable recs

---

## Submission Instructions

When done:
1. Confirm `/outputs/homework/` is complete
2. Read `GRADING_RUBRIC.md`
3. Self-score
4. Submit `/outputs/homework/` per trainer

---

## Need Help?

### Resources
- Workshop refresh -> `SESSION_GUIDE.md`, `QUICK_START.md`
- References -> `/reference/`
- Sample prompts -> `SESSION_GUIDE.md`
- Glossary -> `reference/GLOSSARY.md`

### Common Issues
- Copilot weak? -> refine with RIFCC, add specifics
- SQL confusing? -> ask Copilot to explain, walk through sample rows using schema
- COBOL unclear? -> ask for plain English + targeted sections
- Validation doubts? -> ask for test cases or checks

---

## Learning Objectives

By completing this homework, you will:
- Analyze complex legacy COBOL systems
- Capture business rules clearly
- Run deep data quality checks
- Write production-grade SQL
- Produce professional docs
- Fix analysis defects
- Craft strong RIFCC prompts
- Apply governance + validation

---

## Ready to Begin?

Start: open [EXERCISE_1_Advanced_COBOL.md](EXERCISE_1_Advanced_COBOL.md)

Good luck!
