# Post-Workshop Homework Assignments

## Overview

Congratulations on completing the GitHub Copilot for Business Analysts workshop! 🎉

These homework exercises will help you:
- **Reinforce** what you learned in the workshop
- **Practice** on more complex, real-world scenarios
- **Build confidence** using Copilot independently
- **Develop** production-ready skills

---

## Time Commitment

**Total:** 3-4 hours (spread over 1-2 weeks)

You can complete these exercises at your own pace. We recommend:
- Week 1: Exercises 1-3 (2-3 hours)
- Week 2: Exercises 4-5 (1 hour)

---

## Exercises Overview

| Exercise | Topic | Time | Difficulty | Points |
|----------|-------|------|------------|--------|
| 1 | Advanced COBOL Analysis | 30-45 min | ⭐⭐⭐ | 20 |
| 2 | Real-World Data Analysis | 45-60 min | ⭐⭐⭐⭐ | 25 |
| 3 | End-to-End Documentation | 60-90 min | ⭐⭐⭐⭐⭐ | 30 |
| 4 | Governance Challenge | 30 min | ⭐⭐⭐ | 15 |
| 5 | Prompt Engineering Mastery | 20-30 min | ⭐⭐ | 10 |

**Total:** 100 points

---

## Exercise Files

- **EXERCISE_1_Advanced_COBOL.md** - Analyze complex legacy systems
- **EXERCISE_2_Data_Analysis.md** - Work with messy real-world data
- **EXERCISE_3_Complete_Documentation.md** - Create full documentation package
- **EXERCISE_4_Governance.md** - Find and fix errors in deliverables
- **EXERCISE_5_Prompts.md** - Master advanced prompting techniques

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
Before starting homework, review:
- `SESSION_GUIDE.md` - Refresh on what you learned
- `reference/RIFCC_FRAMEWORK.md` - Advanced prompting techniques
- `reference/GLOSSARY.md` - Technical terms reference

### Step 2: Set Up Your Environment
1. Ensure GitHub Copilot Chat is active
2. Have the homework folder open in VS Code
3. Review the data files to understand what you're working with

### Step 3: Start with Exercise 1
Open `EXERCISE_1_Advanced_COBOL.md` and follow the instructions.

---

## Tips for Success

### ✅ Do This
- **Use RIFCC Framework** - Structure every prompt with Role, Inputs, Format, Constraints, Checks
- **Validate Everything** - Don't trust Copilot's output without testing
- **Document Assumptions** - Note what you're assuming in your analysis
- **Iterate on Prompts** - If you don't get good results, refine your prompt
- **Save Work Frequently** - Save your progress as you go

### ❌ Avoid This
- **Don't Rush** - Quality matters more than speed
- **Don't Skip Validation** - Always test SQL queries and verify calculations
- **Don't Copy Without Understanding** - Make sure you understand what Copilot generated
- **Don't Use Real Data** - Only use the provided synthetic datasets
- **Don't Work in Isolation** - If stuck, review workshop materials or ask for help

---

## Evaluation Criteria

Your work will be evaluated on:

### Completeness (30%)
- All deliverables submitted
- All sections of templates filled out
- Exercises fully completed

### Quality (40%)
- Professional documentation
- Accurate technical details
- Clear, well-structured content
- Proper use of templates

### Correctness (20%)
- SQL queries work and produce correct results
- Business rules accurately extracted
- Errors identified and fixed properly

### Critical Thinking (10%)
- Thoughtful analysis
- Assumptions documented
- Edge cases considered
- Recommendations actionable

---

## Submission Instructions

When complete:
1. Ensure all files are in `/outputs/homework/`
2. Review the grading rubric in `GRADING_RUBRIC.md`
3. Self-assess using the rubric
4. Submit your `/outputs/homework/` folder as instructed by your trainer

---

## Need Help?

### Resources
- **Workshop Materials** - Review `SESSION_GUIDE.md`, `QUICK_START.md`
- **Reference Guides** - Check `/reference/` directory
- **Sample Prompts** - See `SESSION_GUIDE.md` for examples
- **Glossary** - Look up technical terms in `reference/GLOSSARY.md`

### Common Issues

**Problem:** Copilot isn't giving good responses  
**Solution:** Refine your prompt using RIFCC framework. Be more specific.

**Problem:** SQL query doesn't work  
**Solution:** Ask Copilot to explain the query, then test on small dataset first.

**Problem:** Don't understand COBOL code  
**Solution:** Ask Copilot to explain in plain English, then ask for specific sections.

**Problem:** Not sure if work is correct  
**Solution:** Ask Copilot to create test cases or validation queries.

---

## Learning Objectives

By completing this homework, you will demonstrate ability to:

✅ Independently analyze complex legacy COBOL systems  
✅ Extract and document business rules from code  
✅ Perform comprehensive data quality analysis  
✅ Write production-quality SQL queries  
✅ Create professional documentation packages  
✅ Identify and correct errors in analysis work  
✅ Write effective RIFCC-structured prompts  
✅ Apply governance and validation principles  

---

## Ready to Begin?

👉 **Start Here:** Open [EXERCISE_1_Advanced_COBOL.md](EXERCISE_1_Advanced_COBOL.md)

Good luck! You've got this! 💪
