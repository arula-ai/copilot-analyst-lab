# Participant Guide – GitHub Copilot for Business & Systems Analysts

## Session Goals
- Create 5+ RIFCC-structured prompts during the lab
- Document legacy-system business rules in plain English
- Translate Excel thinking into SQL logic without running a database
- Detect and correct intentional AI errors
- Build a personal "verify-before-send" habit

## Quick Start Checklist
- [ ] VS Code open with GitHub Copilot Chat enabled
- [ ] [`copilot-analyst-lab`](README.md) repository cloned or opened in Codespaces
- [ ] [`schema.sql`](data/schema.sql) available (no database install required)
- [ ] Comfortable using Copilot file references: `#filename` (e.g., `#fees_calc.cob`)

## Repository Map
```
copilot-analyst-lab/
├── data/          # CSVs and schema reference
├── legacy/        # COBOL + JCL sources
├── templates/     # Documentation starting points
├── exercises/     # Deliberately flawed materials
├── outputs/       # Save your work here
└── reference/     # Quick guides & frameworks
```

## RIFCC Prompt Framework
- **Role** – pick the perspective you need (`"Act as a modernization analyst"`)
- **Inputs** – attach files via `#` selectors (e.g., `#fees_calc.cob`, `#schema.sql`)
- **Format** – specify narrative/table/Mermaid/etc.
- **Constraints** – state rules (handle COMP-3, stay in USD, log missing data)
- **Checks** – ask for validations (flag assumptions, list edge cases)

Use RIFCC for every major prompt. After Copilot responds, inspect the source files yourself and note assumptions in your deliverables.

---

## Lab 1 – Legacy Discovery & Assessment (15 min)
**Mission:** Turn a 30-year-old COBOL batch system into modern business requirements.

### Files to Reference
- [`fees_calc.cob`](legacy/fees_calc.cob) – fee logic (attach with `#fees_calc.cob`)
- [`job_stream.jcl`](legacy/job_stream.jcl) – batch flow (attach with `#job_stream.jcl`)
- [`copybook.cpy`](legacy/copybook.cpy) – data layout (attach with `#copybook.cpy`)
- [`schema.sql`](data/schema.sql) – target schema (attach with `#schema.sql`)

### Step-by-Step Workflow
1. **Understand the fee program**  
   Use the [Understanding Business Logic](#understanding-business-logic-template) template with [`fees_calc.cob`](legacy/fees_calc.cob).  
   - Re-read the COBOL to confirm hard-coded rates, thresholds, date handling, and missing error paths.  
   - Capture numbered business rules plus a decision table in [`REQ_Logic.md`](outputs/REQ_Logic.md).

2. **Create the data mapping**  
   Run the [Creating Data Mappings](#creating-data-mappings-template) template with [`copybook.cpy`](legacy/copybook.cpy) and [`schema.sql`](data/schema.sql).  
   - Normalize PIC/COMP-3 types (e.g., COMP-3(7,2) → DECIMAL(9,2)).  
   - Record results in [`REQ_DataMap.md`](outputs/REQ_DataMap.md) with transformation + validation notes.

3. **Assess modernization risks**  
   Execute the [Risk Assessment](#risk-assessment-template) template referencing [`fees_calc.cob`](legacy/fees_calc.cob) and [`job_stream.jcl`](legacy/job_stream.jcl).  
   - Note at least five risks spanning logic, data, performance, and testing in [`RISK_Register.md`](outputs/RISK_Register.md).

4. **Describe the batch flow**  
   Outline the process using the [Batch Flow Outline](#batch-flow-outline-template) template with [`job_stream.jcl`](legacy/job_stream.jcl) and [`fees_calc.cob`](legacy/fees_calc.cob).  
   - Convert Copilot’s outline into a Mermaid diagram stored in [`REQ_Flow.md`](outputs/REQ_Flow.md).

5. **Translate the fee logic to SQL**  
   Apply the [SQL Translation](#sql-translation-template) template with [`fees_calc.cob`](legacy/fees_calc.cob) and [`schema.sql`](data/schema.sql).  
   - Refine and save the query in [`modern_equiv.sql`](outputs/modern_equiv.sql).

### Success Checklist
- [`REQ_Logic.md`](outputs/REQ_Logic.md) – narrative + decision table with assumptions noted
- [`REQ_DataMap.md`](outputs/REQ_DataMap.md) – mapping table for every relevant copybook field
- [`REQ_Flow.md`](outputs/REQ_Flow.md) – Mermaid diagram mirroring the JCL + COBOL steps
- [`RISK_Register.md`](outputs/RISK_Register.md) – ≥5 risks with likelihood/impact/mitigation
- [`modern_equiv.sql`](outputs/modern_equiv.sql) – SQL translation reviewed against [`schema.sql`](data/schema.sql)

**Tips:** stay focused on business intent, log every hard-coded value, and capture validation steps you expect downstream teams to run.

### Prompt Templates

#### Understanding Business Logic Template
Attach in Copilot: `#fees_calc.cob` ([open file](legacy/fees_calc.cob))
```
As a business analyst specializing in legacy systems, explain this COBOL program (#fees_calc.cob) in plain English. Break down inputs, processing steps, outputs, and hidden assumptions. Format as a business requirements narrative with examples.
```

#### Creating Data Mappings Template
Attach in Copilot: `#copybook.cpy`, `#schema.sql` ([open](legacy/copybook.cpy), [open](data/schema.sql))
```
Create a source-to-target field mapping for modernization using #copybook.cpy and #schema.sql. Include source PIC/COMP-3 definitions, target SQL types, transformations, and validation rules. Format as a markdown table.
```

#### Risk Assessment Template
Attach in Copilot: `#fees_calc.cob`, `#job_stream.jcl` ([open](legacy/fees_calc.cob), [open](legacy/job_stream.jcl))
```
Analyze #fees_calc.cob and #job_stream.jcl for modernization risks. List at least five covering business logic complexity, data quality, performance, and testing. Output a risk register with likelihood, impact, and mitigation.
```

#### Batch Flow Outline Template
Attach in Copilot: `#job_stream.jcl`, `#fees_calc.cob` ([open](legacy/job_stream.jcl), [open](legacy/fees_calc.cob))
```
Summarize the batch process steps in #job_stream.jcl and the sub-steps inside #fees_calc.cob. Provide the sequence and decision points suitable for a Mermaid flow diagram.
```

#### SQL Translation Template
Attach in Copilot: `#fees_calc.cob`, `#schema.sql` ([open](legacy/fees_calc.cob), [open](data/schema.sql))
```
Write ANSI SQL that reproduces the fee calculation in #fees_calc.cob using the tables in #schema.sql. Include tier logic, discounts, and error conditions as comments.
```

---

## Lab 2 – Automating Data Tasks (15 min)
**Mission:** Convert Excel-formula logic into SQL answers using the CSV data and the schema file.

### Inputs
- Data files: [`transactions.csv`](data/transactions.csv), [`customers.csv`](data/customers.csv)
- Schema reference: [`schema.sql`](data/schema.sql)

### How to Work
1. Ask Copilot to profile each CSV (row counts, nulls, duplicates) and log results in [`DATA_NOTES.md`](outputs/DATA_NOTES.md).
2. For each business question below, paste the Excel-style formula into Copilot and request an ANSI SQL equivalent. Capture the query plus validation notes in [`REPORT_NOTES.md`](outputs/REPORT_NOTES.md).
3. Validate logic manually against [`schema.sql`](data/schema.sql); note assumptions and edge cases (no database execution required).

### Business Questions + Excel Baselines
- **Monthly revenue trend**  
  Excel baseline: `=SUMIFS($B:$B, $A:$A, ">=" & EOMONTH(TODAY(),-1)+1, $A:$A, "<=" & EOMONTH(TODAY(),0), $C:$C, "Completed")`
- **Customer 80/20 concentration**  
  Excel baseline: `=LET(sorted, SORT(by_total, -1), running, SCAN(0, sorted, LAMBDA(a,b, a+b)), running/total)`
- **Failed transaction alerts**  
  Excel baseline: `=COUNTIFS(Status,"Failed",Amount,">=500",Date,">="&TODAY()-7)`

Use these baselines as context when asking Copilot for SQL, then double-check the generated query against the CSV structure.

---

## Lab 3 – Governance & Accuracy (10 min)
**Mission:** QA Copilot outputs.

Attach these files in prompts, make sure to use the # symbol:
- [`flawed_sql_example.sql`](exercises/flawed_sql_example.sql)
- [`flawed_analysis.md`](exercises/flawed_analysis.md)
- [`flawed_join_logic.py`](exercises/flawed_join_logic.py)

Deliverables:
- Corrections documented in [`governance_corrections/`](outputs/governance_corrections)
- Updated validation notes in [`VERIFY_BEFORE_SEND.md`](VERIFY_BEFORE_SEND.md)

---

## Quick References

### Essential Copilot Commands
| Command | Purpose |
| --- | --- |
| `/explain` | Summarize legacy code or SQL |
| `/fix` | Suggest repairs for selected text |
| `/tests` | Generate validation plans |
| `/doc` | Draft documentation or requirements |

### Common Issues & Fixes
- Copilot ignores context → add explicit `#file` references.
- SQL feels off → restate expected result and include schema snippets.
- COBOL jargon unclear → ask Copilot to define the term and cite the paragraph.

### Safety Reminders
- Use only the provided synthetic data.
- Record assumptions and validation steps in every deliverable.
- Require independent verification before sharing externally.

---

## Post-Session Actions
- Review [`homework/`](homework) exercises for deeper practice (attach homework files with `#` selectors in prompts).
- Document personal prompts and lessons learned in [`outputs/`](outputs).
- Capture follow-up questions in the **Questions for Office Hours** section at the end of this guide.
