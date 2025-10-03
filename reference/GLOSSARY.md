# Glossary - Technical Terms for Business Analysts

This glossary helps non-programmers understand technical terms encountered in legacy systems and modern development.

---

## COBOL Terms

### Data Types & Storage

**COMP-3 (Packed Decimal)**
- Efficient storage format for numbers
- Stores two digits per byte
- Example: The number 12345 stored in 3 bytes instead of 5
- **Why you care:** Used in financial calculations for precision

**PIC (PICTURE)**
- Defines the format and size of a field
- **PIC X(10)** = Text field, 10 characters
- **PIC 9(5)** = Numeric field, 5 digits
- **PIC 9(5)V99** = Numeric with 5 digits + 2 decimals (like 12345.67)
- **PIC S9(7)V99** = Same as above but can be negative (S = Signed)

**88-level (Condition Name)**
- Named condition that makes code readable
- Example: `88 GOLD-TIER VALUE 'G'` means "if tier equals 'G', it's a Gold tier customer"
- **Why you care:** These are business rules in disguise!

### Program Structure

**DIVISION**
- COBOL programs are split into 4 divisions:
  - **IDENTIFICATION** = Program name and metadata
  - **ENVIRONMENT** = File locations and hardware config
  - **DATA** = Variable and file structure definitions
  - **PROCEDURE** = The actual business logic (this is what you want!)

**PERFORM**
- Calls a subroutine or repeats an action
- Like calling a function in modern languages
- Example: `PERFORM CALCULATE-FEE` = Run the fee calculation routine

**WORKING-STORAGE SECTION**
- Where temporary variables are defined
- Think of it as "scratch paper" for calculations
- **Why you care:** This is where you'll find hard-coded values and constants

### File Operations

**FD (File Description)**
- Defines the structure of a data file
- Shows what fields are in each record
- **Why you care:** This is your data dictionary for legacy files!

**SEQUENTIAL vs INDEXED**
- **SEQUENTIAL** = Read records one by one, start to finish (like reading a book)
- **INDEXED** = Jump directly to specific records (like using an index)
- **Why you care:** Affects performance and how data is accessed

---

## JCL (Job Control Language) Terms

### Job Structure

**JOB**
- A complete batch process with multiple steps
- Example: Daily fee calculation job

**STEP**
- One task within a job
- Example: STEP010 = Delete old files, STEP020 = Sort data, STEP030 = Calculate fees

**DD (Data Definition)**
- Defines where input/output files are located
- Think of it as file path configuration
- Example: `//TRANS DD DSN=PROD.TRANS.DAILY` points to the transaction file

### File Management

**DISP (Disposition)**
- What to do with a file (create, use existing, delete)
- **DISP=SHR** = Share with other programs (read-only)
- **DISP=(NEW,CATLG,DELETE)** = Create new, catalog if successful, delete if failed

**DSN (Dataset Name)**
- The full name/path of a file in mainframe systems
- Example: `PROD.TRANS.DAILY` = Production transaction file for today

**SYSOUT**
- System output (where print/log messages go)
- Think of it as "standard output" or "console output"

### Advanced JCL

**SORT**
- Program that sorts data by specified fields
- Example: `SORT FIELDS=(1,10,CH,A)` = Sort by first 10 characters, ascending

**IDCAMS**
- Utility for managing VSAM files (indexed files)
- Used to create, delete, or copy indexed datasets

**VSAM (Virtual Storage Access Method)**
- Advanced file system for mainframe
- Supports indexed and direct access to records
- **Why you care:** High-performance file system for critical data

**COND (Condition)**
- Controls whether a step runs based on previous step results
- Example: `COND=(4,LT)` = Skip this step if previous step failed

---

## Database & SQL Terms

### Query Components

**SELECT**
- Choose which columns to display
- Example: `SELECT customer_id, amount`

**WHERE**
- Filter rows based on conditions
- Example: `WHERE status = 'Completed'`

**GROUP BY**
- Combine rows with same values for summary
- Example: `GROUP BY region` = One row per region

**HAVING**
- Filter grouped results (like WHERE but for aggregates)
- Example: `HAVING SUM(amount) > 1000`

**JOIN**
- Combine data from multiple tables
- **INNER JOIN** = Only matching records
- **LEFT JOIN** = All from left table + matching from right

### Aggregate Functions

**SUM()**
- Add up all values
- Example: `SUM(amount)` = Total revenue

**COUNT()**
- Count number of rows
- Example: `COUNT(*)` = Total transactions

**AVG()**
- Calculate average
- Example: `AVG(amount)` = Average transaction size

**MAX() / MIN()**
- Find highest or lowest value
- Example: `MAX(amount)` = Largest transaction

### Advanced SQL

**CTE (Common Table Expression)**
- Temporary result set you can reference
- Starts with `WITH`
- **Why you care:** Makes complex queries more readable

**Window Function**
- Perform calculations across rows related to current row
- Example: `RANK() OVER (ORDER BY amount)` = Rank customers by spending

**CASE WHEN**
- If-then-else logic in SQL
- Example: `CASE WHEN amount > 1000 THEN 'High' ELSE 'Low' END`

---

## GitHub Copilot Terms

### Commands

**/explain**
- Ask Copilot to explain code or queries
- Example: `/explain what does this COBOL paragraph do?`

**/fix**
- Ask Copilot to fix errors in code
- Example: `/fix this SQL has a syntax error`

**/doc**
- Generate documentation for code
- Example: `/doc create requirements doc for this logic`

**/tests**
- Create test cases
- Example: `/tests create validation queries for this calculation`

### Concepts

**Prompt**
- Your question or instruction to Copilot
- Quality of prompt = Quality of response
- See RIFCC_FRAMEWORK.md for advanced prompting

**Context**
- Information Copilot uses to understand your request
- Includes: current file, selected code, conversation history
- **Why you care:** Copilot is smarter when you provide context

**Chat**
- Conversation interface with Copilot
- Ask questions, get explanations, generate code
- Press Ctrl+Shift+I (or Cmd+Shift+I on Mac) to open

---

## Data Quality Terms

### Data Issues

**NULL / Missing Value**
- Field has no value
- Different from zero or empty string!
- **Impact:** Can break calculations or cause incorrect results

**Duplicate**
- Same record appears multiple times
- **Impact:** Inflated counts, double-counting in totals

**Referential Integrity**
- Every foreign key points to a valid primary key
- Example: Every customer_id in transactions exists in customers table
- **Impact:** Orphaned records if violated

**Data Type Mismatch**
- Storing wrong type of data in a field
- Example: Storing "500" (text) instead of 500 (number)
- **Impact:** Sorting and math operations fail

### Quality Metrics

**Completeness**
- Percentage of required fields that are filled
- Example: 95% completeness = 5% missing values

**Accuracy**
- How correct the data is
- Example: Are dates in valid ranges? Are amounts reasonable?

**Consistency**
- Same data means same thing everywhere
- Example: "Completed" vs "Complete" vs "COMP"

**Uniqueness**
- Each record is distinct where it should be
- Example: No duplicate transaction IDs

---

## Business Analysis Terms

### Documentation Types

**Business Requirements**
- What the business needs the system to do
- Written in plain language, not technical jargon

**Decision Table**
- Shows all combinations of conditions and resulting actions
- Great for complex business rules

**Process Flow / Flowchart**
- Visual diagram showing steps in a process
- Uses shapes: rectangles = steps, diamonds = decisions

**Data Map**
- Shows how data moves from source to target
- Includes transformations and data type conversions

**Risk Register**
- List of identified risks with likelihood, impact, and mitigation
- Helps plan for things that could go wrong

### Analysis Concepts

**Use Case**
- Describes how a user interacts with the system
- Format: Actor, Goal, Steps, Success Criteria

**Acceptance Criteria**
- Specific conditions that must be met for requirement to be satisfied
- Used to verify the requirement was implemented correctly

**Traceability**
- Linking requirements to design to code to tests
- Ensures nothing gets lost in translation

---

## Modernization Terms

**Legacy System**
- Old system still in use (often COBOL, mainframe)
- Usually critical to business operations
- **Challenge:** Hard to maintain, lacks modern features

**Technical Debt**
- Cost of maintaining old/messy code
- Like financial debt - accumulates over time
- **Impact:** Slows down new development

**Refactoring**
- Improving code structure without changing behavior
- Makes code easier to understand and maintain

**API (Application Programming Interface)**
- Way for systems to talk to each other
- Modern alternative to file transfers and batch jobs

**Event-Driven Architecture**
- System reacts to events in real-time
- Opposite of batch processing (process everything at once)

---

## Common Abbreviations

| Term | Meaning | Context |
|------|---------|---------|
| CRUD | Create, Read, Update, Delete | Basic data operations |
| ETL | Extract, Transform, Load | Data integration process |
| GL | General Ledger | Accounting system of record |
| KPI | Key Performance Indicator | Business metric to track |
| SLA | Service Level Agreement | Performance/uptime guarantee |
| UAT | User Acceptance Testing | Final testing before go-live |
| EDI | Electronic Data Interchange | B2B data exchange standard |
| ACH | Automated Clearing House | Electronic funds transfer |

---

## Need More Help?

- **Don't understand a COBOL term?** Ask Copilot: "Explain what COMP-3 means in COBOL for a non-programmer"
- **SQL confusing you?** Try: "Explain this SQL query in plain English"
- **JCL looks like gibberish?** Ask: "What does this JCL step do in business terms?"

**Pro Tip:** When you encounter a new term, ask Copilot to explain it with a real business example!
