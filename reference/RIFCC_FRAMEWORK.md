# RIFCC Framework for Effective Prompts

## R - Role
Define Copilot's perspective:
- "Acting as a senior business analyst..."
- "As a data quality specialist..."
- "Taking the perspective of an auditor..."

## I - Inputs
Specify exact context:
- Data sources and formats
- Business rules and constraints
- Existing documentation
- Sample data

## F - Format
Define output structure:
- "Provide as a markdown table..."
- "Format as SQL with comments..."
- "Structure as business requirements..."
- "Create a decision matrix..."

## C - Constraints
Set boundaries:
- Technology limitations
- Compliance requirements
- Performance needs
- Business rules

## C - Checks
Build in validation:
- "Include test cases..."
- "Add validation queries..."
- "Provide reconciliation steps..."
- "List assumptions to verify..."

## Example Using RIFCC

**Poor Prompt:**
"Help me understand this code"

**RIFCC-Structured Prompt:**
```
Role: Acting as a senior business analyst specializing in legacy modernization
Inputs: Given this COBOL program that calculates transaction fees
Format: Provide a business requirements document with:
  - Executive summary
  - Detailed business rules
  - Decision table
  - Process flow diagram in Mermaid
Constraints: 
  - Must maintain current calculation precision
  - Compatible with SQL Server 2019
  - Support real-time processing
Checks:
  - Include 5 test cases covering edge cases
  - Provide SQL to validate calculations match COBOL
  - List all assumptions made
```

## Tips for Analysts
1. Always specify the business context
2. Request examples with your documentation
3. Ask for validation methods
4. Require assumptions to be stated
5. Request both positive and negative test cases
