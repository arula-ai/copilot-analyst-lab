# GitHub Copilot Quick Reference for Analysts

## Essential Commands

### /explain
Use when: You need to understand existing code/query
Example: "/explain what does this COBOL paragraph do?"

### /fix
Use when: You have an error in SQL or code
Example: "/fix this SQL join"

### /doc
Use when: You need to document logic
Example: "/doc create business rules documentation for this logic"

### /tests
Use when: You need to validate logic
Example: "/tests create test cases for this calculation"

## Prompt Patterns for Analysts

### Pattern 1: Requirements Extraction
```
"Acting as a business analyst, extract all business rules from [code/document]. 
Format as numbered requirements with acceptance criteria."
```

### Pattern 2: Data Analysis
```
"Analyze [dataset] to identify:
1. Data quality issues
2. Statistical patterns  
3. Business insights
Format findings as executive summary with evidence."
```

### Pattern 3: Legacy Modernization
```
"Explain this [COBOL/legacy code] in plain English:
1. What are the inputs?
2. What is the business logic?
3. What are the outputs?
Then provide equivalent modern SQL/Python."
```

### Pattern 4: Documentation Generation
```
"Create [type] documentation for [subject]:
- Include assumptions
- List validation rules
- Provide examples
- Note limitations"
```

## Common Analyst Tasks

| Task | Prompt Start |
|------|--------------|
| Understand legacy code | "Explain in business terms..." |
| Create data mapping | "Map these fields from source to target..." |
| Generate test data | "Create sample data that includes..." |
| Write SQL query | "Write SQL to answer: [business question]" |
| Document process | "Document this process flow including..." |
| Identify risks | "What risks exist in this logic..." |
