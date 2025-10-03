# Exercise 1: Advanced COBOL Analysis

## Learning Objectives
- Extract complete business requirements from complex COBOL programs
- Apply RIFCC framework to legacy code analysis
- Create comprehensive documentation from undocumented systems
- Understand mainframe business logic patterns

## Time Estimate: 30-45 minutes

## Overview
You will analyze two complex COBOL programs from the `/legacy/` directory to extract business requirements and propose modernization strategies. This exercise simulates real-world scenarios where analysts must understand legacy systems before modernization.

## Files to Analyze
- `/legacy/customer_risk.cob` - Customer risk assessment program
- `/legacy/batch_reconcile.cob` - Financial reconciliation batch job

## Tasks

### Task 1: Customer Risk Analysis (15 minutes)
Analyze `customer_risk.cob` and create `REQ_CustomerRisk.md` containing:
- **Business Purpose**: What the program does and why
- **Input Requirements**: Data sources, file formats, dependencies
- **Processing Logic**: Key algorithms, calculations, decision points
- **Output Specifications**: Reports, files, data updates produced
- **Business Rules**: Credit limits, risk thresholds, validation rules
- **Error Handling**: Exception scenarios and responses

### Task 2: Batch Reconciliation Analysis (15 minutes)
Analyze `batch_reconcile.cob` and create `REQ_BatchReconcile.md` with:
- **Reconciliation Process**: What accounts/transactions are reconciled
- **Matching Logic**: How records are paired and verified
- **Exception Handling**: Unmatched items, out-of-balance scenarios
- **Reporting Requirements**: Summary reports, exception reports
- **Timing Dependencies**: When the job runs, prerequisites
- **Data Sources**: Input files, database tables accessed

### Task 3: Modernization Proposal (10-15 minutes)
Create `MODERNIZATION_Proposal.md` addressing:
- **Current State Assessment**: Complexity, maintainability issues
- **Proposed Technology Stack**: Recommended modern alternatives
- **Migration Strategy**: Phased approach, risk mitigation
- **Business Benefits**: Cost savings, performance improvements
- **Implementation Timeline**: High-level milestones

## Sample RIFCC Prompts

### For Customer Risk Analysis:
```
**Role**: Senior Business Analyst specializing in financial systems modernization
**Input**: COBOL source code from customer_risk.cob program
**Format**: Structured business requirements document
**Context**: Legacy mainframe system being modernized to cloud-native architecture
**Constraints**: Must capture all business rules, calculations, and exception handling logic

Analyze this COBOL program and extract complete business requirements. Focus on:
1. Customer risk calculation methodology
2. Data validation rules and thresholds
3. Decision trees for risk categorization
4. Exception handling procedures
5. Output report specifications

Create a comprehensive requirements document that a development team could use to rebuild this functionality in a modern platform.
```

### For Batch Reconciliation Analysis:
```
**Role**: Financial Systems Analyst with expertise in reconciliation processes
**Input**: COBOL batch reconciliation program source code
**Format**: Technical and business requirements specification
**Context**: End-of-day financial processing modernization project
**Constraints**: Zero tolerance for reconciliation errors, must maintain audit trail

Extract all business logic from this reconciliation program including:
1. Matching algorithms and tolerance settings
2. Exception identification and reporting
3. Balance verification procedures
4. Audit trail requirements
5. Performance and timing constraints

Document requirements suitable for regulatory compliance review.
```

## Deliverables Checklist

### REQ_CustomerRisk.md Must Include:
- [ ] Clear business purpose statement
- [ ] Complete input data specifications
- [ ] All calculation formulas and logic
- [ ] Risk category definitions and thresholds
- [ ] Validation rules for customer data
- [ ] Exception handling procedures
- [ ] Output report layouts and content
- [ ] Performance requirements
- [ ] Audit and compliance considerations

### REQ_BatchReconcile.md Must Include:
- [ ] Reconciliation scope and objectives
- [ ] Data source specifications
- [ ] Matching criteria and tolerance levels
- [ ] Exception identification rules
- [ ] Report generation requirements
- [ ] Error handling and recovery procedures
- [ ] Timing and dependency constraints
- [ ] Audit trail specifications

### MODERNIZATION_Proposal.md Must Include:
- [ ] Current system assessment
- [ ] Technology recommendations with rationale
- [ ] Migration approach and phases
- [ ] Risk assessment and mitigation strategies
- [ ] Resource requirements estimate
- [ ] Business case and ROI projection
- [ ] Implementation timeline

## Success Criteria
- **Completeness**: All business rules and logic captured
- **Clarity**: Requirements understandable to non-technical stakeholders
- **Accuracy**: Faithful representation of COBOL program functionality
- **Actionability**: Sufficient detail for development team implementation
- **Professional Quality**: Documentation suitable for client delivery

## Tips for COBOL Analysis
1. **Start with PROCEDURE DIVISION**: Main business logic resides here
2. **Identify Data Structures**: Map WORKING-STORAGE to understand data flow
3. **Follow PERFORM Statements**: Trace program execution flow
4. **Note COPY Members**: External dependencies and shared code
5. **Check File Operations**: READ, WRITE, REWRITE patterns reveal data processing
6. **Look for Calculations**: COMPUTE, ADD, SUBTRACT, MULTIPLY operations
7. **Find Decision Points**: IF statements contain business rules
8. **Trace Error Handling**: Error codes and exception processing
9. **Understand Reports**: Output formatting reveals business requirements
10. **Check Comments**: Legacy programs often have embedded business knowledge

## Time Management Tips
- **10 minutes**: Initial code review and structure understanding
- **15 minutes**: Deep dive into business logic extraction
- **10 minutes**: Documentation creation and formatting
- **5-10 minutes**: Review and validation of requirements

## Extension Activities
If you complete early:
- Compare the two programs for common patterns and shared functionality
- Identify potential integration points between customer risk and reconciliation
- Research modern alternatives to COBOL batch processing
- Create a data flow diagram showing system interactions

## Resources
- Use `/templates/REQ_Logic.md` for requirement structure guidance
- Reference `/reference/RIFCC_FRAMEWORK.md` for prompt engineering
- Check `/reference/GLOSSARY.md` for mainframe terminology

---
**Note**: This exercise tests your ability to reverse-engineer business requirements from undocumented legacy code - a critical skill for modernization projects.