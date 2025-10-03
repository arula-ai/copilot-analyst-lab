# Exercise 5: Master Prompt Engineering

## Learning Objectives
- Master advanced RIFCC prompt engineering techniques
- Create production-ready prompts for complex analysis scenarios
- Develop prompt libraries for reusable consulting workflows
- Apply prompt optimization strategies for consistent, high-quality outputs
- Build expertise in AI-assisted business analysis methodologies

## Time Estimate: 20-30 minutes

## Overview
You will create a library of 10+ master prompts covering various business analysis scenarios. These prompts must use the complete RIFCC framework and produce consistent, professional results suitable for client engagements.

## Prompt Categories Required

### 1. Legacy Code Analysis (2 prompts)
- COBOL program business logic extraction
- Mainframe job stream dependency analysis

### 2. Data Analysis (2 prompts)
- Data quality assessment and remediation
- Business intelligence insight generation

### 3. Documentation Creation (2 prompts)
- Technical specification writing
- Executive summary generation

### 4. Test Planning (2 prompts)
- Comprehensive test case generation
- Performance testing strategy development

### 5. Risk Management (2 prompts)
- Project risk identification and assessment
- Technical risk mitigation planning

### 6. Additional Scenarios (2+ prompts)
Choose from: SQL optimization, process flow analysis, requirements gathering, change management planning, compliance assessment, or integration architecture

## RIFCC Framework Requirements

### Each Prompt Must Include:

**Role**: Specific expertise and perspective
- Industry specialization (financial services, healthcare, etc.)
- Technical expertise level (senior, principal, expert)
- Functional domain (business analysis, data architecture, etc.)

**Input**: Clear description of what you're analyzing
- File types, data sources, system specifications
- Scope and boundaries of analysis
- Context and background information

**Format**: Specific output structure and style
- Document type (technical specification, executive report, etc.)
- Level of detail and technical depth
- Target audience and communication style

**Context**: Business situation and constraints
- Project type and objectives
- Stakeholder environment and expectations
- Timeline and resource constraints

**Constraints**: Specific requirements and limitations
- Quality standards and accuracy requirements
- Compliance and regulatory considerations
- Performance and scalability needs

## Sample Master Prompts

### Example 1: COBOL Business Logic Extraction
```
**Role**: Senior Legacy Systems Analyst with 15+ years experience in mainframe modernization and financial services domain expertise

**Input**: COBOL source code file containing business logic for [specific function], including PROCEDURE DIVISION, WORKING-STORAGE definitions, and any referenced copybooks

**Format**: Structured business requirements document with numbered sections, including executive summary, detailed functional specifications, business rules catalog, and implementation notes suitable for development team handoff

**Context**: Enterprise modernization project migrating critical financial systems from mainframe to cloud-native architecture, requiring 100% functional parity and regulatory compliance

**Constraints**: Must capture all business logic including edge cases and exception handling, maintain audit trail requirements, ensure zero functional gaps that could impact financial calculations or regulatory reporting

Extract complete business requirements from this COBOL program including:
1. Primary business function and purpose
2. Input data sources and validation rules
3. All calculation logic and business formulas
4. Decision trees and conditional processing
5. Output specifications and report formats
6. Error handling and exception procedures
7. Performance requirements and batch timing
8. Audit trail and compliance considerations

Structure output for technical and business stakeholder review.
```

### Example 2: Comprehensive Risk Assessment
```
**Role**: Enterprise Risk Management Specialist with expertise in technology modernization projects and regulatory compliance in financial services

**Input**: Project scope document, technical architecture specifications, business requirements, and stakeholder analysis for [specific modernization project]

**Format**: Comprehensive risk register with categorized risks, probability/impact matrix, detailed mitigation strategies, ownership assignments, and executive dashboard summary

**Context**: High-visibility system modernization with regulatory oversight, zero tolerance for operational disruption, and aggressive timeline constraints

**Constraints**: Must identify risks across all project dimensions (technical, business, regulatory, operational), provide actionable mitigation strategies with realistic timelines, include contingency planning for high-impact scenarios

Develop complete risk assessment covering:
1. Technical risks (integration, performance, data migration)
2. Business risks (process disruption, user adoption, training)
3. Regulatory risks (compliance gaps, audit findings, reporting)
4. Project risks (timeline, resources, scope changes)
5. Operational risks (business continuity, support, monitoring)
6. Financial risks (budget overruns, opportunity costs)

Include mitigation strategies, ownership, timelines, and monitoring procedures.
```

## Tasks

### Task 1: Create Master Prompt Library (20 minutes)
Develop `Master_Prompts.md` containing:
- **10+ production-ready prompts** covering all required categories
- **Complete RIFCC structure** for each prompt
- **Customization notes** for adapting prompts to different scenarios
- **Expected output examples** showing prompt effectiveness
- **Quality validation criteria** for each prompt type

### Task 2: Prompt Effectiveness Evaluation (5-10 minutes)
For each prompt, document:
- **Target Use Case**: Specific scenarios where prompt excels
- **Customization Options**: How to adapt for different industries/contexts
- **Quality Indicators**: How to measure prompt success
- **Common Pitfalls**: What to avoid when using the prompt
- **Enhancement Opportunities**: Ways to improve prompt performance

## Required Prompt Categories with Specifications

### 1. COBOL Analysis Prompts

**Prompt A: Business Logic Extraction**
- Extract functional requirements from complex COBOL programs
- Must handle nested PERFORM statements, complex calculations
- Output: Structured requirements document

**Prompt B: Data Flow Analysis**  
- Map data movement through COBOL job streams
- Identify file dependencies and processing sequences
- Output: Data lineage documentation

### 2. Data Analysis Prompts

**Prompt C: Data Quality Assessment**
- Systematic identification of data quality issues
- Statistical analysis and business impact assessment
- Output: Executive data quality report

**Prompt D: Business Intelligence Generation**
- Extract actionable insights from raw datasets
- Focus on revenue drivers and performance metrics
- Output: Executive dashboard insights

### 3. Documentation Prompts

**Prompt E: Technical Specification Creation**
- Transform informal requirements into technical specifications
- Include non-functional requirements and constraints
- Output: Development-ready technical documentation

**Prompt F: Executive Summary Generation**
- Synthesize complex technical information for leadership
- Focus on business value and decision-making support
- Output: C-level executive briefing

### 4. Testing Prompts

**Prompt G: Test Case Generation**
- Create comprehensive test scenarios from requirements
- Include positive, negative, and edge cases
- Output: Detailed test plan with acceptance criteria

**Prompt H: Performance Testing Strategy**
- Design performance validation approaches
- Include load, stress, and scalability testing
- Output: Performance testing methodology

### 5. Risk Management Prompts

**Prompt I: Risk Identification**
- Systematic identification across all project dimensions
- Industry-specific risk considerations
- Output: Comprehensive risk catalog

**Prompt J: Mitigation Planning**
- Develop actionable risk mitigation strategies
- Include contingency planning and monitoring
- Output: Risk mitigation playbook

## Deliverable Requirements

### Master_Prompts.md Must Include:

#### For Each Prompt:
- [ ] Complete RIFCC structure with detailed, specific components
- [ ] Clear use case description and target scenarios
- [ ] Expected output format and quality standards
- [ ] Customization guidance for different contexts
- [ ] Example output demonstrating prompt effectiveness

#### Overall Structure:
- [ ] Professional formatting suitable for team distribution
- [ ] Categorized organization for easy reference
- [ ] Quick reference guide for prompt selection
- [ ] Best practices for prompt optimization
- [ ] Quality validation checklist for prompt outputs

## Advanced Prompt Engineering Techniques

### Prompt Optimization Strategies:
1. **Specificity Over Generality**: Detailed role definitions and contexts
2. **Output Structure Definition**: Clear formatting and organization requirements
3. **Constraint Specification**: Explicit limitations and quality standards
4. **Example Integration**: Include sample outputs when beneficial
5. **Iterative Refinement**: Test and improve prompts based on results

### Quality Indicators for Effective Prompts:
1. **Consistency**: Same prompt produces similar quality outputs
2. **Completeness**: Outputs address all required elements
3. **Actionability**: Results are immediately usable for next steps
4. **Professionalism**: Output quality suitable for client delivery
5. **Efficiency**: Minimal prompt iterations needed for desired results

## Success Criteria
- **Completeness**: All 10+ prompts cover required categories thoroughly
- **RIFCC Compliance**: Every prompt uses complete framework structure
- **Professional Quality**: Prompts produce client-ready outputs
- **Reusability**: Prompts work across multiple similar scenarios
- **Effectiveness**: Outputs consistently meet quality standards

## Common Prompt Engineering Mistakes to Avoid

### Role Definition Issues:
- Vague expertise descriptions ("experienced analyst")
- Missing domain specialization
- Unclear perspective or bias expectations

### Input Specification Problems:
- Ambiguous data source descriptions
- Missing context about input quality or completeness
- Unclear scope boundaries

### Format Specification Gaps:
- Vague output requirements ("detailed report")
- Missing audience considerations
- No structure or organization guidance

### Context and Constraint Omissions:
- Missing business environment details
- Unclear quality or compliance requirements
- No timeline or resource considerations

## Time Management Strategy
- **15 minutes**: Create 10 core prompts across required categories
- **5 minutes**: Add customization notes and quality criteria
- **5-10 minutes**: Review and optimize prompt effectiveness

## Extension Activities
If you complete early:
- Create industry-specific prompt variations
- Develop prompt chaining strategies for complex workflows
- Build prompt effectiveness measurement framework
- Design team training materials for prompt library usage
- Create automated prompt testing and validation procedures

## Prompt Testing and Validation

### Validation Approach:
1. **Test with Sample Inputs**: Use known scenarios to validate outputs
2. **Cross-Reference Results**: Compare outputs with expert analysis
3. **Iterative Refinement**: Improve prompts based on testing results
4. **Peer Review**: Have colleagues evaluate prompt effectiveness
5. **Real-World Application**: Use prompts in actual project scenarios

### Quality Metrics:
- **Accuracy**: Technical correctness of outputs
- **Completeness**: Coverage of all required elements
- **Usability**: Ease of applying outputs to real work
- **Consistency**: Reliability across multiple uses
- **Efficiency**: Time saved versus manual analysis

## Real-World Applications
These master prompts will be invaluable for:
- **Client Engagements**: Consistent, high-quality deliverable creation
- **Team Standardization**: Uniform analysis approaches across analysts
- **Knowledge Transfer**: Codifying expert analysis techniques
- **Quality Assurance**: Ensuring comprehensive coverage of analysis areas
- **Efficiency Improvement**: Reducing time to produce quality outputs

## Resources
- Reference `/reference/RIFCC_FRAMEWORK.md` for framework details
- Use actual project files for prompt testing and validation
- Check industry standards for output quality benchmarks

---
**Note**: This exercise culminates your training by creating reusable assets that will enhance your effectiveness as an AI-assisted business analyst throughout your career.