# Exercise 2: Comprehensive Data Analysis

## Learning Objectives
- Perform systematic data quality assessment on large datasets
- Generate actionable business insights from raw data
- Write complex SQL queries for business intelligence
- Apply RIFCC framework to data analysis scenarios
- Create professional data analysis reports

## Time Estimate: 45-60 minutes

## Overview
You will analyze a comprehensive sales dataset (1000+ records) to identify data quality issues, extract business insights, and answer critical business questions. This exercise simulates real-world data analysis scenarios common in business intelligence and system modernization projects.

## Dataset Description
**File**: `/homework/data/sales_data.csv` (1000 rows)
**Contents**: Sales transactions with customer, product, territory, and performance data
**Columns**: 
- Transaction_ID, Customer_ID, Customer_Name, Territory, Sales_Rep
- Product_ID, Product_Name, Product_Category, Unit_Price, Quantity
- Sale_Date, Total_Amount, Commission_Rate, Target_Amount
- Payment_Method, Order_Status, Delivery_Date

## Tasks

### Task 1: Data Quality Analysis (20 minutes)
Create `DATA_Quality_Report.md` identifying:
- **Missing Data**: Null values, empty fields, incomplete records
- **Data Inconsistencies**: Format variations, duplicate entries, conflicting values
- **Outliers**: Unusual values that may indicate errors
- **Referential Integrity**: Broken relationships between related data
- **Business Rule Violations**: Values outside acceptable ranges
- **Data Completeness**: Coverage gaps by territory, product, time period

**Target**: Identify minimum 10 distinct data quality issues with specific examples

### Task 2: SQL Analysis (15 minutes)
Create `SQL_Analysis_Queries.sql` with queries answering:
1. **Revenue Performance**: Total revenue by territory and month
2. **Product Analysis**: Top 10 products by revenue and quantity
3. **Customer Segmentation**: Customer ranking by total purchases
4. **Sales Rep Performance**: Commission earned vs. targets achieved
5. **Seasonal Patterns**: Sales trends by quarter and product category
6. **Payment Analysis**: Revenue distribution by payment method
7. **Delivery Performance**: Average delivery time by territory
8. **Target Achievement**: Percentage of sales reps meeting targets

### Task 3: Business Insights Generation (10-15 minutes)
Create `Business_Insights.md` with:
- **Executive Summary**: Key findings and recommendations
- **Performance Trends**: Growth patterns and concerning declines
- **Opportunity Identification**: Underperforming segments with potential
- **Risk Assessment**: Data quality risks and business impacts
- **Action Items**: Prioritized recommendations with business impact

## Sample RIFCC Prompts

### For Data Quality Assessment:
```
**Role**: Senior Data Quality Analyst with expertise in sales data systems
**Input**: Sales dataset CSV file with 1000+ transaction records
**Format**: Comprehensive data quality assessment report
**Context**: Pre-migration data audit for CRM system modernization project
**Constraints**: Must identify all data quality issues that could impact business reporting accuracy

Perform a thorough data quality analysis of this sales dataset. Examine:
1. Data completeness and missing value patterns
2. Consistency in customer and product information
3. Valid ranges for financial amounts and dates
4. Duplicate detection across all dimensions
5. Referential integrity between related fields
6. Business rule compliance (commissions, targets, etc.)

Provide specific examples of each issue type with row numbers and recommended remediation approaches. Prioritize issues by business impact.
```

### For Business Intelligence Analysis:
```
**Role**: Business Intelligence Analyst specializing in sales performance analytics
**Input**: Complete sales transaction dataset with customer, product, and performance metrics
**Format**: Executive dashboard insights with supporting SQL queries
**Context**: Monthly business review requiring actionable insights for sales leadership
**Constraints**: Focus on revenue drivers, performance gaps, and growth opportunities

Analyze this sales data to generate strategic business insights including:
1. Revenue performance by territory and product category
2. Sales representative effectiveness and target achievement
3. Customer behavior patterns and segmentation opportunities
4. Product performance and profitability analysis
5. Seasonal trends and forecasting implications
6. Operational efficiency metrics (delivery, payment processing)

Provide SQL queries supporting each insight and executive-level recommendations.
```

### For Outlier Detection:
```
**Role**: Data Scientist focused on anomaly detection in financial datasets
**Input**: Sales transaction data requiring statistical analysis for outliers
**Format**: Technical analysis report with statistical validation
**Context**: Data preparation for machine learning model training
**Constraints**: Distinguish between legitimate business outliers and data errors

Identify statistical outliers in this sales dataset using:
1. Standard deviation analysis for numerical fields
2. Frequency analysis for categorical data
3. Business logic validation for calculated fields
4. Time-series analysis for seasonal anomalies
5. Cross-field validation for logical consistency

Explain methodology, provide specific examples, and recommend treatment for each outlier type.
```

## Business Questions to Answer

### Revenue Analysis:
1. Which territories are underperforming relative to targets?
2. What is the average deal size by product category?
3. How does commission rate correlate with sales performance?
4. Which months show seasonal revenue spikes or declines?

### Customer Intelligence:
5. What percentage of customers are repeat buyers?
6. Which customer segments generate highest lifetime value?
7. How does payment method preference vary by territory?
8. What is the average time from order to delivery?

### Product Performance:
9. Which products have the highest profit margins?
10. Are there products with declining sales trends?
11. How does product mix vary across territories?
12. Which product categories drive the most volume vs. revenue?

### Sales Operations:
13. What percentage of sales reps are meeting targets?
14. How does performance vary by territory size?
15. Are there delivery delays impacting customer satisfaction?
16. Which payment methods have the highest transaction values?

## Deliverables Checklist

### DATA_Quality_Report.md Must Include:
- [ ] Executive summary of data quality status
- [ ] Detailed catalog of 10+ specific issues
- [ ] Examples with row numbers or specific values
- [ ] Impact assessment for each issue type
- [ ] Remediation recommendations prioritized by business impact
- [ ] Data quality metrics and scoring
- [ ] Visualization suggestions for quality dashboards

### SQL_Analysis_Queries.sql Must Include:
- [ ] Properly formatted, executable SQL queries
- [ ] Comments explaining business purpose of each query
- [ ] Performance considerations for large datasets
- [ ] Query results interpretation
- [ ] Aggregation functions appropriate for business questions
- [ ] JOIN operations where needed for comprehensive analysis
- [ ] Date/time functions for temporal analysis
- [ ] Statistical functions for performance metrics

### Business_Insights.md Must Include:
- [ ] Executive summary with top 3 insights
- [ ] Revenue trend analysis with specific metrics
- [ ] Performance gap identification with root causes
- [ ] Customer behavior insights with segmentation
- [ ] Product performance recommendations
- [ ] Operational efficiency observations
- [ ] Data-driven action items with expected business impact
- [ ] Risk assessment and mitigation strategies

## Success Criteria
- **Accuracy**: All identified issues are genuine data problems
- **Completeness**: Analysis covers all major data dimensions
- **Business Relevance**: Insights directly actionable by management
- **Technical Quality**: SQL queries are optimized and executable
- **Professional Presentation**: Reports suitable for executive review

## Advanced Analysis Techniques
1. **Cohort Analysis**: Customer retention patterns over time
2. **RFM Segmentation**: Recency, Frequency, Monetary customer grouping
3. **Pareto Analysis**: 80/20 rules in customer and product performance
4. **Correlation Analysis**: Relationships between variables
5. **Trend Analysis**: Moving averages and growth rate calculations
6. **Anomaly Detection**: Statistical methods for outlier identification

## Time Management Strategy
- **15 minutes**: Initial data exploration and structure understanding
- **20 minutes**: Systematic data quality assessment
- **15 minutes**: SQL query development and testing
- **10-15 minutes**: Business insights synthesis and documentation

## Extension Activities
If you complete early:
- Create visualizations using data analysis tools
- Develop customer lifetime value calculations
- Build forecasting models for revenue prediction
- Design data quality monitoring dashboards
- Research industry benchmarks for performance comparison

## Common Data Quality Issues to Look For
1. **Missing Values**: NULL, empty strings, placeholder values
2. **Format Inconsistencies**: Date formats, name capitalization, phone numbers
3. **Duplicates**: Exact matches and fuzzy duplicates
4. **Outliers**: Extreme values in price, quantity, dates
5. **Invalid Ranges**: Negative quantities, future dates, invalid codes
6. **Orphaned Records**: Foreign key violations
7. **Business Logic Violations**: Commission > 100%, negative totals
8. **Inconsistent Hierarchies**: Product category mismatches

## Resources
- Use `/templates/DATA_NOTES.md` for analysis structure
- Reference `/data/schema.sql` for data relationship understanding
- Check `/reference/RIFCC_FRAMEWORK.md` for prompt optimization

---
**Note**: This exercise develops critical skills for data-driven decision making and system modernization data preparation.