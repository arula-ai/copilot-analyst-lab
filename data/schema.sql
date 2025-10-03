-- Schema documentation for analyst reference
-- This represents the target modern database structure

CREATE TABLE transactions (
    transaction_id VARCHAR(10) PRIMARY KEY,
    transaction_date DATE NOT NULL,
    customer_id VARCHAR(10) NOT NULL,
    product_category VARCHAR(50),
    amount DECIMAL(10,2),
    status VARCHAR(20),
    payment_method VARCHAR(20),
    region VARCHAR(20),
    FOREIGN KEY (customer_id) REFERENCES customers(customer_id)
);

CREATE TABLE customers (
    customer_id VARCHAR(10) PRIMARY KEY,
    company_name VARCHAR(100) NOT NULL,
    tier VARCHAR(20),
    annual_revenue DECIMAL(12,2),
    region VARCHAR(20),
    account_manager VARCHAR(10)
);

-- Sample business rules to implement:
-- 1. Gold tier customers get 10% discount on amounts > 1000
-- 2. Failed transactions should trigger alert if > 500
-- 3. Regional rollups calculated monthly
