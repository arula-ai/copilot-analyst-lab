-- This SQL has intentional errors for the governance lab
-- Students must identify and fix the issues
-- CHALLENGE: Find and fix all 6 errors across the 3 queries below

-- Query 1: Monthly Revenue by Customer Tier (HAS 1 ERROR - check GROUP BY clause)
SELECT 
    MONTH(transaction_date) as month,
    c.tier,
    SUM(t.amount) as total_revenue
FROM transactions t
JOIN customers c ON t.customer_id = c.customer_id
WHERE t.status = 'Completed'
    AND YEAR(transaction_date) = 2024
GROUP BY MONTH(transaction_date);

-- Query 2: Find High-Value Failed Transactions (HAS 2 ERRORS - column name + data type comparison)
SELECT 
    transaction_id,
    date,
    amount,
    customer_id
FROM transactions
WHERE status = 'Failed'
    AND amount > '500';

-- Query 3: Customer Ranking (HAS 3 ERRORS - alias usage in OVER + HAVING + wrong status value)
SELECT 
    customer_id,
    SUM(amount) as total_spent,
    RANK() OVER (ORDER BY total_spent DESC) as ranking
FROM transactions
WHERE status = 'Complete'
GROUP BY customer_id
HAVING total_spent > 10000;
