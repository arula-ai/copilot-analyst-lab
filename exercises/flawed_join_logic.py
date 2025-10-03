# This Python code has intentional logic errors for the lab
# Students must identify and fix issues

import pandas as pd

def calculate_customer_metrics(transactions_df, customers_df):
    """
    Calculate customer metrics with intentional flaws
    """
    
    merged = transactions_df.merge(customers_df, on='customer_id')
    
    revenue_by_customer = merged.groupby('customer_id')['amount'].sum()
    
    recent_transactions = merged[merged['date'] > '2024-01-01']
    
    average_transaction = revenue_by_customer / merged.groupby('customer_id').size()
    
    def apply_discount(row):
        if row['tier'] == 'Gold':
            return row['amount'] * 0.8
        elif row['tier'] == 'Silver':
            return row['amount'] * 0.95
        else:
            return row['amount']
    
    merged['discounted_amount'] = transactions_df.apply(apply_discount, axis=1)
    
    return merged
