# Excel-to-SQL Formula Baselines

Use these snippets as copy/paste context when asking Copilot to transform spreadsheet logic into SQL. Attach the relevant CSV files (e.g., `#transactions.csv`, `#customers.csv`) and `#schema.sql` in your prompts.

## Monthly Revenue Trend
```excel
=SUMIFS(Amount, Status, "Completed", Date, ">=" & start_of_month, Date, "<=" & end_of_month)
```

## Customer 80/20 Concentration
```excel
=LET(sorted, SORT(Table[Revenue],,-1), running, SCAN(0, sorted, LAMBDA(a,b,a+b)), running / SUM(sorted))
```

## Failed Transaction Alert (Last 7 Days)
```excel
=COUNTIFS(Status, "Failed", Amount, ">=500", Date, ">=" & TODAY()-7)
```

## Average Amount by Tier
```excel
=AVERAGEIFS(Amount, Tier, "Gold")
```

## Monthly Failure Rate by Region
```excel
=IFERROR(
    COUNTIFS(Status, "Failed", Region, A2) /
    COUNTIFS(Status, "Completed", Region, A2 + Status, "Failed"),
0)
```
