WITH 
  -- 第一個暫存表
  AAA AS (
      SELECT ... FROM ...
  ), -- ⚠️ 注意這裡：多個暫存表之間「必須」用逗號隔開

  -- 第二個暫存表 (它甚至可以直接引用上面的 AAA)
  BBB AS (
      SELECT ... FROM AAA WHERE ...
  ),

  -- 第三個暫存表
  CCC AS (
      SELECT ... FROM ...
  )

-- 最後的主操作 (必須緊接著最後一個括號，不能有逗號)
SELECT * FROM BBB 
JOIN CCC ON ... ;