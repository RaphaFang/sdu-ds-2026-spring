/* ----------------------------------------------------------------------------
 * 1. IN (實戰出場率：⭐⭐⭐⭐⭐)
 * ----------------------------------------------------------------------------
 * [核心邏輯]：用來取代多個 OR 條件，或檢查某值是否包含在子查詢的結果清單中。
 * 這邊是你要整張table 回傳的資料
 */

-- 【傳統寫法】使用多個 OR 拼接，不易閱讀
SELECT product_name, price 
FROM products 
WHERE category = 'Electronics' OR category = 'Books' OR category = 'Clothing';

-- 【現代推薦】使用 IN，簡潔明瞭
SELECT product_name, price 
FROM products 
WHERE category IN ('Electronics', 'Books', 'Clothing');

-- 【實戰情境】找出有下過訂單的活躍客戶
SELECT customer_name 
FROM customers 
WHERE customer_id IN (SELECT DISTINCT customer_id FROM orders);


/* ----------------------------------------------------------------------------
 * 2. EXISTS (實戰出場率：⭐⭐⭐⭐ 進階與效能優化必備)
 * ----------------------------------------------------------------------------
 * [核心邏輯]：檢查子查詢是否有回傳「任何一筆」資料。它只在乎「有沒有」，不在乎「是什麼」。
 * [實戰價值]：效能極佳！具備 Short-circuit (短路) 特性，找到第一筆就停，適合百萬級大表。
 */

-- 【實戰情境】找出「至少有一筆訂單」的客戶
-- 當 orders 資料量極大時，EXISTS 效能通常輾壓 IN
SELECT c.customer_name 
FROM customers c
WHERE EXISTS (
    SELECT 1            -- 這裡 SELECT 什麼不重要，通常寫 1
    FROM orders o
    WHERE o.customer_id = c.customer_id
);


/* ----------------------------------------------------------------------------
 * 3. ANY / SOME (實戰出場率：⭐ 幾乎被邊緣化)
 * ----------------------------------------------------------------------------
 * [核心邏輯]：大於「任何一個」值就算通過 -> 其實就是「大於最小值」。
 * [淘汰原因]：語意不夠直覺，且現代開發者為了防呆與可讀性，已全面改用 MIN()。
 */

-- 情境：找出價格大於「競爭對手任何一款產品」的自家商品 (大於底價即可)

-- 【冷門舊寫法】大於 ANY
SELECT product_name, price 
FROM my_products 
WHERE price > ANY (SELECT price FROM competitor_products);

-- 【現代推薦】使用 MIN()
-- 理由：人類一眼就能看懂，且聚合函數會自動忽略 NULL 值，避免比對時產生 UNKNOWN 錯誤。
SELECT product_name, price 
FROM my_products 
WHERE price > (SELECT MIN(price) FROM competitor_products);


/* ----------------------------------------------------------------------------
 * 4. ALL (實戰出場率：⭐ 充滿暗坑，極少使用)
 * ----------------------------------------------------------------------------
 * [核心邏輯]：大於「所有」值才算通過 -> 其實就是「大於最大值」。
 * [淘汰原因]：除了可讀性低，還有兩個致命底層坑：
 * 1. 遇到 NULL：條件判斷會變成 UNKNOWN，導致應該撈出的資料被過濾掉。
 * 2. 遇到空集合：會產生「空泛為真(Vacuous truth)」，導致條件永遠成立，引發全表撈出的 Bug。
 */

-- 情境：找出價格大於「競爭對手所有產品」的自家商品 (最頂級昂貴的產品)

-- 【危險寫法】大於 ALL
-- ⚠️ 致命 Bug：如果 competitor_products 是空表，這段語法會把 my_products 的「所有」資料都撈出來！
SELECT product_name, price 
FROM my_products 
WHERE price > ALL (SELECT price FROM competitor_products);

-- 【現代推薦】使用 MAX()
-- 理由：絕對安全！如果對手沒產品 (空表)，MAX() 回傳 NULL，比較結果不成立，不會誤撈資料。
SELECT product_name, price 
FROM my_products 
WHERE price > (SELECT MAX(price) FROM competitor_products);


/* ============================================================================
 * [總結 Takeaway]
 * 1. 處理關聯檢查：用 IN 或 EXISTS (資料量大時選 EXISTS)。
 * 2. 處理極值比較：忘記 ANY 和 ALL 吧！一律用 MIN() 和 MAX() 保平安。

 * ============================================================================ */