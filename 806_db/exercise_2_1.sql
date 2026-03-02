SELECT p.model
FROM Product as p 
JOIN PC as pc ON p.model = pc.model
WHERE pc.speed >= 3;


SELECT DISTINCT p.maker
FROM Product as p 
JOIN Laptop as lp ON p.model = lp.model
WHERE lp.hd >= 100;
-- 這邊用到 DISTINCT 是要去除可能有多個同樣名稱的 maker


SELECT p.model, new_t.price
FROM Product as p
JOIN (
    SELECT model, price from PC
    UNION
    SELECT model, price from Laptop
    UNION
    SELECT model, price FROM Printer
) as new_t ON new_t.model = p.model
WHERE p.maker = 'B' 


-- ------------------------------------------------


SELECT model 
FROM Printer 
WHERE color = true AND type = 'laser';


Select maker from Product
where type = 'laptop'
Except
Select maker from Product
where type = 'pc';


-- ------------------------------------------------
-- f) 
-- ! 這邊寫錯了
-- Select hd 
-- from PC
-- where COUNT(hd) >= 2;


select hd
from PC
GROUP BY hd
Having COUNT(hd) >= 2;

-- ------------------------------------------------
-- g)
select l1.model, l2.model
from PC as l1
Join PC as l2 ON l1.speed = l2.speed and l1.ram = l2.ram
Where

-- ! 實際會是多對多
-- P1.model,P2.model,狀況分析
-- a,a,自己配自己
-- a,b,"有效的配對 (a, b)"
-- a,c,"有效的配對 (a, c)"
-- b,a,"跟 (a, b) 重複了"
-- b,b,自己配自己
-- b,c,"有效的配對 (b, c)"
-- c,a,"跟 (a, c) 重複了"
-- c,b,"跟 (b, c) 重複了"
-- c,c,自己配自己
-- 在 SQL 中執行 JOIN 時，系統並非只做一對一的綁定，而是會將左右兩邊符合條件的資料展開成「所有可能的排列組合」（類似分支樹）。
-- 因此，當我們使用 Self-Join（自己與自己合併，例如 P1 JOIN P2）來尋找規格相同的資料時，如果只設定規格相等的條件，結果會長出「自己配自己 (a, a)」以及「順序顛倒的重複配對 (a, b 與 b, a)」。
-- 要完美過濾這些無效資料，最經典的技巧是加上 P1.model < P2.model 這個過濾條件：因為一個號碼永遠不會小於自己（這濾掉了自我配對），且當 a < b 成立時，b < a 必定不成立（這濾掉了顛倒的重複項）。
-- 透過這個簡單的大小於比較，就能確保最終留下來的，是純粹且絕對不重複的「兩兩一組」配對結果。

-- ------------------------------------------------
-- h) Find those manufacturers of at least two different computers (PC’s or
-- laptops) with speeds of at least 2.80.

select maker
from Product
Join(
    select model, speed from PC
    UNION
    select model, speed from Laptop
) as s_t On Product.model = s_t.model
where speed >= 2.8
GROUP BY p.maker -- ! 要用 having 就一定要用 group by
HAVING COUNT(maker) >= 2;

-- ------------------------------------------------
-- i) Find the manufacturer(s) of the computer (PC or laptop) with the highest available speed.
select maker
from Product
Join(
    select model, speed from PC
    UNION
    select model, speed from Laptop
) as s_t ON p.model = s_t.model
ORDER BY s_t.speed DESC
LIMIT 1;
-- !where max(speed);  這是錯誤的寫法，where 只能放條件

-- SELECT p.maker, MAX(s_t.speed)  
-- 這個語法完全合法，不會報錯。但是，它回答的問題變了！


-- ------------------------------------------------
-- j) Find the manufacturers of PC ’s with at least three different speeds.
select p.maker
from PC as pc
Join Product as p ON p.model = p.model
GROUP BY p.maker
Having COUNT(DISTINCT pc.speed) >=3;

-- DISTINCT 不加上這速度同樣的還是會算進去

-- ------------------------------------------------
-- k) Find the manufacturers who sell exactly three different models of PC.
Select p.maker
from Product as p
where p.type = 'pc'
GROUP BY p.maker
Having COUNT(p.model) = 3;

-- ------------------------------------------------
-- ------------------------------------------------
-- SELECT * FROM Product as p JOIN PC as pc ON p.model = pc.model;
-- 這情況中是兩側都樹枝都會長出（如果有）並且會呈現兩種同樣的col


-- SELECT * FROM Product NATURAL JOIN PC;
-- 資料庫會自動去檢查兩張表，發現「咦，你們都有一個欄位叫做 model 耶！」然後它會自動用 model 相等作為條件來合併。它會做自動去重（Projection），把同名的欄位合併成一個。
-- 一樣會長樹枝
-- but, 如果兩張表有太多重名，他會把全部的東西都自動合併，很可能讓篩選出來東西變成空表


-- using（這會變成除了你合併對象外，其他col 都有機會長出兩條）
-- 當這 4 根樹枝準備印成表格的時候，USING 的魔法才開始發揮作用。
-- 它會跟資料庫說：「既然這 4 根樹枝都是因為 speed 相等才接在一起的，那結果表格裡面，speed 只要留一格就好了，把它移到最前面！」
-- speed (魔法融合),pc.model,pc.hd,lap.model,lap.hd,lap.screen