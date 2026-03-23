-- =====================================================================================
-- 3.1
-- the dif is the use of comma between A and B

-- =====================================================================================
-- 3.2
-- =====================================================================================
-- a. 
SELECT address
FROM Studio
WHERE name = 'MGM';

-- b.
SELECT birthdate
FROM moviestar
WHERE name = 'Sandra Bullock';

-- c.
SELECT starName FROM StarsIn
WHERE movieTitle LIKE '%Love%' or movieYear = 1980;

-- d.
SELECT name
FROM movieexec
WHERE netWorth >=10000000;

-- e.
SELECT name FROM MovieStar
WHERE gender = 'male' or address LIKE '%Malibu%';

-- =====================================================================================
-- 3.3
-- =====================================================================================
SELECT model,speed,hd
FROM PC
WHERE price < 1000;

SELECT model,speed as gigahertz,hd as gigabytes
FROM PC
WHERE price < 1000;

SELECT DISTINCT maker 
FROM Product 
WHERE type = 'Printer';

SELECT model, ram, screen
FROM Laptop WHERE price > 1500;

SELECT * FROM Printer WHERE color = TRUE;

SELECT model, hd FROM PC WHERE speed = 3.2 and price < 2000;

-- =====================================================================================
-- 3.5
-- =====================================================================================
-- a.
SELECT s.starName as s FROM Starsln as s
JOIN MovieStar as m ON s.starName = m.name
WHERE movieTitle = 'Titanic' and m.gender = 'male';

-- b.
SELECT s.starName
From Movies as m
JOIN Starsln as s On m.title = s.movieTitle AND m.year = s.movieYear -- 這才是比保險的作法
WHERE m.studioName = 'MGM' and m.year = 1995;
-- c.
SELECT name 
FROM MovieExec 
WHERE cert# = (
    SELECT presC# FROM Studio WHERE name = 'MGM'
);
-- d.
SELECT title FROM movies 
WHERE length > 
(SELECT length From Movies WHERE title = 'Gone With the Wind')
-- e.
SELECT name from MovieExec
WHERE netWorth > (SELECT netWorth from MovieExec WHERE name = 'Merv Griffin')

-- ===================================================================================== 56
-- 3.6
-- =====================================================================================
-- a.
SELECT s.name
from Ships as s
JOIN Classes as c ON s.class = c.class
WHERE c.displacement > 35000;

-- b.
SELECT name, c.displacement, c.numGuns
from Ships as s
JOIN Classes as c ON s.class = c.class
JOIN Outcomes as o ON s.name = o.ship
WHERE o.battle = 'Guadalcanal';

-- c. 
SELECT name from Ships
UNION
SELECT ship from Outcomes;

-- d.
SELECT country from Classes WHERE type = 'bb'
INTERSECT
SELECT country from Classes WHERE type = 'bc';

SELECT DISTINCT c1.country
from Classes as c1
JOIN Classes as c2 ON c1.country = c2.country
WHERE c1.type = 'bb' AND c2.type = 'bc';

-- e.
With
    ob AS (
        SELECT * from Outcomes as o
        Join Battles as b ON o.battle = b.name
    )

SELECT ob1.ship
from ob as ob1
JOIN ob as ob2 ON ob1.ship = ob2.ship
WHERE ob1.date < ob2.date and ob1.result = 'damaged';

-- f.
SELECT o.battle
from Outcomes as o
Join Ships as s on o.ship = s.name
Join Classes as c ON s.class = c.class
GROUP BY o.battle, c.country
having COUNT(o.ship) >=3;

-- =====================================================================================
-- 3.7
-- =====================================================================================
SELECT p.maker
from PC as pc
Join Product as p ON pc.model = p.model
WHERE pc.speed >= 3.0;

    -- ! =====================================================================================
        -- SELECT max(price)
        -- from Printer
        -- GPT 建計寫下面這樣
        -- ! 問題點：SQL 的死穴 —— 「混合一般欄位與聚合函數」
        -- 在 SQL 中，你絕對不能在同一個 SELECT 裡面，同時放上一般欄位（model）和聚合函數（MAX(price)），
        -- 除非你有寫 GROUP BY model。如果你這樣跑，PostgreSQL 會直接噴錯。
        -- 為什麼？因為 MAX() 會把所有資料縮成一行，但 model 有好幾行，資料庫不知道要把哪個 model 對齊那個最大值。
        SELECT * FROM Printer 
        WHERE price = (SELECT MAX(price) FROM Printer);
    -- !=====================================================================================

SELECT model
from Laptop
WHERE speed < (SELECT min(speed) from PC);

With
    all_t as (
        SELECT model, price from PC
        UNION
        SELECT model, price from Laptop
        UNION
        SELECT model, price from Printer
    )
SELECT model
from all_t
WHERE price = (SELECT MAX(price) FROM all_t);;

SELECT p.maker
from Printer as pr
Join Product as p ON pr.model = p.model
WHERE pr.color = TRUE and pr.price = (SELECT min(price) from Printer where color = TRUE);


-- =====================================================================================
-- 3.8
-- =====================================================================================

-- Classes(class, type, country, numGuns, bore, displacement)
-- Ships(name, class, launched)
-- Battles(name, date)
-- Outcomes(ship, battle, result)

-- of Exercise 2.4.3. Use subqueries as often as you can.
-- a) Find the countries whose ships had the largest number of guns.
SELECT c.country
FROM Ships as s
Join Classes as c ON s.class = c.class
where c.numGuns = (
    SELECT max(numGuns)
    FROM Classes
)

    -- ! =====================================================================================
    -- b) Find the classes of ships, at least one of which was sunk in a battle.
    SELECT DISTINCT s.class
    FROM Ships AS s
    WHERE EXISTS (
        SELECT 1
        FROM Outcomes AS o
        WHERE o.result = 'sunk' 
        AND o.ship = s.name  -- 👈 關鍵救命符！把外層的船名跟內層綁定
    );
    
    -- 👨‍💻 業界寫法：使用 EXISTS (這叫做 Semi-Join 半連接)
    SELECT c.class
    FROM Classes c
    WHERE EXISTS (
        SELECT 1
        FROM Ships s
        JOIN Outcomes o ON s.name = o.ship
        WHERE s.class = c.class 
        AND o.result = 'sunk'
    );
    -- ! =====================================================================================

-- c) Find the names of the ships with a 16-inch bore.
SELECT s.name
from Ships as s
JOIN Classes as c ON s.class = c.class
WHERE c.bore = 16;

-- d) Find the battles in which ships of the Kongo class participated.

SELECT DISTINCT o.battle
from Outcomes as o
JOIN Ships as s ON o.ship = s.name
where (s.class = 'Kongo');