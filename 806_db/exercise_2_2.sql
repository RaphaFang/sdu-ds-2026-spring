-- a) Give the class names and countries of the classes that carried guns of at least 16-inch bore.
SELECT class, country
FROM Classes
WHERE bore >= 16;


-- ------------------------------------------------------------
-- b) Find the ships launched prior to 1921.
SELECT name
FROM Ships
where launched < 1921;


-- ------------------------------------------------------------
-- c) Find the ships sunk in the battle of the Denmark Strait.
SELECT ship
FROM Outcomes
WHERE battle = 'Denmark Strait' and result = 'sunk';


-- ------------------------------------------------------------
-- d) The treaty of Washington in 1921 prohibited capital ships heavier than 35,000 tons. List the ships that violated the treaty of Washington.
SELECT s.name
FROM Ships as s
JOIN Classes as c ON s.class = c.class
WHERE c.displacement > 35000;


-- ------------------------------------------------------------
-- e) List the name, displacement, and number of guns of the ships engaged in the battle of Guadalcanal.
-- Guadalcanal
SELECT o.ship, c.displacement, c.numGuns
FROM Outcomes as o
JOIN Ships as s ON o.ship = s.name
JOIN Classes as c ON s.class = c.class
WHERE o.battle = 'Guadalcanal';




-- ------------------------------------------------------------
-- f) List all the capital ships mentioned in the database. (Remember that all these ships may not appear in the Ships relation.)
-- SELECT * DISTINCT 
-- from (SELECT ship from Outcomes
-- UNION
-- SELECT name FROM Ships);

-- UNION 預設就是 set 所以不用額外的去重
SELECT ship from Outcomes
UNION
SELECT name FROM Ships

-- ------------------------------------------------------------
-- g) Find the classes that had only one ship as a member of that class.
SELECT c.class
FROM Classes as c
JOIN Ships as s ON c.class = s.class
GROUP BY c.class
HAVING COUNT(c.class) = 1;

-- ------------------------------------------------------------
-- h) Find those countries that had both battleships and battle cruisers.
-- SELECT c.country
-- FROM Classes as c
-- JOIN Ships as s ON c.class = s.class
-- GROUP BY c.country
-- HAVING COUNT(c.type = 'bb') >= 1 and COUNT(c.type = 'bc') >= 1;

-- 上面寫法是錯誤的
SELECT country FROM Classes WHERE type = 'bb'
INTERSECT
SELECT country FROM Classes WHERE type = 'bc';


-- ------------------------------------------------------------
-- i) Find those ships that “lived to fight another day”; they were damaged in one battle, but later fought in another.
SELECT o1.ship
FROM Outcomes as o1
JOIN Outcomes as o2 ON o1.ship = o2.ship
Join Battles as b1 ON o1.battle = b1.name
Join Battles as b2 ON o2.battle = b2.name
WHERE o1.result = 'damaged' and o2.result IS NOT NULL and b1.date < b2.date;


-- 在 SQL 裡面，當你只寫 JOIN 這個字的時候，資料庫預設執行的就是 INNER JOIN（內部合併）。

-- 它的鐵則就是：「必須雙方都有，樹枝才能牽得起來。只要有一方沒有，這筆資料就會直接消失在結果裡。」

-- 結果： 如果左邊的資料在右邊找不到對應的對象，左邊這筆資料就會被「丟棄」，不會出現在最終的表格中。