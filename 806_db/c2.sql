SELECT * from Person
WHERE Hobby = 'photography';

-- 這邊Hobby 不用引號
-- 單引號 ' '：專屬用來包裝「字串值」（例如：'photography'）。
-- 雙引號 " "：專屬用來包裝「物件名稱 / 識別字」（例如："Person" 資料表、"Hobby" 欄位）。

SELECT DISTINCT Name, Address FROM Person;

-- DISTINCT 是drop duplicated row

SELECT id, Name FROM Person
WHERE Hobby = 'photography' or Hobby = 'cycling'


------------------------------------------------
-- ! Vertical 
-- UNION, INTERSECT, Except
------------------------------------------------
-- union，上下疊加
SELECT * form Person 
UNION
select * form New_Person;

-- 會比較少用，因為場景會是同col的表，新內容加入。是上下疊加
-- 有自動去重複

------------------------------------------------
SELECT * form Person 
INTERSECT
select * form New_Person;

------------------------------------------------
SELECT * form Person 
Except
select * form New_Person; 

------------------------------------------------
-- ! Horizontal, 
-- JOIN (default inner join)
------------------------------------------------
-- CROSS Join
select
    p.id as P_id,
    p.Name,
    p.Address,
    p.Hobby,
    c.id as C_id,
    c.course

from Person as p
CROSS Join Course as c;

------------------------------------------------
select * FROM Courses as c
Join Teachers as t
ON c.course = t.course;

------------------------------------------------
-- NATURAL JOIN 會自動找兩張表同名的 col

select * from Course
NATURAL JOIN Teachers;

------------------------------------------------
------------------------------------------------
-- exercise
select
    s.name,
    s.class as s_class,
    c.displacement
from Ships as s
Join Classes as c On s.class = c.class
where c.displacement > 35000;

------------------------------------------------
select 
    o.ship as name,
    c.displacement,
    c.numGuns
from 
(SELECT * from Outcomes
where battle = 'Guadalcanal' 
) as o
Join Ships as s ON o.ship = s.name
Join Classes as c ON s.class = c.class;

------------------------------------------------
Select s.name from Ships as s
UNION
Select o.ship from Outcomes as o;
