-- 2023

SELECT * FROM R WHERE C > A;


SELECT DISTINCT C, B FROM R 
UNION
SELECT DISTINCT C, B FROM S;

-- 他這邊的 join 可以寫大於條件
SELECT S.A, S.B, S.D, R.A, R.B, R.C -- 不知道順序有沒差，但誰知道
FROM S CROSS JOIN R
Where R.C > S.D;

SELECT * FROM S Join R On R.C > S.D;


-- SELECT DISTINCT C, B
-- FROM (
--     SELECT *
--     FROM S CROSS JOIN R
--     WHERE S.B > R.C
-- );
SELECT DISTINCT R.C, R.B
FROM S CROSS JOIN R
WHERE S.B < R.C;


SELECT R.C, AVG(S.D)
FROM R JOIN S ON R.A = S.A AND R.B = S.B  -- natural join
GROUP BY R.C;


/* 
S ⋈ R  Natural Join — 自動用同名欄位做等值 join
S ⋈_{條件} R  Theta Join — 用指定條件做 join
S × R  Cross Join — 無條件笛卡爾積
*/


-- 2025.b.

SELECT R.A, S.B, C, D 
FROM R JOIN S ON C = D AND R.A>S.A

theta 
--  2025

-- SELECT * 
-- FROM T Cross JOIN R
-- WHERE T.b > T.C;
SELECT *
FROM (SELECT * FROM T WHERE B > C) AS filtered_T
CROSS JOIN R;

SELECT S.B, S.D FROM S
EXCEPT ALL
Select R.C, R.A from R;

-- SELECT * FROM T JOIN S on T.B = S.B and T.D = S.D
-- INTERSECT
-- SELECT * FROM R JOIN S on R.A = S.A and R.B = S.B;
SELECT * FROM T NATURAL JOIN S
INTERSECT
SELECT * FROM R NATURAL JOIN S;


SELECT *
FROM R cross JOIN S
WHERE R.C > S.D;

-- SELECT T.C, sum(S.A) FROM T JOIN S ON T.B = S.B and T.D = S.D
-- GROUP BY T.B;
SELECT T.B, T.C, SUM(S.A)
FROM T NATURAL JOIN S
GROUP BY T.B, T.C;




-- ========================================================================
-- ========================================================================

SELECT DISTINCT c.CourseName
form Courses as c Join StudentCourses as s
ON c.CourseID = s.CourseID
Where s.Semester = 'Spring23';
-- 我理解是這StudentCourses 必定是有選生選才會有，所以本身就是filter了 一定大於1

-- SELECT CourseID
-- from StudentCourses
-- Group by CourseID
-- HAVING COUNT(DISTINCT StudentID) < 2;
SELECT c.CourseID
FROM Courses AS c
LEFT JOIN StudentCourses AS sc ON c.CourseID = sc.CourseID
GROUP BY c.CourseID
HAVING COUNT(DISTINCT sc.StudentID) < 2;


SELECT StudentID FROM Students
WHERE StudentID not in (SELECT DISTINCT StudentID from StudentCourses);

SELECT StudentID, StudentName FROM Students
WHERE EnrollYear = 2022
ORDER BY StudentName ascending
UNION ALL --  這邊要這樣 才會避免去重和自動排序
SELECT StudentID, StudentName FROM Students
WHERE EnrollYear = 2021
ORDER BY StudentName descending;


select c.CourseName
from Courses as c Join StudentCourses as s ON c.CourseID = s.CourseID
where s.Semester = 'Spring23'
INTERSECT
select c.CourseName
from Courses as c Join StudentCourses as s ON c.CourseID = s.CourseID
where s.Semester = 'Fall23';


-- ========================================================================
-- ========================================================================
