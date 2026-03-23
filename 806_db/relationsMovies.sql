	DROP TABLE IF EXISTS movies;

	CREATE TABLE movies(
	title CHAR(100),
	year INT,
	length INT,
	genre CHAR(10),
	studioname CHAR(30),
	producerc INT,
	PRIMARY KEY (title, year)
	);

	INSERT INTO movies("title", "year", "length", "genre", "studioname", "producerc")
	VALUES
	('Gone with the wind', 1939, 231, 'drama', 'Selznick Int. Pictures', 1),
	('Star Wars', 1977, 124, 'scifi', 'Lucasfilm Ltd. LLC', 2),
	('Waynes World', 1992, 95, 'comedy', 'Paramount Pictures', 3),
	('Titanic', 1997, 194, 'drama', 'Paramount Pictures', 4),
	('Gravity', 2013, 91, 'scifi', 'Heyday Films', 5),
	('So This is Love', 1953, 104, 'musical', 'Warner Bros.', 6),
	('Ben-Hur', 1959, 212, 'drama', 'MGM', 7),
	('Casper the friendly ghost movie', 1995, 101,'comedy','MGM',8),
	('The Cure for Insomnia', 1987, 5220, 'experiment', NULL, 9);


	DROP TABLE IF EXISTS moviestar;

	CREATE TABLE moviestar(
	name CHAR(30) PRIMARY KEY,
	address VARCHAR(255),
	gender CHAR(1),
	birthdate DATE
	);

	INSERT INTO moviestar("name", "address", "gender", "birthdate")
	VALUES
	('Leonardo DiCaprio', 'Malibustreet 1, Malibu', 'm', '1974-12-11'),
	('Sandra Bullock', 'Hollywood address 2', 'f', '1964-07-26'),
	('Charlton Heston', NULL, 'm', '1923-10-04'),
	('Kathryn Grayson', NULL, 'f', '1922-02-09');

	DROP TABLE IF EXISTS starsin;

	CREATE TABLE starsin(
	movietitle CHAR(100),
	movieyear INT,
	starname CHAR(30),
	PRIMARY KEY(starname, movietitle)
	);

	INSERT INTO starsin("movietitle", "movieyear", "starname")
	VALUES
	('Titanic', 1997, 'Leonardo DiCaprio'),
	('Gravity', 2013, 'Sandra Bullock'),
	('Ben-Hur', 1959, 'Charlton Heston'),
	('So This is Love', 1953, 'Kathryn Grayson'),
	('Casper the friendly ghost movie', 1995, 'Clint Eastwood');

	DROP TABLE IF EXISTS movieexec;

	CREATE TABLE movieexec(
	name CHAR(30),
	address VARCHAR(255),
	cert INT,
	networth INT,
	PRIMARY KEY(name)
	);

	INSERT INTO movieexec("name", "address", "cert", "networth")
	VALUES
	('David O. Selznick', 'Davids address', 1, 7000000),
	('Gary Kurtz', 'Garys address', 2, 7000000),
	('Lorne Michaels', 'Lornes address', 3, 500000000),
	('James Cameron', 'James address', 4, 700000000),
	('David Heyman', 'Heymans address', 5, 175000000),
	('Merv Griffin', 'Mervs address', 6, 1000000000),
	('Sam Zimbalist', 'Sams address', 7, 3000000);

	DROP TABLE IF EXISTS studio;

	CREATE TABLE studio(
	name CHAR(30),
	address VARCHAR(255),
	presc INT,
	PRIMARY KEY (name)
	);

	INSERT INTO studio("name", "address", "presc")
	VALUES
	('MGM', '245 N Beverly Dr. Beverly Hills, CA 90210', '6');


