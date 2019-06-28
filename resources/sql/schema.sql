CREATE TABLE people (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  username varchar(255) NOT NULL UNIQUE,
  firstname varchar(255),
  lastname varchar(255)
);
