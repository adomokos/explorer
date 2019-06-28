CREATE TABLE people (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  username varchar(255) NOT NULL UNIQUE,
  firstname varchar(255),
  lastname varchar(255)
);

CREATE TABLE github_info (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  people_id INTEGER NOT NULL,
  login varchar(255) NOT NULL UNIQUE,
  name varchar(255) NOT NULL,
  FOREIGN KEY (people_id) REFERENCES people(id)
);
