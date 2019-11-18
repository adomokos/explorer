CREATE TABLE people (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  email varchar(255) NOT NULL UNIQUE,
  firstname varchar(255),
  lastname varchar(255),
  git_hub_username varchar(255),
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE git_hub_metrics (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  people_id INTEGER NOT NULL,
  login varchar(255) NOT NULL,
  name varchar(255) NOT NULL,
  public_gists INTEGER NOT NULL,
  public_repos INTEGER NOT NULL,
  followers INTEGER NOT NULL,
  following INTEGER NOT NULL,
  account_created_at DATETIME NOT NULL,
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY (people_id) REFERENCES people(id)
);
