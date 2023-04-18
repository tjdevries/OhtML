CREATE TABLE comments (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  content TEXT NOT NULL,
  -- Add a foreign key to exhibits
  exhibit_id INTEGER NOT NULL,

  FOREIGN KEY (exhibit_id) REFERENCES exhibits(id)
)
