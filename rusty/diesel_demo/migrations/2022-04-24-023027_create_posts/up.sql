-- Your SQL goes here
CREATE TABLE posts (
  --id SERIAL PRIMARY KEY, -- change a bit
  id integer PRIMARY KEY,
  some_uuid UUID NOT NULL,
  title VARCHAR NOT NULL,
  body TEXT NOT NULL,
  published BOOLEAN NOT NULL DEFAULT 'f'
)
