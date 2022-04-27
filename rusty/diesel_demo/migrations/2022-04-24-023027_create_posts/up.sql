-- Your SQL goes here
CREATE TABLE posts (
  id SERIAL PRIMARY KEY,
  --id integer PRIMARY KEY, -- if it is integer, I have to add it in create_post function manully
  some_uuid UUID NOT NULL,
  title VARCHAR NOT NULL,
  body TEXT NOT NULL,
  published BOOLEAN NOT NULL DEFAULT 'f'
)
