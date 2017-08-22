-- psql --username=testuser test

CREATE TABLE notes (
    id serial primary key,
    property VARCHAR(20) NOT NULL,
    value VARCHAR(40) NOT NULL,
    created timestamp
);
