CREATE TABLE project (
  id serial primary key not null,
  repo varchar(256) not null,
  project varchar(256) not null,
  giturl varchar(1024) not null unique,
  creation_date date not null default CURRENT_DATE,
  update_date date not null default CURRENT_DATE
);

