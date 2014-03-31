ALTER TABLE project ADD COLUMN language varchar(256);
UPDATE project SET language='erlang';
ALTER TABLE project ALTER COLUMN language SET NOT NULL;
ALTER TABLE project ALTER COLUMN language SET DEFAULT 'erlang';

