CREATE TABLE webarea_readers(
  area_id VARCHAR(32) NOT NULL,
  idx INT NOT NULL,
  name VARCHAR(255)
) ENGINE=${mysql.engine} CHARACTER SET=${mysql.charset} COLLATE=${mysql.collation};
CREATE TABLE structentry_readers(
  struct_id VARCHAR(32) NOT NULL,
  idx INT NOT NULL,
  name VARCHAR(255)
) ENGINE=${mysql.engine} CHARACTER SET=${mysql.charset} COLLATE=${mysql.collation};
ALTER TABLE structentry_readers ADD PRIMARY KEY (struct_id, idx);
ALTER TABLE structentry_readers ADD CONSTRAINT FK_structentry_readers_0 FOREIGN KEY (struct_id) REFERENCES structentry (id);
CREATE INDEX IDX_T34_STRUCT_ID ON structentry_readers (struct_id);
ALTER TABLE webarea_readers ADD PRIMARY KEY (area_id, idx);
ALTER TABLE webarea_readers ADD CONSTRAINT FK_webarea_readers_0 FOREIGN KEY (area_id) REFERENCES webarea (id);
CREATE INDEX IDX_T35_AREA_ID ON webarea_readers (area_id);