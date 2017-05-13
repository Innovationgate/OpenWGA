CREATE TABLE webarea_readers(
  area_id VARCHAR(32) NOT NULL,
  idx INTEGER NOT NULL,
  name VARCHAR(255),
  PRIMARY KEY (area_id, idx),
  FOREIGN KEY (area_id) REFERENCES webarea (id)
);
CREATE TABLE structentry_readers(
  struct_id VARCHAR(32) NOT NULL,
  idx INTEGER NOT NULL,
  name VARCHAR(255),
  PRIMARY KEY (struct_id, idx),
  FOREIGN KEY (struct_id) REFERENCES structentry (id)
);
CREATE INDEX IDX_T35_AREA_ID ON webarea_readers (area_id);
CREATE INDEX IDX_T34_STRUCT_ID ON structentry_readers (struct_id);
