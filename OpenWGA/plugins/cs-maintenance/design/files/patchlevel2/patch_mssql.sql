ALTER TABLE content_relations ADD relgroup VARCHAR(255) DEFAULT NULL;
CREATE INDEX IDX_T16_RELGROUP ON content_relations (content_id, relgroup);