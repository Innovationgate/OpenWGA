ALTER TABLE content_filederivates ADD COLUMN parent_id VARCHAR(32) DEFAULT NULL AFTER filesize;
CREATE INDEX T38_PARENT_ID ON content_filederivates (parent_id);
ALTER TABLE extensiondata ADD COLUMN binaryvalue_sha512 VARCHAR(128) DEFAULT NULL AFTER datevalue;
CREATE INDEX T28_BINARYVALUE_SHA512 ON extensiondata (binaryvalue_sha512);
