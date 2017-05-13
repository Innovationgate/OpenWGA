ALTER TABLE userprofile_items DROP CONSTRAINT IDX_T24_NAME;
ALTER TABLE userprofile_items ADD CONSTRAINT IDX_T24_NAME UNIQUE (profile_id, portlet_id, name);
UPDATE userprofile_items SET name=LOWER(name);
UPDATE structentry set uniquename=LOWER(uniquename) WHERE NOT uniquename IS NULL;