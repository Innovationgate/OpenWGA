ALTER TABLE userprofile_items DROP INDEX IDX_T24_NAME,
ADD UNIQUE INDEX IDX_T24_NAME USING BTREE(`portlet_id`, `name`, `profile_id`);
UPDATE userprofile_items SET name=LCASE(name);
UPDATE structentry set uniquename=LCASE(uniquename) WHERE NOT uniquename IS NULL;