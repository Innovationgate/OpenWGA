DROP INDEX IDX_T24_NAME;
CREATE INDEX IDX_T24_NAME ON userprofile_items (profile_id, portlet_id, name);
UPDATE structentry set uniquename=LCASE(uniquename) WHERE NOT uniquename IS NULL;