
ALTER TABLE historylog ADD COLUMN `logtime_ms` BIGINT AFTER `logtime`;
UPDATE historylog set logtime_ms = UNIX_TIMESTAMP(logtime)*1000;