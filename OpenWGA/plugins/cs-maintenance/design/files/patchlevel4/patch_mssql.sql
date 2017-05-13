CREATE TABLE content_filecontents(
		id                            		VARCHAR(32)		 NOT NULL,
		ordinalnr                     		INT		 NOT NULL,
		checksum_sha512               		VARCHAR(128)		 NULL ,
		filesize                      		INT		 NULL 
);
ALTER TABLE content_filecontents ADD CONSTRAINT IDX_content_filecontents_PK PRIMARY KEY (id);
ALTER TABLE content_filecontents ADD CONSTRAINT T36_ORDINAL_NR UNIQUE (ordinalnr);
CREATE INDEX T36_CHECKSUM_SHA512 ON content_filecontents (checksum_sha512);
CREATE TABLE content_filecontents_data(
		contents_id                   		VARCHAR(32)		 NOT NULL,
		partnr                        		INT		 NOT NULL,
		data                          		VARBINARY(max)		 NULL ,
		id                            		VARCHAR(32)		 NULL 
);
ALTER TABLE content_filecontents_data ADD CONSTRAINT IDX_content_filecontents_data_PK PRIMARY KEY (contents_id, partnr);
ALTER TABLE content_filecontents_data ADD CONSTRAINT IDX_content_filecontents_data_FK0 FOREIGN KEY (contents_id) REFERENCES content_filecontents (id);
CREATE INDEX T37_CONTENTS_ID ON content_filecontents_data (contents_id);
ALTER TABLE content_files_meta ADD checksum_sha512 VARCHAR(128) DEFAULT NULL;
CREATE INDEX T18_CHECKSUM_SHA512 ON content_files_meta (checksum_sha512);
CREATE TABLE content_filederivates(
		id                            		VARCHAR(32)		 NOT NULL,
		creator                       		VARCHAR(32)		 NOT NULL,
		name                          		VARCHAR(200)		 NOT NULL,
		created                       		DATETIME2		 NULL ,
		lastmodified                  		DATETIME2		 NULL ,
		parent_sha512                 		VARCHAR(128)		 NOT NULL,
		derivate_sha512               		VARCHAR(128)		 NOT NULL,
		filesize                      		INT		 NOT NULL
);
ALTER TABLE content_filederivates ADD CONSTRAINT content_filederivates_PK PRIMARY KEY (id);
CREATE INDEX T38_PARENT_SHA512 ON content_filederivates (parent_sha512);
CREATE INDEX T38_DERIVATE_SHA512 ON content_filederivates (derivate_sha512);
CREATE INDEX T38_CREATOR ON content_filederivates (creator);
