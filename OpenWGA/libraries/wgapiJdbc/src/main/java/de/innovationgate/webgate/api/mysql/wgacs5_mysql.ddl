CREATE TABLE acl(
		id                            		VARCHAR(32)		 NOT NULL,
		name                          		VARCHAR(200)		 NOT NULL,
		entrytype                     		TINYINT		 NULL ,
		entrylevel                    		TINYINT		 NULL ,
		flags                         		LONGTEXT		 NULL 
);

CREATE TABLE webarea(
		id                            		VARCHAR(32)		 NOT NULL,
		name                          		VARCHAR(200)		 NOT NULL,
		created                       		DATETIME		 NOT NULL,
		lastmodified                  		DATETIME		 NOT NULL
);

CREATE TABLE webarea_editors(
		webarea_id                    		VARCHAR(32)		 NOT NULL,
		idx                           		INT		 NOT NULL,
		name                          		VARCHAR(245)		 NULL 
);

CREATE TABLE contenttype(
		id                            		VARCHAR(32)		 NOT NULL,
		name                          		VARCHAR(200)		 NOT NULL,
		created                       		DATETIME		 NOT NULL,
		lastmodified                  		DATETIME		 NOT NULL,
		workflow                      		VARCHAR(255)		 NULL ,
		outerlayout                   		VARCHAR(255)		 NULL ,
		innerlayout                   		VARCHAR(255)		 NULL ,
		positioning                   		VARCHAR(255)		 NULL ,
		preferredparent               		LONGTEXT		 NULL 
);

CREATE TABLE structentry(
		id                            		VARCHAR(32)		 NOT NULL,
		structkey                     		VARCHAR(32)		 NOT NULL,
		created                       		DATETIME		 NOT NULL,
		lastmodified                  		DATETIME		 NOT NULL,
		structposition                		INT		 NOT NULL,
		title                         		VARCHAR(255)		 NULL ,
		workflow                      		VARCHAR(255)		 NULL ,
		parententry_id                		VARCHAR(32)		 NULL ,
		contenttype_id                		VARCHAR(32)		 NULL ,
		webarea_id                    		VARCHAR(32)		 NULL ,
		uniquename                    		VARCHAR(255)		 NULL 
);

CREATE TABLE lang(
		id                            		VARCHAR(32)		 NOT NULL,
		name                          		VARCHAR(10)		 NOT NULL,
		created                       		DATETIME		 NOT NULL,
		lastmodified                  		DATETIME		 NOT NULL,
		title                         		VARCHAR(255)		 NULL 
);

CREATE TABLE content(
		id                            		VARCHAR(32)		 NOT NULL,
		created                       		DATETIME		 NOT NULL,
		lastmodified                  		DATETIME		 NOT NULL,
		published                     		DATETIME		 NULL ,
		title                         		VARCHAR(255)		 NULL ,
		status                        		VARCHAR(1)		 NOT NULL,
		uniquename                    		VARCHAR(255)		 NULL ,
		contentversion                		INT		 NOT NULL,
		visible                       		BIT		 NOT NULL,
		validfrom                     		DATETIME		 NULL ,
		validto                       		DATETIME		 NULL ,
		struct_id                     		VARCHAR(32)		 NULL ,
		lang_id                       		VARCHAR(32)		 NULL ,
		contentclass                  		VARCHAR(255)		 NULL ,
		author                        		VARCHAR(255)		 NULL ,
		owner                         		VARCHAR(255)		 NULL ,
		linktarget                    		VARCHAR(50)		 NULL 
);

CREATE TABLE content_files_meta(
		id                            		VARCHAR(32)		 NOT NULL,
		content_id                    		VARCHAR(32)		 NOT NULL,
		name                          		VARCHAR(100)		 NOT NULL,
		created                       		DATETIME		 NULL ,
		lastmodified                  		DATETIME		 NULL ,
		filesize                      		BIGINT		 NULL ,
		checksum                      		VARCHAR(32)		 NULL ,
		checksum_sha512               		VARCHAR(128)		 NULL 
);

CREATE TABLE content_files_data(
		file_id                       		VARCHAR(32)		 NOT NULL,
		partnr                        		INT		 NOT NULL,
		data                          		BLOB		 NULL 
);

CREATE TABLE content_ishiddenfrom(
		content_id                    		VARCHAR(32)		 NOT NULL,
		idx                           		INT		 NOT NULL,
		name                          		VARCHAR(255)		 NULL 
);

CREATE TABLE content_items(
		id                            		VARCHAR(32)		 NOT NULL,
		content_id                    		VARCHAR(32)		 NOT NULL,
		name                          		VARCHAR(200)		 NOT NULL,
		datatype                      		TINYINT		 NOT NULL,
		textvalue                     		LONGTEXT		 NULL ,
		numbervalue                   		DOUBLE		 NULL ,
		datevalue                     		DATETIME		 NULL 
);

CREATE TABLE content_keywords(
		content_id                    		VARCHAR(32)		 NOT NULL,
		idx                           		INT		 NOT NULL,
		name                          		VARCHAR(255)		 NULL 
);

CREATE TABLE content_readers(
		content_id                    		VARCHAR(32)		 NOT NULL,
		idx                           		INT		 NOT NULL,
		name                          		VARCHAR(255)		 NULL 
);

CREATE TABLE content_relations(
		id                            		VARCHAR(32)		 NOT NULL,
		content_id                    		VARCHAR(32)		 NOT NULL,
		name                          		VARCHAR(255)		 NOT NULL,
		reltype                       		TINYINT		 NOT NULL,
		targetstructentry             		VARCHAR(32)		 NOT NULL,
		targetlang                    		VARCHAR(10)		 NOT NULL,
		targetcontent                 		VARCHAR(32)		 NULL ,
		relgroup                      		VARCHAR(255)		 NULL 
);

CREATE TABLE content_wfhistory(
		content_id                    		VARCHAR(32)		 NOT NULL,
		idx                           		INT		 NOT NULL,
		name                          		VARCHAR(255)		 NULL 
);

CREATE TABLE contenttype_positions(
		contenttype_id                		VARCHAR(32)		 NOT NULL,
		idx                           		INT		 NOT NULL,
		name                          		VARCHAR(255)		 NULL 
);

CREATE TABLE contenttype_editors(
		contenttype_id                		VARCHAR(32)		 NOT NULL,
		idx                           		INT		 NOT NULL,
		name                          		VARCHAR(255)		 NULL 
);

CREATE TABLE scripts(
		id                            		VARCHAR(32)		 NOT NULL,
		name                          		VARCHAR(200)		 NOT NULL,
		codetype                      		VARCHAR(50)		 NOT NULL,
		created                       		DATETIME		 NOT NULL,
		lastmodified                  		DATETIME		 NOT NULL,
		code                          		LONGTEXT		 NULL 
);

CREATE TABLE filecontainer(
		id                            		VARCHAR(32)		 NOT NULL,
		name                          		VARCHAR(200)		 NOT NULL,
		created                       		DATETIME		 NOT NULL,
		lastmodified                  		DATETIME		 NOT NULL
);

CREATE TABLE filecontainer_files_meta(
		id                            		VARCHAR(32)		 NOT NULL,
		fc_id                         		VARCHAR(32)		 NOT NULL,
		name                          		VARCHAR(100)		 NOT NULL,
		created                       		DATETIME		 NULL ,
		lastmodified                  		DATETIME		 NULL ,
		filesize                      		BIGINT		 NULL ,
		checksum                      		VARCHAR(32)		 NULL 
);

CREATE TABLE filecontainer_files_data(
		file_id                       		VARCHAR(32)		 NOT NULL,
		partnr                        		INT		 NOT NULL,
		data                          		BLOB		 NULL 
);

CREATE TABLE historylog(
		id                            		INT		 NOT NULL,
		logtime                       		TIMESTAMP		 NULL ,
		entrytype                     		TINYINT		 NULL ,
		target_id                     		VARCHAR(32)		 NULL ,
		target                        		VARCHAR(255)		 NULL ,
		loguser                       		VARCHAR(255)		 NULL ,
		operation                     		VARCHAR(255)		 NULL 
);

CREATE TABLE lang_editors(
		lang_id                       		VARCHAR(32)		 NOT NULL,
		idx                           		INT		 NOT NULL,
		name                          		VARCHAR(255)		 NULL 
);

CREATE TABLE structentry_pageeditors(
		struct_id                     		VARCHAR(32)		 NOT NULL,
		idx                           		INT		 NOT NULL,
		name                          		VARCHAR(255)		 NULL 
);

CREATE TABLE structentry_childeditors(
		struct_id                     		VARCHAR(32)		 NOT NULL,
		idx                           		INT		 NOT NULL,
		name                          		VARCHAR(255)		 NULL 
);

CREATE TABLE tml(
		id                            		VARCHAR(32)		 NOT NULL,
		name                          		VARCHAR(200)		 NOT NULL,
		mediakey                      		VARCHAR(10)		 NOT NULL,
		created                       		DATETIME		 NOT NULL,
		lastmodified                  		DATETIME		 NOT NULL,
		code                          		LONGTEXT		 NULL ,
		directaccess                  		BIT		 NOT NULL,
		cacheable                     		BIT		 NOT NULL
);

CREATE TABLE userprofile(
		id                            		VARCHAR(32)		 NOT NULL,
		name                          		VARCHAR(200)		 NOT NULL,
		created                       		DATETIME		 NOT NULL,
		lastmodified                  		DATETIME		 NOT NULL,
		profiletype                   		TINYINT		 NULL ,
		lastaccess                    		DATETIME		 NULL ,
		hits                          		INT		 NULL ,
		sessions                      		INT		 NULL ,
		client                        		VARCHAR(255)		 NULL ,
		login                         		VARCHAR(255)		 NULL ,
		profilepassword               		VARCHAR(255)		 NULL 
);

CREATE TABLE userprofile_items(
		id                            		VARCHAR(32)		 NOT NULL,
		profile_id                    		VARCHAR(32)		 NOT NULL,
		portlet_id                    		VARCHAR(32)		 NULL ,
		name                          		VARCHAR(200)		 NOT NULL,
		datatype                      		TINYINT		 NULL ,
		textvalue                     		LONGTEXT		 NULL ,
		numbervalue                   		DOUBLE		 NULL ,
		datevalue                     		DATETIME		 NULL 
);

CREATE TABLE userprofile_langs(
		profile_id                    		VARCHAR(32)		 NOT NULL,
		idx                           		INT		 NOT NULL,
		lang                          		VARCHAR(255)		 NULL 
);

CREATE TABLE userprofile_portlets(
		id                            		VARCHAR(32)		 NOT NULL,
		portletkey                    		VARCHAR(32)		 NULL ,
		profile_id                    		VARCHAR(32)		 NOT NULL,
		appdb                         		VARCHAR(255)		 NULL ,
		designdb                      		VARCHAR(255)		 NULL ,
		design                        		VARCHAR(255)		 NULL ,
		parentportlet_id              		VARCHAR(32)		 NULL ,
		name                          		VARCHAR(255)		 NULL 
);

CREATE TABLE content_coauthors(
		content_id                    		VARCHAR(32)		 NOT NULL,
		idx                           		INT		 NOT NULL,
		name                          		VARCHAR(255)		 NULL 
);

CREATE TABLE extensiondata(
		id                            		VARCHAR(32)		 NOT NULL,
		entity_id                     		VARCHAR(32)		 NULL ,
		name                          		VARCHAR(200)		 NOT NULL,
		datatype                      		TINYINT		 NULL ,
		textvalue                     		LONGTEXT		 NULL ,
		numbervalue                   		DOUBLE		 NULL ,
		datevalue                     		DATETIME		 NULL ,
		binaryvalue_sha512            		VARCHAR(128)		 NULL 
);

CREATE TABLE cs_sequences(
		name                          		VARCHAR(255)		 NOT NULL,
		value                         		INT		 NULL 
);

CREATE TABLE structentry_published(
		struct_id                     		VARCHAR(32)		 NOT NULL,
		lang                          		VARCHAR(10)		 NOT NULL,
		published                     		DATETIME		 NULL 
);

CREATE TABLE structentry_readers(
		struct_id                     		VARCHAR(32)		 NOT NULL,
		idx                           		INT		 NOT NULL,
		name                          		VARCHAR(255)		 NULL 
);

CREATE TABLE webarea_readers(
		area_id                       		VARCHAR(32)		 NOT NULL,
		idx                           		INT		 NOT NULL,
		name                          		VARCHAR(255)		 NULL 
);

CREATE TABLE content_filecontents(
		id                            		VARCHAR(32)		 NOT NULL,
		ordinalnr                     		INT		 NOT NULL,
		checksum_sha512               		VARCHAR(128)		 NULL ,
		filesize                      		BIGINT		 NOT NULL
);

CREATE TABLE content_filecontents_data(
		contents_id                   		VARCHAR(32)		 NOT NULL,
		partnr                        		INT		 NOT NULL,
		data                          		BLOB		 NOT NULL
);

CREATE TABLE content_filederivates(
		id                            		VARCHAR(32)		 NOT NULL,
		creator                       		VARCHAR(32)		 NOT NULL,
		name                          		VARCHAR(200)		 NOT NULL,
		created                       		DATETIME		 NULL ,
		lastmodified                  		DATETIME		 NULL ,
		parent_sha512                 		VARCHAR(128)		 NOT NULL,
		derivate_sha512               		VARCHAR(128)		 NOT NULL,
		filesize                      		BIGINT		 NOT NULL,
		parent_id                     		VARCHAR(32)		 NULL 
);


ALTER TABLE acl ADD CONSTRAINT acl_PK PRIMARY KEY (id);
ALTER TABLE acl ADD CONSTRAINT T3_NAME UNIQUE (name);

ALTER TABLE webarea ADD CONSTRAINT webarea_PK PRIMARY KEY (id);
ALTER TABLE webarea ADD CONSTRAINT T2_NAME UNIQUE (name);

ALTER TABLE webarea_editors ADD CONSTRAINT webarea_editors_PK PRIMARY KEY (webarea_id, idx);
ALTER TABLE webarea_editors ADD CONSTRAINT webarea_editors_FK0 FOREIGN KEY (webarea_id) REFERENCES webarea (id);
CREATE INDEX T1_WEBAREA_ID ON webarea_editors (webarea_id);

ALTER TABLE contenttype ADD CONSTRAINT contenttype_PK PRIMARY KEY (id);
ALTER TABLE contenttype ADD CONSTRAINT T5_NAME UNIQUE (name);

ALTER TABLE structentry ADD CONSTRAINT structentry_PK PRIMARY KEY (id);
ALTER TABLE structentry ADD CONSTRAINT structentry_FK0 FOREIGN KEY (contenttype_id) REFERENCES contenttype (id);
ALTER TABLE structentry ADD CONSTRAINT T23_UNIQUENAME UNIQUE (uniquename);
ALTER TABLE structentry ADD CONSTRAINT T23_STRUCTKEY UNIQUE (structkey);
CREATE INDEX T23_WEBAREA_ID ON structentry (webarea_id);
CREATE INDEX T23_CONTENTTYPE_ID ON structentry (contenttype_id);
CREATE INDEX T23_PARENTENTRY ON structentry (parententry_id);

ALTER TABLE lang ADD CONSTRAINT lang_PK PRIMARY KEY (id);
CREATE INDEX T8_NAME ON lang (name);

ALTER TABLE content ADD CONSTRAINT content_PK PRIMARY KEY (id);
ALTER TABLE content ADD CONSTRAINT content_FK0 FOREIGN KEY (struct_id) REFERENCES structentry (id);
ALTER TABLE content ADD CONSTRAINT content_FK1 FOREIGN KEY (lang_id) REFERENCES lang (id);
CREATE INDEX T20_UNIQUENAME ON content (uniquename);
CREATE INDEX T20_CONTENTCLASS ON content (contentclass);
CREATE INDEX T20_STRUCTENTRY ON content (struct_id);
CREATE INDEX T20_LANGUAGE ON content (lang_id);

ALTER TABLE content_files_meta ADD CONSTRAINT content_files_meta_PK PRIMARY KEY (id);
ALTER TABLE content_files_meta ADD CONSTRAINT content_files_meta_FK0 FOREIGN KEY (content_id) REFERENCES content (id);
CREATE INDEX T18_CONTENT_ID ON content_files_meta (content_id);
ALTER TABLE content_files_meta ADD CONSTRAINT T18_NAME UNIQUE (content_id, name);
CREATE INDEX T18_CHECKSUM_SHA512 ON content_files_meta (checksum_sha512);

ALTER TABLE content_files_data ADD CONSTRAINT content_files_data_PK PRIMARY KEY (file_id, partnr);
ALTER TABLE content_files_data ADD CONSTRAINT content_files_data_FK0 FOREIGN KEY (file_id) REFERENCES content_files_meta (id);
CREATE INDEX T32_FILE_ID ON content_files_data (file_id);

ALTER TABLE content_ishiddenfrom ADD CONSTRAINT content_ishiddenfrom_PK PRIMARY KEY (content_id, idx);
ALTER TABLE content_ishiddenfrom ADD CONSTRAINT content_ishiddenfrom_FK0 FOREIGN KEY (content_id) REFERENCES content (id);
CREATE INDEX T12_CONTENT_ID ON content_ishiddenfrom (content_id);

ALTER TABLE content_items ADD CONSTRAINT content_items_PK PRIMARY KEY (id);
ALTER TABLE content_items ADD CONSTRAINT content_items_FK0 FOREIGN KEY (content_id) REFERENCES content (id);
CREATE INDEX T13_CONTENT_ID ON content_items (content_id);
ALTER TABLE content_items ADD CONSTRAINT T13_NAME UNIQUE (content_id, name);
CREATE INDEX T13_DATE ON content_items (datevalue);
CREATE INDEX T13_NUMBER ON content_items (numbervalue);

ALTER TABLE content_keywords ADD CONSTRAINT content_keywords_PK PRIMARY KEY (content_id, idx);
ALTER TABLE content_keywords ADD CONSTRAINT content_keywords_FK0 FOREIGN KEY (content_id) REFERENCES content (id);
CREATE INDEX T14_CONTENT_ID ON content_keywords (content_id);

ALTER TABLE content_readers ADD CONSTRAINT content_readers_PK PRIMARY KEY (content_id, idx);
ALTER TABLE content_readers ADD CONSTRAINT content_readers_FK0 FOREIGN KEY (content_id) REFERENCES content (id);
CREATE INDEX T15_CONTENT_ID ON content_readers (content_id);

ALTER TABLE content_relations ADD CONSTRAINT content_relations_PK PRIMARY KEY (id);
ALTER TABLE content_relations ADD CONSTRAINT content_relations_FK0 FOREIGN KEY (content_id) REFERENCES content (id);
CREATE INDEX T16_TARGETCONTENT ON content_relations (targetcontent);
CREATE INDEX T16_CONTENT_ID ON content_relations (content_id);
ALTER TABLE content_relations ADD CONSTRAINT T16_NAME UNIQUE (content_id, name);
CREATE INDEX T16_RELGROUP ON content_relations (content_id, relgroup);

ALTER TABLE content_wfhistory ADD CONSTRAINT content_wfhistory_PK PRIMARY KEY (content_id, idx);
ALTER TABLE content_wfhistory ADD CONSTRAINT content_wfhistory_FK0 FOREIGN KEY (content_id) REFERENCES content (id);
CREATE INDEX T17_CONTENT_ID ON content_wfhistory (content_id);

ALTER TABLE contenttype_positions ADD CONSTRAINT contenttype_positions_PK PRIMARY KEY (contenttype_id, idx);
ALTER TABLE contenttype_positions ADD CONSTRAINT contenttype_positions_FK0 FOREIGN KEY (contenttype_id) REFERENCES contenttype (id);
CREATE INDEX T4_CONTENTTYPE_ID ON contenttype_positions (contenttype_id);

ALTER TABLE contenttype_editors ADD CONSTRAINT contenttype_editors_PK PRIMARY KEY (contenttype_id, idx);
ALTER TABLE contenttype_editors ADD CONSTRAINT contenttype_editors_FK0 FOREIGN KEY (contenttype_id) REFERENCES contenttype (id);
CREATE INDEX T6_CONTENTTYPE_ID ON contenttype_editors (contenttype_id);

ALTER TABLE scripts ADD CONSTRAINT scripts_PK PRIMARY KEY (id);
ALTER TABLE scripts ADD CONSTRAINT T9_NAME UNIQUE (name, codetype);

ALTER TABLE filecontainer ADD CONSTRAINT filecontainer_PK PRIMARY KEY (id);
ALTER TABLE filecontainer ADD CONSTRAINT T31_NAME UNIQUE (name);

ALTER TABLE filecontainer_files_meta ADD CONSTRAINT filecontainer_files_meta_PK PRIMARY KEY (id);
ALTER TABLE filecontainer_files_meta ADD CONSTRAINT filecontainer_files_meta_FK0 FOREIGN KEY (fc_id) REFERENCES filecontainer (id);
CREATE INDEX T30_FC_ID ON filecontainer_files_meta (fc_id);
ALTER TABLE filecontainer_files_meta ADD CONSTRAINT T30_NAME UNIQUE (fc_id, name);

ALTER TABLE filecontainer_files_data ADD CONSTRAINT filecontainer_files_data_PK PRIMARY KEY (file_id, partnr);
ALTER TABLE filecontainer_files_data ADD CONSTRAINT filecontainer_files_data_FK0 FOREIGN KEY (file_id) REFERENCES filecontainer_files_meta (id);
CREATE INDEX T29_FILE_ID ON filecontainer_files_data (file_id);

ALTER TABLE historylog ADD CONSTRAINT historylog_PK PRIMARY KEY (id);
CREATE INDEX T10_TARGETID ON historylog (target_id);

ALTER TABLE lang_editors ADD CONSTRAINT lang_editors_PK PRIMARY KEY (lang_id, idx);
ALTER TABLE lang_editors ADD CONSTRAINT lang_editors_FK0 FOREIGN KEY (lang_id) REFERENCES lang (id);
CREATE INDEX T7_LANG_ID ON lang_editors (lang_id);

ALTER TABLE structentry_pageeditors ADD CONSTRAINT structentry_pageeditors_PK PRIMARY KEY (struct_id, idx);
ALTER TABLE structentry_pageeditors ADD CONSTRAINT structentry_pageeditors_FK0 FOREIGN KEY (struct_id) REFERENCES structentry (id);
CREATE INDEX T21_STRUCT_ID ON structentry_pageeditors (struct_id);

ALTER TABLE structentry_childeditors ADD CONSTRAINT structentry_childeditors_PK PRIMARY KEY (struct_id, idx);
ALTER TABLE structentry_childeditors ADD CONSTRAINT structentry_childeditors_FK0 FOREIGN KEY (struct_id) REFERENCES structentry (id);
CREATE INDEX T22_STRUCT_ID ON structentry_childeditors (struct_id);

ALTER TABLE tml ADD CONSTRAINT tml_PK PRIMARY KEY (id);
ALTER TABLE tml ADD CONSTRAINT INDEX_T11_NAME_MEDIAKEY UNIQUE (name, mediakey);

ALTER TABLE userprofile ADD CONSTRAINT userprofile_PK PRIMARY KEY (id);
ALTER TABLE userprofile ADD CONSTRAINT T27_NAME UNIQUE (name);

ALTER TABLE userprofile_items ADD CONSTRAINT userprofile_items_PK PRIMARY KEY (id);
ALTER TABLE userprofile_items ADD CONSTRAINT userprofile_items_FK0 FOREIGN KEY (profile_id) REFERENCES userprofile (id);
CREATE INDEX T24_PROFILE_ID ON userprofile_items (profile_id);
CREATE INDEX T24_PORTLET_ID ON userprofile_items (portlet_id);
ALTER TABLE userprofile_items ADD CONSTRAINT T24_NAME UNIQUE (profile_id, portlet_id, name);

ALTER TABLE userprofile_langs ADD CONSTRAINT userprofile_langs_PK PRIMARY KEY (profile_id, idx);
ALTER TABLE userprofile_langs ADD CONSTRAINT userprofile_langs_FK0 FOREIGN KEY (profile_id) REFERENCES userprofile (id);
CREATE INDEX T25_PROFILE_ID ON userprofile_langs (profile_id);

ALTER TABLE userprofile_portlets ADD CONSTRAINT userprofile_portlets_PK PRIMARY KEY (id);
ALTER TABLE userprofile_portlets ADD CONSTRAINT userprofile_portlets_FK0 FOREIGN KEY (profile_id) REFERENCES userprofile (id);
CREATE INDEX T26_PARENTPORTLET_ID ON userprofile_portlets (parentportlet_id);
CREATE INDEX T26_APPDB ON userprofile_portlets (appdb);
ALTER TABLE userprofile_portlets ADD CONSTRAINT T26_KEY UNIQUE (profile_id, appdb, portletkey);

ALTER TABLE content_coauthors ADD CONSTRAINT content_coauthors_PK PRIMARY KEY (content_id, idx);
ALTER TABLE content_coauthors ADD CONSTRAINT content_coauthors_FK0 FOREIGN KEY (content_id) REFERENCES content (id);
CREATE INDEX T19_CONTENT_ID ON content_coauthors (content_id);

ALTER TABLE extensiondata ADD CONSTRAINT extensiondata_PK PRIMARY KEY (id);
CREATE INDEX T28_ENTITY_ID ON extensiondata (entity_id);
ALTER TABLE extensiondata ADD CONSTRAINT T28_NAME UNIQUE (entity_id, name);
CREATE INDEX T28_BINARYVALUE_SHA512 ON extensiondata (binaryvalue_sha512);

ALTER TABLE cs_sequences ADD CONSTRAINT cs_sequences_PK PRIMARY KEY (name);

ALTER TABLE structentry_published ADD CONSTRAINT structentry_published_PK PRIMARY KEY (struct_id, lang);
ALTER TABLE structentry_published ADD CONSTRAINT structentry_published_FK0 FOREIGN KEY (struct_id) REFERENCES structentry (id);
CREATE INDEX T33_STRUCT_ID ON structentry_published (struct_id);

ALTER TABLE structentry_readers ADD CONSTRAINT structentry_readers_PK PRIMARY KEY (struct_id, idx);
ALTER TABLE structentry_readers ADD CONSTRAINT structentry_readers_FK0 FOREIGN KEY (struct_id) REFERENCES structentry (id);
CREATE INDEX T34_STRUCT_ID ON structentry_readers (struct_id);

ALTER TABLE webarea_readers ADD CONSTRAINT webarea_readers_PK PRIMARY KEY (area_id, idx);
ALTER TABLE webarea_readers ADD CONSTRAINT webarea_readers_FK0 FOREIGN KEY (area_id) REFERENCES webarea (id);
CREATE INDEX T35_AREA_ID ON webarea_readers (area_id);

ALTER TABLE content_filecontents ADD CONSTRAINT content_filecontents_PK PRIMARY KEY (id);
CREATE INDEX T36_CHECKSUM_SHA512 ON content_filecontents (checksum_sha512);
ALTER TABLE content_filecontents ADD CONSTRAINT T36_ORDINAL_NR UNIQUE (ordinalnr);

ALTER TABLE content_filecontents_data ADD CONSTRAINT content_filecontents_data_PK PRIMARY KEY (contents_id, partnr);
ALTER TABLE content_filecontents_data ADD CONSTRAINT content_filecontents_data_FK0 FOREIGN KEY (contents_id) REFERENCES content_filecontents (id);
CREATE INDEX T37_CONTENTS_ID ON content_filecontents_data (contents_id);

ALTER TABLE content_filederivates ADD CONSTRAINT content_filederivates_PK PRIMARY KEY (id);
CREATE INDEX T38_PARENT_SHA512 ON content_filederivates (parent_sha512);
CREATE INDEX T38_DERIVATE_SHA512 ON content_filederivates (derivate_sha512);
CREATE INDEX T38_CREATOR ON content_filederivates (creator);
CREATE INDEX T38_PARENT_ID ON content_filederivates (parent_id);


INSERT INTO extensiondata (id, name, datatype, numbervalue) VALUES ('staticid:ddlpatchlevel', 'ddlpatchlevel', 2, 5);
