SET SQL DIALECT 3;
SET AUTODDL ON;

--CREATE DATABASE 'localhost/3030:employee' PAGE_SIZE 16384
CREATE DATABASE 'localhost/3030:/opt/firebird/examples/empbuild/employee_10.fdb' PAGE_SIZE 16384


DEFAULT CHARACTER SET NONE; 

/* Domain definitions */

CREATE DOMAIN ADDRESSLINE AS VARCHAR(30);
CREATE DOMAIN BUDGET AS DECIMAL(12,2)
	 DEFAULT 50000;
CREATE DOMAIN COUNTRYNAME AS VARCHAR(15);
CREATE DOMAIN CUSTNO AS INTEGER;
CREATE DOMAIN DEPTNO AS CHAR(3);
CREATE DOMAIN EMPNO AS SMALLINT;
CREATE DOMAIN FIRSTNAME AS VARCHAR(15);
CREATE DOMAIN FTS$D_CHANGE_TYPE AS CHAR(1);
COMMENT ON DOMAIN FTS$D_CHANGE_TYPE IS 'Type of record change. I - INSERT, U - UPDATE, D - DELETE.';
CREATE DOMAIN FTS$D_INDEX_STATUS AS CHAR(1);
COMMENT ON DOMAIN FTS$D_INDEX_STATUS IS 'Full-text index status. I - Inactive, N - New index (need rebuild), C - complete and active, U - updated metadata (need rebuild).';
CREATE DOMAIN JOBCODE AS VARCHAR(5);
CREATE DOMAIN JOBGRADE AS SMALLINT;
CREATE DOMAIN LASTNAME AS VARCHAR(20);
CREATE DOMAIN PHONENUMBER AS VARCHAR(20);
CREATE DOMAIN PONUMBER AS CHAR(8);
CREATE DOMAIN PRODTYPE AS VARCHAR(12)
	 DEFAULT 'software' NOT NULL;
CREATE DOMAIN PROJNO AS CHAR(5);
CREATE DOMAIN SALARY AS NUMERIC(10,2)
	 DEFAULT 0;
CREATE DOMAIN TEST_CHAR AS CHAR(1);
CREATE DOMAIN TY$POINTER AS CHAR(8);

/* Table: COUNTRIES, Owner: SYSDBA */

CREATE TABLE COUNTRIES
(
	COUNTRY	VARCHAR(64) NOT NULL,
	NONEU	SMALLINT,
	ISO2	CHAR(2),
	ISO3	CHAR(3),
PRIMARY KEY (COUNTRY)
);
COMMENT ON COLUMN COUNTRIES.COUNTRY IS '(IsFieldDomainSystemGenerated(trim(FieldByname(''field_source'').AsString))) then _FieldType:= GetFBTypeName(FieldByName(''field_type_int'').AsInteger,  // in turbocommon';

/* Table: COUNTRIES_3, Owner: SYSDBA */

CREATE TABLE COUNTRIES_3
(
	COUNTRY	VARCHAR(83),
	NONEU	SMALLINT,
	ISO2	VARCHAR(83),
	ISO3	VARCHAR(83)
);

/* Table: COUNTRIES_EXT, Owner: SYSDBA */

CREATE TABLE COUNTRIES_EXT
EXTERNAL FILE '/home/maurog/common/countries_ext.bin' 
(
	COUNTRY	CHAR(64),
	NONEU	SMALLINT,
	ISO2	CHAR(2),
	ISO3	CHAR(3)
);

/* Table: COUNTRY, Owner: SYSDBA */

CREATE TABLE COUNTRY
(
	COUNTRY	COUNTRYNAME NOT NULL,
	CURRENCY	VARCHAR(10) NOT NULL,
PRIMARY KEY (COUNTRY)
);

/* Table: COUNTRY_2, Owner: SYSDBA */

CREATE TABLE COUNTRY_2
(
	COUNTRY_2	COUNTRYNAME NOT NULL,
	CURRENCY_2	VARCHAR(10) NOT NULL,
PRIMARY KEY (COUNTRY_2)
);

/* Table: COUNTRY_EXT, Owner: SYSDBA */

CREATE TABLE COUNTRY_EXT
EXTERNAL FILE '/home/maurog/common/employee_ext.bin' 
(
	COUNTRY	CHAR(15),
	CURRENCY	CHAR(10)
);

/* Table: CUSTOMER, Owner: SYSDBA */

CREATE TABLE CUSTOMER
(
	CUST_NO	CUSTNO NOT NULL,
	CUSTOMER	VARCHAR(25) NOT NULL,
	CONTACT_FIRST	FIRSTNAME,
	CONTACT_LAST	LASTNAME,
	PHONE_NO	PHONENUMBER,
	ADDRESS_LINE1	ADDRESSLINE,
	ADDRESS_LINE2	ADDRESSLINE,
	CITY	VARCHAR(25),
	STATE_PROVINCE	VARCHAR(15),
	COUNTRY	COUNTRYNAME,
	POSTAL_CODE	VARCHAR(12),
	ON_HOLD	CHAR(1) DEFAULT NULL,
PRIMARY KEY (CUST_NO)
);

/* Table: CaseSensitiveTable_FB3, Owner: SYSDBA */

CREATE TABLE "CaseSensitiveTable_FB3"
(
	"Id"	INTEGER NOT NULL,
	"smallInt_COLUMN"	SMALLINT,
	"integer_COLUMN"	INTEGER,
	"bigInt_COLUMN"	BIGINT,
	"decimal_Column"	DECIMAL(18,10),
	"numeric_Column"	NUMERIC(18,10),
	"float_Column"	FLOAT,
	"double_Precision_COLUMN"	DOUBLE PRECISION,
	"char_Column"	CHAR(20),
	"varchar_COLUMN"	VARCHAR(100),
	"binary_COLUMN"	CHAR(50) CHARACTER SET OCTETS,
	"varbinary_COLUMN"	VARCHAR(100) CHARACTER SET OCTETS,
	"date_COLUMN"	DATE,
	"time_COLUMN"	TIME,
	"timestamp_COLUMN"	TIMESTAMP,
	"blob_Text_COLUMN"	BLOB SUB_TYPE TEXT SEGMENT SIZE 80,
	"blob_Binary_COLUMN"	BLOB SUB_TYPE 0 SEGMENT SIZE 80,
	"boolean_COLUMN"	BOOLEAN,
PRIMARY KEY ("Id")
);
COMMENT ON COLUMN "CaseSensitiveTable_FB3"."boolean_COLUMN" IS 'Test_Boolean_Field_1 
Test_Boolean_Field_2  
Test_Boolean_Field_3 
Test_Boolean_Field_4';

/* Table: DEPARTMENT, Owner: SYSDBA */

CREATE TABLE DEPARTMENT
(
	DEPT_NO	DEPTNO NOT NULL,
	DEPARTMENT	VARCHAR(25) NOT NULL,
	HEAD_DEPT	DEPTNO,
	MNGR_NO	EMPNO,
	BUDGET	BUDGET,
	LOCATION	VARCHAR(15),
	PHONE_NO	PHONENUMBER DEFAULT '555-1234',
UNIQUE (DEPARTMENT),
PRIMARY KEY (DEPT_NO)
);

/* Table: DEPARTMENT_EXT, Owner: SYSDBA */

CREATE TABLE DEPARTMENT_EXT
EXTERNAL FILE '/home/maurog/common/DEPARTMENT_EXT.bin' 
(
	DEPT_NO	CHAR(3),
	DEPARTMENT	CHAR(25),
	HEAD_DEPT	CHAR(3),
	MNGR_NO	SMALLINT,
	BUDGET	DOUBLE PRECISION,
	LOCATION	CHAR(15),
	PHONE_NO	CHAR(20)
);

/* Table: EMPLOYEE, Owner: SYSDBA */

CREATE TABLE EMPLOYEE
(
	EMP_NO	EMPNO NOT NULL,
	FIRST_NAME	FIRSTNAME NOT NULL,
	LAST_NAME	LASTNAME NOT NULL,
	PHONE_EXT	VARCHAR(4),
	HIRE_DATE	TIMESTAMP DEFAULT 'NOW' NOT NULL,
	DEPT_NO	DEPTNO NOT NULL,
	JOB_CODE	JOBCODE NOT NULL,
	JOB_GRADE	JOBGRADE NOT NULL,
	JOB_COUNTRY	COUNTRYNAME NOT NULL,
	SALARY	SALARY NOT NULL,
	FULL_NAME COMPUTED BY (last_name || ', ' || first_name),
PRIMARY KEY (EMP_NO)
);
COMMENT ON COLUMN EMPLOYEE.EMP_NO IS 'No Description';

/* Table: EMPLOYEE_PROJECT, Owner: SYSDBA */

CREATE TABLE EMPLOYEE_PROJECT
(
	EMP_NO	EMPNO NOT NULL,
	PROJ_ID	PROJNO NOT NULL,
PRIMARY KEY (EMP_NO, PROJ_ID)
);

/* Table: EXT_EMPLOYEE, Owner: SYSDBA */

CREATE TABLE EXT_EMPLOYEE
EXTERNAL FILE '/opt/firebird/examples/empbuild/EXT_EMPLOYEE.dat' 
(
	EMP_NO	SMALLINT NOT NULL,
	FIRST_NAME	VARCHAR(15) NOT NULL,
	LAST_NAME	VARCHAR(20) NOT NULL,
	PHONE_EXT	VARCHAR(4),
	HIRE_DATE	TIMESTAMP DEFAULT 'NOW' NOT NULL,
	DEPT_NO	CHAR(3) NOT NULL,
	JOB_CODE	VARCHAR(5) NOT NULL,
	JOB_GRADE	SMALLINT NOT NULL,
	JOB_COUNTRY	VARCHAR(15) NOT NULL,
	SALARY	NUMERIC(10,2) NOT NULL
);

/* Table: EXT_EMPLOYEE_2, Owner: SYSDBA */

CREATE TABLE EXT_EMPLOYEE_2
EXTERNAL FILE '/home/maurog/common//EXT_EMPLOYEE_2.dat' 
(
	EMP_NO	SMALLINT NOT NULL,
	FIRST_NAME	VARCHAR(15) NOT NULL,
	LAST_NAME	VARCHAR(20) NOT NULL,
	PHONE_EXT	VARCHAR(4),
	HIRE_DATE	TIMESTAMP DEFAULT 'NOW' NOT NULL,
	DEPT_NO	CHAR(3) NOT NULL,
	JOB_CODE	VARCHAR(5) NOT NULL,
	JOB_GRADE	SMALLINT NOT NULL,
	JOB_COUNTRY	VARCHAR(15) NOT NULL,
	SALARY	NUMERIC(10,2) NOT NULL
);

/* Table: EXT_EMPLOYEE_3, Owner: SYSDBA */

CREATE TABLE EXT_EMPLOYEE_3
EXTERNAL FILE '/opt/firebird/examples/empbuild/EXT_EMPLOYEE_3.dat' 
(
	EMP_NO	SMALLINT NOT NULL,
	FIRST_NAME	VARCHAR(15) NOT NULL,
	LAST_NAME	VARCHAR(20) NOT NULL,
	PHONE_EXT	VARCHAR(4),
	HIRE_DATE	TIMESTAMP DEFAULT 'NOW' NOT NULL,
	DEPT_NO	CHAR(3) NOT NULL,
	JOB_CODE	VARCHAR(5) NOT NULL,
	JOB_GRADE	SMALLINT NOT NULL,
	JOB_COUNTRY	VARCHAR(15) NOT NULL,
	SALARY	NUMERIC(10,2) NOT NULL
);

/* Table: EXT_EMPLOYEE_4, Owner: SYSDBA */

CREATE TABLE EXT_EMPLOYEE_4
EXTERNAL FILE '/opt/firebird/examples/empbuild/EXT_EMPLOYEE_4.bin' 
(
	EMP_NO	SMALLINT NOT NULL,
	FIRST_NAME	VARCHAR(15) NOT NULL,
	LAST_NAME	VARCHAR(20) NOT NULL,
	PHONE_EXT	VARCHAR(4),
	HIRE_DATE	TIMESTAMP DEFAULT 'NOW' NOT NULL,
	DEPT_NO	CHAR(3) NOT NULL,
	JOB_CODE	VARCHAR(5) NOT NULL,
	JOB_GRADE	SMALLINT NOT NULL,
	JOB_COUNTRY	VARCHAR(15) NOT NULL,
	SALARY	NUMERIC(10,2) NOT NULL
);

/* Table: EXT_EMPLOYEE_5, Owner: SYSDBA */

CREATE TABLE EXT_EMPLOYEE_5
EXTERNAL FILE '/opt/firebird/examples/empbuild/EXT_EMPLOYEE_5.bin' 
(
	EMP_NO	SMALLINT NOT NULL,
	FIRST_NAME	VARCHAR(15) NOT NULL,
	LAST_NAME	VARCHAR(20) NOT NULL,
	PHONE_EXT	VARCHAR(4),
	HIRE_DATE	TIMESTAMP DEFAULT 'NOW' NOT NULL,
	DEPT_NO	CHAR(3) NOT NULL,
	JOB_CODE	VARCHAR(5) NOT NULL,
	JOB_GRADE	SMALLINT NOT NULL,
	JOB_COUNTRY	VARCHAR(15) NOT NULL,
	SALARY	NUMERIC(10,2) NOT NULL
);

/* Table: FTS$ANALYZERS, Owner: SYSDBA */

CREATE TABLE FTS$ANALYZERS
(
	FTS$ANALYZER_NAME	VARCHAR(63) CHARACTER SET UTF8 NOT NULL,
	FTS$BASE_ANALYZER	VARCHAR(63) CHARACTER SET UTF8 NOT NULL,
	FTS$DESCRIPTION	BLOB SUB_TYPE TEXT SEGMENT SIZE 80 CHARACTER SET UTF8,
CONSTRAINT PK_FTS$ANALYZER PRIMARY KEY (FTS$ANALYZER_NAME)
);
COMMENT ON TABLE FTS$ANALYZERS IS 'Custom full-text search analyzers';
COMMENT ON COLUMN FTS$ANALYZERS.FTS$ANALYZER_NAME IS 'Description of analyzer';

/* Table: FTS$INDEX_SEGMENTS, Owner: SYSDBA */

CREATE TABLE FTS$INDEX_SEGMENTS
(
	FTS$INDEX_NAME	VARCHAR(63) CHARACTER SET UTF8 NOT NULL,
	FTS$FIELD_NAME	VARCHAR(63) CHARACTER SET UTF8 NOT NULL,
	FTS$BOOST	DOUBLE PRECISION,
	FTS$KEY	BOOLEAN DEFAULT FALSE NOT NULL,
CONSTRAINT UK_FTS$INDEX_SEGMENTS UNIQUE (FTS$INDEX_NAME, FTS$FIELD_NAME)
);
COMMENT ON TABLE FTS$INDEX_SEGMENTS IS 'Segments of the full-text index.';
COMMENT ON COLUMN FTS$INDEX_SEGMENTS.FTS$INDEX_NAME IS 'Full-text index name.';
COMMENT ON COLUMN FTS$INDEX_SEGMENTS.FTS$FIELD_NAME IS 'Name of the indexed field.';
COMMENT ON COLUMN FTS$INDEX_SEGMENTS.FTS$BOOST IS 'Boost significance';
COMMENT ON COLUMN FTS$INDEX_SEGMENTS.FTS$KEY IS 'Is the field a key';

/* Table: FTS$INDICES, Owner: SYSDBA */

CREATE TABLE FTS$INDICES
(
	FTS$INDEX_NAME	VARCHAR(63) CHARACTER SET UTF8 NOT NULL,
	FTS$RELATION_NAME	VARCHAR(63) CHARACTER SET UTF8 NOT NULL,
	FTS$ANALYZER	VARCHAR(63) CHARACTER SET UTF8 DEFAULT 'STANDARD' NOT NULL,
	FTS$DESCRIPTION	BLOB SUB_TYPE TEXT SEGMENT SIZE 80 CHARACTER SET UTF8,
	FTS$INDEX_STATUS	FTS$D_INDEX_STATUS DEFAULT 'N' NOT NULL,
CONSTRAINT PK_FTS$INDEX_NAME PRIMARY KEY (FTS$INDEX_NAME)
);
COMMENT ON TABLE FTS$INDICES IS 'Indexes for full-text search.';
COMMENT ON COLUMN FTS$INDICES.FTS$INDEX_NAME IS 'Full-text index name.';
COMMENT ON COLUMN FTS$INDICES.FTS$RELATION_NAME IS 'Name of the indexed table.';
COMMENT ON COLUMN FTS$INDICES.FTS$ANALYZER IS 'The analyzer. If not specified, it uses STANDARD (StandardAnalyzer) by default.';
COMMENT ON COLUMN FTS$INDICES.FTS$DESCRIPTION IS 'Description of the full-text index.';
COMMENT ON COLUMN FTS$INDICES.FTS$INDEX_STATUS IS 'Full-text index status.';

/* Table: FTS$LOG, Owner: SYSDBA */

CREATE TABLE FTS$LOG
(
	FTS$LOG_ID	BIGINT GENERATED BY DEFAULT AS IDENTITY (START WITH 0) NOT NULL,
	FTS$RELATION_NAME	VARCHAR(63) CHARACTER SET UTF8 NOT NULL,
	FTS$DB_KEY	CHAR(8) CHARACTER SET OCTETS,
	FTS$REC_UUID	CHAR(16) CHARACTER SET OCTETS,
	FTS$REC_ID	BIGINT,
	FTS$CHANGE_TYPE	FTS$D_CHANGE_TYPE NOT NULL,
CONSTRAINT PK_FTS$LOG_ID PRIMARY KEY (FTS$LOG_ID)
);
COMMENT ON TABLE FTS$LOG IS 'Changelog for maintaining full-text indexes.';
COMMENT ON COLUMN FTS$LOG.FTS$LOG_ID IS 'Identifier.';
COMMENT ON COLUMN FTS$LOG.FTS$RELATION_NAME IS 'Name of the indexed table.';
COMMENT ON COLUMN FTS$LOG.FTS$DB_KEY IS 'Record ID by RDB$DB_KEY';
COMMENT ON COLUMN FTS$LOG.FTS$REC_UUID IS 'Record ID by UUID (GUID)';
COMMENT ON COLUMN FTS$LOG.FTS$REC_ID IS 'Record ID by Integer ID';
COMMENT ON COLUMN FTS$LOG.FTS$CHANGE_TYPE IS 'Type of record change.';

/* Table: FTS$STOP_WORDS, Owner: SYSDBA */

CREATE TABLE FTS$STOP_WORDS
(
	FTS$ANALYZER_NAME	VARCHAR(63) CHARACTER SET UTF8 NOT NULL,
	FTS$WORD	VARCHAR(63) CHARACTER SET UTF8 NOT NULL COLLATE UNICODE_CI,
CONSTRAINT UNQ_FTS$ANALYZER_STOPWORDS UNIQUE (FTS$ANALYZER_NAME, FTS$WORD)
);
COMMENT ON TABLE FTS$STOP_WORDS IS 'Stop words for a custom analyzer';
COMMENT ON COLUMN FTS$STOP_WORDS.FTS$ANALYZER_NAME IS 'Analyzer name';
COMMENT ON COLUMN FTS$STOP_WORDS.FTS$WORD IS 'Stop word';

/* Table: JOB, Owner: SYSDBA */

CREATE TABLE JOB
(
	JOB_CODE	JOBCODE NOT NULL,
	JOB_GRADE	JOBGRADE NOT NULL,
	JOB_COUNTRY	COUNTRYNAME NOT NULL,
	JOB_TITLE	VARCHAR(25) NOT NULL,
	MIN_SALARY	SALARY NOT NULL,
	MAX_SALARY	SALARY NOT NULL,
	JOB_REQUIREMENT	BLOB SUB_TYPE TEXT SEGMENT SIZE 400,
	LANGUAGE_REQ	VARCHAR(15)[1:5],
PRIMARY KEY (JOB_CODE, JOB_GRADE, JOB_COUNTRY)
);

/* Table: JOB_2, Owner: SYSDBA */

CREATE TABLE JOB_2
(
	JOB_CODE	JOBCODE NOT NULL,
	JOB_GRADE	JOBGRADE NOT NULL,
	JOB_COUNTRY	COUNTRYNAME NOT NULL,
	JOB_TITLE	VARCHAR(25) NOT NULL,
	MIN_SALARY	SALARY NOT NULL,
	MAX_SALARY	SALARY NOT NULL,
	JOB_REQUIREMENT	BLOB SUB_TYPE TEXT SEGMENT SIZE 400,
	LANGUAGE_REQ	VARCHAR(15)[1:5],
	TEST_BOOL	BOOLEAN
);

/* Table: JOB_2_EXT, Owner: SYSDBA */

CREATE TABLE JOB_2_EXT
EXTERNAL FILE '/home/maurog/common/JOB_2_EXT.bin' 
(
	JOB_CODE	CHAR(5),
	JOB_GRADE	SMALLINT,
	JOB_COUNTRY	CHAR(15),
	JOB_TITLE	CHAR(25),
	MIN_SALARY	DOUBLE PRECISION,
	MAX_SALARY	DOUBLE PRECISION,
	LANGUAGE_REQ	CHAR(15),
	TEST_BOOL	CHAR(1)
);

/* Table: JOB_EXT, Owner: SYSDBA */

CREATE TABLE JOB_EXT
EXTERNAL FILE '/home/maurog/common/JOB_EXT.bin' 
(
	JOB_CODE	CHAR(5),
	JOB_GRADE	SMALLINT,
	JOB_COUNTRY	CHAR(15),
	JOB_TITLE	CHAR(25),
	MIN_SALARY	DOUBLE PRECISION,
	MAX_SALARY	DOUBLE PRECISION,
	LANGUAGE_REQ	CHAR(15)
);

/* Table: JOB_EXT_COMPLETT, Owner: SYSDBA */

CREATE TABLE JOB_EXT_COMPLETT
EXTERNAL FILE '/media/maurog/BIG_DATA/dbs/firebird/JOB_EXT_COMPLETT.bin' 
(
	JOB_CODE	CHAR(5),
	JOB_GRADE	SMALLINT,
	JOB_COUNTRY	CHAR(15),
	JOB_TITLE	CHAR(25),
	MIN_SALARY	DOUBLE PRECISION,
	MAX_SALARY	DOUBLE PRECISION,
	LANGUAGE_REQ	CHAR(15)
);

/* Table: KUNDEN, Owner: SYSDBA */

CREATE TABLE KUNDEN
(
	ID	INTEGER NOT NULL,
	NAME	VARCHAR(50),
PRIMARY KEY (ID)
);

/* Table: PROJECT, Owner: SYSDBA */

CREATE TABLE PROJECT
(
	PROJ_ID	PROJNO NOT NULL,
	PROJ_NAME	VARCHAR(20) NOT NULL,
	PROJ_DESC	BLOB SUB_TYPE TEXT SEGMENT SIZE 800,
	TEAM_LEADER	EMPNO,
	PRODUCT	PRODTYPE,
UNIQUE (PROJ_NAME),
PRIMARY KEY (PROJ_ID)
);

/* Table: PROJ_DEPT_BUDGET, Owner: SYSDBA */

CREATE TABLE PROJ_DEPT_BUDGET
(
	FISCAL_YEAR	INTEGER NOT NULL,
	PROJ_ID	PROJNO NOT NULL,
	DEPT_NO	DEPTNO NOT NULL,
	QUART_HEAD_CNT	INTEGER[1:4],
	PROJECTED_BUDGET	BUDGET,
PRIMARY KEY (FISCAL_YEAR, PROJ_ID, DEPT_NO)
);

/* Table: SALARY_HISTORY, Owner: SYSDBA */

CREATE TABLE SALARY_HISTORY
(
	EMP_NO	EMPNO NOT NULL,
	CHANGE_DATE	TIMESTAMP DEFAULT 'NOW' NOT NULL,
	UPDATER_ID	VARCHAR(20) NOT NULL,
	OLD_SALARY	SALARY NOT NULL,
	PERCENT_CHANGE	DOUBLE PRECISION DEFAULT 0 NOT NULL,
	NEW_SALARY COMPUTED BY (old_salary + old_salary * percent_change / 100),
PRIMARY KEY (EMP_NO, CHANGE_DATE, UPDATER_ID)
);

/* Table: SALES, Owner: SYSDBA */

CREATE TABLE SALES
(
	PO_NUMBER	PONUMBER NOT NULL,
	CUST_NO	CUSTNO NOT NULL,
	SALES_REP	EMPNO,
	ORDER_STATUS	VARCHAR(7) DEFAULT 'new' NOT NULL,
	ORDER_DATE	TIMESTAMP DEFAULT 'NOW' NOT NULL,
	SHIP_DATE	TIMESTAMP,
	DATE_NEEDED	TIMESTAMP,
	PAID	CHAR(1) DEFAULT 'n',
	QTY_ORDERED	INTEGER DEFAULT 1 NOT NULL,
	TOTAL_VALUE	DECIMAL(9,2) NOT NULL,
	DISCOUNT	FLOAT DEFAULT 0 NOT NULL,
	ITEM_TYPE	PRODTYPE,
	AGED COMPUTED BY (ship_date - order_date),
PRIMARY KEY (PO_NUMBER)
);

/* Table: TEST_ALL_TYPES_COPY, Owner: SYSDBA */

CREATE TABLE TEST_ALL_TYPES_COPY
(
	ID	INTEGER NOT NULL,
	NAME	VARCHAR(100),
	DESCRIPTION	BLOB SUB_TYPE 0 SEGMENT SIZE 80,
	PRICE	NUMERIC(15,2),
	QUANTITY	SMALLINT,
	IS_ACTIVE	BOOLEAN,
	CREATED_DATE	DATE,
	CREATED_AT	TIMESTAMP,
	SALARY	FLOAT,
	RATING	DOUBLE PRECISION,
	CODE	CHAR(3),
	DATA_ARRAY	INTEGER[1:9],
	FULL_NAME COMPUTED BY ((NAME || ' - ' || CODE))
);

/*  Index definitions for all user tables */

CREATE INDEX CUSTNAMEX ON CUSTOMER(CUSTOMER);
CREATE INDEX CUSTREGION ON CUSTOMER(COUNTRY, CITY);
CREATE DESCENDING INDEX BUDGETX ON DEPARTMENT(BUDGET);
CREATE INDEX NAMEX ON EMPLOYEE(LAST_NAME, FIRST_NAME);
CREATE INDEX IDX_FTS$INDICES_ANALYZER ON FTS$INDICES(FTS$ANALYZER);
CREATE INDEX IDX_FTS$INDICES_RELATION ON FTS$INDICES(FTS$RELATION_NAME);
CREATE DESCENDING INDEX MAXSALX ON JOB(JOB_COUNTRY, MAX_SALARY);
CREATE INDEX MINSALX ON JOB(JOB_COUNTRY, MIN_SALARY);
CREATE UNIQUE INDEX PRODTYPEX ON PROJECT(PRODUCT, PROJ_NAME);
CREATE DESCENDING INDEX CHANGEX ON SALARY_HISTORY(CHANGE_DATE);
CREATE INDEX UPDATERX ON SALARY_HISTORY(UPDATER_ID);
CREATE INDEX NEEDX ON SALES(DATE_NEEDED);
CREATE DESCENDING INDEX QTYX ON SALES(ITEM_TYPE, QTY_ORDERED);
CREATE INDEX SALESTATX ON SALES(ORDER_STATUS, PAID);
ALTER TABLE CUSTOMER ADD FOREIGN KEY (COUNTRY) REFERENCES COUNTRY (COUNTRY);
ALTER TABLE DEPARTMENT ADD FOREIGN KEY (HEAD_DEPT) REFERENCES DEPARTMENT (DEPT_NO);
ALTER TABLE DEPARTMENT ADD FOREIGN KEY (MNGR_NO) REFERENCES EMPLOYEE (EMP_NO);
ALTER TABLE EMPLOYEE ADD FOREIGN KEY (DEPT_NO) REFERENCES DEPARTMENT (DEPT_NO);
ALTER TABLE EMPLOYEE ADD FOREIGN KEY (JOB_CODE, JOB_GRADE, JOB_COUNTRY) REFERENCES JOB (JOB_CODE, JOB_GRADE, JOB_COUNTRY);
ALTER TABLE EMPLOYEE_PROJECT ADD FOREIGN KEY (EMP_NO) REFERENCES EMPLOYEE (EMP_NO);
ALTER TABLE EMPLOYEE_PROJECT ADD FOREIGN KEY (PROJ_ID) REFERENCES PROJECT (PROJ_ID);
ALTER TABLE FTS$INDEX_SEGMENTS ADD CONSTRAINT FK_FTS$INDEX_SEGMENTS FOREIGN KEY (FTS$INDEX_NAME) REFERENCES FTS$INDICES (FTS$INDEX_NAME) ON DELETE CASCADE;
ALTER TABLE FTS$STOP_WORDS ADD CONSTRAINT FK_FTS$STOP_WORDS_ANALYZER FOREIGN KEY (FTS$ANALYZER_NAME) REFERENCES FTS$ANALYZERS (FTS$ANALYZER_NAME) ON DELETE CASCADE;
ALTER TABLE JOB ADD FOREIGN KEY (JOB_COUNTRY) REFERENCES COUNTRY (COUNTRY);
ALTER TABLE PROJECT ADD FOREIGN KEY (TEAM_LEADER) REFERENCES EMPLOYEE (EMP_NO);
ALTER TABLE PROJ_DEPT_BUDGET ADD FOREIGN KEY (DEPT_NO) REFERENCES DEPARTMENT (DEPT_NO);
ALTER TABLE PROJ_DEPT_BUDGET ADD FOREIGN KEY (PROJ_ID) REFERENCES PROJECT (PROJ_ID);
ALTER TABLE SALARY_HISTORY ADD FOREIGN KEY (EMP_NO) REFERENCES EMPLOYEE (EMP_NO);
ALTER TABLE SALES ADD FOREIGN KEY (CUST_NO) REFERENCES CUSTOMER (CUST_NO);
ALTER TABLE SALES ADD FOREIGN KEY (SALES_REP) REFERENCES EMPLOYEE (EMP_NO);

CREATE SEQUENCE CUST_NO_GEN;
CREATE SEQUENCE EMP_NO_GEN;

/* View: PHONE_LIST, Owner: SYSDBA */

CREATE VIEW PHONE_LIST (
  EMP_NO,
  FIRST_NAME,
  LAST_NAME,
  PHONE_EXT,
  LOCATION,
  PHONE_NO
) AS
SELECT
    emp_no, first_name, last_name, phone_ext, location, phone_no
    FROM employee, department
    WHERE employee.dept_no = department.dept_no
;

/* Add Domain Check Constraints */


ALTER DOMAIN BUDGET ADD CONSTRAINT
	 CHECK (VALUE > 10000 AND VALUE <= 2000000);

ALTER DOMAIN CUSTNO ADD CONSTRAINT
	 CHECK (VALUE > 1000);
ALTER DOMAIN DEPTNO ADD CONSTRAINT
	 CHECK (VALUE = '000' OR (VALUE > '0' AND VALUE <= '999') OR VALUE IS NULL);


ALTER DOMAIN FTS$D_CHANGE_TYPE ADD CONSTRAINT
	 CHECK (VALUE IN ('I', 'U', 'D'));
ALTER DOMAIN FTS$D_INDEX_STATUS ADD CONSTRAINT
	 CHECK (VALUE IN ('I', 'N', 'C', 'U'));
ALTER DOMAIN JOBCODE ADD CONSTRAINT
	 CHECK (VALUE > '99999');
ALTER DOMAIN JOBGRADE ADD CONSTRAINT
	 CHECK (VALUE BETWEEN 0 AND 6);


ALTER DOMAIN PONUMBER ADD CONSTRAINT
	 CHECK (VALUE STARTING WITH 'V');
ALTER DOMAIN PRODTYPE ADD CONSTRAINT
	 CHECK (VALUE IN ('software', 'hardware', 'other', 'N/A'));
ALTER DOMAIN PROJNO ADD CONSTRAINT
	 CHECK (VALUE = UPPER (VALUE));
ALTER DOMAIN SALARY ADD CONSTRAINT
	 CHECK (VALUE > 0);


ALTER TABLE JOB ADD
CHECK (min_salary < max_salary);

ALTER TABLE EMPLOYEE ADD
CHECK ( salary >= (SELECT min_salary FROM job WHERE
                        job.job_code = employee.job_code AND
                        job.job_grade = employee.job_grade AND
                        job.job_country = employee.job_country) AND
            salary <= (SELECT max_salary FROM job WHERE
                        job.job_code = employee.job_code AND
                        job.job_grade = employee.job_grade AND
                        job.job_country = employee.job_country));

ALTER TABLE PROJ_DEPT_BUDGET ADD
CHECK (FISCAL_YEAR >= 1993);

ALTER TABLE SALARY_HISTORY ADD
CHECK (percent_change between -50 and 50);

ALTER TABLE CUSTOMER ADD
CHECK (on_hold IS NULL OR on_hold = '*');

ALTER TABLE SALES ADD
CHECK (order_status in
                            ('new', 'open', 'shipped', 'waiting'));

ALTER TABLE SALES ADD
CHECK (ship_date >= order_date OR ship_date IS NULL);

ALTER TABLE SALES ADD
CHECK (date_needed > order_date OR date_needed IS NULL);

ALTER TABLE SALES ADD
CHECK (paid in ('y', 'n'));

ALTER TABLE SALES ADD
CHECK (qty_ordered >= 1);

ALTER TABLE SALES ADD
CHECK (total_value >= 0);

ALTER TABLE SALES ADD
CHECK (discount >= 0 AND discount <= 1);

ALTER TABLE SALES ADD
CHECK (NOT (order_status = 'shipped' AND ship_date IS NULL));

ALTER TABLE SALES ADD
CHECK (NOT (order_status = 'shipped' AND
            EXISTS (SELECT on_hold FROM customer
                    WHERE customer.cust_no = sales.cust_no
                    AND customer.on_hold = '*')));


/*  Exceptions */

CREATE EXCEPTION CUSTOMER_CHECK 'Overdue balance -- can not ship.';
CREATE EXCEPTION CUSTOMER_ON_HOLD 'This customer is on hold.';
CREATE EXCEPTION FTS$EXCEPTION 'Custom FTS error';
CREATE EXCEPTION ORDER_ALREADY_SHIPPED 'Order status is "shipped."';
CREATE EXCEPTION REASSIGN_SALES 'Reassign the sales records before deleting this employee.';
CREATE EXCEPTION UNKNOWN_EMP_ID 'Invalid employee number or project id.';
COMMIT WORK;
SET AUTODDL OFF;
SET TERM ^;

/* Package Definitions */

CREATE PACKAGE DEMO_PACKAGE
AS
BEGIN
procedure nativ_matches(
    text    varchar(8191) character set UTF8,
    pattern varchar(8191) character set UTF8
) returns (
    number  integer,
    groups  varchar(8191) character set UTF8
);
procedure nativ_groups(
    groups varchar(8191) character set UTF8
) returns (
    number integer,
    origin integer,
    finish integer
);
procedure nativ_find(
    text    varchar(8191) character set UTF8,
    pattern varchar(8191) character set UTF8,
    amount  integer,
    pass    integer
) returns (
    number  integer,
    match   varchar(8191) character set UTF8
);
function nativ_find_first(
    text    varchar(8191) character set UTF8,
    pattern varchar(8191) character set UTF8,
    pass    integer
) returns varchar(8191) character set UTF8;
function nativ_replace(
    text        varchar(8191) character set UTF8,
    pattern     varchar(8191) character set UTF8,
    replacement varchar(8191) character set UTF8,
    amount      integer,
    pass        integer
) returns varchar(8191) character set UTF8;
procedure nativ_split_words(
    text varchar(8191) character set UTF8
) returns (
    number integer,
    word   varchar(8191) character set UTF8
);
procedure nativ_split(
    text      varchar(8191) character set UTF8,
    separator varchar(8191) character set UTF8
) returns (
    number integer,
    part   varchar(8191) character set UTF8
);
procedure regex_matches(
    text    varchar(8191) character set UTF8,
    pattern varchar(8191) character set UTF8
) returns (
    number  integer,
    groups  varchar(8191) character set UTF8
);
procedure regex_groups(
    groups varchar(8191) character set UTF8
) returns (
    number integer,
    origin integer,
    finish integer
);
procedure regex_find(
    text    varchar(8191) character set UTF8,
    pattern varchar(8191) character set UTF8,
    amount  integer,
    pass    integer
) returns (
    number  integer,
    match   varchar(8191) character set UTF8
);
function regex_find_first(
    text    varchar(8191) character set UTF8,
    pattern varchar(8191) character set UTF8,
    pass    integer
) returns varchar(8191) character set UTF8;
function regex_replace(
    text        varchar(8191) character set UTF8,
    pattern     varchar(8191) character set UTF8,
    replacement varchar(8191) character set UTF8,
    amount      integer,
    pass        integer
) returns varchar(8191) character set UTF8;
procedure regex_split_words(
    text varchar(8191) character set UTF8
) returns (
    number integer,
    word   varchar(8191) character set UTF8
);
procedure regex_split(
    text      varchar(8191) character set UTF8,
    separator varchar(8191) character set UTF8
) returns (
    number integer,
    part   varchar(8191) character set UTF8
);
end
^

CREATE PACKAGE FTS$MANAGEMENT
AS
BEGIN 
  /** 
   * Returns the directory where the files and folders 
   * of the full-text index for the current database are located. 
  **/ 
  FUNCTION FTS$GET_DIRECTORY () 
  RETURNS VARCHAR(255) CHARACTER SET UTF8 
  DETERMINISTIC; 
  /** 
   * Returns a list of system analyzers. 
   * 
   * Output parameters: 
   *   FTS$ANALYZER - analyzer name; 
   *   FTS$STOP_WORDS_SUPPORTED - stop words supported. 
  **/ 
  PROCEDURE FTS$SYSTEM_ANALYZERS 
  RETURNS ( 
    FTS$ANALYZER VARCHAR(63) CHARACTER SET UTF8, 
    FTS$STOP_WORDS_SUPPORTED BOOLEAN 
  ); 
  /** 
   * Returns info of system analyzers. 
   * 
   * Input parameters: 
   *   FTS$ANALYZER_NAME - analyzer name. 
   * 
   * Output parameters: 
   *   FTS$ANALYZER - analyzer name; 
   *   FTS$STOP_WORDS_SUPPORTED - stop words supported. 
  **/ 
  PROCEDURE FTS$GET_SYSTEM_ANALYZER ( 
    FTS$ANALYZER_NAME VARCHAR(63) CHARACTER SET UTF8 
  ) 
  RETURNS ( 
    FTS$ANALYZER VARCHAR(63) CHARACTER SET UTF8, 
    FTS$STOP_WORDS_SUPPORTED BOOLEAN 
  ); 
  /** 
   * Returns true if system analyzer exists, othewise - false. 
   * 
   * Input parameters: 
   *   FTS$ANALYZER - analyzer name. 
  **/ 
  FUNCTION FTS$HAS_SYSTEM_ANALYZER ( 
    FTS$ANALYZER VARCHAR(63) CHARACTER SET UTF8 
  ) 
  RETURNS BOOLEAN; 
  /** 
   * Returns a list of available analyzers. 
   * 
   * Output parameters: 
   *   FTS$ANALYZER - analyzer name; 
   *   FTS$BASE_ANALYZER - name of base analyzer; 
   *   FTS$STOP_WORDS_SUPPORTED - stop words supported; 
   *   FTS$SYSTEM_FLAG - is system analyzer; 
   *   FTS$DESCRIPTION - description of the analyzer. 
  **/ 
  PROCEDURE FTS$ALL_ANALYZERS 
  RETURNS ( 
    FTS$ANALYZER             VARCHAR(63) CHARACTER SET UTF8, 
    FTS$BASE_ANALYZER        VARCHAR(63) CHARACTER SET UTF8, 
    FTS$STOP_WORDS_SUPPORTED BOOLEAN, 
    FTS$SYSTEM_FLAG          BOOLEAN, 
    FTS$DESCRIPTION          BLOB SUB_TYPE TEXT CHARACTER SET UTF8 
  ); 
  /** 
   * Returns true if analyzer exists, othewise - false. 
   * 
   * Input parameters: 
   *   FTS$ANALYZER - analyzer name. 
  **/ 
  FUNCTION FTS$HAS_ANALYZER ( 
    FTS$ANALYZER VARCHAR(63) CHARACTER SET UTF8 
  ) 
  RETURNS BOOLEAN; 
  /** 
   * Create custom analyzer. 
   * 
   * Input parameters: 
   *   FTS$ANALYZER - analyzer name; 
   *   FTS$BASE_ANALYZER - name of base analyzer; 
   *   FTS$DESCRIPTION - description of the analyzer. 
  **/ 
  PROCEDURE FTS$CREATE_ANALYZER ( 
      FTS$ANALYZER VARCHAR(63) CHARACTER SET UTF8 NOT NULL, 
      FTS$BASE_ANALYZER VARCHAR(63) CHARACTER SET UTF8 NOT NULL, 
      FTS$DESCRIPTION BLOB SUB_TYPE TEXT CHARACTER SET UTF8 DEFAULT NULL 
  ); 
  /** 
   * Drop custom analyzer. 
   * 
   * Input parameters: 
   *   FTS$ANALYZER - analyzer name. 
  **/ 
  PROCEDURE FTS$DROP_ANALYZER ( 
      FTS$ANALYZER VARCHAR(63) CHARACTER SET UTF8 NOT NULL 
  ); 
  /** 
   * Returns a list of stop words by analyzer. 
   * 
   * Input parameters: 
   *   FTS$ANALYZER - analyzer name. 
   * 
   * Output parameters: 
   *   FTS$WORD - stop word. 
  **/ 
  PROCEDURE FTS$ANALYZER_STOP_WORDS ( 
      FTS$ANALYZER VARCHAR(63) CHARACTER SET UTF8 NOT NULL) 
  RETURNS ( 
      FTS$WORD VARCHAR(63) CHARACTER SET UTF8 
  ); 
  /** 
   * Add stop word to custom analyzer. 
   * 
   * Input parameters: 
   *   FTS$ANALYZER_NAME - analyzer name; 
   *   FTS$WORD - stop word. 
  **/ 
  PROCEDURE FTS$ADD_STOP_WORD ( 
      FTS$ANALYZER_NAME VARCHAR(63) CHARACTER SET UTF8 NOT NULL, 
      FTS$WORD VARCHAR(63) CHARACTER SET UTF8 NOT NULL 
  ); 
  /** 
   * Delete stop word from custom analyzer. 
   * 
   * Input parameters: 
   *   FTS$ANALYZER_NAME - analyzer name; 
   *   FTS$WORD - stop word. 
  **/ 
  PROCEDURE FTS$DROP_STOP_WORD ( 
      FTS$ANALYZER_NAME VARCHAR(63) CHARACTER SET UTF8 NOT NULL, 
      FTS$WORD VARCHAR(63) CHARACTER SET UTF8 NOT NULL 
  ); 
  /** 
   * Create a new full-text index. 
   * 
   * Input parameters: 
   *   FTS$INDEX_NAME - name of the index; 
   *   FTS$RELATION_NAME - name of the table to be indexed; 
   *   FTS$ANALYZER - analyzer name; 
   *   FTS$KEY_FIELD_NAME - key field name; 
   *   FTS$DESCRIPTION - description of the index. 
  **/ 
  PROCEDURE FTS$CREATE_INDEX ( 
      FTS$INDEX_NAME     VARCHAR(63) CHARACTER SET UTF8 NOT NULL, 
      FTS$RELATION_NAME  VARCHAR(63) CHARACTER SET UTF8 NOT NULL, 
      FTS$ANALYZER       VARCHAR(63) CHARACTER SET UTF8 DEFAULT 'STANDARD', 
      FTS$KEY_FIELD_NAME VARCHAR(63) CHARACTER SET UTF8 DEFAULT NULL, 
      FTS$DESCRIPTION BLOB SUB_TYPE TEXT CHARACTER SET UTF8 DEFAULT NULL 
  ); 
  /** 
   * Delete the full-text index. 
   * 
   * Input parameters: 
   *   FTS$INDEX_NAME - name of the index. 
  **/ 
  PROCEDURE FTS$DROP_INDEX ( 
      FTS$INDEX_NAME VARCHAR(63) CHARACTER SET UTF8 NOT NULL 
  ); 
  /** 
   * Allows to make the index active or inactive. 
   * 
   * Input parameters: 
   *   FTS$INDEX_NAME - name of the index; 
   *   FTS$INDEX_ACTIVE - activity flag. 
  **/ 
  PROCEDURE FTS$SET_INDEX_ACTIVE ( 
      FTS$INDEX_NAME   VARCHAR(63) CHARACTER SET UTF8 NOT NULL, 
      FTS$INDEX_ACTIVE BOOLEAN NOT NULL 
  ); 
  /** 
   * Sets the index description. 
   * 
   * Input parameters: 
   *   FTS$INDEX_NAME - name of the index; 
   *   FTS$DESCRIPTION - index description. 
  **/ 
  PROCEDURE FTS$COMMENT_ON_INDEX ( 
      FTS$INDEX_NAME   VARCHAR(63) CHARACTER SET UTF8 NOT NULL, 
      FTS$DESCRIPTION BLOB SUB_TYPE TEXT CHARACTER SET UTF8 
  ); 
  /** 
   * Add a new segment (indexed table field) of the full-text index. 
   * 
   * Input parameters: 
   *   FTS$INDEX_NAME - name of the index; 
   *   FTS$FIELD_NAME - the name of the field to be indexed; 
   *   FTS$BOOST - the coefficient of increasing the significance of the segment. 
  **/ 
  PROCEDURE FTS$ADD_INDEX_FIELD ( 
      FTS$INDEX_NAME    VARCHAR(63) CHARACTER SET UTF8 NOT NULL, 
      FTS$FIELD_NAME    VARCHAR(63) CHARACTER SET UTF8 NOT NULL, 
      FTS$BOOST         DOUBLE PRECISION DEFAULT NULL 
  ); 
  /** 
   * Delete a segment (indexed table field) of the full-text index. 
   * 
   * Input parameters: 
   *   FTS$INDEX_NAME - index name; 
   *   FTS$FIELD_NAME - field name. 
  **/ 
  PROCEDURE FTS$DROP_INDEX_FIELD ( 
      FTS$INDEX_NAME    VARCHAR(63) CHARACTER SET UTF8 NOT NULL, 
      FTS$FIELD_NAME    VARCHAR(63) CHARACTER SET UTF8 NOT NULL 
  ); 
  /** 
   * Sets the significance multiplier for the full-text index field. 
   * 
   * Input parameters: 
   *   FTS$INDEX_NAME - name of the index; 
   *   FTS$FIELD_NAME - name of the field; 
   *   FTS$BOOST - the coefficient of increasing the significance of the segment. 
  **/ 
  PROCEDURE FTS$SET_INDEX_FIELD_BOOST ( 
      FTS$INDEX_NAME VARCHAR(63) CHARACTER SET UTF8 NOT NULL, 
      FTS$FIELD_NAME VARCHAR(63) CHARACTER SET UTF8 NOT NULL, 
      FTS$BOOST DOUBLE PRECISION 
  ); 
  /** 
   * Rebuild the full-text index. 
   * 
   * Input parameters: 
   *   FTS$INDEX_NAME - index name. 
   **/ 
  PROCEDURE FTS$REBUILD_INDEX ( 
      FTS$INDEX_NAME VARCHAR(63) CHARACTER SET UTF8 NOT NULL 
  ); 
  /** 
   * Rebuild all full-text indexes for the specified table. 
   * 
   * Input parameters: 
   *   FTS$RELATION_NAME - table name. 
  **/ 
  PROCEDURE FTS$REINDEX_TABLE ( 
      FTS$RELATION_NAME VARCHAR(63) CHARACTER SET UTF8 NOT NULL 
  ); 
  /** 
   * Rebuild all full-text indexes in the database. 
  **/ 
  PROCEDURE FTS$FULL_REINDEX; 
  /** 
   * Optimize the full-text index. 
   * 
   * Input parameters: 
   *   FTS$INDEX_NAME - index name. 
   **/ 
  PROCEDURE FTS$OPTIMIZE_INDEX ( 
      FTS$INDEX_NAME VARCHAR(63) CHARACTER SET UTF8 NOT NULL 
  ); 
  /** 
   * Optimize all full-text indexes. 
   **/ 
  PROCEDURE FTS$OPTIMIZE_INDEXES; 
END
^

CREATE PACKAGE REGEX
AS
begin

procedure matches(
    text    varchar(8191) character set UTF8
  , pattern varchar(8191) character set UTF8
)returns(
    number  integer
  , groups  varchar(8191) character set UTF8
);

procedure groups(
    groups varchar(8191) character set UTF8
)returns(
    number integer
  , origin integer
  , finish integer
);

procedure find(
    text    varchar(8191) character set UTF8
  , pattern varchar(8191) character set UTF8
  , amount  integer
  , pass    integer
)returns(
    number  integer
  , match   varchar(8191) character set UTF8
);

function find_first(
    text    varchar(8191) character set UTF8
  , pattern varchar(8191) character set UTF8
  , pass    integer
)returns    varchar(8191) character set UTF8;

function replace(
    text        varchar(8191) character set UTF8
  , pattern     varchar(8191) character set UTF8
  , replacement varchar(8191) character set UTF8
  , amount      integer
  , pass        integer
)returns        varchar(8191) character set UTF8;

procedure split_words(
    text   varchar(8191) character set UTF8
)returns(
    number integer
  , word   varchar(8191) character set UTF8
);

procedure split(
    text      varchar(8191) character set UTF8
  , separator varchar(8191) character set UTF8
)returns(
    number    integer
  , part      varchar(8191) character set UTF8
);

end
^

CREATE PACKAGE TBUTILS
AS
BEGIN
  FUNCTION TB_UPPER(s VARCHAR(4000)) RETURNS VARCHAR(4000);
  /*  Fügen Sie hier alle anderen Funktionen hinzu (TB_TRIM, TB_MD5, etc.) */
END
^

SET TERM ;^
COMMIT WORK;
SET AUTODDL ON;
COMMIT WORK;
SET AUTODDL OFF;
SET TERM ^;

/* Stored procedures Definitions*/

/* Stored Procedure: SHOW_LANGS, Owner: SYSDBA */

CREATE PROCEDURE SHOW_LANGS
(
  CODE VARCHAR(5) CHARACTER SET NONE,
  GRADE SMALLINT,
  CTY VARCHAR(15) CHARACTER SET NONE
)
RETURNS
(
  LANGUAGES VARCHAR(15) CHARACTER SET NONE
)
AS BEGIN SUSPEND; EXIT; END
^

/* Stored Procedure: ADD_EMP_PROJ, Owner: SYSDBA */

CREATE PROCEDURE ADD_EMP_PROJ
(
  EMP_NO SMALLINT,
  PROJ_ID CHAR(5) CHARACTER SET NONE
)
AS BEGIN SUSPEND; EXIT; END
^

/* Stored Procedure: ALL_LANGS, Owner: SYSDBA */

CREATE PROCEDURE ALL_LANGS
RETURNS
(
  CODE VARCHAR(5) CHARACTER SET NONE,
  GRADE VARCHAR(5) CHARACTER SET NONE,
  COUNTRY VARCHAR(15) CHARACTER SET NONE,
  LANG VARCHAR(15) CHARACTER SET NONE
)
AS BEGIN SUSPEND; EXIT; END
^

/* Stored Procedure: DELETE_EMPLOYEE, Owner: SYSDBA */

CREATE PROCEDURE DELETE_EMPLOYEE
(
  EMP_NUM INTEGER
)
AS BEGIN SUSPEND; EXIT; END
^

/* Stored Procedure: DEPT_BUDGET, Owner: SYSDBA */

CREATE PROCEDURE DEPT_BUDGET
(
  DNO CHAR(3) CHARACTER SET NONE
)
RETURNS
(
  TOT DECIMAL(12,2)
)
AS BEGIN SUSPEND; EXIT; END
^

/* Stored Procedure: GET_EMP_PROJ, Owner: SYSDBA */

CREATE PROCEDURE GET_EMP_PROJ
(
  EMP_NO SMALLINT
)
RETURNS
(
  PROJ_ID CHAR(5) CHARACTER SET NONE
)
AS BEGIN SUSPEND; EXIT; END
^

/* Stored Procedure: MAIL_LABEL, Owner: SYSDBA */

CREATE PROCEDURE MAIL_LABEL
(
  CUST_NO INTEGER
)
RETURNS
(
  LINE1 CHAR(40) CHARACTER SET NONE,
  LINE2 CHAR(40) CHARACTER SET NONE,
  LINE3 CHAR(40) CHARACTER SET NONE,
  LINE4 CHAR(40) CHARACTER SET NONE,
  LINE5 CHAR(40) CHARACTER SET NONE,
  LINE6 CHAR(40) CHARACTER SET NONE
)
AS BEGIN SUSPEND; EXIT; END
^

/* Stored Procedure: MYPROC1, Owner: SYSDBA */

CREATE PROCEDURE MYPROC1
AS BEGIN EXIT; END
^

/* Stored Procedure: MYPROC2, Owner: SYSDBA */

CREATE PROCEDURE MYPROC2
AS BEGIN EXIT; END
^

/* Stored Procedure: MYPROC3, Owner: SYSDBA */

CREATE PROCEDURE MYPROC3
AS BEGIN EXIT; END
^

/* Stored Procedure: ORG_CHART, Owner: SYSDBA */

CREATE PROCEDURE ORG_CHART
RETURNS
(
  HEAD_DEPT CHAR(25) CHARACTER SET NONE,
  DEPARTMENT CHAR(25) CHARACTER SET NONE,
  MNGR_NAME CHAR(20) CHARACTER SET NONE,
  TITLE CHAR(5) CHARACTER SET NONE,
  EMP_CNT INTEGER
)
AS BEGIN SUSPEND; EXIT; END
^

/* Stored Procedure: SHIP_ORDER, Owner: SYSDBA */

CREATE PROCEDURE SHIP_ORDER
(
  PO_NUM CHAR(8) CHARACTER SET NONE
)
AS BEGIN SUSPEND; EXIT; END
^

/* Stored Procedure: SUB_TOT_BUDGET, Owner: SYSDBA */

CREATE PROCEDURE SUB_TOT_BUDGET
(
  HEAD_DEPT CHAR(3) CHARACTER SET NONE
)
RETURNS
(
  TOT_BUDGET DECIMAL(12,2),
  AVG_BUDGET DECIMAL(12,2),
  MIN_BUDGET DECIMAL(12,2),
  MAX_BUDGET DECIMAL(12,2)
)
AS BEGIN SUSPEND; EXIT; END
^

SET TERM ;^
COMMIT WORK;
SET AUTODDL ON;
COMMIT WORK;
SET AUTODDL OFF;
SET TERM ^;

/* Stored Function declarations */

CREATE FUNCTION FIND_FIRST (TEXT VARCHAR(8191) CHARACTER SET UTF8PATTERN VARCHAR(8191) CHARACTER SET UTF8PASS INTEGER)
 RETURNS VARCHAR(8191) CHARACTER SET UTF8
 AS BEGIN END
^

CREATE FUNCTION FTS$GET_DIRECTORY
 RETURNS VARCHAR(255) CHARACTER SET UTF8
 AS BEGIN END
^

CREATE FUNCTION FTS$HAS_ANALYZER (FTS$ANALYZER VARCHAR(63) CHARACTER SET UTF8)
 RETURNS BOOLEAN
 AS BEGIN END
^

CREATE FUNCTION FTS$HAS_SYSTEM_ANALYZER (FTS$ANALYZER VARCHAR(63) CHARACTER SET UTF8)
 RETURNS BOOLEAN
 AS BEGIN END
^

CREATE FUNCTION NATIV_FIND_FIRST (TEXT VARCHAR(8191) CHARACTER SET UTF8PATTERN VARCHAR(8191) CHARACTER SET UTF8PASS INTEGER)
 RETURNS VARCHAR(8191) CHARACTER SET UTF8
 AS BEGIN END
^

CREATE FUNCTION NATIV_REPLACE (TEXT VARCHAR(8191) CHARACTER SET UTF8PATTERN VARCHAR(8191) CHARACTER SET UTF8REPLACEMENT VARCHAR(8191) CHARACTER SET UTF8AMOUNT INTEGERPASS INTEGER)
 RETURNS VARCHAR(8191) CHARACTER SET UTF8
 AS BEGIN END
^

CREATE FUNCTION REGEX_FIND_FIRST (TEXT VARCHAR(8191) CHARACTER SET UTF8PATTERN VARCHAR(8191) CHARACTER SET UTF8PASS INTEGER)
 RETURNS VARCHAR(8191) CHARACTER SET UTF8
 AS BEGIN END
^

CREATE FUNCTION REGEX_REPLACE (TEXT VARCHAR(8191) CHARACTER SET UTF8PATTERN VARCHAR(8191) CHARACTER SET UTF8REPLACEMENT VARCHAR(8191) CHARACTER SET UTF8AMOUNT INTEGERPASS INTEGER)
 RETURNS VARCHAR(8191) CHARACTER SET UTF8
 AS BEGIN END
^

CREATE FUNCTION REPLACE (TEXT VARCHAR(8191) CHARACTER SET UTF8PATTERN VARCHAR(8191) CHARACTER SET UTF8REPLACEMENT VARCHAR(8191) CHARACTER SET UTF8AMOUNT INTEGERPASS INTEGER)
 RETURNS VARCHAR(8191) CHARACTER SET UTF8
 AS BEGIN END
^

CREATE FUNCTION TB_UPPER (S VARCHAR(4000) CHARACTER SET NONE)
 RETURNS VARCHAR(4000) CHARACTER SET NONE
 AS BEGIN END
^

SET TERM ;^
COMMIT WORK;
SET AUTODDL ON;
SET TERM ^;

CREATE TRIGGER SET_CUST_NO FOR CUSTOMER
ACTIVE BEFORE INSERT POSITION 0
AS
BEGIN
    if (new.cust_no is null) then
    new.cust_no = gen_id(cust_no_gen, 1);
END
^

CREATE TRIGGER SET_EMP_NO FOR EMPLOYEE
ACTIVE BEFORE INSERT POSITION 0
AS
BEGIN
    if (new.emp_no is null) then
    new.emp_no = gen_id(emp_no_gen, 1);
END
^

CREATE TRIGGER SAVE_SALARY_CHANGE FOR EMPLOYEE
ACTIVE AFTER UPDATE POSITION 0
AS
BEGIN
    IF (old.salary <> new.salary) THEN
        INSERT INTO salary_history
            (emp_no, change_date, updater_id, old_salary, percent_change)
        VALUES (
            old.emp_no,
            'NOW',
            user,
            old.salary,
            (new.salary - old.salary) * 100 / old.salary);
END
^

CREATE TRIGGER POST_NEW_ORDER FOR SALES
ACTIVE AFTER INSERT POSITION 0
AS
BEGIN
    POST_EVENT 'new_order';
END
^

COMMIT WORK^
SET TERM ;^
COMMIT WORK;
SET AUTODDL OFF;
SET TERM ^;

/* Package Definitions */

CREATE PACKAGE BODY DEMO_PACKAGE
AS
begin
procedure nativ_matches(
    text varchar(8191) character set UTF8,
    pattern varchar(8191) character set UTF8
)
returns (
    number integer,
    groups varchar(8191) character set UTF8
)
as
declare variable pos integer = 1;
declare variable idx integer;
declare variable found varchar(8191);
declare variable res varchar(8191) = '';
declare variable count_matches integer = 0;
begin
  if (pattern is null or pattern = '') then
  begin
    number = 0;
    groups = '';
    suspend;
    exit;
  end

  idx = position(pattern, text, pos);
  while (idx > 0) do
  begin
    count_matches = count_matches + 1;
    found = substring(text from idx for char_length(pattern));
    if (res = '') then
      res = found;
    else
      res = res || ',' || found;

    pos = idx + char_length(pattern);
    idx = position(pattern, text, pos);
  end

  number = count_matches;
  groups = res;
  suspend;
end
procedure nativ_groups(
    groups varchar(8191) character set utf8
)
returns (
    number integer,
    origin integer,
    finish integer
)
as
declare variable pos integer = 1;
declare variable idx integer;
declare variable part varchar(8191);
declare variable len_part integer;
begin
  number = 0;

  if (groups is null or groups = '') then
  begin
    number = 0;
    origin = 0;
    finish = 0;
    suspend;
    exit;
  end

  while (pos <= char_length(groups)) do
  begin
    idx = position(',' , groups, pos);

    if (idx > 0) then
    begin
      part = substring(groups from pos for idx - pos);
      pos = idx + 1;
    end
    else
    begin
      part = substring(groups from pos);
      pos = char_length(groups) + 1;
    end

    len_part = char_length(part);
    number = number + 1;
    origin = 0;
    finish = len_part - 1;

    suspend;
  end
end
procedure nativ_find(
    text varchar(8191) character set UTF8,
    pattern varchar(8191) character set UTF8,
    amount integer,
    pass integer
)
returns (
    number integer,
    match varchar(8191) character set UTF8
)
as
declare variable pos integer = 1;
declare variable idx integer;
declare variable count_matches integer = 0;
declare variable output_count integer = 0;
begin
  if (pattern is null or pattern = '' or amount <= 0) then
  begin
    number = 0;
    match = '';
    suspend;
    exit;
  end

  idx = position(pattern, text, pos);
  while (idx > 0) do
  begin
    count_matches = count_matches + 1;

    if (count_matches >= pass and output_count < amount) then
    begin
      number = count_matches;
      match = substring(text from idx for char_length(pattern));
      output_count = output_count + 1;
      suspend;
    end

    pos = idx + char_length(pattern);
    idx = position(pattern, text, pos);

    if (output_count >= amount) then
      leave;
  end
end
function nativ_find_first(
    text varchar(8191) character set UTF8,
    pattern varchar(8191) character set UTF8,
    pass integer
) returns varchar(8191) character set UTF8
as
declare variable pos integer = 1;
declare variable idx integer;
declare variable count_matches integer = 0;
declare variable result varchar(8191);
begin
  if (pattern is null or pattern = '' or pass < 1) then
    return null;

  idx = position(pattern, text, pos);
  while (idx > 0) do
  begin
    count_matches = count_matches + 1;
    if (count_matches = pass) then
    begin
      result = substring(text from idx for char_length(pattern));
      return result;
    end
    pos = idx + char_length(pattern);
    idx = position(pattern, text, pos);
  end

  return null;
end
function nativ_replace(
    text varchar(8191) character set utf8,
    pattern varchar(8191) character set utf8,
    replacement varchar(8191) character set utf8,
    amount integer,
    pass integer
) returns varchar(8191) character set utf8
as
declare variable pos integer = 1;
declare variable idx integer;
declare variable count_matches integer = 0;
declare variable result varchar(8191) = '';
declare variable prev_pos integer = 1;
declare variable replaced integer = 0;
declare variable text_len integer;
declare variable pat_len integer;
begin
  if (pattern is null or pattern = '' or amount = 0) then
    return text;

  text_len = character_length(text);
  pat_len = character_length(pattern);

  idx = position(pattern, text, pos);

  while (idx > 0 and replaced < amount) do
  begin
    count_matches = count_matches + 1;

    if (count_matches >= pass) then
    begin
      result = result || substring(text from prev_pos for idx - prev_pos);
      result = result || replacement;
      replaced = replaced + 1;
      prev_pos = idx + pat_len;
    end

    pos = idx + pat_len;
    idx = position(pattern, text, pos);
  end

  if (prev_pos <= text_len) then
    result = result || substring(text from prev_pos);

  return result;
end
procedure nativ_split_words(
    text varchar(8191) character set utf8
)
returns (
    number integer,
    word varchar(8191) character set utf8
)
as
declare variable pos integer = 1;
declare variable start_pos integer;
declare variable end_pos integer;
declare variable len integer;
declare variable w varchar(8191);
begin
  number = 0;
  len = character_length(text);

  while (pos <= len) do
  begin
    while (pos <= len and substring(text from pos for 1) = ' ') do
      pos = pos + 1;

    if (pos > len) then
      exit;

    start_pos = pos;

    while (pos <= len and substring(text from pos for 1) <> ' ') do
      pos = pos + 1;

    end_pos = pos - 1;
    number = number + 1;
    word = substring(text from start_pos for end_pos - start_pos + 1);
    suspend;
  end
end
procedure nativ_split(
    text varchar(8191) character set utf8,
    separator varchar(8191) character set utf8
)
returns (
    number integer,
    part varchar(8191) character set utf8
)
as
declare variable pos integer = 1;
declare variable start_pos integer = 1;
declare variable idx integer;
declare variable count_parts integer = 0;
declare variable sep_char char(1);
begin
  if (separator is null or character_length(separator) = 0) then
  begin
    number = 1;
    part = text;
    suspend;
    exit;
  end

  sep_char = substring(separator from 1 for 1);
  idx = position(sep_char, text, start_pos);

  while (true) do
  begin
    if (idx = 0) then
    begin
      count_parts = count_parts + 1;
      number = count_parts;
      part = substring(text from start_pos);
      suspend;
      exit;
    end

    count_parts = count_parts + 1;
    number = count_parts;
    part = substring(text from start_pos for idx - start_pos);
    suspend;

    start_pos = idx + 1;
    idx = position(sep_char, text, start_pos);
  end
end
procedure regex_matches(
    text    varchar(8191) character set UTF8,
    pattern varchar(8191) character set UTF8
) returns (
    number  integer,
    groups  varchar(8191) character set UTF8
) external name
    'fb_regex!matches'
engine
    udr;
procedure regex_groups(
    groups varchar(8191) character set UTF8
) returns (
    number integer,
    origin integer,
    finish integer
) external name
    'fb_regex!groups'
engine
    udr;
procedure regex_find(
    text    varchar(8191) character set UTF8,
    pattern varchar(8191) character set UTF8,
    amount  integer,
    pass    integer
) returns (
    number integer,
    match  varchar(8191) character set UTF8
) external name
    'fb_regex!find'
engine
    udr;
function regex_find_first(
    text    varchar(8191) character set UTF8,
    pattern varchar(8191) character set UTF8,
    pass    integer
) returns varchar(8191) character set UTF8
external name
    'fb_regex!find_first'
engine
    udr;
function regex_replace(
    text        varchar(8191) character set UTF8,
    pattern     varchar(8191) character set UTF8,
    replacement varchar(8191) character set UTF8,
    amount      integer,
    pass        integer
) returns varchar(8191) character set UTF8
external name
    'fb_regex!replace'
engine
    udr;
procedure regex_split_words(
    text varchar(8191) character set UTF8
) returns (
    number integer,
    word   varchar(8191) character set UTF8
) external name
    'fb_regex!split_words'
engine
    udr;
procedure regex_split(
    text      varchar(8191) character set UTF8,
    separator varchar(8191) character set UTF8
) returns (
    number integer,
    part   varchar(8191) character set UTF8
) external name
    'fb_regex!split'
engine
    udr;
end
^

CREATE PACKAGE BODY FTS$MANAGEMENT
AS
^

CREATE PACKAGE BODY REGEX
AS
begin

procedure matches(
    text    varchar(8191) character set UTF8
  , pattern varchar(8191) character set UTF8
)returns(
    number  integer
  , groups  varchar(8191) character set UTF8
)external name
    'fb_regex!matches'
engine
    udr
;

procedure groups(
    groups varchar(8191) character set UTF8
)returns(
    number integer
  , origin integer
  , finish integer
)external name
    'fb_regex!groups'
engine
    udr
;

procedure find(
    text    varchar(8191) character set UTF8
  , pattern varchar(8191) character set UTF8
  , amount  integer
  , pass    integer
)returns(
    number  integer
  , match   varchar(8191) character set UTF8
)external name
    'fb_regex!find'
engine
    udr
;

function find_first(
    text    varchar(8191) character set UTF8
  , pattern varchar(8191) character set UTF8
  , pass    integer
)returns    varchar(8191) character set UTF8
external name
    'fb_regex!find_first'
engine
    udr
;

function replace(
    text        varchar(8191) character set UTF8
  , pattern     varchar(8191) character set UTF8
  , replacement varchar(8191) character set UTF8
  , amount      integer
  , pass        integer
)returns        varchar(8191) character set UTF8
external name
    'fb_regex!replace'
engine
    udr
;

procedure split_words(
    text   varchar(8191) character set UTF8
)returns(
    number integer
  , word   varchar(8191) character set UTF8
)external name
    'fb_regex!split_words'
engine
    udr
;

procedure split(
    text      varchar(8191) character set UTF8
  , separator varchar(8191) character set UTF8
)returns(
    number    integer
  , part      varchar(8191) character set UTF8
)external name
    'fb_regex!split'
engine
    udr
;

end
^

CREATE PACKAGE BODY TBUTILS
AS
^

SET TERM ;^
COMMIT WORK;
SET AUTODDL ON;
COMMIT WORK;
SET AUTODDL OFF;
SET TERM ^;

/* Stored procedure Bodies */

/* Stored Procedure: SHOW_LANGS, Owner: SYSDBA */

ALTER PROCEDURE SHOW_LANGS
(
  CODE VARCHAR(5) CHARACTER SET NONE,
  GRADE SMALLINT,
  CTY VARCHAR(15) CHARACTER SET NONE
)
RETURNS
(
  LANGUAGES VARCHAR(15) CHARACTER SET NONE
)
AS
DECLARE VARIABLE i INTEGER;
BEGIN
  i = 1;
  WHILE (i <= 5) DO
  BEGIN
    SELECT language_req[:i] FROM joB
    WHERE ((job_code = :code) AND (job_grade = :grade) AND (job_country = :cty)
           AND (language_req IS NOT NULL))
    INTO :languages;
    IF (languages = ' ') THEN  /* Prints 'NULL' instead of blanks */
       languages = 'NULL';         
    i = i +1;
    SUSPEND;
  END
END
^

/* Stored Procedure: ADD_EMP_PROJ, Owner: SYSDBA */

ALTER PROCEDURE ADD_EMP_PROJ
(
  EMP_NO SMALLINT,
  PROJ_ID CHAR(5) CHARACTER SET NONE
)
AS
BEGIN
	BEGIN
	INSERT INTO employee_project (emp_no, proj_id) VALUES (:emp_no, :proj_id);
	WHEN SQLCODE -530 DO
		EXCEPTION unknown_emp_id;
	END
	SUSPEND;
END
^

/* Stored Procedure: ALL_LANGS, Owner: SYSDBA */

ALTER PROCEDURE ALL_LANGS
RETURNS
(
  CODE VARCHAR(5) CHARACTER SET NONE,
  GRADE VARCHAR(5) CHARACTER SET NONE,
  COUNTRY VARCHAR(15) CHARACTER SET NONE,
  LANG VARCHAR(15) CHARACTER SET NONE
)
AS
BEGIN 
  	FOR SELECT job_code, job_grade, job_country FROM job  
  		INTO :code, :grade, :country 
  	DO 
  	BEGIN 
  	    FOR SELECT languages FROM show_langs  
   		    (:code, :grade, :country) INTO :lang DO 
  	        SUSPEND; 
  	    /* Put nice separators between rows */ 
  	    code = '====='; 
  	    grade = '====='; 
  	    country = '==============='; 
  	    lang = '=============='; 
  	    SUSPEND; 
  	END 
      END
^

/* Stored Procedure: DELETE_EMPLOYEE, Owner: SYSDBA */

ALTER PROCEDURE DELETE_EMPLOYEE
(
  EMP_NUM INTEGER
)
AS
DECLARE VARIABLE any_sales INTEGER;
BEGIN
	any_sales = 0;

	/*
	 *	If there are any sales records referencing this employee,
	 *	can't delete the employee until the sales are re-assigned
	 *	to another employee or changed to NULL.
	 */
	SELECT count(po_number)
	FROM sales
	WHERE sales_rep = :emp_num
	INTO :any_sales;

	IF (any_sales > 0) THEN
	BEGIN
		EXCEPTION reassign_sales;
		SUSPEND;
	END

	/*
	 *	If the employee is a manager, update the department.
	 */
	UPDATE department
	SET mngr_no = NULL
	WHERE mngr_no = :emp_num;

	/*
	 *	If the employee is a project leader, update project.
	 */
	UPDATE project
	SET team_leader = NULL
	WHERE team_leader = :emp_num;

	/*
	 *	Delete the employee from any projects.
	 */
	DELETE FROM employee_project
	WHERE emp_no = :emp_num;

	/*
	 *	Delete old salary records.
	 */
	DELETE FROM salary_history
	WHERE emp_no = :emp_num;

	/*
	 *	Delete the employee.
	 */
	DELETE FROM employee
	WHERE emp_no = :emp_num;

	SUSPEND;
END
^

/* Stored Procedure: DEPT_BUDGET, Owner: SYSDBA */

ALTER PROCEDURE DEPT_BUDGET
(
  DNO CHAR(3) CHARACTER SET NONE
)
RETURNS
(
  TOT DECIMAL(12,2)
)
AS
DECLARE VARIABLE sumb DECIMAL(12, 2);
	DECLARE VARIABLE rdno CHAR(3);
	DECLARE VARIABLE cnt INTEGER;
BEGIN
	tot = 0;

	SELECT budget FROM department WHERE dept_no = :dno INTO :tot;

	SELECT count(budget) FROM department WHERE head_dept = :dno INTO :cnt;

	IF (cnt = 0) THEN
		SUSPEND;

	FOR SELECT dept_no
		FROM department
		WHERE head_dept = :dno
		INTO :rdno
	DO
		BEGIN
			EXECUTE PROCEDURE dept_budget :rdno RETURNING_VALUES :sumb;
			tot = tot + sumb;
		END

	SUSPEND;
END
^

/* Stored Procedure: GET_EMP_PROJ, Owner: SYSDBA */

ALTER PROCEDURE GET_EMP_PROJ
(
  EMP_NO SMALLINT
)
RETURNS
(
  PROJ_ID CHAR(5) CHARACTER SET NONE
)
AS
BEGIN
	FOR SELECT proj_id
		FROM employee_project
		WHERE emp_no = :emp_no
		INTO :proj_id
	DO
		SUSPEND;
END
^

/* Stored Procedure: MAIL_LABEL, Owner: SYSDBA */

ALTER PROCEDURE MAIL_LABEL
(
  CUST_NO INTEGER
)
RETURNS
(
  LINE1 CHAR(40) CHARACTER SET NONE,
  LINE2 CHAR(40) CHARACTER SET NONE,
  LINE3 CHAR(40) CHARACTER SET NONE,
  LINE4 CHAR(40) CHARACTER SET NONE,
  LINE5 CHAR(40) CHARACTER SET NONE,
  LINE6 CHAR(40) CHARACTER SET NONE
)
AS
DECLARE VARIABLE customer	VARCHAR(25);
	DECLARE VARIABLE first_name		VARCHAR(15);
	DECLARE VARIABLE last_name		VARCHAR(20);
	DECLARE VARIABLE addr1		VARCHAR(30);
	DECLARE VARIABLE addr2		VARCHAR(30);
	DECLARE VARIABLE city		VARCHAR(25);
	DECLARE VARIABLE state		VARCHAR(15);
	DECLARE VARIABLE country	VARCHAR(15);
	DECLARE VARIABLE postcode	VARCHAR(12);
	DECLARE VARIABLE cnt		INTEGER;
BEGIN
	line1 = '';
	line2 = '';
	line3 = '';
	line4 = '';
	line5 = '';
	line6 = '';

	SELECT customer, contact_first, contact_last, address_line1,
		address_line2, city, state_province, country, postal_code
	FROM CUSTOMER
	WHERE cust_no = :cust_no
	INTO :customer, :first_name, :last_name, :addr1, :addr2,
		:city, :state, :country, :postcode;

	IF (customer IS NOT NULL) THEN
		line1 = customer;
	IF (first_name IS NOT NULL) THEN
		line2 = first_name || ' ' || last_name;
	ELSE
		line2 = last_name;
	IF (addr1 IS NOT NULL) THEN
		line3 = addr1;
	IF (addr2 IS NOT NULL) THEN
		line4 = addr2;

	IF (country = 'USA') THEN
	BEGIN
		IF (city IS NOT NULL) THEN
			line5 = city || ', ' || state || '  ' || postcode;
		ELSE
			line5 = state || '  ' || postcode;
	END
	ELSE
	BEGIN
		IF (city IS NOT NULL) THEN
			line5 = city || ', ' || state;
		ELSE
			line5 = state;
		line6 = country || '    ' || postcode;
	END

	SUSPEND;
END
^

/* Stored Procedure: MYPROC1, Owner: SYSDBA */

ALTER PROCEDURE MYPROC1
AS
Declare THECOUNTRY VarChar(32);
Begin
 /* Begin */
 THECOUNTRY = '';
  Update COUNTRY SET COUNTRY = 'None' Where COUNTRY = :THECOUNTRY;
 /* End */
End
^

/* Stored Procedure: MYPROC2, Owner: SYSDBA */

ALTER PROCEDURE MYPROC2
AS
Begin
  Update COUNTRY SET COUNTRY = 'None' Where COUNTRY = '';
End
^

/* Stored Procedure: MYPROC3, Owner: SYSDBA */

ALTER PROCEDURE MYPROC3
AS
Begin
  Update COUNTRY SET COUNTRY = 'None' Where COUNTRY = '';
End
^

/* Stored Procedure: ORG_CHART, Owner: SYSDBA */

ALTER PROCEDURE ORG_CHART
RETURNS
(
  HEAD_DEPT CHAR(25) CHARACTER SET NONE,
  DEPARTMENT CHAR(25) CHARACTER SET NONE,
  MNGR_NAME CHAR(20) CHARACTER SET NONE,
  TITLE CHAR(5) CHARACTER SET NONE,
  EMP_CNT INTEGER
)
AS
DECLARE VARIABLE mngr_no INTEGER;
	DECLARE VARIABLE dno CHAR(3);
BEGIN
	FOR SELECT h.department, d.department, d.mngr_no, d.dept_no
		FROM department d
		LEFT OUTER JOIN department h ON d.head_dept = h.dept_no
		ORDER BY d.dept_no
		INTO :head_dept, :department, :mngr_no, :dno
	DO
	BEGIN
		IF (:mngr_no IS NULL) THEN
		BEGIN
			mngr_name = '--TBH--';
			title = '';
		END

		ELSE
			SELECT full_name, job_code
			FROM employee
			WHERE emp_no = :mngr_no
			INTO :mngr_name, :title;

		SELECT COUNT(emp_no)
		FROM employee
		WHERE dept_no = :dno
		INTO :emp_cnt;

		SUSPEND;
	END
END
^

/* Stored Procedure: SHIP_ORDER, Owner: SYSDBA */

ALTER PROCEDURE SHIP_ORDER
(
  PO_NUM CHAR(8) CHARACTER SET NONE
)
AS
DECLARE VARIABLE ord_stat CHAR(7);
	DECLARE VARIABLE hold_stat CHAR(1);
	DECLARE VARIABLE cust_no INTEGER;
	DECLARE VARIABLE any_po CHAR(8);
BEGIN
	SELECT s.order_status, c.on_hold, c.cust_no
	FROM sales s, customer c
	WHERE po_number = :po_num
	AND s.cust_no = c.cust_no
	INTO :ord_stat, :hold_stat, :cust_no;

	/* This purchase order has been already shipped. */
	IF (ord_stat = 'shipped') THEN
	BEGIN
		EXCEPTION order_already_shipped;
		SUSPEND;
	END

	/*	Customer is on hold. */
	ELSE IF (hold_stat = '*') THEN
	BEGIN
		EXCEPTION customer_on_hold;
		SUSPEND;
	END

	/*
	 *	If there is an unpaid balance on orders shipped over 2 months ago,
	 *	put the customer on hold.
	 */
	FOR SELECT po_number
		FROM sales
		WHERE cust_no = :cust_no
		AND order_status = 'shipped'
		AND paid = 'n'
		AND ship_date < CAST('NOW' AS TIMESTAMP) - 60
		INTO :any_po
	DO
	BEGIN
		EXCEPTION customer_check;

		UPDATE customer
		SET on_hold = '*'
		WHERE cust_no = :cust_no;

		SUSPEND;
	END

	/*
	 *	Ship the order.
	 */
	UPDATE sales
	SET order_status = 'shipped', ship_date = 'NOW'
	WHERE po_number = :po_num;

	SUSPEND;
END
^

/* Stored Procedure: SUB_TOT_BUDGET, Owner: SYSDBA */

ALTER PROCEDURE SUB_TOT_BUDGET
(
  HEAD_DEPT CHAR(3) CHARACTER SET NONE
)
RETURNS
(
  TOT_BUDGET DECIMAL(12,2),
  AVG_BUDGET DECIMAL(12,2),
  MIN_BUDGET DECIMAL(12,2),
  MAX_BUDGET DECIMAL(12,2)
)
AS
BEGIN
	SELECT SUM(budget), AVG(budget), MIN(budget), MAX(budget)
		FROM department
		WHERE head_dept = :head_dept
		INTO :tot_budget, :avg_budget, :min_budget, :max_budget;
	SUSPEND;
END
^

SET TERM ;^
COMMIT WORK;
SET AUTODDL ON;
COMMIT WORK;
SET AUTODDL OFF;
SET TERM ^;

/* Stored Function Body */

ALTER FUNCTION FIND_FIRST (TEXT VARCHAR(8191) CHARACTER SET UTF8PATTERN VARCHAR(8191) CHARACTER SET UTF8PASS INTEGER)
 RETURNS VARCHAR(8191) CHARACTER SET UTF8
EXTERNAL NAME 'fb_regex!find_first                                                                                                                                                                                                                                            ' ENGINE UDR                            ^
^

ALTER FUNCTION FTS$GET_DIRECTORY
 RETURNS VARCHAR(255) CHARACTER SET UTF8
AS BEGIN END
^

ALTER FUNCTION FTS$HAS_ANALYZER (FTS$ANALYZER VARCHAR(63) CHARACTER SET UTF8)
 RETURNS BOOLEAN
AS BEGIN END
^

ALTER FUNCTION FTS$HAS_SYSTEM_ANALYZER (FTS$ANALYZER VARCHAR(63) CHARACTER SET UTF8)
 RETURNS BOOLEAN
AS BEGIN END
^

ALTER FUNCTION NATIV_FIND_FIRST (TEXT VARCHAR(8191) CHARACTER SET UTF8PATTERN VARCHAR(8191) CHARACTER SET UTF8PASS INTEGER)
 RETURNS VARCHAR(8191) CHARACTER SET UTF8
AS BEGIN END
^

ALTER FUNCTION NATIV_REPLACE (TEXT VARCHAR(8191) CHARACTER SET UTF8PATTERN VARCHAR(8191) CHARACTER SET UTF8REPLACEMENT VARCHAR(8191) CHARACTER SET UTF8AMOUNT INTEGERPASS INTEGER)
 RETURNS VARCHAR(8191) CHARACTER SET UTF8
AS BEGIN END
^

ALTER FUNCTION REGEX_FIND_FIRST (TEXT VARCHAR(8191) CHARACTER SET UTF8PATTERN VARCHAR(8191) CHARACTER SET UTF8PASS INTEGER)
 RETURNS VARCHAR(8191) CHARACTER SET UTF8
EXTERNAL NAME 'fb_regex!find_first                                                                                                                                                                                                                                            ' ENGINE UDR                            ^
^

ALTER FUNCTION REGEX_REPLACE (TEXT VARCHAR(8191) CHARACTER SET UTF8PATTERN VARCHAR(8191) CHARACTER SET UTF8REPLACEMENT VARCHAR(8191) CHARACTER SET UTF8AMOUNT INTEGERPASS INTEGER)
 RETURNS VARCHAR(8191) CHARACTER SET UTF8
EXTERNAL NAME 'fb_regex!replace                                                                                                                                                                                                                                               ' ENGINE UDR                            ^
^

ALTER FUNCTION REPLACE (TEXT VARCHAR(8191) CHARACTER SET UTF8PATTERN VARCHAR(8191) CHARACTER SET UTF8REPLACEMENT VARCHAR(8191) CHARACTER SET UTF8AMOUNT INTEGERPASS INTEGER)
 RETURNS VARCHAR(8191) CHARACTER SET UTF8
EXTERNAL NAME 'fb_regex!replace                                                                                                                                                                                                                                               ' ENGINE UDR                            ^
^

ALTER FUNCTION TB_UPPER (S VARCHAR(4000) CHARACTER SET NONE)
 RETURNS VARCHAR(4000) CHARACTER SET NONE
AS BEGIN END
^

SET TERM ;^
COMMIT WORK;
SET AUTODDL ON;

/* Comments on System Objects */


/* Grant Roles for this database */


/* Grant permissions for this database */

/* Access Rights on TABLE COUNTRY */
GRANT DELETE,INSERT,REFERENCES,SELECT,UPDATE ON TABLE COUNTRY TO USER PUBLIC  WITH GRANT OPTION ;
/* Access Rights on TABLE CUSTOMER */
GRANT DELETE,INSERT,REFERENCES,SELECT,UPDATE ON TABLE CUSTOMER TO USER PUBLIC  WITH GRANT OPTION ;
/* Access Rights on TABLE CaseSensitiveTable_FB3 */
/* Access Rights on TABLE DEPARTMENT */
GRANT DELETE,INSERT,REFERENCES,SELECT,UPDATE ON TABLE DEPARTMENT TO USER PUBLIC  WITH GRANT OPTION ;
/* Access Rights on TABLE EMPLOYEE */
GRANT DELETE,INSERT,REFERENCES,SELECT,UPDATE ON TABLE EMPLOYEE TO USER PUBLIC  WITH GRANT OPTION ;
/* Access Rights on TABLE EMPLOYEE_PROJECT */
GRANT DELETE,INSERT,REFERENCES,SELECT,UPDATE ON TABLE EMPLOYEE_PROJECT TO USER PUBLIC  WITH GRANT OPTION ;
/* Access Rights on TABLE JOB */
GRANT DELETE,INSERT,REFERENCES,SELECT,UPDATE ON TABLE JOB TO USER PUBLIC  WITH GRANT OPTION ;
/* Access Rights on TABLE PHONE_LIST */
GRANT DELETE,INSERT,REFERENCES,SELECT,UPDATE ON TABLE PHONE_LIST TO USER PUBLIC  WITH GRANT OPTION ;
/* Access Rights on TABLE PROJECT */
GRANT DELETE,INSERT,REFERENCES,SELECT,UPDATE ON TABLE PROJECT TO USER PUBLIC  WITH GRANT OPTION ;
/* Access Rights on TABLE PROJ_DEPT_BUDGET */
GRANT DELETE,INSERT,REFERENCES,SELECT,UPDATE ON TABLE PROJ_DEPT_BUDGET TO USER PUBLIC  WITH GRANT OPTION ;
/* Access Rights on TABLE SALARY_HISTORY */
GRANT DELETE,INSERT,REFERENCES,SELECT,UPDATE ON TABLE SALARY_HISTORY TO USER PUBLIC  WITH GRANT OPTION ;
/* Access Rights on TABLE SALES */
GRANT DELETE,INSERT,REFERENCES,SELECT,UPDATE ON TABLE SALES TO USER PUBLIC  WITH GRANT OPTION ;
/* Access Rights on PROCEDURE ADD_EMP_PROJ */
GRANT EXECUTE ON PROCEDURE ADD_EMP_PROJ TO USER PUBLIC  WITH GRANT OPTION ;
/* Access Rights on PROCEDURE ALL_LANGS */
GRANT EXECUTE ON PROCEDURE ALL_LANGS TO USER PUBLIC  WITH GRANT OPTION ;
/* Access Rights on PROCEDURE DELETE_EMPLOYEE */
GRANT EXECUTE ON PROCEDURE DELETE_EMPLOYEE TO USER PUBLIC  WITH GRANT OPTION ;
/* Access Rights on PROCEDURE DEPT_BUDGET */
GRANT EXECUTE ON PROCEDURE DEPT_BUDGET TO USER PUBLIC  WITH GRANT OPTION ;
/* Access Rights on PROCEDURE GET_EMP_PROJ */
GRANT EXECUTE ON PROCEDURE GET_EMP_PROJ TO USER PUBLIC  WITH GRANT OPTION ;
/* Access Rights on PROCEDURE MAIL_LABEL */
GRANT EXECUTE ON PROCEDURE MAIL_LABEL TO USER PUBLIC  WITH GRANT OPTION ;
/* Access Rights on PROCEDURE ORG_CHART */
GRANT EXECUTE ON PROCEDURE ORG_CHART TO USER PUBLIC  WITH GRANT OPTION ;
/* Access Rights on PROCEDURE SHIP_ORDER */
GRANT EXECUTE ON PROCEDURE SHIP_ORDER TO USER PUBLIC  WITH GRANT OPTION ;
/* Access Rights on PROCEDURE SHOW_LANGS */
GRANT EXECUTE ON PROCEDURE SHOW_LANGS TO USER PUBLIC  WITH GRANT OPTION ;
/* Access Rights on PROCEDURE SUB_TOT_BUDGET */
GRANT EXECUTE ON PROCEDURE SUB_TOT_BUDGET TO USER PUBLIC  WITH GRANT OPTION ;
