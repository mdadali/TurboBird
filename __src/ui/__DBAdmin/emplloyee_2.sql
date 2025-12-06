
/*  Index definitions for all user tables */

CREATE INDEX CUSTNAMEX ON CUSTOMER(CUSTOMER);
CREATE INDEX CUSTREGION ON CUSTOMER(COUNTRY, CITY);
CREATE DESCENDING INDEX BUDGETX ON DEPARTMENT(BUDGET);
CREATE INDEX NAMEX ON EMPLOYEE(LAST_NAME, FIRST_NAME);
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
CREATE EXCEPTION ORDER_ALREADY_SHIPPED 'Order status is "shipped."';
CREATE EXCEPTION REASSIGN_SALES 'Reassign the sales records before deleting this employee.';
CREATE EXCEPTION UNKNOWN_EMP_ID 'Invalid employee number or project id.';
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
AS BEGIN EXIT; END
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
AS BEGIN EXIT; END
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
AS BEGIN EXIT; END
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
	END

	/*	Customer is on hold. */
	ELSE IF (hold_stat = '*') THEN
	BEGIN
		EXCEPTION customer_on_hold;
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
	END

	/*
	 *	Ship the order.
	 */
	UPDATE sales
	SET order_status = 'shipped', ship_date = 'NOW'
	WHERE po_number = :po_num;
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

/* Comments on System Objects */


/* Grant Roles for this database */


/* Grant permissions for this database */

/* Access Rights on TABLE COUNTRY */
GRANT DELETE,INSERT,REFERENCES,SELECT,UPDATE ON TABLE COUNTRY TO USER PUBLIC  WITH GRANT OPTION ;
/* Access Rights on TABLE CUSTOMER */
GRANT DELETE,INSERT,REFERENCES,SELECT,UPDATE ON TABLE CUSTOMER TO USER PUBLIC  WITH GRANT OPTION ;
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
