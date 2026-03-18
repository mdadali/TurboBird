-- mon_tools.sql
-- Monitoring tools for Firebird: show attachments, transactions and statements.
-- ========================================================================
-- QUICK CHECKLIST (read BEFORE running any DELETE / KILL commands)
-- 1) Run the SELECT sections below first to identify the target IDs:
--      - MON$ATTACHMENT_ID for sessions (attachments)
--      - MON$TRANSACTION_ID for transactions
--      - MON$STATEMENT_ID for statements (informational)
-- 2) Verify the chosen ID(s) carefully. Copy/paste to avoid typos.
-- 3) Make sure you are connected as SYSDBA or the DB owner in a DIFFERENT
--    session than the one you plan to kill.
-- 4) Prefer deleting from MON$TRANSACTIONS (less disruptive) before deleting
--    from MON$ATTACHMENTS (terminates whole session).
-- 5) Do NOT run DELETE statements on production systems without coordination.
-- 6) Keep a backup and notify affected users — killing transactions can cause
--    lost/uncommitted work.
-- ========================================================================

/* ============================
   1) Show active attachments
   ============================ */
SELECT
  MON$ATTACHMENT_ID,
  MON$REMOTE_ADDRESS,
  MON$REMOTE_PID,
  MON$USER,
  MON$ROLE,
  MON$TIMESTAMP,
  MON$SYSTEM_FLAG
FROM MON$ATTACHMENTS
ORDER BY MON$TIMESTAMP DESC;

/* ================================
   2) Show active transactions
   ================================ */
SELECT
  MON$TRANSACTION_ID,
  MON$ATTACHMENT_ID,
  MON$STATE,
  MON$TIMESTAMP,
  MON$CONFLICT_COUNT
FROM MON$TRANSACTIONS
ORDER BY MON$TIMESTAMP DESC;

/* ================================
   3) Show active statements
   ================================ */
SELECT
  MON$STATEMENT_ID,
  MON$TRANSACTION_ID,
  MON$ATTACHMENT_ID,
  MON$STATE,
  MON$SQL_TEXT
FROM MON$STATEMENTS
ORDER BY MON$TIMESTAMP DESC;

/* ============================================================
   Helpful derived view: active statements with attachment info
   ============================================================ */
SELECT
  s.MON$STATEMENT_ID,
  s.MON$TRANSACTION_ID,
  s.MON$ATTACHMENT_ID,
  a.MON$REMOTE_ADDRESS,
  a.MON$USER,
  s.MON$STATE,
  s.MON$SQL_TEXT
FROM MON$STATEMENTS s
LEFT JOIN MON$ATTACHMENTS a ON s.MON$ATTACHMENT_ID = a.MON$ATTACHMENT_ID
ORDER BY s.MON$TIMESTAMP DESC;

/* ============================================================
   NOTE: The following commands will terminate transactions or sessions.
         They are commented out intentionally. To execute, REMOVE the
         leading comment characters and replace the placeholder ID.
         Only run as SYSDBA or DB owner. Use with extreme caution.
   ============================================================ */

-- Example: Kill a specific transaction (breaks all statements in that transaction)
-- DELETE FROM MON$TRANSACTIONS WHERE MON$TRANSACTION_ID = 98765;
-- COMMIT;

-- Example: Kill a specific attachment/session (terminates the connection)
-- DELETE FROM MON$ATTACHMENTS WHERE MON$ATTACHMENT_ID = 12345;
-- COMMIT;

-- End of mon_tools.sql
