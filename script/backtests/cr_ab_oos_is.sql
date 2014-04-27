--
-- ~/spy611/script/backtests/cr_ab_oos_is.sql
--

-- Demo:
-- ./psqlmad.bash -f cr_ab_oos_is.sql -v yr="'$YR'"
-- or
-- ./psqlmad.bash -f cr_ab_oos_is.sql -v yr="'2010'"

-- This script is called by cr_ab_oos_is.bash
-- Which is called by
-- ~/spy611/script/backtests/bt_ab_yr.bash

DROP   TABLE IF EXISTS x_is;
CREATE TABLE x_is AS
SELECT
cpma
,n1dg1
,n1dg2
,n1dg3
,n2dlagd
,n1wlagd
,n2wlagd
,n1mlagd
,n2mlagd
FROM my_vectors
WHERE ydate > (:yr||'-01-01')::DATE - (365 * 25) - 20
AND   ydate < (:yr||'-01-01')::DATE - 20
ORDER BY ydate
;

DROP   TABLE IF EXISTS y_is_n1dg;
CREATE TABLE y_is_n1dg AS
SELECT
CASE WHEN yvalue1d=false THEN -1 ELSE 1 END yvalue1d
FROM my_vectors
WHERE ydate > (:yr||'-01-01')::DATE - (365 * 25) - 20
AND   ydate < (:yr||'-01-01')::DATE - 20
ORDER BY ydate
;

DROP   TABLE IF EXISTS y_is_n2dg;
CREATE TABLE y_is_n2dg AS
SELECT
CASE WHEN yvalue2d=false THEN -1 ELSE 1 END yvalue2d
FROM my_vectors
WHERE ydate > (:yr||'-01-01')::DATE - (365 * 25) - 20
AND   ydate < (:yr||'-01-01')::DATE - 20
ORDER BY ydate
;



DROP   TABLE IF EXISTS y_is_n1wg;
CREATE TABLE y_is_n1wg AS
SELECT
CASE WHEN yvalue1w=false THEN -1 ELSE 1 END yvalue1w
FROM my_vectors
WHERE ydate > (:yr||'-01-01')::DATE - (365 * 25) - 20
AND   ydate < (:yr||'-01-01')::DATE - 20
ORDER BY ydate
;

-- oos tables:

DROP   TABLE IF EXISTS x_oos;
CREATE TABLE x_oos AS
SELECT
cpma
,n1dg1
,n1dg2
,n1dg3
,n2dlagd
,n1wlagd
,n2wlagd
,n1mlagd
,n2mlagd
FROM my_vectors
WHERE TO_CHAR(ydate,'YYYY') = :yr
ORDER BY ydate
;

DROP   TABLE IF EXISTS y_oos_n1dg;
CREATE TABLE y_oos_n1dg AS
SELECT
CASE WHEN yvalue1d=false THEN -1 ELSE 1 END yvalue1d
FROM my_vectors
WHERE TO_CHAR(ydate,'YYYY') = :yr
ORDER BY ydate
;

DROP   TABLE IF EXISTS y_oos_n2dg;
CREATE TABLE y_oos_n2dg AS
SELECT
CASE WHEN yvalue2d=false THEN -1 ELSE 1 END yvalue2d
FROM my_vectors
WHERE TO_CHAR(ydate,'YYYY') = :yr
ORDER BY ydate
;

DROP   TABLE IF EXISTS y_oos_n1wg;
CREATE TABLE y_oos_n1wg AS
SELECT
CASE WHEN yvalue1w=false THEN -1 ELSE 1 END yvalue1w
FROM my_vectors
WHERE TO_CHAR(ydate,'YYYY') = :yr
ORDER BY ydate
;

SELECT COUNT(*) FROM x_is;
SELECT COUNT(*) FROM x_oos;

COPY x_is TO '/tmp/x_is.csv' csv;
COPY x_oos TO '/tmp/x_oos.csv' csv;

COPY y_is_n1dg TO '/tmp/y_is_n1dg.csv' csv;
COPY y_is_n2dg TO '/tmp/y_is_n2dg.csv' csv;
COPY y_is_n1wg TO '/tmp/y_is_n1wg.csv' csv;
               	            
COPY y_oos_n1dg TO '/tmp/y_oos_n1dg.csv' csv;
COPY y_oos_n2dg TO '/tmp/y_oos_n2dg.csv' csv;
COPY y_oos_n1wg TO '/tmp/y_oos_n1wg.csv' csv;

-- I need to track the oos data for later reporting

DROP   TABLE IF EXISTS ab_track;
CREATE TABLE ab_track AS
SELECT
row_number() OVER (ORDER BY ydate) tnum
,CASE WHEN yvalue1d=false THEN -1 ELSE 1 END yvalue1d
,CASE WHEN yvalue2d=false THEN -1 ELSE 1 END yvalue2d
,CASE WHEN yvalue1w=false THEN -1 ELSE 1 END yvalue1w
,n1dg
,n2dg
,n1wg
,ydate
,cpma
,n1dg1
,n1dg2
,n1dg3
,n2dlagd
,n1wlagd
,n2wlagd
,n1mlagd
,n2mlagd
FROM my_vectors
WHERE TO_CHAR(ydate,'YYYY') = :yr
ORDER BY ydate
;

SELECT COUNT(*) FROM y_oos_n1dg;
SELECT MIN(ydate), COUNT(ydate), MAX(ydate) FROM ab_track;
-- Above 2 counts should match.
-- More Info:
SELECT MIN(ydate), COUNT(ydate), MAX(ydate) FROM my_vectors;
SELECT COUNT(*) FROM y_is_n1dg;
