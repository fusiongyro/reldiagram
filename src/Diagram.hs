module Diagram where

import Database.HDBC
import Database.HDBC.PostgreSQL

{-

the query for all tables:

SELECT 
  tc.constraint_name, 
  tc.table_name as owning_table, 
  ctu.table_name as destination_table 
FROM 
  information_schema.table_constraints tc, 
  information_schema.constraint_table_usage ctu 
WHERE
  constraint_type = 'FOREIGN KEY' AND
  ctu.constraint_name = tc.constraint_name

the query for just one table:

SELECT 
  tc.constraint_name, 
  tc.table_name as owning_table, 
  ctu.table_name as destination_table 
FROM 
  information_schema.table_constraints tc, 
  information_schema.constraint_table_usage ctu 
WHERE
  constraint_type = 'FOREIGN KEY' AND
  ctu.constraint_name = tc.constraint_name AND
  (tc.table_name = '%s' OR ctu.table_name = '%s')


examine tc.table_schema for the schema

-}