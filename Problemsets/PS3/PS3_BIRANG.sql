.mode csv
.import /home/ouecon021/DScourseS20/Problemsets/PS3/FL_insurance_sample.csv d
SELECT * FROM df LIMIT 10;
SELECT DISTINCT county FROM df;
SELECT AVG(tiv_2012-tiv_2011) FROM df;
SELECT construction, COUNT(*) FROM df GROUP BY construction;
