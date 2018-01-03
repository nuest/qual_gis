select * from main_qual_gis where fid_citavi is null;

update main_qual_gis set fid_citavi = 478 where fid_citavi is null; -- set missing fid

--check if there are duplicated fid_citavi in main_qual_gis

select fid_citavi from main_qual_gis group by fid_citavi having ( count(fid_citavi) > 1 );

select * from main_qual_gis where fid_citavi in (63,116,249,48);

update main_qual_gis set fid_citavi = 479 where id_qualgis = 90; -- set missing new id
update main_qual_gis set fid_citavi = 480 where id_qualgis = 93; -- set missing new id
update main_qual_gis set fid_citavi = 481 where id_qualgis = 302; -- set missing new id
update main_qual_gis set fid_citavi = 482 where id_qualgis = 458; -- set missing new id

--check if there are duplicated wos or doi in abstracts
SELECT
    wos, id, COUNT(*)
FROM
    abstract
GROUP BY
    wos, id 
HAVING 
    COUNT(*) > 1;

SELECT
    doi,id , COUNT(*)
FROM
    abstract
GROUP BY
    doi, id
HAVING 
    COUNT(*) > 1;



