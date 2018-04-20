-- double fid_citavi

select * from main_qual_gis where fid_citavi = 48;

/* comment
id_qualgis == 302 -> fid_citavi 48
id_qualgis == 304 -> fid_citavi 48 change to 155
*/
update main_qual_gis set fid_citavi = 155 where id_qualgis = 304; -- set missing new id

select * from main_qual_gis where fid_citavi = 249;

/*
id_qualgis == 458 -> fid_citavi 249
id qualgis == 459 --> no wos/doi or pdf present --> delete
*/
delete from main_qual_gis where id_qualgis = 459;

--################################
-- fid_citavi 478 without reference
-- fid_citavi 478 --> no wos/doi or pdf present --> delete

delete from main_qual_gis where fid_citavi = 478;
delete from wos where id_citavi = 478;

