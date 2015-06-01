mysqldump -uroot -p --no-data CMT > CMT_`(date +%Y%m%d%H%M)`.DDL

mysql -e "SELECT * FROM CMT.T0_PlateData" > T0_PlateData_`(date +%Y%m%d%H%M)`.tab
mysql -e "SELECT * FROM CMT.T0_WellData" > T0_WellData_`(date +%Y%m%d%H%M)`.tab
mysql -e "SELECT * FROM CMT.ICdata" > ICdata_`(date +%Y%m%d%H%M)`.tab
mysql -r CMT <Export_DrugSets.sql > DrugSets_`(date +%Y%m%d%H%M)`.tab
mysql -e "SELECT * FROM CMT.CellLines" > CellLines_`(date +%Y%m%d%H%M)`.tab
mysql -r CMT <Export_SangerIDs.sql > SangerIDs_`(date +%Y%m%d%H%M)`.tab
mysql -e "SELECT * FROM CMT.CosmicID" > CosmicID_`(date +%Y%m%d%H%M)`.tab
/bin/bash /data/mysql/DumpFiles/CMT_backup.sh
/bin/bash /data/mysql/DumpFiles/COMBO_backup.sh
/bin/bash /data/mysql/DumpFiles/D1D4_backup.sh
/bin/bash /data/mysql/DumpFiles/HMS_backup.sh
