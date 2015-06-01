mysql -e "SELECT * FROM CustomData.CellLines" > CellLines_`(date +%Y%m%d%H%M)`.tab
mysql -e "SELECT * FROM CustomData.COMBO_Library" > COMBO_Library_`(date +%Y%m%d%H%M)`.tab
/bin/bash /home/CustomData/DumpFiles/COMBO_backup.sh
/bin/bash /home/CustomData/DumpFiles/CTG_backup.sh
/bin/bash /home/CustomData/DumpFiles/D1D4_backup.sh
mysqldump -uroot -p --no-data CustomData > CustomData_`(date +%Y%m%d%H%M)`.DDL

