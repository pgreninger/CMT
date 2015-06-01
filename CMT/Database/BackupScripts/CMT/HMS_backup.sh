mysql -r CMT <Export_HMS_PlateData.sql > HMS_PlateData_`(date +%Y%m%d%H%M)`.tab
mysql -e "SELECT * FROM CMT.HMS_WellData" > HMS_WellData_`(date +%Y%m%d%H%M)`.tab

