mysql -r CMT <Export_PlateData.sql > plateData_`(date +%Y%m%d%H%M)`.tab
mysql -e "SELECT * FROM CMT.WellData" > wellData_`(date +%Y%m%d%H%M)`.tab

