mysql -r CustomData <Export_CTG_PlateData.sql > CTG_PlateData_`(date +%Y%m%d%H%M)`.tab
mysql -e "SELECT * FROM CustomData.COMBO_WellDataCTG" > CTG_WellData_`(date +%Y%m%d%H%M)`.tab

