mysql -r CustomData <Export_COMBO_PlateData.sql > COMBO_PlateData_`(date +%Y%m%d%H%M)`.tab
mysql -r CustomData <Export_COMBO_WellData.sql > COMBO_WellData_`(date +%Y%m%d%H%M)`.tab
