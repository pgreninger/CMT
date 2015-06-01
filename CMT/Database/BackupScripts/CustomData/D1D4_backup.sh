mysql -r CustomData <Export_D1D4_PlateData.sql > D1D4_PlateData_`(date +%Y%m%d%H%M)`.tab
mysql -r CustomData <Export_D1D4_WellData.sql > D1D4_WellData_`(date +%Y%m%d%H%M)`.tab
mysql -e "SELECT * FROM CustomData.D1D4_ScreeningForms" > D1D4_ScreeningForms_`(date +%Y%m%d%H%M)`.tab

