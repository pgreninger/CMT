mysql -r CMT <Export_COMBO_PlateData.sql > COMBO_PlateData_`(date +%Y%m%d%H%M)`.tab
mysql -r CMT <Export_COMBO_WellData.sql > COMBO_WellData_`(date +%Y%m%d%H%M)`.tab
mysql -e "SELECT * FROM CMT.COMBO_PolishedWellData" > COMBO_PolishedWellData_`(date +%Y%m%d%H%M)`.tab
mysql -e "SELECT * FROM CMT.COMBO_Library" > COMBO_Library_`(date +%Y%m%d%H%M)`.tab
mysql -e "SELECT * FROM CMT.COMBO_ScreeningForms" > COMBO_ScreeningForms_`(date +%Y%m%d%H%M)`.tab

