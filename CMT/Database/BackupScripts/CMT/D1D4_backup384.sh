mysql -r CMT <Export_D1D4_PlateData384.sql > D1D4_PlateData384_`(date +%Y%m%d)`.tab
mysql -r CMT <Export_D1D4_WellData384.sql > D1D4_WellData384_`(date +%Y%m%d)`.tab
mysql -e "SELECT * FROM CMT.D1D4_ScreeningForms384" > D1D4_ScreeningForms384_`(date +%Y%m%d)`.tab
mysql -e "SELECT * FROM CMT.D1D4_SuspensionForms384" > D1D4_SuspensionForms384_`(date +%Y%m%d)`.tab
mysql -e "SELECT * FROM CMT.D1D4_CellCounts384" > D1D4_CellCounts384_`(date +%Y%m%d)`.tab

