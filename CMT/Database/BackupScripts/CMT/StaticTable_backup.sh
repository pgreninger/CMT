##
## These tables contain static data that is no longer being updated
## so no need for ongoing backup
##
# For mapping old plate layouts - empty tables
# mysql -e "SELECT * FROM CMT.Library" > Library1536HML_`(date +%Y%m%d%H%M)`.tab
# mysql -e "SELECT * FROM CMT.D1D4_PlateLayout" > D1D4_PlateLayout_`(date +%Y%m%d%H%M)`.tab
# mysql -e "SELECT * FROM CMT.D1D4_PositionMap" > D1D4_PositionMap_`(date +%Y%m%d%H%M)`.tab
mysql -e "SELECT * FROM CMT.MutationsStudy4" > MutationsStudy4_`(date +%Y%m%d%H%M)`.tab
mysql -e "SELECT * FROM CMT.spreadsheetResults" > spreadsheetResults_`(date +%Y%m%d%H%M)`.tab

