SELECT ID, WellID, IFNULL(PlatePosition, '\\N'), IFNULL(CellID, '\\N'), PlateID, 
RunSettingsID, WellX, WellY, AvgIntensity, DAPI, Modified, Created FROM D1D4_WellData;
