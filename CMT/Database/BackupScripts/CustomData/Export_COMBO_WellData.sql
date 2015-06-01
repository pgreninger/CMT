SELECT ID, CID, Instance, PlateID, RunSettingsID, SeriesID, WellID, SiteID, WellX, WellY, 
IFNULL(Concentration, '\\N'), IFNULL(Grp, '\\N'), IFNULL(Compound, '\\N'), 
IFNULL(Unit, '\\N'), IFNULL(WellName, '\\N'), X_Position, Y_Position, T_Position, 
FITC, AvgIntensity, DAPI, IFNULL(AnchorDrug, '\\N'), IFNULL(AnchorConc, '\\N'), 
Modified, Created FROM COMBO_WellData;
