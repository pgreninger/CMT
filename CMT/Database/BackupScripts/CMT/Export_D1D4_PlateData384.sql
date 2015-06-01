SELECT ID, Barcode, Day, IFNULL(Tech, '\\N'), SeedDate, Filename, MeasurementName, AcquistionName, 
MeasurementCreator, PlateCreator, MeasurementDate, PlateDate, MeasurementTimestamp, PlateTimestamp, 
MeasurementDescription, PlateDescription, ExperimentSet, PlateName, MeasurementSettingsName, 
PlateID, X_Wells, Y_Wells, Modified, Created FROM D1D4_PlateData384;
