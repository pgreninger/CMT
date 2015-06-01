SELECT ID, Barcode, CellID, LibraryID, AnchorID, AnchorVer, Filename, MeasurementName, AcquistionName, 
MeasurementCreator, PlateCreator, IFNULL(AnnotationDate, '\\N'), MeasurementDate, PlateDate, 
IFNULL(AnnotationTimestamp, '\\N'), MeasurementTimestamp, PlateTimestamp, MeasurementDescription, 
PlateDescription, ExperimentSet, GUID, IFNULL(LaserFocusScore, '\\N'), PlateName, IFNULL(PID, '\\N'), 
MeasurementSettingsName, PlateID, X_Wells, Y_Wells, Modified, Created, IFNULL(QCScore, '\\N'), 
IFNULL(PassFail, '\\N') FROM COMBO_PlateData;
