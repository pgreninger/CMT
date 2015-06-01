SELECT Barcode, CellID, IFNULL(LibraryID, '\\N'), AnchorID, IFNULL(AnchorVer, '\\N'), Filename, AssayID, ReadDate, ReadTime, ProtocolID, ProtocolName, 
Rows, Columns, Wells, ReaderSN, ExportSW, IFNULL(Notifications, '\\N'), Modified, Created, IFNULL(QCScore, '\\N'), IFNULL(PassFail, '\\N') FROM COMBO_PlateDataCTG;
