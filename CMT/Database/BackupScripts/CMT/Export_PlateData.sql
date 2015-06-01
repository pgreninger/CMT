SELECT Barcode, CellID, Filename, AssayID, ReadDate, ReadTime, ProtocolID, ProtocolName, 
Rows, Columns, Wells, ReaderSN, ExportSW, IFNULL(Notifications, '\\N'), Modified, Created, QCScore, IFNULL(PassFail, '\\N') FROM PlateData;
