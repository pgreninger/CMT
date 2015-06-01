SELECT ProtocolID, DS_Version, DrugSet, AssayType, DrugDay, IFNULL(StartDate, '\\N'), IFNULL(EndDate, '\\N') FROM DrugSets;
