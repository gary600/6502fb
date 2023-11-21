MEMORY {
    SRAM:       start = $0000,  size = $8000;
    EEPROM:     start = $E000,  size = $2000,   fillval = 0;
}

SEGMENTS {
    CODE:       load = EEPROM,  type = ro;
    VECTORS:    load = EEPROM,  type = ro,      start = $FFFA;
    ZEROPAGE:   load = SRAM,    type = zp,      start = $80; # incl scratch
}

FILES {
    %O: format = bin;
}