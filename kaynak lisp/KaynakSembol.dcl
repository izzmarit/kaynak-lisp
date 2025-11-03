// KaynakSembol DCL Dosyasi
// AutoCAD 2025 icin Kaynak Sembolu Yerlestirme Sistemi
// AWS A2.4 Standardina Uyumlu
// V3.4 - METOD KAYNAK Ust ve Alt Parametreler Ayrildi

KaynakSembol : dialog {
    label = "Kaynak Sembolu Yerlestirme - AWS A2.4";
    initial_focus = "wps_no";
    
    : boxed_column {
        label = "Kaynak Yontemi Secimi";
        
        : popup_list {
            key = "kaynak_yontemi";
            label = "Kaynak Yontemi:";
            width = 40;
        }
        
        // METOD KAYNAK parametreleri (gizli baslar)
        : boxed_column {
            key = "metod_parametreler";
            label = "METOD KAYNAK Parametreleri";
            
            : column {
                label = "UST TARAF (Ok Tarafi)";
                
                : row {
                    : edit_box {
                        key = "ust_kaynak_adeti";
                        label = "Kaynak Adeti:";
                        edit_width = 10;
                        value = "20";
                    }
                    
                    : edit_box {
                        key = "ust_kaynak_uzunlugu";
                        label = "Kaynak Uzunlugu (mm):";
                        edit_width = 10;
                        value = "100";
                    }
                    
                    : edit_box {
                        key = "ust_kaynak_boslugu";
                        label = "Kaynak Boslugu (mm):";
                        edit_width = 10;
                        value = "100";
                    }
                }
            }
            
            : column {
                label = "ALT TARAF (Diger Taraf)";
                
                : row {
                    : edit_box {
                        key = "alt_kaynak_adeti";
                        label = "Kaynak Adeti:";
                        edit_width = 10;
                        value = "25";
                    }
                    
                    : edit_box {
                        key = "alt_kaynak_uzunlugu";
                        label = "Kaynak Uzunlugu (mm):";
                        edit_width = 10;
                        value = "100";
                    }
                    
                    : edit_box {
                        key = "alt_kaynak_boslugu";
                        label = "Kaynak Boslugu (mm):";
                        edit_width = 10;
                        value = "100";
                    }
                }
            }
        }
        
        : row {
            : column {
                label = "UST TARAF (Ok Tarafi)";
                width = 35;
                
                : popup_list {
                    key = "ust_yontem";
                    label = "Kaynak Tipi:";
                    width = 28;
                }
                
                : edit_box {
                    key = "ust_olcu";
                    label = "Kaynak Olcusu (mm):";
                    edit_width = 10;
                    value = "";
                }
                
                : popup_list {
                    key = "ust_kontur";
                    label = "Kontur:";
                    width = 28;
                }
                
                : popup_list {
                    key = "ust_yuzey";
                    label = "Yuzey Bitirme:";
                    width = 28;
                }
            }
            
            : column {
                label = "ALT TARAF (Diger Taraf)";
                width = 35;
                
                : popup_list {
                    key = "alt_yontem";
                    label = "Kaynak Tipi:";
                    width = 28;
                }
                
                : edit_box {
                    key = "alt_olcu";
                    label = "Kaynak Olcusu (mm):";
                    edit_width = 10;
                    value = "";
                }
                
                : popup_list {
                    key = "alt_kontur";
                    label = "Kontur:";
                    width = 28;
                }
                
                : popup_list {
                    key = "alt_yuzey";
                    label = "Yuzey Bitirme:";
                    width = 28;
                }
            }
        }
    }
    
    : boxed_column {
        label = "WPS ve Ozel Isaretler";
        
        : edit_box {
            key = "wps_no";
            label = "WPS Numarasi:";
            edit_width = 30;
            value = "";
        }
        
        : row {
            : toggle {
                key = "santiye_kaynak";
                label = "Santiyede Yapilacak (Bayrak)";
                value = "0";
            }
            
            : toggle {
                key = "cevre_kaynak";
                label = "Cevre Kaynak (Daire)";
                value = "0";
            }
        }
        
        : row {
            : toggle {
                key = "tam_nufuziyet";
                label = "Tam Nufuziyet (F.P.)";
                value = "0";
            }
            
            : toggle {
                key = "ndt_gerekli";
                label = "NDT Gerekli";
                value = "0";
            }
        }
    }
    
    : boxed_column {
        label = "Ek Bilgiler";
        
        : edit_box {
            key = "not1";
            label = "Not 1:";
            edit_width = 50;
            value = "";
        }
        
        : edit_box {
            key = "not2";
            label = "Not 2:";
            edit_width = 50;
            value = "";
        }
        
        : edit_box {
            key = "not3";
            label = "Not 3:";
            edit_width = 50;
            value = "";
        }
    }
    
    : row {
        fixed_width = true;
        alignment = centered;
        
        : button {
            key = "accept";
            label = "Tamam";
            is_default = true;
            width = 12;
            fixed_width = true;
        }
        
        : button {
            key = "cancel";
            label = "Iptal";
            is_cancel = true;
            width = 12;
            fixed_width = true;
        }
        
        : button {
            key = "help";
            label = "Yardim";
            width = 12;
            fixed_width = true;
        }
    }
}

// Yardim Dialog Kutusu
KaynakYardim : dialog {
    label = "Kaynak Sembolu Yardim";
    
    : text {
        label = "KAYNAK SEMBOLU YERLESTIRME SISTEMI";
        alignment = centered;
    }
    
    : text {
        label = "";
    }
    
    : text {
        label = "KAYNAK YONTEMLERI:";
    }
    
    : text {
        label = "STANDART - Normal kaynak sembolu (mevcut)";
    }
    
    : text {
        label = "METOD KAYNAK - Kesikli kaynak (52mm referans)";
    }
    
    : text {
        label = "  Ust ve Alt taraf icin ayri parametreler";
    }
    
    : text {
        label = "";
    }
    
    : text {
        label = "KAYNAK TIPLERI (AWS A2.4):";
    }
    
    : text {
        label = "YOK - Kaynak belirtilmemis";
    }
    
    : text {
        label = "FILLET - Kose kaynagi (en yaygin)";
    }
    
    : text {
        label = "V-GROOVE - Iki taraf pahlanmis";
    }
    
    : text {
        label = "BEVEL - Tek taraf pahlanmis";
    }
    
    : text {
        label = "U-GROOVE - Iki taraf icbukey";
    }
    
    : text {
        label = "J-GROOVE - Tek taraf icbukey";
    }
    
    : text {
        label = "SQUARE - Duz kenarli";
    }
    
    : text {
        label = "PLUG - Tapa kaynagi";
    }
    
    : text {
        label = "";
    }
    
    : text {
        label = "KONTUR SECENEKLERI:";
    }
    
    : text {
        label = "YOK - Kontur belirtilmemis";
    }
    
    : text {
        label = "DUZ - Duz yuzey";
    }
    
    : text {
        label = "DIS BOMBE - Disa dogru kavimli";
    }
    
    : text {
        label = "IC BOMBE - Ice dogru kavimli";
    }
    
    : text {
        label = "";
    }
    
    : text {
        label = "YUZEY BITIRME YONTEMLERI:";
    }
    
    : text {
        label = "YOK, G-Taslama, M-Isleme, C-Keskiyle, R-Silindir";
    }
    
    : text {
        label = "";
    }
    
    : text {
        label = "OZEL ISARETLER:";
    }
    
    : text {
        label = "Santiye Kaynagi: Bayrak simgesi ekler";
    }
    
    : text {
        label = "Cevre Kaynak: Ok noktasinda daire ekler";
    }
    
    : text {
        label = "Tam Nufuziyet: F.P. isareti ve daire ekler";
    }
    
    : text {
        label = "NDT Gerekli: NDT isareti ve daire ekler";
    }
    
    : text {
        label = "";
    }
    
    : text {
        label = "DUZENLEME: Sembole cift tikla ve duzenle";
    }
    
    ok_only;
}
