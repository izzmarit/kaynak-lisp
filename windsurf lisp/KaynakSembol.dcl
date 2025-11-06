// ========================================================================
// KaynakSembol.dcl - Kaynak Sembolu Dialog Arayuzu V4.0
// AutoCAD 2025 LT icin - AWS A2.4 Standardi
// Optimize Edilmis ve Kullanici Dostu Versiyon
// Onizleme Penceresi Eklendi
// ========================================================================

KaynakSembol : dialog {
    label = "Kaynak Sembolu Yerlestirme - AWS A2.4 Standardi V4.0";
    initial_focus = "ust_yontem";
    
    : row {
        // SOL PANEL - PARAMETRELER
        : boxed_column {
            label = "Kaynak Parametreleri";
            width = 50;
            
            // KAYNAK YONTEMI SECIMI
            : boxed_column {
                label = "Kaynak Yontemi";
                
                : popup_list {
                    key = "kaynak_yontemi";
                    label = "Yontem:";
                    width = 40;
                }
            }
            
            // METOD KAYNAK PARAMETRELERI (Gizli baslar)
            : boxed_column {
                key = "metod_parametreler";
                label = "METOD KAYNAK Parametreleri (Kesikli Kaynak)";
                
                : column {
                    label = "UST TARAF (Ok Tarafi)";
                    
                    : edit_box {
                        key = "ust_kaynak_adeti";
                        label = "Adet:";
                        edit_width = 8;
                        value = "20";
                    }
                    
                    : edit_box {
                        key = "ust_kaynak_uzunlugu";
                        label = "Uzunluk (mm):";
                        edit_width = 8;
                        value = "100";
                    }
                    
                    : edit_box {
                        key = "ust_kaynak_boslugu";
                        label = "Bosluk (mm):";
                        edit_width = 8;
                        value = "100";
                    }
                }
                
                : column {
                    label = "ALT TARAF (Diger Taraf)";
                    
                    : edit_box {
                        key = "alt_kaynak_adeti";
                        label = "Adet:";
                        edit_width = 8;
                        value = "25";
                    }
                    
                    : edit_box {
                        key = "alt_kaynak_uzunlugu";
                        label = "Uzunluk (mm):";
                        edit_width = 8;
                        value = "100";
                    }
                    
                    : edit_box {
                        key = "alt_kaynak_boslugu";
                        label = "Bosluk (mm):";
                        edit_width = 8;
                        value = "100";
                    }
                }
            }
            
            // KAYNAK TIPLERI VE PARAMETRELER
            : boxed_row {
                label = "Kaynak Tipleri ve Parametreler";
                
                : column {
                    label = "UST TARAF (Ok Tarafi)";
                    
                    : popup_list {
                        key = "ust_yontem";
                        label = "Kaynak Tipi:";
                        width = 22;
                    }
                    
                    : edit_box {
                        key = "ust_olcu";
                        label = "Olcu (mm):";
                        edit_width = 10;
                        value = "6";
                    }
                    
                    : popup_list {
                        key = "ust_kontur";
                        label = "Kontur:";
                        width = 22;
                    }
                    
                    : popup_list {
                        key = "ust_yuzey";
                        label = "Yuzey Bitirme:";
                        width = 22;
                    }
                }
                
                : column {
                    label = "ALT TARAF (Diger Taraf)";
                    
                    : popup_list {
                        key = "alt_yontem";
                        label = "Kaynak Tipi:";
                        width = 22;
                    }
                    
                    : edit_box {
                        key = "alt_olcu";
                        label = "Olcu (mm):";
                        edit_width = 10;
                        value = "";
                    }
                    
                    : popup_list {
                        key = "alt_kontur";
                        label = "Kontur:";
                        width = 22;
                    }
                    
                    : popup_list {
                        key = "alt_yuzey";
                        label = "Yuzey Bitirme:";
                        width = 22;
                    }
                }
            }
            
            // WPS VE OZEL ISARETLER
            : boxed_column {
                label = "WPS ve Ozel Isaretler";
                
                : edit_box {
                    key = "wps_no";
                    label = "WPS Numarasi:";
                    edit_width = 40;
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
            
            // EK BILGILER
            : boxed_column {
                label = "Ek Bilgiler ve Notlar";
                
                : edit_box {
                    key = "not1";
                    label = "Not 1:";
                    edit_width = 40;
                    value = "";
                }
                
                : edit_box {
                    key = "not2";
                    label = "Not 2:";
                    edit_width = 40;
                    value = "";
                }
                
                : edit_box {
                    key = "not3";
                    label = "Not 3:";
                    edit_width = 40;
                    value = "";
                }
            }
        }
        
        // SAG PANEL - ONIZLEME VE ACIKLAMALAR
        : boxed_column {
            label = "Onizleme ve Aciklamalar";
            width = 35;
            
            // ONIZLEME ALANI
            : boxed_column {
                label = "Kaynak Sembolu Onizleme";
                
                : image {
                    key = "onizleme_img";
                    width = 50;
                    height = 15;
                    color = 0;
                    aspect_ratio = 0;
                    fixed_width = true;
                    fixed_height = true;
                }
                
                : text {
                    key = "onizleme_durum";
                    label = "Parametreleri secin...";
                    alignment = centered;
                }
            }
            
            // KAYNAK TIPLERI ACIKLAMALARI
            : boxed_column {
                label = "KAYNAK TIPLERI";
                
                : text {
                    label = "FILLET: Kose kaynagi (ucgen)";
                    alignment = left;
                }
                
                : text {
                    label = "V-GROOVE: V oluk (60 derece)";
                    alignment = left;
                }
                
                : text {
                    label = "BEVEL: Pahli (tek taraf)";
                    alignment = left;
                }
                
                : text {
                    label = "U-GROOVE: U oluk (ic bukey)";
                    alignment = left;
                }
                
                : text {
                    label = "J-GROOVE: J oluk (tek taraf)";
                    alignment = left;
                }
                
                : text {
                    label = "SQUARE: Kare kenar (duz)";
                    alignment = left;
                }
                
                : text {
                    label = "PLUG: Tapa kaynagi (delik)";
                    alignment = left;
                }
            }
            
            // KONTUR ACIKLAMALARI
            : boxed_column {
                label = "KONTUR SECENEKLERI";
                
                : text {
                    label = "DUZ: Duz yuzey (yatay cizgi)";
                    alignment = left;
                }
                
                : text {
                    label = "DIS BOMBE: Disa bukey (tasmis)";
                    alignment = left;
                }
                
                : text {
                    label = "IC BOMBE: Ice bukey (cukur)";
                    alignment = left;
                }
            }
            
            // YUZEY BITIRME ACIKLAMALARI
            : boxed_column {
                label = "YUZEY BITIRME YONTEMLERI";
                
                : text {
                    label = "G: Taslama (Grinding)";
                    alignment = left;
                }
                
                : text {
                    label = "M: Tezgah isleme (Machining)";
                    alignment = left;
                }
                
                : text {
                    label = "C: Keski (Chipping)";
                    alignment = left;
                }
                
                : text {
                    label = "R: Silindir (Rolling)";
                    alignment = left;
                }
                
                : text {
                    label = "H: Cekic (Hammering)";
                    alignment = left;
                }
                
                : text {
                    label = "P: Planya (Planing)";
                    alignment = left;
                }
            }
        }
    }
    
    // BUTONLAR
    : row {
        fixed_width = true;
        alignment = centered;
        
        : button {
            key = "accept";
            label = "TAMAM";
            is_default = true;
            width = 12;
            fixed_width = true;
        }
        
        : button {
            key = "cancel";
            label = "IPTAL";
            is_cancel = true;
            width = 12;
            fixed_width = true;
        }
        
        : button {
            key = "help";
            label = "YARDIM";
            width = 12;
            fixed_width = true;
        }
    }
}

// ========================================================================
// YARDIM DIALOG KUTUSU
// ========================================================================
// YARDIM DIALOG - KOMPAKT VERSIYON
KaynakYardim : dialog {
    label = "Kaynak Sembolu Yardim - AWS A2.4";
    width = 50;
    
    : boxed_column {
        label = "KAYNAK SEMBOLU PROGRAMI";
        
        : text {
            label = "AWS A2.4 standardina uygun kaynak sembolleri olusturur.";
        }
        : text {
            label = "Ust taraf (ok yonu) ve Alt taraf (diger taraf) ayri ayri tanimlanir.";
        }
    }
    
    : boxed_column {
        label = "KAYNAK TIPLERI";
        : text { label = "FILLET: Kose kaynagi (ucgen)"; }
        : text { label = "V-GROOVE: V oluklu (V sekli)"; }
        : text { label = "BEVEL: Pahli (dik+pahli)"; }
        : text { label = "U-GROOVE: U oluklu (U sekli)"; }
        : text { label = "J-GROOVE: J oluklu (J sekli)"; }
        : text { label = "SQUARE: Kare kenarli (dikdortgen)"; }
        : text { label = "PLUG: Tapa kaynagi (dolu)"; }
    }
    
    : boxed_column {
        label = "KONTUR VE YUZEY";
        : text { label = "DUZ: Duz yuzey (yatay cizgi)"; }
        : text { label = "DIS BOMBE: Disa bukey (yay)"; }
        : text { label = "IC BOMBE: Ice bukey (yay)"; }
        : text { label = ""; }
        : text { label = "G:Taslama M:Tezgah C:Keski"; }
        : text { label = "R:Silindir H:Cekic P:Planya"; }
    }
    
    : boxed_column {
        label = "KULLANIM";
        : text { label = "1. KS komutu > Kaynak konumu > Leader"; }
        : text { label = "2. Parametreleri girin"; }
        : text { label = "3. Onizlemeyi kontrol edin"; }
        : text { label = "4. TAMAM'a tiklayin"; }
        : text { label = ""; }
        : text { label = "DUZENLEME: Sembole cift tiklayin"; }
    }
    
    : boxed_column {
        label = "HAZIRLAYAN";
        
        : text {
            label = "Murat KARA tarafindan hazirlanmistir.";
            alignment = centered;
        }
        
        : text {
            label = "AutoCAD 2025 LT - AWS A2.4 Standardi";
            alignment = centered;
        }
        
        : text {
            label = "Versiyon 4.0";
            alignment = centered;
        }
    }
    
    ok_only;
}
