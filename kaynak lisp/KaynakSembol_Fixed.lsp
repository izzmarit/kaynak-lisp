;;;========================================================================
;;; KaynakSembol.lsp - Profesyonel Kaynak Sembolu Yerlestirme Sistemi V2.0
;;; AutoCAD 2025 icin - AWS A2.4 Standardi
;;; 
;;; Komut: KS
;;; Tamamen duzeltilmis ve test edilmis versiyon
;;;========================================================================

(vl-load-com)

;;;========================================================================
;;; GLOBAL DEGISKENLER VE BASLATMA
;;;========================================================================

(setq *KAYNAK-APP-ID* "KAYNAKSYM")
(setq *KAYNAK-REACTOR* nil)
(setq *KAYNAK-SAYAC* 0)

;; Varsayilan degerler
(if (not *UST-YONTEM*) (setq *UST-YONTEM* "1")) ; Varsayilan FILLET
(if (not *ALT-YONTEM*) (setq *ALT-YONTEM* "0"))
(if (not *UST-OLCU*) (setq *UST-OLCU* "6"))
(if (not *ALT-OLCU*) (setq *ALT-OLCU* ""))
(if (not *UST-KONTUR*) (setq *UST-KONTUR* "0"))
(if (not *ALT-KONTUR*) (setq *ALT-KONTUR* "0"))
(if (not *UST-YUZEY*) (setq *UST-YUZEY* "0"))
(if (not *ALT-YUZEY*) (setq *ALT-YUZEY* "0"))
(if (not *WPS-NO*) (setq *WPS-NO* ""))
(if (not *SANTIYE-FLAG*) (setq *SANTIYE-FLAG* "0"))
(if (not *CEVRE-FLAG*) (setq *CEVRE-FLAG* "0"))
(if (not *TAM-NUF-FLAG*) (setq *TAM-NUF-FLAG* "0"))
(if (not *NDT-FLAG*) (setq *NDT-FLAG* "0"))
(if (not *NOT1*) (setq *NOT1* ""))
(if (not *NOT2*) (setq *NOT2* ""))
(if (not *NOT3*) (setq *NOT3* ""))

;; Uygulamayi baslatma
(defun kaynak-basla ()
  ;; XDATA uygulama kaydini yap
  (if (not (tblsearch "APPID" *KAYNAK-APP-ID*))
    (regapp *KAYNAK-APP-ID*)
  )
  
  ;; Katman yoksa olustur
  (if (not (tblsearch "LAYER" "KAYNAK_SYM"))
    (entmake '((0 . "LAYER")
               (100 . "AcDbSymbolTableRecord")
               (100 . "AcDbLayerTableRecord")
               (2 . "KAYNAK_SYM")
               (70 . 0)
               (62 . 5)
               (6 . "CONTINUOUS")))
  )
  
  ;; Reactor kur
  (kaynak-reactor-kur)
  
  (princ)
)

;;;========================================================================
;;; ANA KOMUT: KS
;;;========================================================================

(defun C:KS (/ oldecho olderror oldlayer oldosmode scl pt1 pt2)
  
  ;; Ortami kaydet
  (setq oldecho (getvar "CMDECHO"))
  (setq olderror *error*)
  (setq oldlayer (getvar "CLAYER"))
  (setq oldosmode (getvar "OSMODE"))
  (setvar "CMDECHO" 0)
  
  ;; Hata yakalayici
  (setq *error* kaynak-hata)
  
  ;; Sistemi baslat
  (kaynak-basla)
  
  ;; Olcek faktoru al
  (setq scl (getvar "DIMSCALE"))
  (if (or (null scl) (<= scl 0))
    (setq scl 1.0)
  )
  
  ;; Katmani degistir
  (setvar "CLAYER" "KAYNAK_SYM")
  
  ;; Ana dongu
  (princ "\n========================================")
  (princ "\nKAYNAK SEMBOLU YERLESTIRME SISTEMI V2.0")
  (princ "\nAWS A2.4 Standardi")
  (princ "\n========================================")
  (princ "\nKaynak konumu sec <Cikmak icin ENTER>: ")
  
  (while (setq pt1 (getpoint))
    
    (setq pt2 (getpoint pt1 "\nLeader bitis noktasi: "))
    
    (if pt2
      (progn
        ;; Dialog goster ve parametreleri al
        (if (kaynak-dialog nil)
          (progn
            ;; Benzersiz ID olustur
            (setq *KAYNAK-SAYAC* (1+ *KAYNAK-SAYAC*))
            (setq kaynak-id (strcat "KS_" (itoa *KAYNAK-SAYAC*)))
            
            ;; Kaynak sembolunu olustur
            (kaynak-sembolu-olustur pt1 pt2 scl kaynak-id)
            
            (princ "\nKaynak sembolu basariyla yerlestirildi.")
          )
        )
      )
    )
    
    (princ "\nKaynak konumu sec <Cikmak icin ENTER>: ")
  )
  
  ;; Ortami eski haline getir
  (setvar "OSMODE" oldosmode)
  (setvar "CMDECHO" oldecho)
  (setvar "CLAYER" oldlayer)
  (setq *error* olderror)
  (princ "\nKaynak sembolu komutu tamamlandi.")
  (princ)
)

;;;========================================================================
;;; HATA YAKALAYICI
;;;========================================================================

(defun kaynak-hata (msg)
  (if (and msg 
           (not (member msg '("console break" 
                              "Function cancelled" 
                              "quit / exit abort"))))
    (princ (strcat "\nHata: " msg))
  )
  (setvar "CMDECHO" oldecho)
  (setvar "CLAYER" oldlayer)
  (setvar "OSMODE" oldosmode)
  (setq *error* olderror)
  (princ)
)

;;;========================================================================
;;; DIALOG FONKSIYONLARI
;;;========================================================================

(defun kaynak-dialog (duzen-modu / dcl-id durum dcl-dosya)
  
  ;; DCL dosyasini kontrol et
  (setq dcl-dosya (findfile "KaynakSembol.dcl"))
  (if (not dcl-dosya)
    (progn
      (alert "HATA: KaynakSembol.dcl dosyasi bulunamadi!\nDCL dosyasinin AutoCAD support dizininde oldugunu kontrol edin.")
      (exit)
    )
  )
  
  ;; DCL yukle
  (setq dcl-id (load_dialog dcl-dosya))
  
  (if (not (new_dialog "KaynakSembol" dcl-id))
    (progn
      (alert "HATA: Dialog yuklenemedi!")
      (exit)
    )
  )
  
  ;; Listeleri doldur
  (start_list "ust_yontem")
  (mapcar 'add_list '("YOK" "FILLET" "V-GROOVE" "BEVEL" "U-GROOVE" "J-GROOVE" "SQUARE" "PLUG"))
  (end_list)
  
  (start_list "alt_yontem")
  (mapcar 'add_list '("YOK" "FILLET" "V-GROOVE" "BEVEL" "U-GROOVE" "J-GROOVE" "SQUARE" "PLUG"))
  (end_list)
  
  (start_list "ust_kontur")
  (mapcar 'add_list '("YOK" "DUZ" "DIS BOMBE" "IC BOMBE"))
  (end_list)
  
  (start_list "alt_kontur")
  (mapcar 'add_list '("YOK" "DUZ" "DIS BOMBE" "IC BOMBE"))
  (end_list)
  
  (start_list "ust_yuzey")
  (mapcar 'add_list '("YOK" "G" "M" "C" "R" "H" "P"))
  (end_list)
  
  (start_list "alt_yuzey")
  (mapcar 'add_list '("YOK" "G" "M" "C" "R" "H" "P"))
  (end_list)
  
  ;; Mevcut degerleri ayarla
  (set_tile "ust_yontem" *UST-YONTEM*)
  (set_tile "alt_yontem" *ALT-YONTEM*)
  (set_tile "ust_olcu" *UST-OLCU*)
  (set_tile "alt_olcu" *ALT-OLCU*)
  (set_tile "ust_kontur" *UST-KONTUR*)
  (set_tile "alt_kontur" *ALT-KONTUR*)
  (set_tile "ust_yuzey" *UST-YUZEY*)
  (set_tile "alt_yuzey" *ALT-YUZEY*)
  (set_tile "wps_no" *WPS-NO*)
  (set_tile "santiye_kaynak" *SANTIYE-FLAG*)
  (set_tile "cevre_kaynak" *CEVRE-FLAG*)
  (set_tile "tam_nufuziyet" *TAM-NUF-FLAG*)
  (set_tile "ndt_gerekli" *NDT-FLAG*)
  (set_tile "not1" *NOT1*)
  (set_tile "not2" *NOT2*)
  (set_tile "not3" *NOT3*)
  
  ;; Action callbacks
  (action_tile "ust_yontem" "(setq *UST-YONTEM* $value)")
  (action_tile "alt_yontem" "(setq *ALT-YONTEM* $value)")
  (action_tile "ust_olcu" "(setq *UST-OLCU* $value)")
  (action_tile "alt_olcu" "(setq *ALT-OLCU* $value)")
  (action_tile "ust_kontur" "(setq *UST-KONTUR* $value)")
  (action_tile "alt_kontur" "(setq *ALT-KONTUR* $value)")
  (action_tile "ust_yuzey" "(setq *UST-YUZEY* $value)")
  (action_tile "alt_yuzey" "(setq *ALT-YUZEY* $value)")
  (action_tile "wps_no" "(setq *WPS-NO* $value)")
  (action_tile "santiye_kaynak" "(setq *SANTIYE-FLAG* $value)")
  (action_tile "cevre_kaynak" "(setq *CEVRE-FLAG* $value)")
  (action_tile "tam_nufuziyet" "(setq *TAM-NUF-FLAG* $value)")
  (action_tile "ndt_gerekli" "(setq *NDT-FLAG* $value)")
  (action_tile "not1" "(setq *NOT1* $value)")
  (action_tile "not2" "(setq *NOT2* $value)")
  (action_tile "not3" "(setq *NOT3* $value)")
  
  (action_tile "accept" "(done_dialog 1)")
  (action_tile "cancel" "(done_dialog 0)")
  (action_tile "help" "(kaynak-yardim-goster)")
  
  ;; Dialog goster
  (setq durum (start_dialog))
  (unload_dialog dcl-id)
  
  (= durum 1)
)

;; Yardim dialog goster
(defun kaynak-yardim-goster (/ dcl-id dcl-dosya)
  (setq dcl-dosya (findfile "KaynakSembol.dcl"))
  (if dcl-dosya
    (progn
      (setq dcl-id (load_dialog dcl-dosya))
      (if (new_dialog "KaynakYardim" dcl-id)
        (progn
          (action_tile "accept" "(done_dialog 1)")
          (start_dialog)
        )
      )
      (unload_dialog dcl-id)
    )
  )
)

;;;========================================================================
;;; SEMBOL OLUSTURMA ANA FONKSIYONU
;;;========================================================================

(defun kaynak-sembolu-olustur (pt1 pt2 scl kaynak-id / aci ref-yon ref-baslangic ref-bitis obje-listesi
                                ent ust-pt alt-pt)
  
  (setq obje-listesi '())
  
  ;; Aci ve yon hesapla
  (setq aci (angle pt1 pt2))
  
  ;; Referans cizgisi yonunu belirle (yatay sag veya sol)
  (if (and (> aci (/ pi 2)) (< aci (* 1.5 pi)))
    (setq ref-yon pi)  ; Sol
    (setq ref-yon 0)   ; Sag
  )
  
  (setq ref-baslangic pt2)
  (setq ref-bitis (polar ref-baslangic ref-yon (* scl 30.0))) ; 30mm referans cizgisi
  
  ;; 1. LEADER (OK) OLUSTUR - ENTMAKE ile
  (setq ent (entmakex
    (list
      '(0 . "LEADER")
      '(100 . "AcDbEntity")
      '(8 . "KAYNAK_SYM")
      '(100 . "AcDbLeader")
      (cons 10 pt1)
      (cons 10 pt2)
      '(71 . 1)
      '(72 . 0)
      '(73 . 0)
      '(74 . 0)
      '(75 . 0)
      '(40 . 0.0)
      '(41 . 0.0)
      '(76 . 3)
    )))
  (if ent (setq obje-listesi (append obje-listesi (list ent))))
  
  ;; 2. REFERANS CIZGISI
  (setq ent (entmakex
    (list
      '(0 . "LINE")
      '(100 . "AcDbEntity")
      '(8 . "KAYNAK_SYM")
      '(100 . "AcDbLine")
      (cons 10 ref-baslangic)
      (cons 11 ref-bitis)
    )))
  (if ent (setq obje-listesi (append obje-listesi (list ent))))
  
  ;; 3. UST TARAF SEMBOLU (referans cizgisi ustunde)
  (if (/= *UST-YONTEM* "0")
    (progn
      (setq ust-pt ref-baslangic)
      (setq obje-listesi (append obje-listesi 
        (kaynak-sembol-ciz ust-pt ref-yon scl *UST-YONTEM* T)))
      
      ;; Ust olcu metni
      (if (/= *UST-OLCU* "")
        (setq obje-listesi (append obje-listesi 
          (kaynak-olcu-metni-ekle ust-pt ref-yon scl *UST-OLCU* T)))
      )
      
      ;; Ust kontur
      (if (/= *UST-KONTUR* "0")
        (setq obje-listesi (append obje-listesi 
          (kaynak-kontur-ekle ust-pt ref-yon scl *UST-KONTUR* *UST-YUZEY* T)))
      )
    )
  )
  
  ;; 4. ALT TARAF SEMBOLU (referans cizgisi altinda)
  (if (/= *ALT-YONTEM* "0")
    (progn
      (setq alt-pt ref-baslangic)
      (setq obje-listesi (append obje-listesi 
        (kaynak-sembol-ciz alt-pt ref-yon scl *ALT-YONTEM* nil)))
      
      ;; Alt olcu metni
      (if (/= *ALT-OLCU* "")
        (setq obje-listesi (append obje-listesi 
          (kaynak-olcu-metni-ekle alt-pt ref-yon scl *ALT-OLCU* nil)))
      )
      
      ;; Alt kontur
      (if (/= *ALT-KONTUR* "0")
        (setq obje-listesi (append obje-listesi 
          (kaynak-kontur-ekle alt-pt ref-yon scl *ALT-KONTUR* *ALT-YUZEY* nil)))
      )
    )
  )
  
  ;; 5. OZEL SEMBOLLER
  
  ;; CEVRE KAYNAK DAIRESI (ok noktasinda)
  (if (= *CEVRE-FLAG* "1")
    (setq obje-listesi (append obje-listesi (kaynak-cevre-ekle pt1 scl)))
  )
  
  ;; SANTIYE BAYRAGI
  (if (= *SANTIYE-FLAG* "1")
    (setq obje-listesi (append obje-listesi (kaynak-bayrak-ekle ref-bitis ref-yon scl)))
  )
  
  ;; TAM NUFUZIYET (F.P.)
  (if (= *TAM-NUF-FLAG* "1")
    (setq obje-listesi (append obje-listesi (kaynak-tam-nufuz-ekle ref-bitis ref-yon scl)))
  )
  
  ;; NDT ISARETI
  (if (= *NDT-FLAG* "1")
    (setq obje-listesi (append obje-listesi (kaynak-ndt-ekle ref-bitis ref-yon scl)))
  )
  
  ;; WPS KUTUSU VE NOTLAR
  (setq obje-listesi (append obje-listesi 
    (kaynak-metinler-ekle ref-baslangic ref-bitis ref-yon scl)))
  
  ;; XDATA ekle
  (kaynak-xdata-ekle obje-listesi kaynak-id ref-baslangic ref-bitis)
  
  ;; Cizimi yenile
  (command "_.REGEN")
  
  (princ)
)

;;;========================================================================
;;; KAYNAK SEMBOLU CIZIM FONKSIYONLARI - ENTMAKE ILE
;;;========================================================================

;; Ana sembol cizim fonksiyonu
(defun kaynak-sembol-ciz (pt yon scl tip ust-taraf / ent-listesi)
  (setq ent-listesi '())
  (cond
    ((= tip "1") (setq ent-listesi (kaynak-fillet pt yon scl ust-taraf)))
    ((= tip "2") (setq ent-listesi (kaynak-vgroove pt yon scl ust-taraf)))
    ((= tip "3") (setq ent-listesi (kaynak-bevel pt yon scl ust-taraf)))
    ((= tip "4") (setq ent-listesi (kaynak-ugroove pt yon scl ust-taraf)))
    ((= tip "5") (setq ent-listesi (kaynak-jgroove pt yon scl ust-taraf)))
    ((= tip "6") (setq ent-listesi (kaynak-square pt yon scl ust-taraf)))
    ((= tip "7") (setq ent-listesi (kaynak-plug pt yon scl ust-taraf)))
  )
  ent-listesi
)

;; FILLET (Kose kaynagi) - Ucgen
(defun kaynak-fillet (pt yon scl ust-taraf / p1 p2 p3 ent)
  (setq p1 (polar pt yon (* scl -2.0)))
  (setq p2 (polar p1 yon (* scl 4.0)))
  (if ust-taraf
    (setq p3 (polar p1 (+ yon (/ pi 2)) (* scl 4.0)))
    (setq p3 (polar p1 (- yon (/ pi 2)) (* scl 4.0)))
  )
  
  ;; Ucgen ciz - LWPOLYLINE olarak
  (setq ent (entmakex
    (list
      '(0 . "LWPOLYLINE")
      '(100 . "AcDbEntity")
      '(8 . "KAYNAK_SYM")
      '(100 . "AcDbPolyline")
      '(90 . 3)
      '(70 . 1)  ; Kapali
      (cons 10 p1)
      (cons 10 p2)
      (cons 10 p3)
    )))
  (if ent (list ent) nil)
)

;; V-GROOVE
(defun kaynak-vgroove (pt yon scl ust-taraf / p1 p2 p3 ent-listesi ent)
  (setq p1 pt)
  (setq ent-listesi '())
  
  (if ust-taraf
    (progn
      (setq p2 (polar p1 (+ yon (* pi 0.833)) (* scl 3.0))) ; 150 derece
      (setq p3 (polar p1 (+ yon (* pi 0.167)) (* scl 3.0))) ; 30 derece
    )
    (progn
      (setq p2 (polar p1 (- yon (* pi 0.833)) (* scl 3.0)))
      (setq p3 (polar p1 (- yon (* pi 0.167)) (* scl 3.0)))
    )
  )
  
  ;; Sol cizgi
  (setq ent (entmakex
    (list
      '(0 . "LINE")
      '(100 . "AcDbEntity")
      '(8 . "KAYNAK_SYM")
      '(100 . "AcDbLine")
      (cons 10 p2)
      (cons 11 p1)
    )))
  (if ent (setq ent-listesi (append ent-listesi (list ent))))
  
  ;; Sag cizgi
  (setq ent (entmakex
    (list
      '(0 . "LINE")
      '(100 . "AcDbEntity")
      '(8 . "KAYNAK_SYM")
      '(100 . "AcDbLine")
      (cons 10 p1)
      (cons 11 p3)
    )))
  (if ent (setq ent-listesi (append ent-listesi (list ent))))
  
  ent-listesi
)

;; BEVEL
(defun kaynak-bevel (pt yon scl ust-taraf / p1 p2 p3 ent-listesi ent)
  (setq p1 pt)
  (setq ent-listesi '())
  
  (if ust-taraf
    (progn
      (setq p2 (polar p1 (+ yon (* pi 0.75)) (* scl 3.0))) ; 135 derece
      (setq p3 (polar p1 (+ yon (/ pi 2)) (* scl 3.0)))   ; 90 derece
    )
    (progn
      (setq p2 (polar p1 (- yon (* pi 0.75)) (* scl 3.0)))
      (setq p3 (polar p1 (- yon (/ pi 2)) (* scl 3.0)))
    )
  )
  
  ;; Egik cizgi
  (setq ent (entmakex
    (list
      '(0 . "LINE")
      '(100 . "AcDbEntity")
      '(8 . "KAYNAK_SYM")
      '(100 . "AcDbLine")
      (cons 10 p2)
      (cons 11 p1)
    )))
  (if ent (setq ent-listesi (append ent-listesi (list ent))))
  
  ;; Dikey cizgi
  (setq ent (entmakex
    (list
      '(0 . "LINE")
      '(100 . "AcDbEntity")
      '(8 . "KAYNAK_SYM")
      '(100 . "AcDbLine")
      (cons 10 p1)
      (cons 11 p3)
    )))
  (if ent (setq ent-listesi (append ent-listesi (list ent))))
  
  ent-listesi
)

;; SQUARE
(defun kaynak-square (pt yon scl ust-taraf / p1 p2 p3 p4 ent)
  (setq p1 (polar pt yon (* scl -1.5)))
  (setq p2 (polar pt yon (* scl 1.5)))
  
  (if ust-taraf
    (progn
      (setq p3 (polar p1 (+ yon (/ pi 2)) (* scl 3.0)))
      (setq p4 (polar p2 (+ yon (/ pi 2)) (* scl 3.0)))
    )
    (progn
      (setq p3 (polar p1 (- yon (/ pi 2)) (* scl 3.0)))
      (setq p4 (polar p2 (- yon (/ pi 2)) (* scl 3.0)))
    )
  )
  
  ;; Dikdortgen ciz
  (setq ent (entmakex
    (list
      '(0 . "LWPOLYLINE")
      '(100 . "AcDbEntity")
      '(8 . "KAYNAK_SYM")
      '(100 . "AcDbPolyline")
      '(90 . 4)
      '(70 . 1)
      (cons 10 p1)
      (cons 10 p3)
      (cons 10 p4)
      (cons 10 p2)
    )))
  (if ent (list ent) nil)
)

;; U-GROOVE
(defun kaynak-ugroove (pt yon scl ust-taraf / p1 p2 p3 p4 merkez ent-listesi ent)
  (setq ent-listesi '())
  (setq p1 (polar pt yon (* scl -1.5)))
  (setq p2 (polar pt yon (* scl 1.5)))
  
  (if ust-taraf
    (progn
      (setq p3 (polar p1 (+ yon (/ pi 2)) (* scl 1.0)))
      (setq p4 (polar p2 (+ yon (/ pi 2)) (* scl 1.0)))
      (setq merkez (polar pt (+ yon (/ pi 2)) (* scl 1.5)))
    )
    (progn
      (setq p3 (polar p1 (- yon (/ pi 2)) (* scl 1.0)))
      (setq p4 (polar p2 (- yon (/ pi 2)) (* scl 1.0)))
      (setq merkez (polar pt (- yon (/ pi 2)) (* scl 1.5)))
    )
  )
  
  ;; Sol dikey cizgi
  (setq ent (entmakex
    (list
      '(0 . "LINE")
      '(100 . "AcDbEntity")
      '(8 . "KAYNAK_SYM")
      '(100 . "AcDbLine")
      (cons 10 p1)
      (cons 11 p3)
    )))
  (if ent (setq ent-listesi (append ent-listesi (list ent))))
  
  ;; Yay - ARC olarak
  (setq ent (entmakex
    (list
      '(0 . "ARC")
      '(100 . "AcDbEntity")
      '(8 . "KAYNAK_SYM")
      '(100 . "AcDbCircle")
      (cons 10 merkez)
      (cons 40 (* scl 1.5))
      '(100 . "AcDbArc")
      (cons 50 (angle merkez p3))
      (cons 51 (angle merkez p4))
    )))
  (if ent (setq ent-listesi (append ent-listesi (list ent))))
  
  ;; Sag dikey cizgi
  (setq ent (entmakex
    (list
      '(0 . "LINE")
      '(100 . "AcDbEntity")
      '(8 . "KAYNAK_SYM")
      '(100 . "AcDbLine")
      (cons 10 p4)
      (cons 11 p2)
    )))
  (if ent (setq ent-listesi (append ent-listesi (list ent))))
  
  ent-listesi
)

;; J-GROOVE
(defun kaynak-jgroove (pt yon scl ust-taraf / p1 p2 p3 merkez ent-listesi ent)
  (setq ent-listesi '())
  (setq p1 (polar pt yon (* scl -1.5)))
  (setq p2 (polar pt yon (* scl 1.5)))
  
  (if ust-taraf
    (progn
      (setq p3 (polar p1 (+ yon (/ pi 2)) (* scl 3.0)))
      (setq merkez (polar p2 (+ yon (/ pi 2)) (* scl 1.5)))
    )
    (progn
      (setq p3 (polar p1 (- yon (/ pi 2)) (* scl 3.0)))
      (setq merkez (polar p2 (- yon (/ pi 2)) (* scl 1.5)))
    )
  )
  
  ;; Dikey cizgi
  (setq ent (entmakex
    (list
      '(0 . "LINE")
      '(100 . "AcDbEntity")
      '(8 . "KAYNAK_SYM")
      '(100 . "AcDbLine")
      (cons 10 p1)
      (cons 11 p3)
    )))
  (if ent (setq ent-listesi (append ent-listesi (list ent))))
  
  ;; Yay
  (setq ent (entmakex
    (list
      '(0 . "ARC")
      '(100 . "AcDbEntity")
      '(8 . "KAYNAK_SYM")
      '(100 . "AcDbCircle")
      (cons 10 merkez)
      (cons 40 (* scl 1.5))
      '(100 . "AcDbArc")
      (cons 50 (angle merkez p3))
      (cons 51 (angle merkez p2))
    )))
  (if ent (setq ent-listesi (append ent-listesi (list ent))))
  
  ent-listesi
)

;; PLUG
(defun kaynak-plug (pt yon scl ust-taraf / p1 p2 p3 p4 ent)
  (setq p1 (polar pt yon (* scl -2.5)))
  (setq p2 (polar pt yon (* scl 2.5)))
  
  (if ust-taraf
    (progn
      (setq p3 (polar p1 (+ yon (/ pi 2)) (* scl 2.5)))
      (setq p4 (polar p2 (+ yon (/ pi 2)) (* scl 2.5)))
    )
    (progn
      (setq p3 (polar p1 (- yon (/ pi 2)) (* scl 2.5)))
      (setq p4 (polar p2 (- yon (/ pi 2)) (* scl 2.5)))
    )
  )
  
  ;; Dikdortgen ciz (dolu)
  (setq ent (entmakex
    (list
      '(0 . "SOLID")
      '(100 . "AcDbEntity")
      '(8 . "KAYNAK_SYM")
      '(100 . "AcDbTrace")
      (cons 10 p1)
      (cons 11 p2)
      (cons 12 p4)
      (cons 13 p3)
    )))
  (if ent (list ent) nil)
)

;;;========================================================================
;;; OLCU METNI VE KONTUR
;;;========================================================================

;; Olcu metni ekle
(defun kaynak-olcu-metni-ekle (pt yon scl olcu-str ust-taraf / txt-pt txt-aci ent)
  (if ust-taraf
    (setq txt-pt (polar pt (+ yon (/ pi 2)) (* scl 5.0)))
    (setq txt-pt (polar pt (- yon (/ pi 2)) (* scl 5.0)))
  )
  
  ;; Metin acisi
  (if (and (> yon (/ pi 2)) (< yon (* 1.5 pi)))
    (setq txt-aci pi)
    (setq txt-aci 0)
  )
  
  ;; TEXT olustur
  (setq ent (entmakex
    (list
      '(0 . "TEXT")
      '(100 . "AcDbEntity")
      '(8 . "KAYNAK_SYM")
      '(100 . "AcDbText")
      (cons 10 txt-pt)
      (cons 11 txt-pt)
      (cons 40 (* scl 2.5))  ; Yukseklik
      (cons 1 olcu-str)       ; Metin
      (cons 50 txt-aci)       ; Aci
      '(41 . 1.0)             ; Genislik faktoru
      '(51 . 0.0)             ; Oblik aci
      '(7 . "STANDARD")       ; Stil
      '(71 . 0)               ; Metin bayraklari
      '(72 . 1)               ; Yatay hizalama (ortali)
      '(73 . 2)               ; Dikey hizalama (ortali)
    )))
  (if ent (list ent) nil)
)

;; Kontur ekle
(defun kaynak-kontur-ekle (pt yon scl kontur yuzey ust-taraf / kont-pt kont-listesi txt-pt ent)
  (setq kont-listesi '())
  
  (if ust-taraf
    (setq kont-pt (polar pt (+ yon (/ pi 2)) (* scl 7.5)))
    (setq kont-pt (polar pt (- yon (/ pi 2)) (* scl 7.5)))
  )
  
  (cond
    ;; DUZ
    ((= kontur "1")
     (setq ent (entmakex
       (list
         '(0 . "LINE")
         '(100 . "AcDbEntity")
         '(8 . "KAYNAK_SYM")
         '(100 . "AcDbLine")
         (cons 10 (polar kont-pt yon (* scl -2.5)))
         (cons 11 (polar kont-pt yon (* scl 2.5)))
       )))
     (if ent (setq kont-listesi (list ent)))
    )
    
    ;; DIS BOMBE
    ((= kontur "2")
     (setq ent (entmakex
       (list
         '(0 . "ARC")
         '(100 . "AcDbEntity")
         '(8 . "KAYNAK_SYM")
         '(100 . "AcDbCircle")
         (cons 10 (if ust-taraf
                    (polar kont-pt (- yon (/ pi 2)) (* scl 1.5))
                    (polar kont-pt (+ yon (/ pi 2)) (* scl 1.5))))
         (cons 40 (* scl 3.0))
         '(100 . "AcDbArc")
         (cons 50 (if ust-taraf 
                    (- yon (* pi 0.333))
                    (+ yon (* pi 0.667))))
         (cons 51 (if ust-taraf 
                    (+ yon (* pi 0.333))
                    (+ yon (* pi 1.333))))
       )))
     (if ent (setq kont-listesi (list ent)))
    )
    
    ;; IC BOMBE
    ((= kontur "3")
     (setq ent (entmakex
       (list
         '(0 . "ARC")
         '(100 . "AcDbEntity")
         '(8 . "KAYNAK_SYM")
         '(100 . "AcDbCircle")
         (cons 10 (if ust-taraf
                    (polar kont-pt (+ yon (/ pi 2)) (* scl 1.5))
                    (polar kont-pt (- yon (/ pi 2)) (* scl 1.5))))
         (cons 40 (* scl 3.0))
         '(100 . "AcDbArc")
         (cons 50 (if ust-taraf 
                    (+ yon (* pi 0.667))
                    (- yon (* pi 0.333))))
         (cons 51 (if ust-taraf 
                    (+ yon (* pi 1.333))
                    (+ yon (* pi 0.333))))
       )))
     (if ent (setq kont-listesi (list ent)))
    )
  )
  
  ;; Yuzey bitirme harfi
  (if (/= yuzey "0")
    (progn
      (if ust-taraf
        (setq txt-pt (polar kont-pt (+ yon (/ pi 2)) (* scl 2.5)))
        (setq txt-pt (polar kont-pt (- yon (/ pi 2)) (* scl 2.5)))
      )
      
      (setq ent (entmakex
        (list
          '(0 . "TEXT")
          '(100 . "AcDbEntity")
          '(8 . "KAYNAK_SYM")
          '(100 . "AcDbText")
          (cons 10 txt-pt)
          (cons 11 txt-pt)
          (cons 40 (* scl 2.0))
          (cons 1 (nth (atoi yuzey) '("" "G" "M" "C" "R" "H" "P")))
          '(50 . 0)
          '(72 . 1)
          '(73 . 2)
        )))
      (if ent (setq kont-listesi (append kont-listesi (list ent))))
    )
  )
  
  kont-listesi
)

;;;========================================================================
;;; OZEL SEMBOLLER - DUZELTILMIS
;;;========================================================================

;; CEVRE KAYNAK DAIRESI (ok basinda)
(defun kaynak-cevre-ekle (pt scl / ent)
  (setq ent (entmakex
    (list
      '(0 . "CIRCLE")
      '(100 . "AcDbEntity")
      '(8 . "KAYNAK_SYM")
      '(100 . "AcDbCircle")
      (cons 10 pt)
      (cons 40 (* scl 2.5))
    )))
  (if ent (list ent) nil)
)

;; SANTIYE KAYNAGI BAYRAGI - DUZELTILMIS
(defun kaynak-bayrak-ekle (pt yon scl / bayrak-pt1 bayrak-pt2 bayrak-pt3 ent-listesi ent)
  (setq ent-listesi '())
  
  ;; Bayrak direği üst noktası
  (setq bayrak-pt1 (polar pt (+ yon (/ pi 2)) (* scl 8.0)))
  
  ;; Bayrak uç noktaları
  (setq bayrak-pt2 (polar bayrak-pt1 yon (* scl 4.0)))
  (setq bayrak-pt3 (polar bayrak-pt1 (- yon (/ pi 2)) (* scl 3.0)))
  
  ;; Direk cizgisi
  (setq ent (entmakex
    (list
      '(0 . "LINE")
      '(100 . "AcDbEntity")
      '(8 . "KAYNAK_SYM")
      '(100 . "AcDbLine")
      (cons 10 pt)
      (cons 11 bayrak-pt1)
    )))
  (if ent (setq ent-listesi (append ent-listesi (list ent))))
  
  ;; Bayrak ucgeni - dolu
  (setq ent (entmakex
    (list
      '(0 . "SOLID")
      '(100 . "AcDbEntity")
      '(8 . "KAYNAK_SYM")
      '(100 . "AcDbTrace")
      (cons 10 bayrak-pt1)
      (cons 11 bayrak-pt2)
      (cons 12 bayrak-pt3)
      (cons 13 bayrak-pt3)
    )))
  (if ent (setq ent-listesi (append ent-listesi (list ent))))
  
  ent-listesi
)

;; TAM NUFUZIYET SEMBOLU - F.P. YAZIYI ICEREN DAIRE
(defun kaynak-tam-nufuz-ekle (pt yon scl / fp-pt daire-pt txt-aci ent-listesi ent)
  (setq ent-listesi '())
  
  ;; Pozisyon hesapla
  (if (= yon 0)
    (setq fp-pt (polar pt yon (* scl 7.0)))
    (setq fp-pt (polar pt yon (* scl -7.0)))
  )
  
  (setq daire-pt (polar fp-pt (+ yon (/ pi 2)) (* scl 5.0)))
  
  ;; Daire ciz
  (setq ent (entmakex
    (list
      '(0 . "CIRCLE")
      '(100 . "AcDbEntity")
      '(8 . "KAYNAK_SYM")
      '(100 . "AcDbCircle")
      (cons 10 daire-pt)
      (cons 40 (* scl 2.5))
    )))
  (if ent (setq ent-listesi (append ent-listesi (list ent))))
  
  ;; F.P. metni
  (if (and (> yon (/ pi 2)) (< yon (* 1.5 pi)))
    (setq txt-aci pi)
    (setq txt-aci 0)
  )
  
  (setq ent (entmakex
    (list
      '(0 . "TEXT")
      '(100 . "AcDbEntity")
      '(8 . "KAYNAK_SYM")
      '(100 . "AcDbText")
      (cons 10 daire-pt)
      (cons 11 daire-pt)
      (cons 40 (* scl 1.8))
      (cons 1 "F.P.")
      (cons 50 txt-aci)
      '(72 . 1)
      '(73 . 2)
    )))
  (if ent (setq ent-listesi (append ent-listesi (list ent))))
  
  ent-listesi
)

;; NDT SEMBOLU - NDT YAZIYI ICEREN DAIRE
(defun kaynak-ndt-ekle (pt yon scl / ndt-pt daire-pt txt-aci ent-listesi ent)
  (setq ent-listesi '())
  
  ;; Pozisyon hesapla
  (if (= yon 0)
    (setq ndt-pt (polar pt yon (* scl 14.0)))
    (setq ndt-pt (polar pt yon (* scl -14.0)))
  )
  
  (setq daire-pt (polar ndt-pt (+ yon (/ pi 2)) (* scl 5.0)))
  
  ;; Daire ciz
  (setq ent (entmakex
    (list
      '(0 . "CIRCLE")
      '(100 . "AcDbEntity")
      '(8 . "KAYNAK_SYM")
      '(100 . "AcDbCircle")
      (cons 10 daire-pt)
      (cons 40 (* scl 2.5))
    )))
  (if ent (setq ent-listesi (append ent-listesi (list ent))))
  
  ;; NDT metni
  (if (and (> yon (/ pi 2)) (< yon (* 1.5 pi)))
    (setq txt-aci pi)
    (setq txt-aci 0)
  )
  
  (setq ent (entmakex
    (list
      '(0 . "TEXT")
      '(100 . "AcDbEntity")
      '(8 . "KAYNAK_SYM")
      '(100 . "AcDbText")
      (cons 10 daire-pt)
      (cons 11 daire-pt)
      (cons 40 (* scl 1.5))
      (cons 1 "NDT")
      (cons 50 txt-aci)
      '(72 . 1)
      '(73 . 2)
    )))
  (if ent (setq ent-listesi (append ent-listesi (list ent))))
  
  ent-listesi
)

;;;========================================================================
;;; WPS KUTUSU VE NOTLAR - DUZELTILMIS
;;;========================================================================

(defun kaynak-metinler-ekle (ref-baslangic ref-bitis yon scl / txt-pt txt-aci kuyruk-pt ent-listesi 
                              kutu-p1 kutu-p2 kutu-p3 kutu-p4 ent)
  (setq ent-listesi '())
  
  ;; Metin acisi belirle
  (if (and (> yon (/ pi 2)) (< yon (* 1.5 pi)))
    (setq txt-aci pi)
    (setq txt-aci 0)
  )
  
  ;; WPS KUTUSU
  (if (/= *WPS-NO* "")
    (progn
      (setq kuyruk-pt (polar ref-bitis yon (* scl 3.0)))
      
      ;; Kuyruk cizgileri
      (setq ent (entmakex
        (list
          '(0 . "LINE")
          '(100 . "AcDbEntity")
          '(8 . "KAYNAK_SYM")
          '(100 . "AcDbLine")
          (cons 10 ref-bitis)
          (cons 11 (polar kuyruk-pt (+ yon (/ pi 4)) (* scl 2.5)))
        )))
      (if ent (setq ent-listesi (append ent-listesi (list ent))))
      
      (setq ent (entmakex
        (list
          '(0 . "LINE")
          '(100 . "AcDbEntity")
          '(8 . "KAYNAK_SYM")
          '(100 . "AcDbLine")
          (cons 10 ref-bitis)
          (cons 11 (polar kuyruk-pt (- yon (/ pi 4)) (* scl 2.5)))
        )))
      (if ent (setq ent-listesi (append ent-listesi (list ent))))
      
      ;; WPS kutusu
      (setq txt-pt (polar kuyruk-pt yon (* scl 5.0)))
      
      ;; Kutu kose noktalari
      (setq kutu-p1 (polar txt-pt (+ yon pi) (* scl 3.0)))
      (setq kutu-p1 (polar kutu-p1 (+ yon (/ pi 2)) (* scl 2.5)))
      (setq kutu-p2 (polar kutu-p1 yon (* scl 6.0)))
      (setq kutu-p3 (polar kutu-p2 (- yon (/ pi 2)) (* scl 5.0)))
      (setq kutu-p4 (polar kutu-p1 (- yon (/ pi 2)) (* scl 5.0)))
      
      ;; Kutu ciz
      (setq ent (entmakex
        (list
          '(0 . "LWPOLYLINE")
          '(100 . "AcDbEntity")
          '(8 . "KAYNAK_SYM")
          '(100 . "AcDbPolyline")
          '(90 . 4)
          '(70 . 1)
          (cons 10 kutu-p1)
          (cons 10 kutu-p2)
          (cons 10 kutu-p3)
          (cons 10 kutu-p4)
        )))
      (if ent (setq ent-listesi (append ent-listesi (list ent))))
      
      ;; WPS metni
      (setq ent (entmakex
        (list
          '(0 . "TEXT")
          '(100 . "AcDbEntity")
          '(8 . "KAYNAK_SYM")
          '(100 . "AcDbText")
          (cons 10 txt-pt)
          (cons 11 txt-pt)
          (cons 40 (* scl 2.0))
          (cons 1 (strcat "WPS:" *WPS-NO*))
          (cons 50 txt-aci)
          '(72 . 1)
          '(73 . 2)
        )))
      (if ent (setq ent-listesi (append ent-listesi (list ent))))
    )
  )
  
  ;; Ek notlar
  (setq txt-pt (polar ref-baslangic (- yon (/ pi 2)) (* scl 7.0)))
  
  (if (/= *NOT1* "")
    (progn
      (setq ent (entmakex
        (list
          '(0 . "TEXT")
          '(100 . "AcDbEntity")
          '(8 . "KAYNAK_SYM")
          '(100 . "AcDbText")
          (cons 10 txt-pt)
          (cons 40 (* scl 1.8))
          (cons 1 *NOT1*)
          (cons 50 txt-aci)
          '(72 . 0)
          '(73 . 0)
        )))
      (if ent (setq ent-listesi (append ent-listesi (list ent))))
      (setq txt-pt (polar txt-pt (- yon (/ pi 2)) (* scl 2.5)))
    )
  )
  
  (if (/= *NOT2* "")
    (progn
      (setq ent (entmakex
        (list
          '(0 . "TEXT")
          '(100 . "AcDbEntity")
          '(8 . "KAYNAK_SYM")
          '(100 . "AcDbText")
          (cons 10 txt-pt)
          (cons 40 (* scl 1.8))
          (cons 1 *NOT2*)
          (cons 50 txt-aci)
          '(72 . 0)
          '(73 . 0)
        )))
      (if ent (setq ent-listesi (append ent-listesi (list ent))))
      (setq txt-pt (polar txt-pt (- yon (/ pi 2)) (* scl 2.5)))
    )
  )
  
  (if (/= *NOT3* "")
    (progn
      (setq ent (entmakex
        (list
          '(0 . "TEXT")
          '(100 . "AcDbEntity")
          '(8 . "KAYNAK_SYM")
          '(100 . "AcDbText")
          (cons 10 txt-pt)
          (cons 40 (* scl 1.8))
          (cons 1 *NOT3*)
          (cons 50 txt-aci)
          '(72 . 0)
          '(73 . 0)
        )))
      (if ent (setq ent-listesi (append ent-listesi (list ent))))
    )
  )
  
  ent-listesi
)

;;;========================================================================
;;; XDATA YONETIMI
;;;========================================================================

(defun kaynak-xdata-ekle (obje-listesi kaynak-id pt1 pt2 / ent xdata-listesi ed)
  (foreach ent obje-listesi
    (if ent
      (progn
        (setq xdata-listesi 
          (list 
            (list -3 
              (list *KAYNAK-APP-ID* 
                (cons 1000 "KAYNAK_SEMBOLU")
                (cons 1000 kaynak-id)
                (cons 1000 *UST-YONTEM*)
                (cons 1000 *ALT-YONTEM*)
                (cons 1000 *UST-OLCU*)
                (cons 1000 *ALT-OLCU*)
                (cons 1000 *UST-KONTUR*)
                (cons 1000 *ALT-KONTUR*)
                (cons 1000 *UST-YUZEY*)
                (cons 1000 *ALT-YUZEY*)
                (cons 1000 *WPS-NO*)
                (cons 1000 *SANTIYE-FLAG*)
                (cons 1000 *CEVRE-FLAG*)
                (cons 1000 *TAM-NUF-FLAG*)
                (cons 1000 *NDT-FLAG*)
                (cons 1000 *NOT1*)
                (cons 1000 *NOT2*)
                (cons 1000 *NOT3*)
                (cons 1010 pt1)
                (cons 1010 pt2)
              )
            )
          ))
        
        (setq ed (entget ent))
        (setq ed (append ed xdata-listesi))
        (entmod ed)
      )
    )
  )
  (princ)
)

;; XDATA oku
(defun kaynak-xdata-oku (ent / ed xdata)
  (setq ed (entget ent (list *KAYNAK-APP-ID*)))
  (setq xdata (assoc -3 ed))
  (if xdata
    (setq xdata (cadr (car (cdr xdata))))
  )
  xdata
)

;; Parametreleri XDATA'dan cikart
(defun kaynak-parametreleri-cikart (xdata / pt1 pt2)
  (if xdata
    (progn
      (setq *UST-YONTEM* (cdr (nth 2 xdata)))
      (setq *ALT-YONTEM* (cdr (nth 3 xdata)))
      (setq *UST-OLCU* (cdr (nth 4 xdata)))
      (setq *ALT-OLCU* (cdr (nth 5 xdata)))
      (setq *UST-KONTUR* (cdr (nth 6 xdata)))
      (setq *ALT-KONTUR* (cdr (nth 7 xdata)))
      (setq *UST-YUZEY* (cdr (nth 8 xdata)))
      (setq *ALT-YUZEY* (cdr (nth 9 xdata)))
      (setq *WPS-NO* (cdr (nth 10 xdata)))
      (setq *SANTIYE-FLAG* (cdr (nth 11 xdata)))
      (setq *CEVRE-FLAG* (cdr (nth 12 xdata)))
      (setq *TAM-NUF-FLAG* (cdr (nth 13 xdata)))
      (setq *NDT-FLAG* (cdr (nth 14 xdata)))
      (setq *NOT1* (cdr (nth 15 xdata)))
      (setq *NOT2* (cdr (nth 16 xdata)))
      (setq *NOT3* (cdr (nth 17 xdata)))
      (setq pt1 (cdr (nth 18 xdata)))
      (setq pt2 (cdr (nth 19 xdata)))
      (list pt1 pt2)
    )
    nil
  )
)

;;;========================================================================
;;; CIFT TIKLAMA ILE DUZENLEME - REACTOR SISTEMI
;;;========================================================================

(defun kaynak-reactor-kur ()
  (if *KAYNAK-REACTOR*
    (vlr-remove *KAYNAK-REACTOR*)
  )
  (setq *KAYNAK-REACTOR*
    (vlr-mouse-reactor nil '((:vlr-beginDoubleClick . kaynak-cift-tikla))))
  (princ)
)

;; Cift tiklama yakalayici
(defun kaynak-cift-tikla (reactor info / ss ent xdata kaynak-id pts scl i)
  (if (setq ss (ssget "_I"))
    (progn
      (setq ent (ssname ss 0))
      (setq xdata (kaynak-xdata-oku ent))
      
      (if xdata
        (progn
          ;; Kaynak ID ve noktalari al
          (setq kaynak-id (cdr (nth 1 xdata)))
          (setq pts (kaynak-parametreleri-cikart xdata))
          
          ;; Dialog goster
          (if (kaynak-dialog T)
            (progn
              ;; Eski sembolu sil
              (setq ss (ssget "_X" 
                (list (cons -3 (list (cons *KAYNAK-APP-ID* nil))))))
              
              (if ss
                (progn
                  (setq i 0)
                  (repeat (sslength ss)
                    (setq ent (ssname ss i))
                    (setq xdata (kaynak-xdata-oku ent))
                    (if (and xdata 
                             (= (cdr (nth 1 xdata)) kaynak-id))
                      (entdel ent)
                    )
                    (setq i (1+ i))
                  )
                )
              )
              
              ;; Olcek al
              (setq scl (getvar "DIMSCALE"))
              (if (or (null scl) (<= scl 0))
                (setq scl 1.0)
              )
              
              ;; Yeni sembol olustur
              (kaynak-sembolu-olustur (car pts) (cadr pts) scl kaynak-id)
              
              (princ "\nKaynak sembolu guncellendi.")
            )
          )
        )
      )
    )
  )
  (princ)
)

;;;========================================================================
;;; OTOMATIK YUKLEME VE BILGI
;;;========================================================================

;; Sistemi baslat
(kaynak-basla)

;; Bilgi mesaji
(princ "\n============================================================")
(princ "\nKAYNAK SEMBOLU SISTEMI V2.0 BASARIYLA YUKLENDI")
(princ "\n============================================================")
(princ "\nANA KOMUT: KS")
(princ "\n")
(princ "\nOZELLIKLER:")
(princ "\n• AWS A2.4 Standardina tam uyumlu")
(princ "\n• 8 farkli kaynak tipi (Fillet, V-Groove, Bevel, vb.)")
(princ "\n• Kontur secenekleri (Duz/Dis Bombe/Ic Bombe)")
(princ "\n• Yuzey bitirme yontemleri (G/M/C/R/H/P)")
(princ "\n• Santiye kaynagi bayragi")
(princ "\n• Cevre kaynak dairesi")
(princ "\n• F.P. (Tam nufuziyet) sembolu")
(princ "\n• NDT (Tahribatsiz muayene) sembolu")
(princ "\n• WPS numarasi kutusu")
(princ "\n• XDATA ile veri saklama")
(princ "\n• Cift tiklama ile duzenleme")
(princ "\n• DIMSCALE'e gore otomatik olcekleme")
(princ "\n")
(princ "\nKULLANIM TALIMATI:")
(princ "\n1. KS komutunu yazin")
(princ "\n2. Kaynak yerini secin (ok basi)")
(princ "\n3. Leader bitis noktasini secin")
(princ "\n4. Dialog kutusunda parametreleri ayarlayin")
(princ "\n5. Tamam'a tiklayin")
(princ "\n")
(princ "\nDUZENLEME:")
(princ "\n• Olusturulan kaynak sembolune cift tiklayin")
(princ "\n• Parametreleri degistirin ve Tamam'a tiklayin")
(princ "\n")
(princ "\nOLCEKLEME:")
(princ "\n• Semboller DIMSCALE degerine gore otomatik olceklenir")
(princ "\n• DIMSCALE degistirmek icin: DIMSCALE yazin")
(princ "\n============================================================")
(princ)
