;;;========================================================================
;;; KaynakSembol.lsp - Profesyonel Kaynak Sembolu Yerlestirme Sistemi V3.4
;;; AutoCAD 2025 icin - AWS A2.4 Standardi
;;; 
;;; Komut: KS
;;; V3.4 - Kritik yon hesaplama hatalari giderildi
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
(if (not *KAYNAK-YONTEMI*) (setq *KAYNAK-YONTEMI* "0")) ; 0=Standart, 1=Metod Kaynak
(if (not *UST-KAYNAK-ADETI*) (setq *UST-KAYNAK-ADETI* "20"))
(if (not *UST-KAYNAK-UZUNLUGU*) (setq *UST-KAYNAK-UZUNLUGU* "100"))
(if (not *UST-KAYNAK-BOSLUGU*) (setq *UST-KAYNAK-BOSLUGU* "100"))
(if (not *ALT-KAYNAK-ADETI*) (setq *ALT-KAYNAK-ADETI* "25"))
(if (not *ALT-KAYNAK-UZUNLUGU*) (setq *ALT-KAYNAK-UZUNLUGU* "100"))
(if (not *ALT-KAYNAK-BOSLUGU*) (setq *ALT-KAYNAK-BOSLUGU* "100"))

;; Uygulamayi baslatma
(defun kaynak-basla ()
  ;; XDATA uygulama kaydini yap
  (if (not (tblsearch "APPID" *KAYNAK-APP-ID*))
    (regapp *KAYNAK-APP-ID*)
  )
  
  ;; Katman yoksa olustur (sari renk)
  (if (not (tblsearch "LAYER" "KAYNAK_SYM"))
    (entmake '((0 . "LAYER")
               (100 . "AcDbSymbolTableRecord")
               (100 . "AcDbLayerTableRecord")
               (2 . "KAYNAK_SYM")
               (70 . 0)
               (62 . 2)
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
;; ONEMLI: Olcegi yarisina dusurmek icin
(setq scl (/ scl 2.0))
  
  ;; Katmani degistir
  (setvar "CLAYER" "KAYNAK_SYM")
  
  ;; Ana dongu
  (princ "\n========================================")
  (princ "\nKAYNAK SEMBOLU YERLESTIRME SISTEMI V3.4")
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
  
  ;; Kaynak yontemi listesini doldur
  (start_list "kaynak_yontemi")
  (mapcar 'add_list '("STANDART KAYNAK" "METOD KAYNAK"))
  (end_list)
  
  ;; Diger listeleri doldur
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
  (set_tile "kaynak_yontemi" *KAYNAK-YONTEMI*)
  (set_tile "ust_kaynak_adeti" *UST-KAYNAK-ADETI*)
  (set_tile "ust_kaynak_uzunlugu" *UST-KAYNAK-UZUNLUGU*)
  (set_tile "ust_kaynak_boslugu" *UST-KAYNAK-BOSLUGU*)
  (set_tile "alt_kaynak_adeti" *ALT-KAYNAK-ADETI*)
  (set_tile "alt_kaynak_uzunlugu" *ALT-KAYNAK-UZUNLUGU*)
  (set_tile "alt_kaynak_boslugu" *ALT-KAYNAK-BOSLUGU*)
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
  
  ;; METOD parametrelerini goster/gizle
  (defun metod-parametreleri-goster-gizle (deger)
    (if (= deger "1")
      (mode_tile "metod_parametreler" 0) ; Goster
      (mode_tile "metod_parametreler" 1) ; Gizle
    )
  )
  
  ;; Baslangicta kontrol et
  (metod-parametreleri-goster-gizle *KAYNAK-YONTEMI*)
  
  ;; Action callbacks
  (action_tile "kaynak_yontemi" 
    "(progn (setq *KAYNAK-YONTEMI* $value) (metod-parametreleri-goster-gizle $value))")
  (action_tile "ust_kaynak_adeti" "(setq *UST-KAYNAK-ADETI* $value)")
  (action_tile "ust_kaynak_uzunlugu" "(setq *UST-KAYNAK-UZUNLUGU* $value)")
  (action_tile "ust_kaynak_boslugu" "(setq *UST-KAYNAK-BOSLUGU* $value)")
  (action_tile "alt_kaynak_adeti" "(setq *ALT-KAYNAK-ADETI* $value)")
  (action_tile "alt_kaynak_uzunlugu" "(setq *ALT-KAYNAK-UZUNLUGU* $value)")
  (action_tile "alt_kaynak_boslugu" "(setq *ALT-KAYNAK-BOSLUGU* $value)")
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

;; ========== METOD KAYNAK Z CIZIMI - REFERANS UZERINDE ==========

(defun kaynak-z-cizimi (ref-baslangic ref-yon scl yukari-aci / z-baslangic ust-sol ust-sag alt-sol alt-sag ent ent-listesi capraz-aci)
  (setq ent-listesi '())
  
  ;; Z başlangıç noktası: 37.6mm konumunda
  (setq z-baslangic (polar ref-baslangic ref-yon (* scl 37.6)))
  
  ;; 8mm yukarı kaydır
  (setq z-baslangic (polar z-baslangic (+ ref-yon yukari-aci) (* scl 8)))
  
  ;; Üst yatay çizgi - 6mm uzunlukta
  (setq ust-sol z-baslangic)
  (setq ust-sag (polar ust-sol ref-yon (* scl 6)))
  
  ;; Çapraz çizgi - Sağ üstten sol alta, 80 derece aşağı, 16.25mm uzunluk
      (if (< ref-yon 0.01)
          ;; Sağ yön (ref-yon = 0). Normal Z çizimi: 260 derece (Aşağı-Sol)
          (setq capraz-aci (* 260.0 (/ pi 180.0))) 
          ;; Sol yön (ref-yon = pi). Aynalanmış Z çizimi: 280 derece (Aşağı-Sağ)
          (setq capraz-aci (* 280.0 (/ pi 180.0)))
      )
  (setq alt-sol (polar ust-sag capraz-aci (* scl 16.25)))
  
  ;; Alt yatay çizgi - 6mm uzunlukta
  (setq alt-sag (polar alt-sol ref-yon (* scl 6)))
  
  ;; 1. Üst yatay çizgi
  (setq ent (entmakex
    (list
      '(0 . "LINE")
      '(100 . "AcDbEntity")
      '(8 . "KAYNAK_SYM")
      '(100 . "AcDbLine")
      (cons 10 ust-sol)
      (cons 11 ust-sag)
    )))
  (if ent (setq ent-listesi (append ent-listesi (list ent))))
  
  ;; 2. Çapraz çizgi (sağ üstten sol alta)
  (setq ent (entmakex
    (list
      '(0 . "LINE")
      '(100 . "AcDbEntity")
      '(8 . "KAYNAK_SYM")
      '(100 . "AcDbLine")
      (cons 10 ust-sag)
      (cons 11 alt-sol)
    )))
  (if ent (setq ent-listesi (append ent-listesi (list ent))))
  
  ;; 3. Alt yatay çizgi
  (setq ent (entmakex
    (list
      '(0 . "LINE")
      '(100 . "AcDbEntity")
      '(8 . "KAYNAK_SYM")
      '(100 . "AcDbLine")
      (cons 10 alt-sol)
      (cons 11 alt-sag)
    )))
  (if ent (setq ent-listesi (append ent-listesi (list ent))))
  
  ent-listesi
)

;; ========== METOD KAYNAK METINLERI - UST TARAF ==========

(defun kaynak-metod-metinleri-ust (ref-baslangic ref-yon scl txt-aci yukari-aci / txt-pt ent ent-listesi adeti-uzunluk-str)
  (setq ent-listesi '())
  
  ;; Kaynak adeti x uzunlugu metni
  ;; X: 30.5mm konumunda (15.5mm sağa kaydırıldı)
  (setq adeti-uzunluk-str (strcat *UST-KAYNAK-ADETI* " x " *UST-KAYNAK-UZUNLUGU*))
  (setq txt-pt (polar ref-baslangic ref-yon (* scl 30.5)))  ;; DEĞİŞTİ: 15 → 30.5
  (setq txt-pt (polar txt-pt (+ ref-yon yukari-aci) (* scl 3.0)))
  
  (setq ent (entmakex
    (list
      '(0 . "TEXT")
      '(100 . "AcDbEntity")
      '(8 . "KAYNAK_SYM")
      '(100 . "AcDbText")
      (cons 10 txt-pt)
      (cons 11 txt-pt)
      (cons 40 (* scl 3.75))
      (cons 1 adeti-uzunluk-str)
      (cons 50 txt-aci)
      '(72 . 1)
      '(73 . 2)
    )))
  (if ent (setq ent-listesi (append ent-listesi (list ent))))
  
  ;; Kaynak boslugu metni
  ;; X: 48mm konumunda (6mm sağa kaydırıldı)
  (setq txt-pt (polar ref-baslangic ref-yon (* scl 48)))  ;; DEĞİŞTİ: 42 → 48
  (setq txt-pt (polar txt-pt (+ ref-yon yukari-aci) (* scl 3.0)))
  
  (setq ent (entmakex
    (list
      '(0 . "TEXT")
      '(100 . "AcDbEntity")
      '(8 . "KAYNAK_SYM")
      '(100 . "AcDbText")
      (cons 10 txt-pt)
      (cons 11 txt-pt)
      (cons 40 (* scl 3.75))
      (cons 1 *UST-KAYNAK-BOSLUGU*)
      (cons 50 txt-aci)
      '(72 . 1)
      '(73 . 2)
    )))
  (if ent (setq ent-listesi (append ent-listesi (list ent))))
  
  ent-listesi
)

;; ========== METOD KAYNAK METINLERI - ALT TARAF ==========

(defun kaynak-metod-metinleri-alt (ref-baslangic ref-yon scl txt-aci asagi-aci / txt-pt ent ent-listesi adeti-uzunluk-str)
  (setq ent-listesi '())
  
  ;; Kaynak adeti x uzunlugu metni
  ;; X: 30.5mm konumunda (15.5mm sağa kaydırıldı)
  (setq adeti-uzunluk-str (strcat *ALT-KAYNAK-ADETI* " x " *ALT-KAYNAK-UZUNLUGU*))
  (setq txt-pt (polar ref-baslangic ref-yon (* scl 30.5)))  ;; DEĞİŞTİ: 15 → 30.5
  (setq txt-pt (polar txt-pt (+ ref-yon asagi-aci) (* scl 3.0)))
  
  (setq ent (entmakex
    (list
      '(0 . "TEXT")
      '(100 . "AcDbEntity")
      '(8 . "KAYNAK_SYM")
      '(100 . "AcDbText")
      (cons 10 txt-pt)
      (cons 11 txt-pt)
      (cons 40 (* scl 3.75))
      (cons 1 adeti-uzunluk-str)
      (cons 50 txt-aci)
      '(72 . 1)
      '(73 . 2)
    )))
  (if ent (setq ent-listesi (append ent-listesi (list ent))))
  
  ;; Kaynak boslugu metni
  ;; X: 48mm konumunda (6mm sağa kaydırıldı)
  (setq txt-pt (polar ref-baslangic ref-yon (* scl 48)))  ;; DEĞİŞTİ: 42 → 48
  (setq txt-pt (polar txt-pt (+ ref-yon asagi-aci) (* scl 3.0)))
  
  (setq ent (entmakex
    (list
      '(0 . "TEXT")
      '(100 . "AcDbEntity")
      '(8 . "KAYNAK_SYM")
      '(100 . "AcDbText")
      (cons 10 txt-pt)
      (cons 11 txt-pt)
      (cons 40 (* scl 3.75))
      (cons 1 *ALT-KAYNAK-BOSLUGU*)
      (cons 50 txt-aci)
      '(72 . 1)
      '(73 . 2)
    )))
  (if ent (setq ent-listesi (append ent-listesi (list ent))))
  
  ent-listesi
)
;;;========================================================================
;;; SEMBOL OLUSTURMA ANA FONKSIYONU
;;;========================================================================

(defun kaynak-sembolu-olustur (pt1 pt2 scl kaynak-id / aci ref-yon ref-baslangic ref-bitis obje-listesi
                                ent ust-pt alt-pt sembol-merkez yukari-aci asagi-aci)
  
  (setq obje-listesi '())
  
  ;; Aci ve yon hesapla
  (setq aci (angle pt1 pt2))
  
  ;; Referans cizgisi yonunu belirle (yatay sag veya sol)
  (if (and (> aci (/ pi 2)) (< aci (* 1.5 pi)))
    (setq ref-yon pi)  ; Sol
    (setq ref-yon 0)   ; Sag
  )
  
  ;; V3.4 DUZELTME: Yukari ve asagi acilari referans yonune gore hesapla
  (if (< ref-yon 0.01)
    (progn
      (setq yukari-aci (/ pi 2))      ; Sag tarafta: 90 derece
      (setq asagi-aci (- (/ pi 2)))   ; Sag tarafta: -90 derece
    )
    (progn
      (setq yukari-aci (- (/ pi 2)))  ; Sol tarafta: -90 derece
      (setq asagi-aci (/ pi 2))       ; Sol tarafta: 90 derece
    )
  )
  
  (setq ref-baslangic pt2)
  ;; Referans cizgisi uzunlugu - METOD KAYNAK icin 52mm, STANDART icin 24mm
  (if (= *KAYNAK-YONTEMI* "1")
    (setq ref-bitis (polar ref-baslangic ref-yon (* scl 52.0)))
    (setq ref-bitis (polar ref-baslangic ref-yon (* scl 24.0)))
  )
  
;; 1. MULTILEADER (OK) OLUSTUR - Basit ve Garantili Versiyon
(setq acadObj (vlax-get-acad-object))
(setq doc (vla-get-ActiveDocument acadObj))
(setq mspace (vla-get-ModelSpace doc))

;; 3D noktalar olustur
(setq pt1-3d (list (car pt1) (cadr pt1) 0.0))
(setq pt2-3d (list (car pt2) (cadr pt2) 0.0))

;; SafeArray olustur
(setq pointArray (vlax-make-safearray vlax-vbDouble '(0 . 5)))
(vlax-safearray-fill pointArray (append pt1-3d pt2-3d))

;; MULTILEADER olustur
(setq mleaderObj (vla-AddMLeader mspace pointArray 0))

;; Katman
(vla-put-Layer mleaderObj "KAYNAK_SYM")

;; Ok tipi
(vla-put-ArrowheadType mleaderObj acArrowDefault)

;; Ok boyutu - OLCEKLENEBILIR
(vla-put-ArrowheadSize mleaderObj (* scl 2.5))

;; Text yok
(vla-put-TextString mleaderObj "")

;; Landing yok
(vla-put-DoglegLength mleaderObj 0)

;; Entity name
(setq ent (vlax-vla-object->ename mleaderObj))
(if ent (setq obje-listesi (append obje-listesi (list ent))))
  
  ;; 2. REFERANS CIZGISI (24mm)
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
  
  ;; Sembol merkezi pt2'den 15.5mm uzakta
  (setq sembol-merkez (polar ref-baslangic ref-yon (* scl 15.5)))
  
  ;; 3. UST TARAF SEMBOLU (referans cizgisinden 0.5mm yukari)
  ;; V3.4 DUZELTME: yukari-aci kullanildi
  (if (/= *UST-YONTEM* "0")
    (progn
      (setq ust-pt (polar sembol-merkez (+ ref-yon yukari-aci) (* scl 0.5)))
      (setq obje-listesi (append obje-listesi 
        (kaynak-sembol-ciz ust-pt ref-yon scl *UST-YONTEM* T yukari-aci asagi-aci)))
      
      ;; Ust kontur
      (if (/= *UST-KONTUR* "0")
        (setq obje-listesi (append obje-listesi 
          (kaynak-kontur-ekle ust-pt ref-yon scl *UST-KONTUR* *UST-YUZEY* T *UST-YONTEM* yukari-aci asagi-aci)))
      )
    )
  )
  
  ;; 4. ALT TARAF SEMBOLU (referans cizgisinden 0.5mm asagi)
  ;; V3.4 DUZELTME: asagi-aci kullanildi
  (if (/= *ALT-YONTEM* "0")
    (progn
      (setq alt-pt (polar sembol-merkez (+ ref-yon asagi-aci) (* scl 0.5)))
      (setq obje-listesi (append obje-listesi 
        (kaynak-sembol-ciz alt-pt ref-yon scl *ALT-YONTEM* nil yukari-aci asagi-aci)))
      
      ;; Alt kontur
      (if (/= *ALT-KONTUR* "0")
        (setq obje-listesi (append obje-listesi 
          (kaynak-kontur-ekle alt-pt ref-yon scl *ALT-KONTUR* *ALT-YUZEY* nil *ALT-YONTEM* yukari-aci asagi-aci)))
      )
    )
  )
  
  ;; Olcu metni
  (if (/= *UST-OLCU* "")
    (setq obje-listesi (append obje-listesi 
      (kaynak-olcu-metni-ekle ref-baslangic ref-yon scl *UST-OLCU* T yukari-aci asagi-aci)))
  )
  
  (if (/= *ALT-OLCU* "")
    (setq obje-listesi (append obje-listesi 
      (kaynak-olcu-metni-ekle ref-baslangic ref-yon scl *ALT-OLCU* nil yukari-aci asagi-aci)))
  )
  
  ;; 5. OZEL SEMBOLLER
  
  ;; CEVRE KAYNAK DAIRESI (pt2'de)
  (if (= *CEVRE-FLAG* "1")
    (setq obje-listesi (append obje-listesi (kaynak-cevre-ekle pt2 scl)))
  )
  
  ;; SANTIYE BAYRAGI
  (if (= *SANTIYE-FLAG* "1")
    (setq obje-listesi (append obje-listesi (kaynak-bayrak-ekle pt2 ref-yon scl yukari-aci asagi-aci)))
  )
  
  ;; WPS KUTUSU VE NOTLAR - referans cizgisinin sonundan
  (setq obje-listesi (append obje-listesi 
    (kaynak-metinler-ekle ref-baslangic ref-bitis ref-yon scl yukari-aci asagi-aci)))
  
  ;; F.P. ve NDT sembolleri
  (if (= *TAM-NUF-FLAG* "1")
    (setq obje-listesi (append obje-listesi 
      (kaynak-tam-nufuz-ekle ref-bitis ref-yon scl (= *NDT-FLAG* "1"))))
  )
  
  (if (= *NDT-FLAG* "1")
    (setq obje-listesi (append obje-listesi 
      (kaynak-ndt-ekle ref-bitis ref-yon scl (= *TAM-NUF-FLAG* "1"))))
  )
  
  ;; METOD KAYNAK ozel cizimleri
  (if (= *KAYNAK-YONTEMI* "1")
    (progn
      ;; Metin acisi tanimla
      (setq txt-aci 0.0)
      
      ;; Z cizimi - TEK KERE, referans uzerinde, 8mm yukari
      (setq obje-listesi (append obje-listesi 
        (kaynak-z-cizimi ref-baslangic ref-yon scl yukari-aci)))
      
      ;; UST TARAF metinleri - sadece ust kaynak varsa
      (if (/= *UST-YONTEM* "0")
        (setq obje-listesi (append obje-listesi 
          (kaynak-metod-metinleri-ust ref-baslangic ref-yon scl txt-aci yukari-aci)))
      )
      
      ;; ALT TARAF metinleri - sadece alt kaynak varsa
      (if (/= *ALT-YONTEM* "0")
        (setq obje-listesi (append obje-listesi 
          (kaynak-metod-metinleri-alt ref-baslangic ref-yon scl txt-aci asagi-aci)))
      )
    )
  )
  ;; XDATA ekle
  (kaynak-xdata-ekle obje-listesi kaynak-id ref-baslangic ref-bitis)
  
  ;; Cizimi yenile
  (command "_.REGEN")
  
  (princ)
)

;;;========================================================================
;;; KAYNAK SEMBOLU CIZIM FONKSIYONLARI
;;;========================================================================

;; Ana sembol cizim fonksiyonu
(defun kaynak-sembol-ciz (pt yon scl tip ust-taraf yukari-aci asagi-aci / ent-listesi)
  (setq ent-listesi '())
  (cond
    ((= tip "1") (setq ent-listesi (kaynak-fillet pt yon scl ust-taraf yukari-aci asagi-aci)))
    ((= tip "2") (setq ent-listesi (kaynak-vgroove pt yon scl ust-taraf yukari-aci asagi-aci)))
    ((= tip "3") (setq ent-listesi (kaynak-bevel pt yon scl ust-taraf yukari-aci asagi-aci)))
    ((= tip "4") (setq ent-listesi (kaynak-ugroove pt yon scl ust-taraf yukari-aci asagi-aci)))
    ((= tip "5") (setq ent-listesi (kaynak-jgroove pt yon scl ust-taraf yukari-aci asagi-aci)))
    ((= tip "6") (setq ent-listesi (kaynak-square pt yon scl ust-taraf yukari-aci asagi-aci)))
    ((= tip "7") (setq ent-listesi (kaynak-plug pt yon scl ust-taraf yukari-aci asagi-aci)))
  )
  ent-listesi
)

;; FILLET (Kose kaynagi) - Ucgen
;; V3.4 DUZELTME: yukari-aci ve asagi-aci parametreleri eklendi
(defun kaynak-fillet (pt yon scl ust-taraf yukari-aci asagi-aci / p1 p2 p3 ent)
  (setq p1 (polar pt yon (* scl -3.0)))
  (setq p2 (polar p1 yon (* scl 6.0)))
  (if ust-taraf
    (setq p3 (polar p1 (+ yon yukari-aci) (* scl 6.0)))
    (setq p3 (polar p1 (+ yon asagi-aci) (* scl 6.0)))
  )
  
  (setq ent (entmakex
    (list
      '(0 . "LWPOLYLINE")
      '(100 . "AcDbEntity")
      '(8 . "KAYNAK_SYM")
      '(100 . "AcDbPolyline")
      '(90 . 3)
      '(70 . 1)
      (cons 10 p1)
      (cons 10 p2)
      (cons 10 p3)
    )))
  (if ent (list ent) nil)
)

;; V-GROOVE - Ic aci 60 derece, kol uzunlugu 6.92mm
;; V3.4 DUZELTME: yukari-aci parametresi kullanildi
(defun kaynak-vgroove (pt yon scl ust-taraf yukari-aci asagi-aci / p1 p2 p3 ent-listesi ent temel-aci)
  (setq p1 pt)
  (setq ent-listesi '())

  ;; Ic aci 60 derece = her kol merkezden 30 derece
  (if ust-taraf
    (progn
      (setq temel-aci (+ yon yukari-aci))
      (setq p2 (polar p1 (- temel-aci (/ pi 6)) (* scl 6.92)))
      (setq p3 (polar p1 (+ temel-aci (/ pi 6)) (* scl 6.92)))
    )
    (progn
      (setq temel-aci (+ yon asagi-aci))
      (setq p2 (polar p1 (- temel-aci (/ pi 6)) (* scl 6.92)))
      (setq p3 (polar p1 (+ temel-aci (/ pi 6)) (* scl 6.92)))
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

;; BEVEL - Dik kol yukari, 45 derece kol SAGINDA (sag ust)
;; V3.4 DUZELTME: yukari-aci ve asagi-aci parametreleri kullanildi
(defun kaynak-bevel (pt yon scl ust-taraf yukari-aci asagi-aci / p1 p2 p3 ent-listesi ent temel-aci)
  (setq p1 pt)
  (setq ent-listesi '())

  (if ust-taraf
    (progn
      (setq p2 (polar p1 (+ yon yukari-aci) (* scl 6.0)))
      (setq temel-aci (+ yon yukari-aci))
      ;; ÜST taraf: SOLDA iken 45° çizgi dik çizginin SOLUNDA
      (if (>= yon 0.01)
        (setq p3 (polar p1 (+ temel-aci (/ pi 4)) (* scl 6.0)))
        (setq p3 (polar p1 (- temel-aci (/ pi 4)) (* scl 6.0)))
      )
    )
    (progn
      (setq p2 (polar p1 (+ yon asagi-aci) (* scl 6.0)))
      (setq temel-aci (+ yon asagi-aci))
      ;; ALT taraf: SAĞDA iken 45° çizgi dik çizginin SAĞINDA
      (if (< yon 0.01)
        (setq p3 (polar p1 (+ temel-aci (/ pi 4)) (* scl 6.0)))
        (setq p3 (polar p1 (- temel-aci (/ pi 4)) (* scl 6.0)))
      )
    )
  )
  
  ;; Dik cizgi
  (setq ent (entmakex
    (list
      '(0 . "LINE")
      '(100 . "AcDbEntity")
      '(8 . "KAYNAK_SYM")
      '(100 . "AcDbLine")
      (cons 10 p1)
      (cons 11 p2)
    )))
  (if ent (setq ent-listesi (append ent-listesi (list ent))))
  
  ;; 45 derece cizgi (sag tarafta)
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

;; SQUARE - 6mm x 6mm kare
;; V3.4 DUZELTME: yukari-aci ve asagi-aci parametreleri kullanildi
(defun kaynak-square (pt yon scl ust-taraf yukari-aci asagi-aci / p1 p2 p3 p4 ent)
  ;; Merkez noktadan +- 3mm yatay (toplam 6mm)
  (setq p1 (polar pt yon (* scl -3.0)))
  (setq p2 (polar pt yon (* scl 3.0)))

  (if ust-taraf
    (progn
      (setq p3 (polar p1 (+ yon yukari-aci) (* scl 6.0)))
      (setq p4 (polar p2 (+ yon yukari-aci) (* scl 6.0)))
    )
    (progn
      (setq p3 (polar p1 (+ yon asagi-aci) (* scl 6.0)))
      (setq p4 (polar p2 (+ yon asagi-aci) (* scl 6.0)))
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

;; U-GROOVE - Tum sekil 2.75mm yukari kaldirilmis
;; V3.4 DUZELTME: yukari-aci ve asagi-aci parametreleri kullanildi
(defun kaynak-ugroove (pt yon scl ust-taraf yukari-aci asagi-aci / merkez p-sol p-sag kol-sol kol-sag 
                        baslangic-aci bitis-aci ent-listesi ent)
  (setq ent-listesi '())
  
  (if ust-taraf
    (progn
      ;; Yarim daire merkezini 2.75mm yukari kaldir
      (setq merkez (polar pt (+ yon yukari-aci) (* scl 2.75)))
      
      ;; Sol ve sag uclar merkez seviyesinde, yanlarda
      (setq p-sol (polar merkez yon (* scl -2.75)))
      (setq p-sag (polar merkez yon (* scl 2.75)))
      
      ;; Kollar yarim dairenin uclarindan yukari cikiyor
      (setq kol-sol (polar p-sol (+ yon yukari-aci) (* scl 3.25)))
      (setq kol-sag (polar p-sag (+ yon yukari-aci) (* scl 3.25)))
      
      ;; Yay: soldan saga (180 derece) - ALTTA
      (setq baslangic-aci pi)           ; 180° - sol
      (setq bitis-aci 0.0)              ; 0° - sag
    )
    (progn
      ;; Alt taraf: Yarim daire merkezini 2.75mm asagi kaldir
      (setq merkez (polar pt (+ yon asagi-aci) (* scl 2.75)))
      
      (setq p-sol (polar merkez yon (* scl -2.75)))
      (setq p-sag (polar merkez yon (* scl 2.75)))
      
      (setq kol-sol (polar p-sol (+ yon asagi-aci) (* scl 3.25)))
      (setq kol-sag (polar p-sag (+ yon asagi-aci) (* scl 3.25)))
      
      ;; Yay: sagdan sola (180 derece) - USTTE
      (setq baslangic-aci 0.0)          ; 0° - sag
      (setq bitis-aci pi)               ; 180° - sol
    )
  )
  
  ;; Sol dikey kol
  (setq ent (entmakex
    (list
      '(0 . "LINE")
      '(100 . "AcDbEntity")
      '(8 . "KAYNAK_SYM")
      '(100 . "AcDbLine")
      (cons 10 p-sol)
      (cons 11 kol-sol)
    )))
  (if ent (setq ent-listesi (append ent-listesi (list ent))))
  
  ;; Yarim daire (180 derece)
  (setq ent (entmakex
    (list
      '(0 . "ARC")
      '(100 . "AcDbEntity")
      '(8 . "KAYNAK_SYM")
      '(100 . "AcDbCircle")
      (cons 10 merkez)
      (cons 40 (* scl 2.75))
      '(100 . "AcDbArc")
      (cons 50 baslangic-aci)
      (cons 51 bitis-aci)
    )))
  (if ent (setq ent-listesi (append ent-listesi (list ent))))
  
  ;; Sag dikey kol
  (setq ent (entmakex
    (list
      '(0 . "LINE")
      '(100 . "AcDbEntity")
      '(8 . "KAYNAK_SYM")
      '(100 . "AcDbLine")
      (cons 10 p-sag)
      (cons 11 kol-sag)
    )))
  (if ent (setq ent-listesi (append ent-listesi (list ent))))
  
  ent-listesi
)

;; J-GROOVE - Tum sekil 2.75mm yukari, ceyrek daire TAM 90 derece
;; V3.4 DUZELTME: yukari-aci ve asagi-aci parametreleri kullanildi
(defun kaynak-jgroove (pt yon scl ust-taraf yukari-aci asagi-aci / merkez p-sol kol-sol 
                        baslangic-aci bitis-aci ent-listesi ent)
  (setq ent-listesi '())
  
  (if ust-taraf
    (progn
      ;; Ceyrek daire merkezini 2.75mm yukari kaldir
      (setq merkez (polar pt (+ yon yukari-aci) (* scl 2.75)))
      
      ;; Sol nokta merkezin solunda (merkez seviyesinde)
      (setq p-sol (polar merkez yon (* scl -2.75)))
      
      ;; Sol kol yukari
      (setq kol-sol (polar p-sol (+ yon yukari-aci) (* scl 3.25)))
      
      ;; Yay: soldan (180°) asagi (270°) = TAM 90 derece
      (setq baslangic-aci pi)           ; 180° - sol
      (setq bitis-aci (* 1.5 pi))       ; 270° - asagi
      
      ;; SOLDA iken +90 derece döndür
      (if (>= yon 0.01)
        (progn
          (setq baslangic-aci (+ baslangic-aci (/ pi 2)))
          (setq bitis-aci (+ bitis-aci (/ pi 2)))
        ))
    )
    (progn
      ;; Alt taraf: Ceyrek daire merkezini 2.75mm asagi kaldir
      (setq merkez (polar pt (+ yon asagi-aci) (* scl 2.75)))
      
      (setq p-sol (polar merkez yon (* scl -2.75)))
      
      (setq kol-sol (polar p-sol (+ yon asagi-aci) (* scl 3.25)))
      
      ;; Yay: asagidan (90°) sola (180°) = TAM 90 derece
      (setq baslangic-aci (/ pi 2))     ; 90° - asagi
      (setq bitis-aci pi)               ; 180° - sol
      
      ;; SOLDA iken -90 derece döndür
      (if (>= yon 0.01)
        (progn
          (setq baslangic-aci (- baslangic-aci (/ pi 2)))
          (setq bitis-aci (- bitis-aci (/ pi 2)))
        ))
    )
  )
  
  ;; Sol dikey kol
  (setq ent (entmakex
    (list
      '(0 . "LINE")
      '(100 . "AcDbEntity")
      '(8 . "KAYNAK_SYM")
      '(100 . "AcDbLine")
      (cons 10 p-sol)
      (cons 11 kol-sol)
    )))
  (if ent (setq ent-listesi (append ent-listesi (list ent))))
  
  ;; Ceyrek daire (TAM 90 derece)
  (setq ent (entmakex
    (list
      '(0 . "ARC")
      '(100 . "AcDbEntity")
      '(8 . "KAYNAK_SYM")
      '(100 . "AcDbCircle")
      (cons 10 merkez)
      (cons 40 (* scl 2.75))
      '(100 . "AcDbArc")
      (cons 50 baslangic-aci)
      (cons 51 bitis-aci)
    )))
  (if ent (setq ent-listesi (append ent-listesi (list ent))))
  
  ent-listesi
)

;; PLUG
;; V3.4 DUZELTME: yukari-aci ve asagi-aci parametreleri kullanildi
(defun kaynak-plug (pt yon scl ust-taraf yukari-aci asagi-aci / p1 p2 p3 p4 ent)
  (setq p1 (polar pt yon (* scl -3.75)))
  (setq p2 (polar pt yon (* scl 3.75)))

  (if ust-taraf
    (progn
      (setq p3 (polar p1 (+ yon yukari-aci) (* scl 3.75)))
      (setq p4 (polar p2 (+ yon yukari-aci) (* scl 3.75)))
    )
    (progn
      (setq p3 (polar p1 (+ yon asagi-aci) (* scl 3.75)))
      (setq p4 (polar p2 (+ yon asagi-aci) (* scl 3.75)))
    )
  )
  
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
;;; OLCU METNI
;;;========================================================================

;; V3.4 DUZELTME: yukari-aci ve asagi-aci parametreleri kullanildi
(defun kaynak-olcu-metni-ekle (ref-baslangic yon scl olcu-str ust-taraf yukari-aci asagi-aci / txt-pt txt-aci ent)
  ;; pt2'den 6mm uzakta
  (setq txt-pt (polar ref-baslangic yon (* scl 6.0)))
  
  ;; Referans cizgisinden 3mm yukari veya asagi
  (if ust-taraf
    (setq txt-pt (polar txt-pt (+ yon yukari-aci) (* scl 3.0)))
    (setq txt-pt (polar txt-pt (+ yon asagi-aci) (* scl 3.0)))
  )

  ;; Metin acisi: Daima 0.0 (ters cikmasi engellendi)
  (setq txt-aci 0.0)

  (setq ent (entmakex
    (list
      '(0 . "TEXT")
      '(100 . "AcDbEntity")
      '(8 . "KAYNAK_SYM")
      '(100 . "AcDbText")
      (cons 10 txt-pt)
      (cons 11 txt-pt)
      (cons 40 (* scl 3.75))
      (cons 1 olcu-str)
      (cons 50 txt-aci)
      '(41 . 1.0)
      '(51 . 0.0)
      '(7 . "STANDARD")
      '(71 . 0)
      '(72 . 1)
      '(73 . 2)
    )))
  (if ent (list ent) nil)
)

;;;========================================================================
;; KONTUR EKLE
;;;========================================================================

;; V3.4 DUZELTME: yukari-aci ve asagi-aci parametreleri kullanildi
(defun kaynak-kontur-ekle (pt yon scl kontur yuzey ust-taraf kaynak-tipi yukari-aci asagi-aci / 
                           kont-pt kont-listesi txt-pt ent offset-x offset-y 
                           daire-merkez aci-offset cizgi-p1 cizgi-p2 cizgi-aci)
  (setq kont-listesi '())

  ;; KAYNAK TIPINE GORE OFFSET ve ACI AYARLARI
  (cond
    ;; FILLET (tip "1")
    ((= kaynak-tipi "1")
     (cond
       ;; Dis bombe
       ((= kontur "2")
        (setq offset-x (* scl -1.41))
        (setq offset-y (* scl -1.41))
        ;; Alt taraf + Sağda ise +90° döndür
        (if (and (not ust-taraf) (< yon 0.01))
          (setq aci-offset (* pi 0.75))
          (setq aci-offset (* pi 0.25)))
        ;; SOLDA iken kontur açılarını düzelt
        (if (>= yon 0.01)
          (setq aci-offset (+ aci-offset 
            (if ust-taraf (- (/ pi 2)) pi)))))
       ;; Ic bombe
       ((= kontur "3")
        (setq offset-x (* scl 3.54))
        (setq offset-y (* scl -5.46))
        ;; Alt taraf + Sağda ise +90° döndür
        (if (and (not ust-taraf) (< yon 0.01))
          (setq aci-offset (* pi 0.75))
          (setq aci-offset (* pi 0.25)))
        ;; SOLDA iken kontur açılarını düzelt
        (if (>= yon 0.01)
          (setq aci-offset (+ aci-offset 
            (if ust-taraf (- (/ pi 2)) pi)))))
       ;; Duz
       ((= kontur "1")
        (setq offset-x (* scl 0.93))
        (setq offset-y (* scl -3.57))
        ;; Alt taraf + Sağda ise +90° döndür
        (if (and (not ust-taraf) (< yon 0.01))
          (setq cizgi-aci (* pi 0.25))
          (setq cizgi-aci (* pi -0.25)))
        ;; SOLDA iken kontur açılarını düzelt - sadece üst taraf
        (if (and (>= yon 0.01) ust-taraf)
          (setq cizgi-aci (+ cizgi-aci (/ pi 2)))))
     ))
    
    ;; V-GROOVE (tip "2")
    ((= kaynak-tipi "2")
     (cond
       ((= kontur "2")
        (setq offset-x 0.0)
        (setq offset-y (* scl 0.75))
        (setq aci-offset (* pi 0.5))
        ;; SOLDA iken +180° ekle
        (if (>= yon 0.01)
          (setq aci-offset (+ aci-offset pi))))
       ((= kontur "3")
        (setq offset-x 0.0)
        (setq offset-y (* scl -1.61))
        (setq aci-offset (* pi 0.5))
        ;; SOLDA iken +180° ekle
        (if (>= yon 0.01)
          (setq aci-offset (+ aci-offset pi))))
       ((= kontur "1")
        (setq offset-x 0.0)
        (setq offset-y (* scl -0.43))
        (setq cizgi-aci 0.0))
     ))
    
    ;; BEVEL (tip "3")
    ((= kaynak-tipi "3")
     (cond
      ;; Dis bombe - YAY 65 derece, merkez 1mm saga
      ((= kontur "2")
       (setq offset-x (* scl 1.0))
       (setq offset-y 0.0)
       ;; Alt taraf + Sağda ise +50° döndür
       (if (and (not ust-taraf) (< yon 0.01))
         (setq aci-offset (* pi 0.639))
         (setq aci-offset (* pi 0.361)))
       ;; SOLDA iken kontur açılarını düzelt
       (if (>= yon 0.01)
         (setq aci-offset (+ aci-offset 
           (if ust-taraf (* -2.2689) pi)))))
      ;; Ic bombe - YAY 65 derece, merkez 2.48mm asagi, 4.31mm saga
      ((= kontur "3")
       (setq offset-x (* scl 4.31))
       (setq offset-y (* scl -2.48))
       ;; Alt taraf + Sağda ise +50° döndür
       (if (and (not ust-taraf) (< yon 0.01))
         (setq aci-offset (* pi 0.639))
         (setq aci-offset (* pi 0.361)))
       ;; SOLDA iken kontur açılarını düzelt
       (if (>= yon 0.01)
         (setq aci-offset (+ aci-offset 
           (if ust-taraf (* -2.2689) pi)))))
      ;; Duz
      ((= kontur "1")
       (setq offset-x (* scl 2.9))
       (setq offset-y (* scl -1.0))
       ;; Alt taraf + Sağda ise +50° döndür
       (if (and (not ust-taraf) (< yon 0.01))
         (setq cizgi-aci (* pi 0.139))
         (setq cizgi-aci (* pi -0.139)))
       ;; SOLDA iken kontur açılarını düzelt - sadece üst taraf
       (if (and (>= yon 0.01) ust-taraf)
         (setq cizgi-aci (+ cizgi-aci (/ pi 4)))))
    ))
    
    ;; U-GROOVE (tip "4")
    ((= kaynak-tipi "4")
     (cond
       ((= kontur "2")
        (setq offset-x 0.0)
        (setq offset-y (* scl 0.75))
        (setq aci-offset (* pi 0.5))
        ;; SOLDA iken +180° ekle
        (if (>= yon 0.01)
          (setq aci-offset (+ aci-offset pi))))
       ((= kontur "3")
        (setq offset-x 0.0)
        (setq offset-y (* scl -1.61))
        (setq aci-offset (* pi 0.5))
        ;; SOLDA iken +180° ekle
        (if (>= yon 0.01)
          (setq aci-offset (+ aci-offset pi))))
       ((= kontur "1")
        (setq offset-x 0.0)
        (setq offset-y (* scl -0.43))
        (setq cizgi-aci 0.0))
     ))
    
    ;; J-GROOVE (tip "5")
    ((= kaynak-tipi "5")
     (cond
       ((= kontur "2")
        (setq offset-x (* scl -1.41))
        (setq offset-y (* scl -1.41))
        ;; Alt taraf + Sağda ise +90° döndür
        (if (and (not ust-taraf) (< yon 0.01))
          (setq aci-offset (* pi 0.75))
          (setq aci-offset (* pi 0.25)))
        ;; SOLDA iken kontur açılarını düzelt
        (if (>= yon 0.01)
          (setq aci-offset (+ aci-offset 
            (if ust-taraf (* 1.5 pi) pi)))))
       ((= kontur "3")
        (setq offset-x (* scl 3.54))
        (setq offset-y (* scl -5.46))
        ;; Alt taraf + Sağda ise +90° döndür
        (if (and (not ust-taraf) (< yon 0.01))
          (setq aci-offset (* pi 0.75))
          (setq aci-offset (* pi 0.25)))
        ;; SOLDA iken kontur açılarını düzelt
        (if (>= yon 0.01)
          (setq aci-offset (+ aci-offset 
            (if ust-taraf (- (/ pi 2)) pi)))))
       ((= kontur "1")
        (setq offset-x (* scl 0.93))
        (setq offset-y (* scl -3.57))
        ;; Alt taraf + Sağda ise +90° döndür
        (if (and (not ust-taraf) (< yon 0.01))
          (setq cizgi-aci (* pi 0.25))
          (setq cizgi-aci (* pi -0.25)))
        ;; SOLDA iken kontur açılarını düzelt - sadece üst taraf
        (if (and (>= yon 0.01) ust-taraf)
          (setq cizgi-aci (+ cizgi-aci (/ pi 2)))))
     ))
    
    ;; SQUARE (tip "6")
    ((= kaynak-tipi "6")
     (cond
       ((= kontur "2")
        (setq offset-x 0.0)
        (setq offset-y (* scl 0.75))
        (setq aci-offset (* pi 0.5))
        ;; SOLDA iken +180° ekle
        (if (>= yon 0.01)
          (setq aci-offset (+ aci-offset pi))))
       ((= kontur "3")
        (setq offset-x 0.0)
        (setq offset-y (* scl -0.86))
        (setq aci-offset (* pi 0.5))
        ;; SOLDA iken +180° ekle
        (if (>= yon 0.01)
          (setq aci-offset (+ aci-offset pi))))
       ((= kontur "1")
        (setq offset-x 0.0)
        (setq offset-y (* scl -0.43))
        (setq cizgi-aci 0.0))
     ))
    
    ;; PLUG (tip "7")
    ((= kaynak-tipi "7")
     (cond
       ((= kontur "2")
        (setq offset-x 0.0)
        (setq offset-y 0.0)
        (setq aci-offset (* pi 0.5))
        ;; SOLDA iken +180° ekle
        (if (>= yon 0.01)
          (setq aci-offset (+ aci-offset pi))))
       ((= kontur "3")
        (setq offset-x 0.0)
        (setq offset-y (* scl -1.86))
        (setq aci-offset (* pi 0.5))
        ;; SOLDA iken +180° ekle
        (if (>= yon 0.01)
          (setq aci-offset (+ aci-offset pi))))
       ((= kontur "1")
        (setq offset-x 0.0)
        (setq offset-y (* scl -1.43))
        (setq cizgi-aci 0.0))
     ))
    
    ;; Varsayilan (eski degerler)
    (T
     (setq offset-x 0.0)
     (setq offset-y 0.0)
     (setq aci-offset 0.0)
     (setq cizgi-aci 0.0))
  )

  ;; V3.4 DUZELTME: Kontur pozisyonu hesaplama - yukari-aci ve asagi-aci kullanildi
  (if ust-taraf
    (setq kont-pt (polar pt (+ yon yukari-aci) (* scl 7.5)))
    (setq kont-pt (polar pt (+ yon asagi-aci) (* scl 7.5)))
  )
  
  ;; Offset uygula
  (setq kont-pt (polar kont-pt yon offset-x))
  
  ;; Alt taraf için dikey ofseti (offset-y) ters çevir
  (if (not ust-taraf)
    (setq offset-y (- offset-y))
  )
  
  ;; V3.4 DUZELTME: Dikey offset - yukari-aci kullanildi
  (setq kont-pt (polar kont-pt (+ yon yukari-aci) offset-y))

  (cond
    ;; DUZ CIZGI
    ((= kontur "1")
     ;; Cizgi 6mm uzunlukta, merkez kont-pt'de, belirtilen acida
     (setq cizgi-p1 (polar kont-pt (+ yon cizgi-aci) (* scl -3.0)))
     (setq cizgi-p2 (polar kont-pt (+ yon cizgi-aci) (* scl 3.0)))
     
     (setq ent (entmakex
       (list
         '(0 . "LINE")
         '(100 . "AcDbEntity")
         '(8 . "KAYNAK_SYM")
         '(100 . "AcDbLine")
         (cons 10 cizgi-p1)
         (cons 11 cizgi-p2)
       )))
     (if ent (setq kont-listesi (list ent)))
    )
    
    ;; DIS BOMBE
    ;; V3.4 DUZELTME: yukari-aci ve asagi-aci kullanildi
    ((= kontur "2")
     ;; Daire merkezi kont-pt'den 4.5mm asagi/yukari
     (if ust-taraf
       (setq daire-merkez (polar kont-pt (+ yon asagi-aci) (* scl 4.5)))
       (setq daire-merkez (polar kont-pt (+ yon yukari-aci) (* scl 4.5)))
     )
     
     (setq ent (entmakex
       (list
         '(0 . "ARC")
         '(100 . "AcDbEntity")
         '(8 . "KAYNAK_SYM")
         '(100 . "AcDbCircle")
         (cons 10 daire-merkez)
         (cons 40 (* scl 4.5))
         '(100 . "AcDbArc")
         (cons 50 (if ust-taraf 
                    (+ aci-offset (- yon (* pi 0.236)))
                    (+ aci-offset (+ yon (* pi 0.764)))))
         (cons 51 (if ust-taraf 
                    (+ aci-offset (+ yon (* pi 0.236)))
                    (+ aci-offset (+ yon (* pi 1.236)))))
       )))
     (if ent (setq kont-listesi (list ent)))
    )
    
    ;; IC BOMBE
    ;; V3.4 DUZELTME: yukari-aci ve asagi-aci kullanildi
    ((= kontur "3")
     ;; Daire merkezi kont-pt'den 4.5mm yukari/asagi
     (if ust-taraf
       (setq daire-merkez (polar kont-pt (+ yon yukari-aci) (* scl 4.5)))
       (setq daire-merkez (polar kont-pt (+ yon asagi-aci) (* scl 4.5)))
     )
     
     (setq ent (entmakex
       (list
         '(0 . "ARC")
         '(100 . "AcDbEntity")
         '(8 . "KAYNAK_SYM")
         '(100 . "AcDbCircle")
         (cons 10 daire-merkez)
         (cons 40 (* scl 4.5))
         '(100 . "AcDbArc")
         (cons 50 (if ust-taraf 
                    (+ aci-offset (+ yon (* pi 0.764)))
                    (+ aci-offset (- yon (* pi 0.236)))))
         (cons 51 (if ust-taraf 
                    (+ aci-offset (+ yon (* pi 1.236)))
                    (+ aci-offset (+ yon (* pi 0.236)))))
       )))
     (if ent (setq kont-listesi (list ent)))
    )
  )
  
  ;; Yuzey bitirme harfi
  ;; V3.4 DUZELTME: yukari-aci ve asagi-aci kullanildi
  (if (/= yuzey "0")
    (progn
      (if ust-taraf
        (setq txt-pt (polar kont-pt (+ yon yukari-aci) (* scl 3.75)))
        (setq txt-pt (polar kont-pt (+ yon asagi-aci) (* scl 3.75)))
      )

      (setq ent (entmakex
        (list
          '(0 . "TEXT")
          '(100 . "AcDbEntity")
          '(8 . "KAYNAK_SYM")
          '(100 . "AcDbText")
          (cons 10 txt-pt)
          (cons 11 txt-pt)
          (cons 40 (* scl 3.0))
          (cons 1 (nth (atoi yuzey) '("" "G" "M" "C" "R" "H" "P")))
          '(50 . 0.0)
          '(72 . 1)
          '(73 . 2)
        )))
      (if ent (setq kont-listesi (append kont-listesi (list ent))))
    )
  )
  
  kont-listesi
)

;;;========================================================================
;;; OZEL SEMBOLLER
;;;========================================================================

;; CEVRE KAYNAK DAIRESI
(defun kaynak-cevre-ekle (pt scl / ent)
  (setq ent (entmakex
    (list
      '(0 . "CIRCLE")
      '(100 . "AcDbEntity")
      '(8 . "KAYNAK_SYM")
      '(100 . "AcDbCircle")
      (cons 10 pt)
      (cons 40 (* scl 2.0))
    )))
  (if ent (list ent) nil)
)

;; SANTIYE KAYNAGI BAYRAGI
;; V3.4 DUZELTME: yukari-aci ve asagi-aci parametreleri kullanildi - bayrak her iki yonde de yukari bakiyor
(defun kaynak-bayrak-ekle (pt yon scl yukari-aci asagi-aci / bayrak-pt1 bayrak-pt2 bayrak-pt3 ent-listesi ent)
  (setq ent-listesi '())

  ;; Bayrak diregi 15mm yukari
  (setq bayrak-pt1 (polar pt (+ yon yukari-aci) (* scl 15.0)))
  
  ;; Bayrak geometrisi
  (setq bayrak-pt2 (polar bayrak-pt1 yon (* scl 4.0)))
  (setq bayrak-pt3 (polar bayrak-pt1 (+ yon asagi-aci) (* scl 3.0)))
  
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
  
  ;; Bayrak ucgeni (SOLID)
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

;;;========================================================================
;;; F.P. VE NDT SEMBOLLERI
;;;========================================================================

(defun kaynak-tam-nufuz-ekle (ref-bitis yon scl ndt-var / fp-pt txt-aci ent-listesi ent)
  (setq ent-listesi '())

  (setq fp-pt (polar ref-bitis yon (* scl 21.0)))

  ;; Daire ciz
  (setq ent (entmakex
    (list
      '(0 . "CIRCLE")
      '(100 . "AcDbEntity")
      '(8 . "KAYNAK_SYM")
      '(100 . "AcDbCircle")
      (cons 10 fp-pt)
      (cons 40 (* scl 3.75))
    )))
  (if ent (setq ent-listesi (append ent-listesi (list ent))))

  (setq txt-aci 0.0)

  (setq ent (entmakex
    (list
      '(0 . "TEXT")
      '(100 . "AcDbEntity")
      '(8 . "KAYNAK_SYM")
      '(100 . "AcDbText")
      (cons 10 fp-pt)
      (cons 11 fp-pt)
      (cons 40 (* scl 2.7))
      (cons 1 "F.P.")
      (cons 50 txt-aci)
      '(72 . 1)
      '(73 . 2)
    )))
  (if ent (setq ent-listesi (append ent-listesi (list ent))))
  
  ent-listesi
)

(defun kaynak-ndt-ekle (ref-bitis yon scl fp-var / ndt-pt txt-aci offset ent-listesi ent)
  (setq ent-listesi '())

  ;; Eger F.P. varsa ondan sonra (29.5), yoksa WPS'den sonra (21.0)
  (if fp-var
    (setq offset (* scl 29.5))  ; F.P.'den sonra
    (setq offset (* scl 21.0))  ; WPS'den sonra
  )
  
  (setq ndt-pt (polar ref-bitis yon offset))

  ;; Daire ciz
  (setq ent (entmakex
    (list
      '(0 . "CIRCLE")
      '(100 . "AcDbEntity")
      '(8 . "KAYNAK_SYM")
      '(100 . "AcDbCircle")
      (cons 10 ndt-pt)
      (cons 40 (* scl 3.75))
    )))
  (if ent (setq ent-listesi (append ent-listesi (list ent))))

  (setq txt-aci 0.0)

  (setq ent (entmakex
    (list
      '(0 . "TEXT")
      '(100 . "AcDbEntity")
      '(8 . "KAYNAK_SYM")
      '(100 . "AcDbText")
      (cons 10 ndt-pt)
      (cons 11 ndt-pt)
      (cons 40 (* scl 2.25))
      (cons 1 "NDT")
      (cons 50 txt-aci)
      '(72 . 1)
      '(73 . 2)
    )))
  (if ent (setq ent-listesi (append ent-listesi (list ent))))
  
  ent-listesi
)

;;;========================================================================
;;; WPS KUTUSU VE NOTLAR
;;;========================================================================

;; V3.4 DUZELTME: Notlar - asagi-aci parametresi kullanildi
(defun kaynak-metinler-ekle (ref-baslangic ref-bitis yon scl yukari-aci asagi-aci / txt-pt txt-aci kuyruk-pt ent-listesi 
                              kutu-p1 kutu-p2 kutu-p3 kutu-p4 ent)
  (setq ent-listesi '())
  
  (setq txt-aci 0.0)
  
  ;; WPS KUTUSU
  (if (/= *WPS-NO* "")
    (progn
      (setq kuyruk-pt (polar ref-bitis yon (* scl 4.5)))

      (setq ent (entmakex
        (list
          '(0 . "LINE")
          '(100 . "AcDbEntity")
          '(8 . "KAYNAK_SYM")
          '(100 . "AcDbLine")
          (cons 10 ref-bitis)
          (cons 11 (polar kuyruk-pt (+ yon (/ pi 4)) (* scl 3.75)))
        )))
      (if ent (setq ent-listesi (append ent-listesi (list ent))))

      (setq ent (entmakex
        (list
          '(0 . "LINE")
          '(100 . "AcDbEntity")
          '(8 . "KAYNAK_SYM")
          '(100 . "AcDbLine")
          (cons 10 ref-bitis)
          (cons 11 (polar kuyruk-pt (- yon (/ pi 4)) (* scl 3.75)))
        )))
      (if ent (setq ent-listesi (append ent-listesi (list ent))))

      (setq txt-pt (polar kuyruk-pt yon (* scl 7.5)))

      (setq kutu-p1 (polar txt-pt (+ yon pi) (* scl 4.5)))
      (setq kutu-p1 (polar kutu-p1 (+ yon (/ pi 2)) (* scl 3.75)))
      (setq kutu-p2 (polar kutu-p1 yon (* scl 9.0)))
      (setq kutu-p3 (polar kutu-p2 (- yon (/ pi 2)) (* scl 7.5)))
      (setq kutu-p4 (polar kutu-p1 (- yon (/ pi 2)) (* scl 7.5)))
      
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
      
      (setq ent (entmakex
        (list
          '(0 . "TEXT")
          '(100 . "AcDbEntity")
          '(8 . "KAYNAK_SYM")
          '(100 . "AcDbText")
          (cons 10 txt-pt)
          (cons 11 txt-pt)
          (cons 40 (* scl 3.0))
          (cons 1 *WPS-NO*)
          (cons 50 txt-aci)
          '(72 . 1)
          '(73 . 2)
        )))
      (if ent (setq ent-listesi (append ent-listesi (list ent))))
    )
  )
  
  ;; Ek notlar - V3.4 DUZELTME: asagi-aci kullanildi
  (setq txt-pt (polar ref-baslangic (+ yon asagi-aci) (* scl 15))) 
  
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
      (setq txt-pt (polar txt-pt (+ yon asagi-aci) (* scl 2.5)))
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
      (setq txt-pt (polar txt-pt (+ yon asagi-aci) (* scl 2.5)))
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
              (list *KAYNAK-APP-ID* (cons 1000 "KAYNAK_SEMBOLU")
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
                (cons 1000 *KAYNAK-YONTEMI*)
                (cons 1000 *UST-KAYNAK-ADETI*)
                (cons 1000 *UST-KAYNAK-UZUNLUGU*)
                (cons 1000 *UST-KAYNAK-BOSLUGU*)
                (cons 1000 *ALT-KAYNAK-ADETI*)
                (cons 1000 *ALT-KAYNAK-UZUNLUGU*)
                (cons 1000 *ALT-KAYNAK-BOSLUGU*)
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

(defun kaynak-xdata-oku (ent / ed xdata)
  (setq ed (entget ent (list *KAYNAK-APP-ID*)))
  (setq xdata (assoc -3 ed))
  (if xdata
    (setq xdata (cadr (car (cdr xdata))))
  )
  xdata
)

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
      (setq *KAYNAK-YONTEMI* (cdr (nth 18 xdata)))
      (setq *UST-KAYNAK-ADETI* (cdr (nth 19 xdata)))
      (setq *UST-KAYNAK-UZUNLUGU* (cdr (nth 20 xdata)))
      (setq *UST-KAYNAK-BOSLUGU* (cdr (nth 21 xdata)))
      (setq *ALT-KAYNAK-ADETI* (cdr (nth 22 xdata)))
      (setq *ALT-KAYNAK-UZUNLUGU* (cdr (nth 23 xdata)))
      (setq *ALT-KAYNAK-BOSLUGU* (cdr (nth 24 xdata)))
      (setq pt1 (cdr (nth 25 xdata)))
      (setq pt2 (cdr (nth 26 xdata)))
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

(defun kaynak-cift-tikla (reactor info / ss ent xdata kaynak-id pts scl i)
  (if (setq ss (ssget "_I"))
    (progn
      (setq ent (ssname ss 0))
      (setq xdata (kaynak-xdata-oku ent))
      
      (if xdata
        (progn
          (setq kaynak-id (cdr (nth 1 xdata)))
          (setq pts (kaynak-parametreleri-cikart xdata))
          
          (if (kaynak-dialog T)
            (progn
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
              
              (setq scl (getvar "DIMSCALE"))
              (if (or (null scl) (<= scl 0))
                (setq scl 1.0)
              )
              (setq scl (/ scl 2.0))
              
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

(kaynak-basla)

(princ "\n============================================================")
(princ "\nKAYNAK SEMBOLU SISTEMI V3.4 BASARIYLA YUKLENDI")
(princ "\n============================================================")
(princ "\nANA KOMUT: KS")
(princ "\n")
(princ "\nYENI OZELLIKLER V3.4 (Kritik Duzeltmeler):")
(princ "\n• Sol/Sag yon hesaplama hatalari tamamen giderildi")
(princ "\n• Ust kaynak her iki yonde de uste, alt kaynak alta")
(princ "\n• Bayrak her iki yonde de yukari bakar")
(princ "\n• Notlar her iki yonde de asagiya yazilir")
(princ "\n• Kontur ve sekil cizimleri her iki yonde de dogru")
(princ "\n• Tum olculer, WPS, F.P., NDT aynen korundu")
(princ "\n")
(princ "\nOZELLIKLER:")
(princ "\n• AWS A2.4 Standardina tam uyumlu")
(princ "\n• 8 farkli kaynak tipi (Fillet, V-Groove, Bevel, vb.)")
(princ "\n• Kontur secenekleri (Duz/Dis Bombe/Ic Bombe)")
(princ "\n• Yuzey bitirme yontemleri (G/M/C/R/H/P)")
(princ "\n• XDATA ile veri saklama")
(princ "\n• Cift tiklama ile duzenleme")
(princ "\n• DIMSCALE'e gore otomatik olcekleme")
(princ "\n============================================================")
(princ)