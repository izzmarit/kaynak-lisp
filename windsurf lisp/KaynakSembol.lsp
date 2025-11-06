;;;========================================================================
;;; KaynakSembol.lsp - Profesyonel Kaynak Sembolu Yerlestirme Sistemi V4.0
;;; AutoCAD 2025 LT icin - AWS A2.4 Standardi
;;; 
;;; Komut: KS
;;; V4.0 - Optimize Edilmis Versiyon - Murat KARA
;;; 
;;; YENILIKLER V4.0:
;;; - Kullanici dostu DCL arayuzu
;;; - Onizleme penceresi eklendi
;;; - Detayli aciklamalar ve yardim
;;; - Turkce karakter kullanilmadi (o,c,u,s yerine normal karakterler)
;;; - Performans iyilestirmeleri
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
  
  ;; Text style yoksa olustur (Arial fontu)
  (if (not (tblsearch "STYLE" "KAYNAK_TEXT"))
    (entmake '((0 . "STYLE")
               (100 . "AcDbSymbolTableRecord")
               (100 . "AcDbTextStyleTableRecord")
               (2 . "KAYNAK_TEXT")
               (70 . 0)
               (40 . 0.0)
               (41 . 1.0)
               (50 . 0.0)
               (71 . 0)
               (42 . 2.5)
               (3 . "arial.ttf")
               (4 . "")))
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
  (princ "\nKAYNAK SEMBOLU YERLESTIRME SISTEMI V4.0")
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
  
  ;; Baslangicta METOD parametrelerini kontrol et
  (if (= *KAYNAK-YONTEMI* "1")
    (mode_tile "metod_parametreler" 0) ; Goster
    (mode_tile "metod_parametreler" 1) ; Gizle
  )
  
  ;; Action callbacks - Onizleme ile
  (action_tile "kaynak_yontemi" 
    "(progn (setq *KAYNAK-YONTEMI* $value) (metod-parametreleri-goster-gizle $value) (kaynak-onizleme-guncelle))")
  (action_tile "ust_kaynak_adeti" "(progn (setq *UST-KAYNAK-ADETI* $value) (kaynak-onizleme-guncelle))")
  (action_tile "ust_kaynak_uzunlugu" "(progn (setq *UST-KAYNAK-UZUNLUGU* $value) (kaynak-onizleme-guncelle))")
  (action_tile "ust_kaynak_boslugu" "(progn (setq *UST-KAYNAK-BOSLUGU* $value) (kaynak-onizleme-guncelle))")
  (action_tile "alt_kaynak_adeti" "(progn (setq *ALT-KAYNAK-ADETI* $value) (kaynak-onizleme-guncelle))")
  (action_tile "alt_kaynak_uzunlugu" "(progn (setq *ALT-KAYNAK-UZUNLUGU* $value) (kaynak-onizleme-guncelle))")
  (action_tile "alt_kaynak_boslugu" "(progn (setq *ALT-KAYNAK-BOSLUGU* $value) (kaynak-onizleme-guncelle))")
  (action_tile "ust_yontem" "(progn (setq *UST-YONTEM* $value) (kaynak-onizleme-guncelle))")
  (action_tile "alt_yontem" "(progn (setq *ALT-YONTEM* $value) (kaynak-onizleme-guncelle))")
  (action_tile "ust_olcu" "(progn (setq *UST-OLCU* $value) (kaynak-onizleme-guncelle))")
  (action_tile "alt_olcu" "(progn (setq *ALT-OLCU* $value) (kaynak-onizleme-guncelle))")
  (action_tile "ust_kontur" "(progn (setq *UST-KONTUR* $value) (kaynak-onizleme-guncelle))")
  (action_tile "alt_kontur" "(progn (setq *ALT-KONTUR* $value) (kaynak-onizleme-guncelle))")
  (action_tile "ust_yuzey" "(progn (setq *UST-YUZEY* $value) (kaynak-onizleme-guncelle))")
  (action_tile "alt_yuzey" "(progn (setq *ALT-YUZEY* $value) (kaynak-onizleme-guncelle))")
  (action_tile "wps_no" "(progn (setq *WPS-NO* $value) (kaynak-onizleme-guncelle))")
  (action_tile "santiye_kaynak" "(progn (setq *SANTIYE-FLAG* $value) (kaynak-onizleme-guncelle))")
  (action_tile "cevre_kaynak" "(progn (setq *CEVRE-FLAG* $value) (kaynak-onizleme-guncelle))")
  (action_tile "tam_nufuziyet" "(progn (setq *TAM-NUF-FLAG* $value) (kaynak-onizleme-guncelle))")
  (action_tile "ndt_gerekli" "(progn (setq *NDT-FLAG* $value) (kaynak-onizleme-guncelle))")
  (action_tile "not1" "(setq *NOT1* $value)")
  (action_tile "not2" "(setq *NOT2* $value)")
  (action_tile "not3" "(setq *NOT3* $value)")
  
  (action_tile "accept" "(done_dialog 1)")
  (action_tile "cancel" "(done_dialog 0)")
  (action_tile "help" "(kaynak-yardim-goster)")
  
  ;; Ilk onizlemeyi goster - start_dialog oncesi
  (if (start_image "onizleme_img")
    (progn
      (kaynak-onizleme-ilk-cizim)
      (end_image)
    )
    (alert "UYARI: Onizleme yuklenemedı!")
  )
  
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
;;; YARDIMCI FONKSIYONLAR - GLOBAL
;;;========================================================================

;; METOD parametrelerini goster/gizle
(defun metod-parametreleri-goster-gizle (deger)
  (if (= deger "1")
    (mode_tile "metod_parametreler" 0) ; Goster
    (mode_tile "metod_parametreler" 1) ; Gizle
  )
  (princ)
)

;;;========================================================================
;;; ONIZLEME FONKSIYONLARI
;;;========================================================================

;; Yardimci fonksiyon - vector_image icin tam sayi koordinatlari
(defun v-img (x1 y1 x2 y2 col)
  (vector_image (fix x1) (fix y1) (fix x2) (fix y2) col)
)

;; Ilk cizim - dialog acildiginda
(defun kaynak-onizleme-ilk-cizim (/ w h)
  (setq w (fix (dimx_tile "onizleme_img")))
  (setq h (fix (dimy_tile "onizleme_img")))
  
  ;; Arka plan temizle (siyah)
  (fill_image 0 0 w h 0)
  
  ;; Kaynak sembolunu ciz
  (kaynak-onizleme-ciz "onizleme_img")
  
  (princ)
)

;; Onizleme guncelleme ana fonksiyonu - DIALOG ACIKKEN CALISIR
(defun kaynak-onizleme-guncelle ()
  (if (start_image "onizleme_img")
    (progn
      ;; Arka plan temizle (siyah)
      (fill_image 0 0 (fix (dimx_tile "onizleme_img")) (fix (dimy_tile "onizleme_img")) 0)
      
      ;; Kaynak sembolunu ciz
      (kaynak-onizleme-ciz "onizleme_img")
      
      (end_image)
    )
  )
  (princ)
)

;; Onizleme cizim fonksiyonu - ANA CIZIME TAM UYUMLU - EKRANA TAM SIGAR
(defun kaynak-onizleme-ciz (img-key / cx cy w h scl ref-x ref-bitis sembol-merkez txt-x txt-y
                             wps-kuyruk wps-kutu-x fp-x ndt-x bayrak-x bayrak-y r kutu-w kutu-h z-x z-y)
  (setq w (fix (dimx_tile img-key)))
  (setq h (fix (dimy_tile img-key)))
  (setq cx (fix (/ w 2)))
  (setq cy (fix (/ h 2)))
  
  ;; Olcek faktoru - Ekrana tam sigacak sekilde
  ;; Metod kaynak icin: 52mm + 30mm (WPS+FP+NDT) = 82mm toplam
  ;; Standart kaynak icin: 24mm + 30mm = 54mm toplam
  (if (= *KAYNAK-YONTEMI* "1")
    (setq scl (/ (- w 40) 82.0))  ; Metod kaynak - kenarlarda 20px bosluk
    (setq scl (/ (- w 40) 54.0))  ; Standart kaynak
  )
  
  ;; Minimum olcek 1.5, maksimum 3.0
  (if (< scl 1.5) (setq scl 1.5))
  (if (> scl 3.0) (setq scl 3.0))
  
  ;; Referans baslangic noktasi (merkezden sola)
  (if (= *KAYNAK-YONTEMI* "1")
    (setq ref-x (- cx (* scl 41)))  ; Metod: 82/2 = 41
    (setq ref-x (- cx (* scl 27)))  ; Standart: 54/2 = 27
  )
  
  ;; Referans bitis noktasi (uzunluk: 24mm standart, 52mm metod)
  (if (= *KAYNAK-YONTEMI* "1")
    (setq ref-bitis (+ ref-x (* scl 52)))
    (setq ref-bitis (+ ref-x (* scl 24)))
  )
  
  ;; CEVRE KAYNAK DAIRESI (ref-x noktasinda, ok ucu gibi)
  (if (= *CEVRE-FLAG* "1")
    (progn
      (setq r (* scl 2.0))
      (v-img (fix (- ref-x r)) (fix (- cy r)) (fix (- ref-x r)) (fix (+ cy r)) 2)
      (v-img (fix (- ref-x r)) (fix (+ cy r)) (fix (+ ref-x r)) (fix (+ cy r)) 2)
      (v-img (fix (+ ref-x r)) (fix (+ cy r)) (fix (+ ref-x r)) (fix (- cy r)) 2)
      (v-img (fix (+ ref-x r)) (fix (- cy r)) (fix (- ref-x r)) (fix (- cy r)) 2)
    )
  )
  
  ;; SANTIYE BAYRAGI (ref-x noktasinda, yukarida)
  (if (= *SANTIYE-FLAG* "1")
    (progn
      ;; Direk (15mm yukari)
      (setq bayrak-x ref-x)
      (setq bayrak-y (- cy (* scl 15)))
      (v-img (fix bayrak-x) (fix cy) (fix bayrak-x) (fix bayrak-y) 2)
      ;; Bayrak ucgeni
      (v-img (fix bayrak-x) (fix bayrak-y) (fix (+ bayrak-x (* scl 4))) (fix (- bayrak-y (* scl 1.5))) 2)
      (v-img (fix (+ bayrak-x (* scl 4))) (fix (- bayrak-y (* scl 1.5))) (fix bayrak-x) (fix (- bayrak-y (* scl 3))) 2)
      (v-img (fix bayrak-x) (fix (- bayrak-y (* scl 3))) (fix bayrak-x) (fix bayrak-y) 2)
    )
  )
  
  ;; REFERANS CIZGISI (yatay)
  (v-img (fix ref-x) (fix cy) (fix ref-bitis) (fix cy) 2)
  
  ;; Sembol merkezi (ref-x'ten 15.5mm saga)
  (setq sembol-merkez (+ ref-x (* scl 15.5)))
  
  ;; UST SEMBOL (referans yukarisinda, 0.5mm yukari)
  (if (/= *UST-YONTEM* "0")
    (progn
      (setq txt-x sembol-merkez)
      (setq txt-y (- cy (* scl 0.5)))
      
      ;; Kaynak tipi sembolu
      (kaynak-onizleme-tip-ciz img-key txt-x txt-y *UST-YONTEM* T scl)
      
      ;; Ust olcu (ref-x'ten 6mm saga, 3mm yukari)
      (if (and (/= *UST-OLCU* "") (/= *UST-OLCU* "0"))
        (kaynak-onizleme-text-ciz img-key (fix (+ ref-x (* scl 6))) (fix (- cy (* scl 3))) *UST-OLCU*)
      )
      
      ;; Ust kontur (sembolden 7.5mm yukari)
      (if (/= *UST-KONTUR* "0")
        (kaynak-onizleme-kontur-ciz img-key (fix txt-x) (fix (- txt-y (* scl 7.5))) *UST-KONTUR* T scl)
      )
      
      ;; Ust yuzey (konturdan 7.5mm yukari)
      (if (/= *UST-YUZEY* "0")
        (kaynak-onizleme-yuzey-harf-ciz img-key (fix txt-x) (fix (- txt-y (* scl 15))) *UST-YUZEY*)
      )
    )
  )
  
  ;; ALT SEMBOL (referans asagisinda, 0.5mm asagi)
  (if (/= *ALT-YONTEM* "0")
    (progn
      (setq txt-x sembol-merkez)
      (setq txt-y (+ cy (* scl 0.5)))
      
      ;; Kaynak tipi sembolu
      (kaynak-onizleme-tip-ciz img-key txt-x txt-y *ALT-YONTEM* nil scl)
      
      ;; Alt olcu (ref-x'ten 6mm saga, 3mm asagi)
      (if (and (/= *ALT-OLCU* "") (/= *ALT-OLCU* "0"))
        (kaynak-onizleme-text-ciz img-key (fix (+ ref-x (* scl 6))) (fix (+ cy (* scl 3))) *ALT-OLCU*)
      )
      
      ;; Alt kontur (sembolden 7.5mm asagi)
      (if (/= *ALT-KONTUR* "0")
        (kaynak-onizleme-kontur-ciz img-key (fix txt-x) (fix (+ txt-y (* scl 7.5))) *ALT-KONTUR* nil scl)
      )
      
      ;; Alt yuzey (konturdan 7.5mm asagi)
      (if (/= *ALT-YUZEY* "0")
        (kaynak-onizleme-yuzey-harf-ciz img-key (fix txt-x) (fix (+ txt-y (* scl 15))) *ALT-YUZEY*)
      )
    )
  )
  
  ;; WPS KUTUSU (ref-bitis'ten saga)
  (if (and (/= *WPS-NO* "") (/= *WPS-NO* "0"))
    (progn
      ;; Kuyruk baslangici (ref-bitis'ten 4.5mm saga)
      (setq wps-kuyruk (+ ref-bitis (* scl 4.5)))
      
      ;; Kuyruk cizgileri (ok sekli) - FIX ile
      (v-img (fix ref-bitis) (fix cy) (fix (+ wps-kuyruk (* scl 2.5))) (fix (- cy (* scl 2.5))) 2)
      (v-img (fix ref-bitis) (fix cy) (fix (+ wps-kuyruk (* scl 2.5))) (fix (+ cy (* scl 2.5))) 2)
      
      ;; Kutu sol kenari (kuyruktan 3mm saga)
      (setq wps-kutu-x (+ wps-kuyruk (* scl 3)))
      
      ;; Kutu boyutu (9mm x 7.5mm)
      (setq kutu-w (* scl 9))
      (setq kutu-h (* scl 7.5))
      
      ;; Kutu cizimi - FIX ile
      (v-img (fix wps-kutu-x) (fix (- cy (/ kutu-h 2))) (fix wps-kutu-x) (fix (+ cy (/ kutu-h 2))) 2)
      (v-img (fix wps-kutu-x) (fix (- cy (/ kutu-h 2))) (fix (+ wps-kutu-x kutu-w)) (fix (- cy (/ kutu-h 2))) 2)
      (v-img (fix (+ wps-kutu-x kutu-w)) (fix (- cy (/ kutu-h 2))) (fix (+ wps-kutu-x kutu-w)) (fix (+ cy (/ kutu-h 2))) 2)
      (v-img (fix (+ wps-kutu-x kutu-w)) (fix (+ cy (/ kutu-h 2))) (fix wps-kutu-x) (fix (+ cy (/ kutu-h 2))) 2)
      
      ;; WPS metni (kutu ortasinda)
      (kaynak-onizleme-text-ciz img-key (fix (+ wps-kutu-x (/ kutu-w 2))) (fix cy) *WPS-NO*)
      
      ;; F.P. merkez pozisyonu (kutu sag kenarından 0.75mm bosluk + 3.75mm yaricap)
      (setq fp-x (+ wps-kutu-x kutu-w (* scl 0.75) (* scl 3.75)))
    )
    ;; WPS yoksa F.P. varsayilan pozisyonda
    (setq fp-x (+ ref-bitis (* scl 21)))
  )
  
  ;; TAM NUFUZIYET F.P. (daire + metin)
  (if (= *TAM-NUF-FLAG* "1")
    (progn
      (setq r (* scl 3.75))
      ;; Daire - FIX ile
      (v-img (fix (- fp-x r)) (fix (- cy r)) (fix (- fp-x r)) (fix (+ cy r)) 2)
      (v-img (fix (- fp-x r)) (fix (+ cy r)) (fix (+ fp-x r)) (fix (+ cy r)) 2)
      (v-img (fix (+ fp-x r)) (fix (+ cy r)) (fix (+ fp-x r)) (fix (- cy r)) 2)
      (v-img (fix (+ fp-x r)) (fix (- cy r)) (fix (- fp-x r)) (fix (- cy r)) 2)
      ;; Metin
      (kaynak-onizleme-text-ciz img-key (fix fp-x) (fix cy) "FP")
      
      ;; NDT merkez pozisyonu (F.P. sag kenarından 0.75mm bosluk + 3mm yaricap)
      (setq ndt-x (+ fp-x r (* scl 0.75) (* scl 3)))
    )
    ;; F.P. yoksa NDT varsayilan pozisyonda
    (setq ndt-x (+ ref-bitis (* scl 30)))
  )
  
  ;; NDT (daire + metin)
  (if (= *NDT-FLAG* "1")
    (progn
      (setq r (* scl 3))
      ;; Daire - FIX ile
      (v-img (fix (- ndt-x r)) (fix (- cy r)) (fix (- ndt-x r)) (fix (+ cy r)) 2)
      (v-img (fix (- ndt-x r)) (fix (+ cy r)) (fix (+ ndt-x r)) (fix (+ cy r)) 2)
      (v-img (fix (+ ndt-x r)) (fix (+ cy r)) (fix (+ ndt-x r)) (fix (- cy r)) 2)
      (v-img (fix (+ ndt-x r)) (fix (- cy r)) (fix (- ndt-x r)) (fix (- cy r)) 2)
      ;; Metin
      (kaynak-onizleme-text-ciz img-key (fix ndt-x) (fix cy) "NDT")
    )
  )
  
  ;; METOD KAYNAK Z ISARETI VE PARAMETRELER
  (if (= *KAYNAK-YONTEMI* "1")
    (progn
      ;; Z isareti (ref-x'ten 26mm saga, 8mm yukari)
      (setq z-x (+ ref-x (* scl 26)))
      (setq z-y (- cy (* scl 8)))
      (kaynak-onizleme-z-ciz img-key z-x z-y scl)
      
      ;; UST TARAF METOD parametreleri (Z'den saga, 30.5mm ve 48mm, 5mm yukari)
      (if (and (/= *UST-KAYNAK-ADETI* "") (/= *UST-KAYNAK-UZUNLUGU* ""))
        (kaynak-onizleme-text-ciz img-key (fix (+ ref-x (* scl 30.5))) (fix (- cy (* scl 5))) 
          (strcat *UST-KAYNAK-ADETI* "x" *UST-KAYNAK-UZUNLUGU*))
      )
      (if (/= *UST-KAYNAK-BOSLUGU* "")
        (kaynak-onizleme-text-ciz img-key (fix (+ ref-x (* scl 48))) (fix (- cy (* scl 5))) *UST-KAYNAK-BOSLUGU*)
      )
      
      ;; ALT TARAF METOD parametreleri (Z'den saga, 30.5mm ve 48mm, 5mm asagi)
      (if (and (/= *ALT-KAYNAK-ADETI* "") (/= *ALT-KAYNAK-UZUNLUGU* ""))
        (kaynak-onizleme-text-ciz img-key (fix (+ ref-x (* scl 30.5))) (fix (+ cy (* scl 5))) 
          (strcat *ALT-KAYNAK-ADETI* "x" *ALT-KAYNAK-UZUNLUGU*))
      )
      (if (/= *ALT-KAYNAK-BOSLUGU* "")
        (kaynak-onizleme-text-ciz img-key (fix (+ ref-x (* scl 48))) (fix (+ cy (* scl 5))) *ALT-KAYNAK-BOSLUGU*)
      )
    )
  )
  
  (princ)
)

;; Kaynak tipi cizimi
(defun kaynak-onizleme-tip-ciz (img-key x y tip ust-taraf scl / x1 y1 x2 y2 x3 y3 h i)
  (setq h (* 5 scl))
  
  (cond
    ;; FILLET - Ucgen (1)
    ((= tip "1")
     (if ust-taraf
       (progn
         (setq x1 (- x h) y1 y)
         (setq x2 x y2 (- y h))
         (setq x3 x y3 y)
         (v-img x1 y1 x2 y2 2)
         (v-img x2 y2 x3 y3 2)
         (v-img x3 y3 x1 y1 2)
       )
       (progn
         (setq x1 (- x h) y1 y)
         (setq x2 x y2 (+ y h))
         (setq x3 x y3 y)
         (v-img x1 y1 x2 y2 2)
         (v-img x2 y2 x3 y3 2)
         (v-img x3 y3 x1 y1 2)
       )
     )
    )
    
    ;; V-GROOVE - V sekli (2)
    ((= tip "2")
     (if ust-taraf
       (progn
         (v-img (- x (* 0.5 h)) (- y h) x y 2)
         (v-img x y (+ x (* 0.5 h)) (- y h) 2)
       )
       (progn
         (v-img (- x (* 0.5 h)) (+ y h) x y 2)
         (v-img x y (+ x (* 0.5 h)) (+ y h) 2)
       )
     )
    )
    
    ;; BEVEL - Pahli (3)
    ((= tip "3")
     (if ust-taraf
       (progn
         (v-img x y x (- y h) 2)
         (v-img x (- y h) (+ x h) y 2)
       )
       (progn
         (v-img x y x (+ y h) 2)
         (v-img x (+ y h) (+ x h) y 2)
       )
     )
    )
    
    ;; U-GROOVE - U sekli (4)
    ((= tip "4")
     (if ust-taraf
       (progn
         (v-img (- x (* 0.4 h)) y (- x (* 0.4 h)) (- y (* 0.8 h)) 2)
         (v-img (- x (* 0.4 h)) (- y (* 0.8 h)) (+ x (* 0.4 h)) (- y (* 0.8 h)) 2)
         (v-img (+ x (* 0.4 h)) (- y (* 0.8 h)) (+ x (* 0.4 h)) y 2)
       )
       (progn
         (v-img (- x (* 0.4 h)) y (- x (* 0.4 h)) (+ y (* 0.8 h)) 2)
         (v-img (- x (* 0.4 h)) (+ y (* 0.8 h)) (+ x (* 0.4 h)) (+ y (* 0.8 h)) 2)
         (v-img (+ x (* 0.4 h)) (+ y (* 0.8 h)) (+ x (* 0.4 h)) y 2)
       )
     )
    )
    
    ;; J-GROOVE - J sekli (5)
    ((= tip "5")
     (if ust-taraf
       (progn
         (v-img x y x (- y h) 2)
         (v-img x (- y h) (+ x (* 0.5 h)) (- y (* 0.7 h)) 2)
         (v-img (+ x (* 0.5 h)) (- y (* 0.7 h)) (+ x (* 0.5 h)) y 2)
       )
       (progn
         (v-img x y x (+ y h) 2)
         (v-img x (+ y h) (+ x (* 0.5 h)) (+ y (* 0.7 h)) 2)
         (v-img (+ x (* 0.5 h)) (+ y (* 0.7 h)) (+ x (* 0.5 h)) y 2)
       )
     )
    )
    
    ;; SQUARE - Dikdortgen (6)
    ((= tip "6")
     (if ust-taraf
       (progn
         (v-img (- x (* 0.4 h)) y (- x (* 0.4 h)) (- y h) 2)
         (v-img (- x (* 0.4 h)) (- y h) (+ x (* 0.4 h)) (- y h) 2)
         (v-img (+ x (* 0.4 h)) (- y h) (+ x (* 0.4 h)) y 2)
       )
       (progn
         (v-img (- x (* 0.4 h)) y (- x (* 0.4 h)) (+ y h) 2)
         (v-img (- x (* 0.4 h)) (+ y h) (+ x (* 0.4 h)) (+ y h) 2)
         (v-img (+ x (* 0.4 h)) (+ y h) (+ x (* 0.4 h)) y 2)
       )
     )
    )
    
    ;; PLUG - Dolu dikdortgen (7)
    ((= tip "7")
     (if ust-taraf
       (progn
         (v-img (- x (* 0.4 h)) y (- x (* 0.4 h)) (- y h) 2)
         (v-img (- x (* 0.4 h)) (- y h) (+ x (* 0.4 h)) (- y h) 2)
         (v-img (+ x (* 0.4 h)) (- y h) (+ x (* 0.4 h)) y 2)
         (v-img (+ x (* 0.4 h)) y (- x (* 0.4 h)) y 2)
         ;; Doldur - cizgilerle
         (setq i 0)
         (repeat (fix (* 0.8 h))
           (v-img (- x (* 0.4 h)) (- (- y h) i) (+ x (* 0.4 h)) (- (- y h) i) 2)
           (setq i (1+ i))
         )
       )
       (progn
         (v-img (- x (* 0.4 h)) y (- x (* 0.4 h)) (+ y h) 2)
         (v-img (- x (* 0.4 h)) (+ y h) (+ x (* 0.4 h)) (+ y h) 2)
         (v-img (+ x (* 0.4 h)) (+ y h) (+ x (* 0.4 h)) y 2)
         (v-img (+ x (* 0.4 h)) y (- x (* 0.4 h)) y 2)
         ;; Doldur - cizgilerle
         (setq i 0)
         (repeat (fix h)
           (v-img (- x (* 0.4 h)) (+ y i) (+ x (* 0.4 h)) (+ y i) 2)
           (setq i (1+ i))
         )
       )
     )
    )
  )
  (princ)
)

;; Kontur cizimi - BASITLESTIRILMIS AMA DOGRU
(defun kaynak-onizleme-kontur-ciz (img-key x y kontur ust-taraf scl / w cx cy arc-h)
  (setq w (* 3 scl))
  (setq cx x)
  (setq cy y)
  (setq arc-h (* scl 1.5))  ; Yay yuksekligi
  
  (cond
    ;; DUZ CIZGI (1) - Yatay cizgi
    ((= kontur "1")
     (v-img (fix (- cx w)) (fix cy) (fix (+ cx w)) (fix cy) 2)
    )
    
    ;; DIS BOMBE (2) - Disbukey (convex) yay
    ;; Ust taraf: Sembol yukarida, kontur daha yukari -> yay YUKARI bakiyor
    ;; Alt taraf: Sembol asagida, kontur daha asagi -> yay ASAGI bakiyor
    ((= kontur "2")
     (if ust-taraf
       ;; Ust taraf: Yay YUKARI bakiyor (konveks yukari)
       (progn
         (v-img (fix (- cx w)) (fix cy) (fix cx) (fix (- cy arc-h)) 2)
         (v-img (fix cx) (fix (- cy arc-h)) (fix (+ cx w)) (fix cy) 2)
       )
       ;; Alt taraf: Yay ASAGI bakiyor (konveks asagi)
       (progn
         (v-img (fix (- cx w)) (fix cy) (fix cx) (fix (+ cy arc-h)) 2)
         (v-img (fix cx) (fix (+ cy arc-h)) (fix (+ cx w)) (fix cy) 2)
       )
     )
    )
    
    ;; IC BOMBE (3) - Icbukey (concave) yay
    ;; Ust taraf: Sembol yukarida, kontur daha yukari -> yay ASAGI bakiyor
    ;; Alt taraf: Sembol asagida, kontur daha asagi -> yay YUKARI bakiyor
    ((= kontur "3")
     (if ust-taraf
       ;; Ust taraf: Yay ASAGI bakiyor (konkav asagi)
       (progn
         (v-img (fix (- cx w)) (fix cy) (fix cx) (fix (+ cy arc-h)) 2)
         (v-img (fix cx) (fix (+ cy arc-h)) (fix (+ cx w)) (fix cy) 2)
       )
       ;; Alt taraf: Yay YUKARI bakiyor (konkav yukari)
       (progn
         (v-img (fix (- cx w)) (fix cy) (fix cx) (fix (- cy arc-h)) 2)
         (v-img (fix cx) (fix (- cy arc-h)) (fix (+ cx w)) (fix cy) 2)
       )
     )
    )
  )
  (princ)
)

;; Yuzey bitirme harfi cizimi - BASIT HARF GOSTERIMI
(defun kaynak-onizleme-yuzey-harf-ciz (img-key x y harf-kod / harf)
  ;; Harf kodunu harfe cevir
  (setq harf
    (cond
      ((= harf-kod "1") "G")
      ((= harf-kod "2") "M")
      ((= harf-kod "3") "C")
      ((= harf-kod "4") "R")
      ((= harf-kod "5") "H")
      ((= harf-kod "6") "P")
      (T "")
    )
  )
  
  ;; Harfi basit cizgilerle ciz
  (if (/= harf "")
    (kaynak-onizleme-text-ciz img-key x y harf)
  )
  (princ)
)

;; Metin cizimi - BUYUTULMUS KARAKTER GOSTERIMI
(defun kaynak-onizleme-text-ciz (img-key x y txt / i c cx cy w scl)
  ;; Karakter boyutu (lisp ile orantili)
  (setq scl 1.5)  ; Buyutme faktoru
  (setq w (* 4 scl))  ; Karakter genisligi
  
  (setq i 0)
  (setq cx x)
  
  (repeat (strlen txt)
    (setq c (substr txt (1+ i) 1))
    (kaynak-onizleme-char-ciz img-key cx y c scl)
    (setq cx (+ cx w))
    (setq i (1+ i))
  )
  (princ)
)

;; Tek karakter cizimi - BUYUTULMUS CIZGILERLE
(defun kaynak-onizleme-char-ciz (img-key x y c scl / w h)
  ;; Karakter boyutlari
  (setq w (* 2 scl))  ; Genislik
  (setq h (* 3 scl))  ; Yukseklik
  
  (cond
    ;; Rakamlar
    ((= c "0")
     (v-img (fix (- x w)) (fix (- y h)) (fix (- x w)) (fix (+ y h)) 2)
     (v-img (fix (- x w)) (fix (- y h)) (fix (+ x w)) (fix (- y h)) 2)
     (v-img (fix (+ x w)) (fix (- y h)) (fix (+ x w)) (fix (+ y h)) 2)
     (v-img (fix (+ x w)) (fix (+ y h)) (fix (- x w)) (fix (+ y h)) 2)
    )
    ((= c "1")
     (v-img (fix x) (fix (- y h)) (fix x) (fix (+ y h)) 2)
    )
    ((= c "2")
     (v-img (fix (- x w)) (fix (- y h)) (fix (+ x w)) (fix (- y h)) 2)
     (v-img (fix (+ x w)) (fix (- y h)) (fix (+ x w)) (fix y) 2)
     (v-img (fix (+ x w)) (fix y) (fix (- x w)) (fix y) 2)
     (v-img (fix (- x w)) (fix y) (fix (- x w)) (fix (+ y h)) 2)
     (v-img (fix (- x w)) (fix (+ y h)) (fix (+ x w)) (fix (+ y h)) 2)
    )
    ((= c "3")
     (v-img (fix (- x w)) (fix (- y h)) (fix (+ x w)) (fix (- y h)) 2)
     (v-img (fix (+ x w)) (fix (- y h)) (fix (+ x w)) (fix (+ y h)) 2)
     (v-img (fix (+ x w)) (fix (+ y h)) (fix (- x w)) (fix (+ y h)) 2)
     (v-img (fix (- x w)) (fix y) (fix (+ x w)) (fix y) 2)
    )
    ((= c "4")
     (v-img (fix (- x w)) (fix (- y h)) (fix (- x w)) (fix y) 2)
     (v-img (fix (- x w)) (fix y) (fix (+ x w)) (fix y) 2)
     (v-img (fix (+ x w)) (fix (- y h)) (fix (+ x w)) (fix (+ y h)) 2)
    )
    ((= c "5")
     (v-img (fix (+ x w)) (fix (- y h)) (fix (- x w)) (fix (- y h)) 2)
     (v-img (fix (- x w)) (fix (- y h)) (fix (- x w)) (fix y) 2)
     (v-img (fix (- x w)) (fix y) (fix (+ x w)) (fix y) 2)
     (v-img (fix (+ x w)) (fix y) (fix (+ x w)) (fix (+ y h)) 2)
     (v-img (fix (+ x w)) (fix (+ y h)) (fix (- x w)) (fix (+ y h)) 2)
    )
    ((= c "6")
     (v-img (fix (+ x w)) (fix (- y h)) (fix (- x w)) (fix (- y h)) 2)
     (v-img (fix (- x w)) (fix (- y h)) (fix (- x w)) (fix (+ y h)) 2)
     (v-img (fix (- x w)) (fix (+ y h)) (fix (+ x w)) (fix (+ y h)) 2)
     (v-img (fix (+ x w)) (fix (+ y h)) (fix (+ x w)) (fix y) 2)
     (v-img (fix (+ x w)) (fix y) (fix (- x w)) (fix y) 2)
    )
    ((= c "7")
     (v-img (fix (- x w)) (fix (- y h)) (fix (+ x w)) (fix (- y h)) 2)
     (v-img (fix (+ x w)) (fix (- y h)) (fix (+ x w)) (fix (+ y h)) 2)
    )
    ((= c "8")
     (v-img (fix (- x w)) (fix (- y h)) (fix (+ x w)) (fix (- y h)) 2)
     (v-img (fix (- x w)) (fix (- y h)) (fix (- x w)) (fix (+ y h)) 2)
     (v-img (fix (+ x w)) (fix (- y h)) (fix (+ x w)) (fix (+ y h)) 2)
     (v-img (fix (- x w)) (fix (+ y h)) (fix (+ x w)) (fix (+ y h)) 2)
     (v-img (fix (- x w)) (fix y) (fix (+ x w)) (fix y) 2)
    )
    ((= c "9")
     (v-img (fix (- x w)) (fix (- y h)) (fix (+ x w)) (fix (- y h)) 2)
     (v-img (fix (- x w)) (fix (- y h)) (fix (- x w)) (fix y) 2)
     (v-img (fix (- x w)) (fix y) (fix (+ x w)) (fix y) 2)
     (v-img (fix (+ x w)) (fix (- y h)) (fix (+ x w)) (fix (+ y h)) 2)
     (v-img (fix (+ x w)) (fix (+ y h)) (fix (- x w)) (fix (+ y h)) 2)
    )
    
    ;; Harfler
    ((= c "G")
     (v-img (+ x 1) (- y 2) (- x 1) (- y 2) 2)
     (v-img (- x 1) (- y 2) (- x 1) (+ y 2) 2)
     (v-img (- x 1) (+ y 2) (+ x 1) (+ y 2) 2)
     (v-img (+ x 1) (+ y 2) (+ x 1) y 2)
     (v-img (+ x 1) y x y 2)
    )
    ((= c "M")
     (v-img (- x 1) (+ y 2) (- x 1) (- y 2) 2)
     (v-img (- x 1) (- y 2) x y 2)
     (v-img x y (+ x 1) (- y 2) 2)
     (v-img (+ x 1) (- y 2) (+ x 1) (+ y 2) 2)
    )
    ((= c "C")
     (v-img (+ x 1) (- y 2) (- x 1) (- y 2) 2)
     (v-img (- x 1) (- y 2) (- x 1) (+ y 2) 2)
     (v-img (- x 1) (+ y 2) (+ x 1) (+ y 2) 2)
    )
    ((= c "R")
     (v-img (- x 1) (+ y 2) (- x 1) (- y 2) 2)
     (v-img (- x 1) (- y 2) (+ x 1) (- y 2) 2)
     (v-img (+ x 1) (- y 2) (+ x 1) y 2)
     (v-img (+ x 1) y (- x 1) y 2)
     (v-img x y (+ x 1) (+ y 2) 2)
    )
    ((= c "H")
     (v-img (- x 1) (- y 2) (- x 1) (+ y 2) 2)
     (v-img (- x 1) y (+ x 1) y 2)
     (v-img (+ x 1) (- y 2) (+ x 1) (+ y 2) 2)
    )
    ((= c "P")
     (v-img (- x 1) (+ y 2) (- x 1) (- y 2) 2)
     (v-img (- x 1) (- y 2) (+ x 1) (- y 2) 2)
     (v-img (+ x 1) (- y 2) (+ x 1) y 2)
     (v-img (+ x 1) y (- x 1) y 2)
    )
    ((= c "F")
     (v-img (- x 1) (+ y 2) (- x 1) (- y 2) 2)
     (v-img (- x 1) (- y 2) (+ x 1) (- y 2) 2)
     (v-img (- x 1) y (+ x 1) y 2)
    )
    ((= c "N")
     (v-img (- x 1) (+ y 2) (- x 1) (- y 2) 2)
     (v-img (- x 1) (- y 2) (+ x 1) (+ y 2) 2)
     (v-img (+ x 1) (+ y 2) (+ x 1) (- y 2) 2)
    )
    ((= c "D")
     (v-img (- x 1) (- y 2) (- x 1) (+ y 2) 2)
     (v-img (- x 1) (- y 2) x (- y 2) 2)
     (v-img x (- y 2) (+ x 1) (- y 1) 2)
     (v-img (+ x 1) (- y 1) (+ x 1) (+ y 1) 2)
     (v-img (+ x 1) (+ y 1) x (+ y 2) 2)
     (v-img x (+ y 2) (- x 1) (+ y 2) 2)
    )
    ((= c "T")
     (v-img (- x 1) (- y 2) (+ x 1) (- y 2) 2)
     (v-img x (- y 2) x (+ y 2) 2)
    )
    ((= c "W")
     (v-img (- x 1) (- y 2) (- x 1) (+ y 2) 2)
     (v-img (- x 1) (+ y 2) x y 2)
     (v-img x y (+ x 1) (+ y 2) 2)
     (v-img (+ x 1) (+ y 2) (+ x 1) (- y 2) 2)
    )
    ((= c "S")
     (v-img (+ x 1) (- y 2) (- x 1) (- y 2) 2)
     (v-img (- x 1) (- y 2) (- x 1) y 2)
     (v-img (- x 1) y (+ x 1) y 2)
     (v-img (+ x 1) y (+ x 1) (+ y 2) 2)
     (v-img (+ x 1) (+ y 2) (- x 1) (+ y 2) 2)
    )
    ((= c "A")
     (v-img (fix (- x w)) (fix (+ y h)) (fix x) (fix (- y h)) 2)
     (v-img (fix x) (fix (- y h)) (fix (+ x w)) (fix (+ y h)) 2)
     (v-img (fix (- x (* 0.5 w))) (fix y) (fix (+ x (* 0.5 w))) (fix y) 2)
    )
    ((= c "B")
     (v-img (fix (- x w)) (fix (- y h)) (fix (- x w)) (fix (+ y h)) 2)
     (v-img (fix (- x w)) (fix (- y h)) (fix (+ x w)) (fix (- y (* 0.5 h))) 2)
     (v-img (fix (+ x w)) (fix (- y (* 0.5 h))) (fix (- x w)) (fix y) 2)
     (v-img (fix (- x w)) (fix y) (fix (+ x w)) (fix (+ y (* 0.5 h))) 2)
     (v-img (fix (+ x w)) (fix (+ y (* 0.5 h))) (fix (- x w)) (fix (+ y h)) 2)
    )
    ((= c "E")
     (v-img (fix (+ x w)) (fix (- y h)) (fix (- x w)) (fix (- y h)) 2)
     (v-img (fix (- x w)) (fix (- y h)) (fix (- x w)) (fix (+ y h)) 2)
     (v-img (fix (- x w)) (fix (+ y h)) (fix (+ x w)) (fix (+ y h)) 2)
     (v-img (fix (- x w)) (fix y) (fix (+ x w)) (fix y) 2)
    )
    ((= c "I")
     (v-img (fix x) (fix (- y h)) (fix x) (fix (+ y h)) 2)
    )
    ((= c "K")
     (v-img (fix (- x w)) (fix (- y h)) (fix (- x w)) (fix (+ y h)) 2)
     (v-img (fix (+ x w)) (fix (- y h)) (fix (- x w)) (fix y) 2)
     (v-img (fix (- x w)) (fix y) (fix (+ x w)) (fix (+ y h)) 2)
    )
    ((= c "L")
     (v-img (fix (- x w)) (fix (- y h)) (fix (- x w)) (fix (+ y h)) 2)
     (v-img (fix (- x w)) (fix (+ y h)) (fix (+ x w)) (fix (+ y h)) 2)
    )
    ((= c "O")
     (v-img (fix (- x w)) (fix (- y h)) (fix (- x w)) (fix (+ y h)) 2)
     (v-img (fix (- x w)) (fix (- y h)) (fix (+ x w)) (fix (- y h)) 2)
     (v-img (fix (+ x w)) (fix (- y h)) (fix (+ x w)) (fix (+ y h)) 2)
     (v-img (fix (+ x w)) (fix (+ y h)) (fix (- x w)) (fix (+ y h)) 2)
    )
    ((= c "U")
     (v-img (fix (- x w)) (fix (- y h)) (fix (- x w)) (fix (+ y h)) 2)
     (v-img (fix (- x w)) (fix (+ y h)) (fix (+ x w)) (fix (+ y h)) 2)
     (v-img (fix (+ x w)) (fix (+ y h)) (fix (+ x w)) (fix (- y h)) 2)
    )
    ((= c "V")
     (v-img (fix (- x w)) (fix (- y h)) (fix x) (fix (+ y h)) 2)
     (v-img (fix x) (fix (+ y h)) (fix (+ x w)) (fix (- y h)) 2)
    )
    ((= c "X")
     (v-img (fix (- x w)) (fix (- y h)) (fix (+ x w)) (fix (+ y h)) 2)
     (v-img (fix (+ x w)) (fix (- y h)) (fix (- x w)) (fix (+ y h)) 2)
    )
    ((= c "Y")
     (v-img (fix (- x w)) (fix (- y h)) (fix x) (fix y) 2)
     (v-img (fix (+ x w)) (fix (- y h)) (fix x) (fix y) 2)
     (v-img (fix x) (fix y) (fix x) (fix (+ y h)) 2)
    )
    ((= c "Z")
     (v-img (fix (- x w)) (fix (- y h)) (fix (+ x w)) (fix (- y h)) 2)
     (v-img (fix (+ x w)) (fix (- y h)) (fix (- x w)) (fix (+ y h)) 2)
     (v-img (fix (- x w)) (fix (+ y h)) (fix (+ x w)) (fix (+ y h)) 2)
    )
    ((= c "x")
     (v-img (fix (- x (* 0.7 w))) (fix y) (fix (+ x (* 0.7 w))) (fix (+ y h)) 2)
     (v-img (fix (+ x (* 0.7 w))) (fix y) (fix (- x (* 0.7 w))) (fix (+ y h)) 2)
    )
    ((= c ".")
     (v-img (fix x) (fix (+ y h)) (fix x) (fix (+ y h)) 2)
    )
    ((= c "-")
     (v-img (fix (- x w)) (fix y) (fix (+ x w)) (fix y) 2)
    )
    ((= c " ")
     ;; Bosluk - hicbir sey cizme
    )
  )
  (princ)
)

;; Z isareti cizimi (METOD KAYNAK icin) - ANA CIZIME UYGUN
(defun kaynak-onizleme-z-ciz (img-key x y scl / w h ust-sol ust-sag alt-sol alt-sag)
  ;; Ana cizimde: 6mm yatay, 16.25mm capraz
  (setq w (* 3 scl))  ; Yatay cizgi yarisi: 6/2 = 3mm
  (setq h (* 8.125 scl))  ; Capraz yukseklik: 16.25/2 = 8.125mm
  
  ;; Ust sol nokta
  (setq ust-sol (list (- x w) (- y (* 0.5 h))))
  
  ;; Ust sag nokta
  (setq ust-sag (list (+ x w) (- y (* 0.5 h))))
  
  ;; Alt sol nokta (capraz asagi-sol)
  (setq alt-sol (list (- x w) (+ y (* 0.5 h))))
  
  ;; Alt sag nokta
  (setq alt-sag (list (+ x w) (+ y (* 0.5 h))))
  
  ;; Ust yatay cizgi
  (v-img (fix (car ust-sol)) (fix (cadr ust-sol)) (fix (car ust-sag)) (fix (cadr ust-sag)) 2)
  
  ;; Capraz cizgi (sag ustten sol alta)
  (v-img (fix (car ust-sag)) (fix (cadr ust-sag)) (fix (car alt-sol)) (fix (cadr alt-sol)) 2)
  
  ;; Alt yatay cizgi
  (v-img (fix (car alt-sol)) (fix (cadr alt-sol)) (fix (car alt-sag)) (fix (cadr alt-sag)) 2)
  
  (princ)
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
      '(7 . "KAYNAK_TEXT")
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
      '(7 . "KAYNAK_TEXT")
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
      '(7 . "KAYNAK_TEXT")
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
      '(7 . "KAYNAK_TEXT")
    )))
  (if ent (setq ent-listesi (append ent-listesi (list ent))))
  
  ent-listesi
)
;;;========================================================================
;;; SEMBOL OLUSTURMA ANA FONKSIYONU
;;;========================================================================

(defun kaynak-sembolu-olustur (pt1 pt2 scl kaynak-id / aci ref-yon ref-baslangic ref-bitis obje-listesi
                                ent ust-pt alt-pt sembol-merkez yukari-aci asagi-aci old-mleader-style acadObj doc mspace pt1-3d pt2-3d pointArray mleaderObj)
  
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
  
;; 1. MULTILEADER (OK) OLUSTUR - Sabit stil ile
;; Mevcut multileader stilini kaydet ve Standard'a cevir
(setq old-mleader-style (getvar "CMLEADERSTYLE"))
(setvar "CMLEADERSTYLE" "Standard")

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

;; Ok tipi - Sayisal deger (acArrowDefault yerine)
(vla-put-ArrowheadType mleaderObj 0)

;; Ok boyutu - OLCEKLENEBILIR
(vla-put-ArrowheadSize mleaderObj (* scl 2.5))

;; Text yok
(vla-put-TextString mleaderObj "")

;; Landing yok
(vla-put-DoglegLength mleaderObj 0)

;; Entity name
(setq ent (vlax-vla-object->ename mleaderObj))
(if ent (setq obje-listesi (append obje-listesi (list ent))))

;; Eski multileader stilini geri yukle
(setvar "CMLEADERSTYLE" old-mleader-style)
  
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
          (kaynak-kontur-ekle ust-pt ref-yon scl *UST-KONTUR* T *UST-YONTEM* yukari-aci asagi-aci)))
      )
      
      ;; Ust yuzey bitirme harfi - SABIT POZISYON
      (if (/= *UST-YUZEY* "0")
        (setq obje-listesi (append obje-listesi 
          (kaynak-yuzey-bitirme-ekle sembol-merkez ref-yon scl *UST-YUZEY* T yukari-aci asagi-aci)))
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
          (kaynak-kontur-ekle alt-pt ref-yon scl *ALT-KONTUR* nil *ALT-YONTEM* yukari-aci asagi-aci)))
      )
      
      ;; Alt yuzey bitirme harfi - SABIT POZISYON
      (if (/= *ALT-YUZEY* "0")
        (setq obje-listesi (append obje-listesi 
          (kaynak-yuzey-bitirme-ekle sembol-merkez ref-yon scl *ALT-YUZEY* nil yukari-aci asagi-aci)))
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
  
;; WPS, F.P., NDT ve notlari ekle
  (setq obje-listesi (append obje-listesi 
    (kaynak-metinler-ekle ref-baslangic ref-bitis ref-yon scl yukari-aci asagi-aci)))
  
 ;; F.P. varsa ekle
(if (= *TAM-NUF-FLAG* "1")
  (setq obje-listesi (append obje-listesi 
    (kaynak-tam-nufuz-ekle ref-bitis ref-yon scl (= *NDT-FLAG* "1"))))
)

;; NDT varsa ekle
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
      '(7 . "KAYNAK_TEXT")
      '(71 . 0)
      '(72 . 1)
      '(73 . 2)
      '(7 . "KAYNAK_TEXT")
    )))
  (if ent (list ent) nil)
)

;;;========================================================================
;; KONTUR EKLE
;;;========================================================================

;; V3.4 DUZELTME: yukari-aci ve asagi-aci parametreleri kullanildi
(defun kaynak-kontur-ekle (pt yon scl kontur ust-taraf kaynak-tipi yukari-aci asagi-aci / 
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

  kont-listesi
)

;;;========================================================================
;;; YUZEY BITIRME HARFI - SABIT POZISYON
;;;========================================================================

(defun kaynak-yuzey-bitirme-ekle (sembol-merkez yon scl yuzey ust-taraf yukari-aci asagi-aci / txt-pt ent)
  ;; Yuzey harfi kaynak sembolunun tam uzerinde, referans cizgisinden 11.5mm yukari/asagi
  (if ust-taraf
    (setq txt-pt (polar sembol-merkez (+ yon yukari-aci) (* scl 11.5)))
    (setq txt-pt (polar sembol-merkez (+ yon asagi-aci) (* scl 11.5)))
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
      '(7 . "KAYNAK_TEXT")
    )))
  (if ent (list ent) nil)
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

(defun kaynak-tam-nufuz-ekle (ref-bitis yon scl ndt-var / fp-pt txt-aci ent-listesi ent fp-yaricap 
                               fp-mesafe text-genislik kutu-genislik wps-kutu-sag-kenar)
  (setq ent-listesi '())
  (setq fp-yaricap (* scl 3.75))
  
  ;; WPS kutusu varsa sag kenar pozisyonunu hesapla
  (if (/= *WPS-NO* "")
    (progn
      (setq text-genislik (kaynak-text-genislik-hesapla *WPS-NO* (* scl 3.0)))
      (setq kutu-genislik (max (* scl 9.0) (+ text-genislik (* scl 3.0))))
      ;; WPS sol kenari ref-bitis'ten 7.5mm saga, oradan kutu genisligi kadar saga
      (setq wps-kutu-sag-kenar (+ (* scl 7.5) kutu-genislik))
      ;; F.P. pozisyonu: WPS sag kenari + 0.75mm bosluk + F.P. yaricapi
      (setq fp-mesafe (+ wps-kutu-sag-kenar (* scl 0.75) fp-yaricap))
    )
    ;; WPS yoksa varsayilan pozisyon
    (setq fp-mesafe (* scl 21.0))
  )
  
  (setq fp-pt (polar ref-bitis yon fp-mesafe))

  ;; Daire ciz
  (setq ent (entmakex
    (list
      '(0 . "CIRCLE")
      '(100 . "AcDbEntity")
      '(8 . "KAYNAK_SYM")
      '(100 . "AcDbCircle")
      (cons 10 fp-pt)
      (cons 40 fp-yaricap)
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
      '(7 . "KAYNAK_TEXT")
    )))
  (if ent (setq ent-listesi (append ent-listesi (list ent))))
  
  ent-listesi
)

(defun kaynak-ndt-ekle (ref-bitis yon scl fp-var / ndt-pt txt-aci offset ent-listesi ent ndt-yaricap
                        text-genislik kutu-genislik wps-kutu-sag-kenar fp-sag-kenar)
  (setq ent-listesi '())
  (setq ndt-yaricap (* scl 3.75))

  ;; WPS kutusu varsa sag kenar pozisyonunu hesapla
  (if (/= *WPS-NO* "")
    (progn
      (setq text-genislik (kaynak-text-genislik-hesapla *WPS-NO* (* scl 3.0)))
      (setq kutu-genislik (max (* scl 9.0) (+ text-genislik (* scl 3.0))))
      (setq wps-kutu-sag-kenar (+ (* scl 7.5) kutu-genislik))
    )
  )
  
  ;; NDT pozisyonunu hesapla
  (cond
    ;; F.P. varsa: once F.P. pozisyonunu hesapla, sonra ondan saga
    (fp-var
     (if wps-kutu-sag-kenar
       (setq fp-sag-kenar (+ wps-kutu-sag-kenar (* scl 0.75) (* scl 7.5)))
       (setq fp-sag-kenar (+ (* scl 21.0) (* scl 3.75)))
     )
     ;; NDT: F.P. sag kenari + 1.0mm bosluk + NDT yaricapi
     (setq offset (+ fp-sag-kenar (* scl 1.0) ndt-yaricap)))
    
    ;; F.P. yoksa ama WPS varsa: WPS sag kenari + 0.75mm bosluk + NDT yaricapi
    (wps-kutu-sag-kenar
     (setq offset (+ wps-kutu-sag-kenar (* scl 0.75) ndt-yaricap)))
    
    ;; Hicbiri yoksa: varsayilan 21mm
    (T
     (setq offset (* scl 21.0)))
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
      (cons 40 ndt-yaricap)
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
      '(7 . "KAYNAK_TEXT")
    )))
  (if ent (setq ent-listesi (append ent-listesi (list ent))))
  
  ent-listesi
)

;;;========================================================================
;;; TEXT GENISLIK HESAPLAMA
;;;========================================================================

(defun kaynak-text-genislik-hesapla (text-str yukseklik / temp-ent bbox genislik)
  ;; Gecici bir text entity olustur
  (setq temp-ent (entmakex
    (list
      '(0 . "TEXT")
      '(100 . "AcDbEntity")
      '(8 . "KAYNAK_SYM")
      '(100 . "AcDbText")
      '(10 0.0 0.0 0.0)
      (cons 40 yukseklik)
      (cons 1 text-str)
      '(50 . 0.0)
      '(7 . "KAYNAK_TEXT")
    )))
  
  (if temp-ent
    (progn
      ;; Text'in bounding box'ini al
      (setq bbox (textbox (entget temp-ent)))
      ;; Genisligi hesapla (sag ust kose - sol alt kose)
      (setq genislik (- (car (cadr bbox)) (car (car bbox))))
      ;; Gecici entity'i sil
      (entdel temp-ent)
      genislik
    )
    ;; Hata durumunda varsayilan deger
    (* yukseklik (strlen text-str) 0.6)
  )
)

;;;========================================================================
;;; WPS KUTUSU VE NOTLAR
;;;========================================================================

;; V3.4 DUZELTME: Notlar - asagi-aci parametresi kullanildi
;; V3.5 EKLEME: WPS kutusu dinamik genislik - sadece saga buyur
(defun kaynak-metinler-ekle (ref-baslangic ref-bitis yon scl yukari-aci asagi-aci / txt-pt txt-aci kuyruk-pt ent-listesi 
                              kutu-p1 kutu-p2 kutu-p3 kutu-p4 ent text-genislik kutu-genislik kutu-yukseklik
                              wps-sol-kenar-pt)
  (setq ent-listesi '())
  
  (setq txt-aci 0.0)
  
  ;; WPS KUTUSU - DINAMIK GENISLIK (SADECE SAGA BUYUR)
  (if (/= *WPS-NO* "")
    (progn
      ;; Text genisligini hesapla
      (setq text-genislik (kaynak-text-genislik-hesapla *WPS-NO* (* scl 3.0)))
      
      ;; Kutu genisligini belirle: minimum 9mm veya text + 3mm bosluk
      (setq kutu-genislik (max (* scl 9.0) (+ text-genislik (* scl 3.0))))
      (setq kutu-yukseklik (* scl 7.5))
      
      ;; Kuyruk baslangic noktasi
      (setq kuyruk-pt (polar ref-bitis yon (* scl 4.5)))

      ;; Kuyruk cizgileri (ok seklinde)
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

      ;; SOL KENAR SABIT NOKTA (ref-bitis'ten 7.5mm saga)
      (setq wps-sol-kenar-pt (polar kuyruk-pt yon (* scl 3.0)))
      
      ;; Text merkez noktasi: Sol kenardan saga dogru kutu genisliginin yarisi
      (setq txt-pt (polar wps-sol-kenar-pt yon (/ kutu-genislik 2.0)))
      
      ;; Dikdortgen kose noktalari - SOL KENAR SABIT, SAG KENARA DOGRU BUYUR
      ;; Sol ust kose (sabit pozisyon)
      (setq kutu-p1 (polar wps-sol-kenar-pt (+ yon (/ pi 2)) (/ kutu-yukseklik 2.0)))
      
      ;; Sag ust kose (dinamik pozisyon - kutu genisligine gore)
      (setq kutu-p2 (polar kutu-p1 yon kutu-genislik))
      
      ;; Sag alt kose
      (setq kutu-p3 (polar kutu-p2 (- yon (/ pi 2)) kutu-yukseklik))
      
      ;; Sol alt kose
      (setq kutu-p4 (polar kutu-p1 (- yon (/ pi 2)) kutu-yukseklik))
      
      ;; Dikdortgeni ciz
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
      
      ;; WPS text'ini yaz (kutunun ortasinda)
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
          '(7 . "KAYNAK_TEXT")
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
          '(7 . "KAYNAK_TEXT")
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
          '(7 . "KAYNAK_TEXT")
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
          '(7 . "KAYNAK_TEXT")
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
(princ "\nKAYNAK SEMBOLU SISTEMI V4.0 BASARIYLA YUKLENDI")
(princ "\n============================================================")
(princ "\nANA KOMUT: KS")
(princ "\n")
(princ "\nYENILIKLER V4.0:")
(princ "\n- Kullanici dostu DCL arayuzu")
(princ "\n- Onizleme penceresi eklendi")
(princ "\n- Detayli aciklamalar ve yardim")
(princ "\n")
(princ "\nMurat KARA")
(princ "\n============================================================")
(princ)