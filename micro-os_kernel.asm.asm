     name "kernel"        
; bu temel isletim sistemi icin basit bir ornek
;
; bu kernel modulu! 
;
; bu makinenin oldugu kod disket surucusunden 'micro-os_loader.asm'ile yuklendi. 
; 
;   silindir: 0
;   sektor: 2
;   bas: 0


;=================================================
; mikro isletim sistemi nasil kontrol edilir:
;   1. micro-os_loader.asm'yi derle
;   2. micro-os_kernel.asm'yi derle
;   3. writebin.asm'yi derle
;   4. a surucusune bos disket takin
;   5. komut istemi turunden:
;        writebin loader.bin
;        writebin kernel.bin /k
;=================================================
              
; bin dosyasi olusturma yonergesi:
#make_bin#

; nereye yuklenecek? (emulator. tum bu degerler binf dosyasina kaydedilir.)
#load_segment=0800#
#load_offset=0000#

; bu degerler yukteki kayitlara ayarlanmistir, aslinda sadece "ds, es, cs, ip, ss, sp are" onemlidir.
; bu degerler, micro-os_loader kontrol, bu cekirdege aktardiktan sonra (beklendigi gibi) emulatorun
; gercek mikroislemci durumunu taklit etmesi icin kullanilir.
#al=0b#
#ah=00#
#bh=00#
#bl=00#
#ch=00#
#cl=02#
#dh=00#
#dl=00#
#ds=0800#
#es=0800#
#si=7c02#
#di=0000#
#bp=0000#
#cs=0800#
#ip=0000#
#ss=07c0#
#sp=03fe#



; bu makro register'indaki bir karakteri yazdirir.
; mevcut imlec konumu:
putc    macro   char
        push    ax
        mov     al, char
        mov     ah, 0eh
        int     10h     
        pop     ax
endm


; gecerli imlec konumunu ayarla"
gotoxy  macro   col, row
        push    ax
        push    bx
        push    dx
        mov     ah, 02h
        mov     dh, row
        mov     dl, col
        mov     bh, 0
        int     10h
        pop     dx
        pop     bx
        pop     ax
endm


print macro x, y, attrib, sdat
LOCAL   s_dcl, skip_dcl, s_dcl_end
    pusha
    mov dx, cs
    mov es, dx
    mov ah, 13h
    mov al, 1
    mov bh, 0
    mov bl, attrib
    mov cx, offset s_dcl_end - offset s_dcl
    mov dl, x
    mov dh, y
    mov bp, offset s_dcl
    int 10h
    popa
    jmp skip_dcl
    s_dcl DB sdat
    s_dcl_end DB 0
    skip_dcl:    
endm



; kernel, micro-os_loader ile 0800:0000'de yuklenir." 
org 0000h

; veri ve fonksiyon deklarasyon bolumunu atla:
jmp start 
; bu atlama komutunun ilk bayti 0E9h'dir.
; basarili, bir lansman gerceklestirip gerceklestirmedigimizi belirlemek icin kullanilir.
; kernel bulunamazsa yukleyici bir hata mesaji yazdirir. 
; kernel, sektor 2 yerine sektor 1'e yazildiginda 'F' yazdirir.
           



;===================== veri bolumu =====================

; karsilama mesaji
msg  db "mikro i",159,"letim sistemine ho",159, "geldiniz!", 0
 

 
cmd_size        equ 10    ; command_buffer'in boyutu"
command_buffer  db cmd_size dup("b")
clean_str       db cmd_size dup(" "), 0
prompt          db ">", 0

saat    DW 1 DUP (?)
dakika  DW 1 DUP (?)
saniye  DW 1 DUP (?)

; komutlar:
chelp       db "help", 0
chelp_tail:
ccls        db "cls", 0
ccls_tail:
cclock      db "clock", 0
cclock_tail:
ckelime     db "kelime", 0
ckelime_tail:
chello      db "hello", 0
chello_tail:
cquit       db "quit", 0
cquit_tail:
cexit       db "exit", 0
cexit_tail:
creboot     db "reboot", 0
creboot_tail:

help_msg db "mikro i",159,"letim sistemini se",135,"ti",167,"iniz i",135,"in te",159,"ekk",129,"r ederiz.", 0Dh,0Ah
         db "desteklenen komutlar",141,"n k",141,"sa listesi:", 0Dh,0Ah
         db "help   - bu listeyi yazd",141,"r.", 0Dh,0Ah
         db "cls    - ekran",141, " temizle.", 0Dh,0Ah
         db "kelime - kelimenin ka",135," harfli oldu",167,"unu ekrana yazar.",0Dh,0Ah
         db "clock  - zaman",141, " g",148,"ster.",0Dh, 0Ah
         db "hello  - merhaba komutunu yazar.",0Dh, 0Ah
         db "reboot - makineyi yeniden ba",159,"lat.", 0Dh,0Ah
         db "quit   - reboot ile ayn",141,".", 0Dh,0Ah 
         db "exit   - quit ile ayn",141,".", 0Dh,0Ah, 0
clock_msg db "saat:", 0         

bilinmeyen  db "bilinmeyen komut: " , 0    

;======================================

start:

; veri segmentini ayarla:
push    cs
pop     ds

; varsayilan video modunu 80x25 olarak ayarla:
mov     ah, 00h
mov     al, 03h
int     10h

; yanip sonme dos/bios,
; emulator ve windows istemi uyumluluk nedeniyle devre disi birakildi ve hic bir zaman yanip sonmedi.
mov     ax, 1003h
mov     bx, 0      ; yanip sonmeyi devre disi, birak.
int     10h


; *** butunluk kontrolu ***
cmp [0000], 0E9h
jz integrity_check_ok
integrity_failed:  
mov     al, 'F'
mov     ah, 0eh
int     10h  
; herhangi bir anahtari bekle...
mov     ax, 0
int     16h
; yeniden baslat...
mov     ax, 0040h
mov     ds, ax
mov     w.[0072h], 0000h
jmp	0ffffh:0000h	 
integrity_check_ok:
nop
; *** tamam ***
              


; ekrani temizle:
call    clear_screen
                     
                       
; mesaji yazdir:
lea     si, msg
call    print_string


eternal_loop:
call    get_command

call    process_cmd

; sonsuz dongu:
jmp eternal_loop


;===========================================
get_command proc near

; ekranin imlec konumu asagiya:
mov     ax, 40h
mov     es, ax
mov     al, es:[84h]

gotoxy  0, al

; komut satirini temizle:
lea     si, clean_str
call    print_string

gotoxy  0, al

; istemi goster:
lea     si, prompt 
call    print_string


; bir komut bekle:
mov     dx, cmd_size    ; arabellek boyutu.
lea     di, command_buffer
call    get_string


ret
get_command endp
;===========================================

process_cmd proc    near

;//// komutlari buradan kontrol edin ///
; es'i ds'ye ayarla
push    ds
pop     es

cld     ; ileri karsilastirma.

; komut arabellegini 'help' ile karsilastirin.
lea     si, command_buffer
mov     cx, chelp_tail - offset chelp   ; ['help',0] dizesinin boyutu.
lea     di, chelp
repe    cmpsb
je      help_command

; komut arabellegini 'cls' ile karsilastirin.
lea     si, command_buffer
mov     cx, ccls_tail - offset ccls  ; ['cls',0] dizesinin boyutu.
lea     di, ccls
repe    cmpsb
jne     not_cls
jmp     cls_command
not_cls:

; komut arabellegini 'clock' ile karsilastirin.
lea     si, command_buffer
mov     cx, cclock_tail - offset cclock  ; ['clock',0] dizesinin boyutu.
lea     di, cclock
repe    cmpsb
je      clock_command

; komut arabellegini 'kelime' ile karsilastirin.
lea     si, command_buffer
mov     cx, ckelime_tail - offset ckelime  ; ['kelime',0] dizesinin boyutu.
lea     di, ckelime
repe    cmpsb
je      kelime_command
                        
                        
; komut arabellegini 'hello' ile karsilastirin.
lea     si, command_buffer
mov     cx, chello_tail - offset chello  ; ['hello',0] dizesinin boyutu.
lea     di, chello
repe    cmpsb
je      hello_command
                                              

; komut arabellegini 'quit' ile karsilastirin.
lea     si, command_buffer
mov     cx, cquit_tail - offset cquit ; ['quit',0] dizesinin boyutu.
lea     di, cquit
repe    cmpsb
je      reboot_command

; komut arabellegini 'exit' ile karsilastirin.
lea     si, command_buffer
mov     cx, cexit_tail - offset cexit ; ['exit',0] dizesinin boyutu.
lea     di, cexit
repe    cmpsb
je      reboot_command

; komut arabellegini 'reboot' ile karsilastirin.
lea     si, command_buffer
mov     cx, creboot_tail - offset creboot  ; ['reboot',0] dizesinin boyutu.
lea     di, creboot
repe    cmpsb
je      reboot_command

; bos satirlari gormezden gel.
cmp     command_buffer, 0
jz      processed


;////////////////////////////

; eger buraya gelirse o zaman komut bilinmeyen...

mov     al, 1
call    scroll_t_area

; imlec konumunu sadece bilgi istemi satirinin hemen ustune ayarla.
mov     ax, 40h
mov     es, ax
mov     al, es:[84h]
dec     al
gotoxy  0, al

lea     si, bilinmeyen
call    print_string

lea     si, command_buffer
call    print_string

mov     al, 1
call    scroll_t_area

jmp     processed

; +++++ 'help' komut ++++++
help_command:

; metin alanini 24 satir yukari kaydir:
mov     al,10                
call    scroll_t_area

; imlec konumunu bilgi istemi  satirinin 
; 23 satir ustune ayarlayin:

mov     ax, 40h
mov     es, ax
mov     al, es:[84h]
sub     al, 10
gotoxy  0, al

lea     si, help_msg
call    print_string

mov     al, 1
call    scroll_t_area

jmp     processed

; +++++ 'kelime' komut ++++++
kelime_command:
call    get_kelime
jmp     processed


; +++++ 'cls' komut ++++++
cls_command:
call    clear_screen
jmp     processed  

; +++++ 'hello' komut ++++++
hello_command:
call    get_hello
jmp     processed 




; +++++ 'clock' komut ++++++
clock_command:


 
; metin alanini 23 satir yukari kaydir:
mov     al, 23 
call    scroll_t_area

; imlec konumunu bilgi istemi satirinin 23 satir ustune ayarla:
mov     ax, 40h                                                                                    
mov     es, ax
mov     al, es:[84h]
sub     al, 23
gotoxy  0, al

lea     si, clock_msg
call    print_string

call    get_clock

mov     al, 1
call    scroll_t_area

jmp     processed




; +++ 'quit', 'exit', 'reboot' +++
reboot_command:
call    clear_screen
print 5,2,0011_1111b, "l",129,"tfen herhangi bir disketi" ,135,141,"kar",141,"n"
print 5,3,0011_1111b, "ve yeniden ba",159,"latmak i",135,"in herhangi bir tu",159,"a bas",141,"n..."
mov ax, 0  ; herhangi bir anahtari bekle....
int 16h

; sihirli degeri 0040h:0072h'de saklayin:
;   0000h - soguk bot.
;   1234h - sicak bot.
mov     ax, 0040h
mov     ds, ax
mov     w.[0072h], 0000h ; soguk bot.
jmp	0ffffh:0000h	 ; yeniden baslat!

; ++++++++++++++++++++++++++

processed:
ret
process_cmd endp

;===========================================

; son satir haric tum ekrani,
; alanda belirtilen deger kadar yukari  kaydir.

scroll_t_area   proc    near

mov dx, 40h
mov es, dx  ; ekran parametrelerini almak icin.
mov ah, 06h ; islev kimligini yukari kaydirin.
mov bh, 07  ; yeni satirlarin ozelligi.
mov ch, 0   ; ust satir.
mov cl, 0   ; ust sutun.
mov di, 84h ; ekrandaki satirlar -1,
mov dh, es:[di] ; alt satir (byte).
dec dh  ; alt satiri kaydirmayin.
mov di, 4ah ; ekrandaki sutunlar,
mov dl, es:[di]
dec dl  ; alt sutun.
int 10h

ret
scroll_t_area   endp

;===========================================




; klavyeden karakterleri alin ve DS:DI'deki arabellege bos sonlandirilmis bir dize yazin. 
; maksimum arabellek boyutu DX'tir.
; 'enter' girisi durdurur.
get_string      proc    near
push    ax
push    cx
push    di
push    dx

mov     cx, 0                   ; karakter sayaci

cmp     dx, 1                   ; arabellek cok mu kucuk?
jbe     empty_buffer            ;

dec     dx                      ; son sifir icin yer ayir.


;============================           
; elde edilecek sonsuz dongu
; ve tum vuruslarini isler:
                                                       
wait_for_key:

mov     ah, 0                   ; alinan tusu getir.
int     16h

cmp     al, 0Dh                 ; 'return' tusuna basildi mi?
jz      exit


cmp     al, 8                   ; 'backspace' tusuna basildi mi?
jne     add_to_buffer
jcxz    wait_for_key            ; kaldirilacak bir sey yok!
dec     cx
dec     di
putc    8                       ; geri tusu.
putc    ' '                     ; posizyonu temizle.
putc    8                       ; geri tusu  tekrar.
jmp     wait_for_key

add_to_buffer:

        cmp     cx, dx          ; arabellek dolu mu?
        jae     wait_for_key    ; eger oyleyse 'backspace' veya 'return' tuslarina kadar bekleyin...

        mov     [di], al
        inc     di
        inc     cx
        
        ; anahtari yazdir:
        mov     ah, 0eh
        int     10h

jmp     wait_for_key
;============================

exit:

; "null ile sonlandir:
mov     [di], 0

empty_buffer:

pop     dx
pop     di
pop     cx
pop     ax
ret
get_string      endp




; gecerli imlec   
; konumunda dize adresi: ds:si olan bir null ile sonlandirilmis,
print_string proc near
push    ax      ; kayitlari sakla...
push    si      ;

next_char:      
        mov     al, [si]
        cmp     al, 0
        jz      printed
        inc     si
        mov     ah, 0eh ; teletip fonksiyonu.
        int     10h
        jmp     next_char
printed:

pop     si      ; kayitlari yeniden sakla...
pop     ax      ;

ret
print_string endp



; ekrani tamamen kaydirarak temizle ve imlec posizyonunu en uste ayarla.
; varsayilan oznitelik beyaz uzerine mavi olarak ayarlanmistir.
clear_screen proc near
        push    ax      ; kayitlari sakla...
        push    ds      ;
        push    bx      ;
        push    cx      ;
        push    di      ;

        mov     ax, 40h
        mov     ds, ax  ; ekran parametrelerini almak icin.
        mov     ah, 06h ; yukari kaydirma islev kimligi. 
        mov     al, 0   ; tum satirlari kaydir!
        mov     bh, 0000_1111b  ; yeni satirlar icin nitelik.
        mov     ch, 0   ; ust satir.                                      
        mov     cl, 0   ; ust sutun.
        mov     di, 84h ; ekrandaki satirlar -1,
        mov     dh, [di]; alt satir (byte).
        mov     di, 4ah ; ekranin sutunlari,
        mov     dl, [di]
        dec     dl      ; alt sutun.
        int     10h

        ; ekranin ustune imleci ayarla:
        mov     bh, 0   ; mevcut sayfa.
        mov     dl, 0   ; sutun.
        mov     dh, 0   ; satir.
        mov     ah, 02
        int     10h

        pop     di      ; kayitlari yeniden sakla...
        pop     cx      ;
        pop     bx      ;
        pop     ds      ;
        pop     ax      ;

        ret
clear_screen endp

get_kelime proc near

;kullanicidan bir metin girmesini ister.
mov ah, 09h
mov dx, offset enterWords ;'bir kelime dizisini girin' mesajini gonderir.
int 21h

mov ah, 0Ah
mov dx, offset buffer ;kullanici girisini 'buffer'in icine al.
int 21h

call NewLine ;yeni bir satira gecç

;harf sayma dongusune baslatir.
mov si, 2
xor dx, dx
xor cx, cx

letterCount: 
;kelimenin harf sayisini sayar.

mov bl, [buffer + si]
cmp bl, 0Dh		 ;eger satir sonuysa bitir.
je endOfString
cmp bl, 20h      ;eger bosluksa yeni kelimeye gec
je newWord
inc cx           ;harf sayisini arttir.
inc si
xor bx, bx
jmp letterCount

newWord:
;harf sayisini ekrana yazdir.

cmp cx, 9
ja prepareTwoDigits  ;9'dan fazla ise iki basamakli olarak hazirlar
mov dx, cx
add dx, 30h
mov ah, 02h
int 21h              ;sayicilari sifirlar.
xor cx, cx
xor dx, dx
inc si
        
;bosluk birak        
        
mov ah, 02h
mov dx, 20h
int 21h
jmp letterCount
                 
;iki basamakli sayiyi hazirlar
                 
prepareTwoDigits: 

xor di, di
mov bl, 0Ah

TwoDigits:
mov ax, cx
div bl    ;sayiyi 10'a boler
inc di
xor cx, cx
mov cl, al
xor al, al
xchg al, ah
push ax    ;sonucu saklar.
cmp cl, 0   

je printTwoDigits
jmp TwoDigits

printTwoDigits:
dec di
pop dx
add dx, 30h
mov ah, 02h
int 21h
cmp di, 0
jne printTwoDigits

;bosluk birak
call Space
         
;son kontrol         
         
cmp [buffer + si], 0Dh
je theEnd
xor cx, cx
inc si
jmp letterCount
     
;string sonunda harf sayisini yazdir.     
     
endOfString:
cmp cx, 9
ja prepareTwoDigits
mov dx, cx
add dx, 30h
mov ah, 02h
int 21h
   
;programi sonlandir.   
   
theEnd:
mov ah, 4Ch
int 21h
enterWords db "Bir kelime dizisi girin: $"
buffer db 255, ?, 255 dup('$')
lineBreak db 0Ah, 24h
td db "iki rakam $" 

;yeni satira gecer.ç

NewLine:
	mov ah, 09h
	mov dx, offset lineBreak
	int 21h
	ret
      
;bosluk birak      
      
Space:
	mov ah, 02h
	mov dx, 20h
	int 21h
	
get_kelime endp



get_clock proc near
        push    ax
        push    bx
        push    cx
        push    dx
    
        mov     ah, 00h
        
        int     1Ah     ; zaman kesintisi
        
        mov     [saat], cx  ; saati hafizaya ayarla    
                           

                           
        mov     ax, dx  ; saniye basina toplam saat tik taklari
                        
        xor     dx, dx
        mov     bx, 12h ; bolen: 18 
        div     bx      ; ikinciyi bul, (toplam onay / 18)
        sub     ax, 0Fh ; ikinci gecikme d",129,"zeltmesi                   
                           
        xor     dx, dx                    
        mov     bx, 3Ch ; bolen: 60
        div     bx      ; dakikayi (saniye / 60)  
        
        mov     [dakika], ax  ; dakikayi hafizaya ayarla.
        
        
        mov     al, ":"
        mov     [saniye], dx

       
        mov     ax, [saat]
        
        xor     dx, dx  ; hizli sifirlama dx
        mov     bx, 0Ah ; saati boler
        div     bx      ; saat / 10
        
        add     al, '0' ; ascii donusturucu, mevcut al ascii + 30h ('0')
        
        mov     ah, 0Eh
        int     10h     ; saati yazdir.
        
        
        mov     ax, dx  ; kalani saate tasi
        add     al, '0' ; ascii d",148,"n",129,159,t",129,"r",129,"c",129, mevcut al ascii + 30h ('0')
        
        mov     ah, 0Eh
        int     10h     ; saati yazdir
        
        
        mov     al, ':'
        mov     ah, 0Eh
        int     10h
        
        
        mov     ax, [dakika]
        
        xor     dx, dx  ; hizli sifirlama dx
        mov     bx, 0Ah ; dakikayi boler
        div     bx      ; dakika / 10
        
        add     al, '0' ; ascii donusturucu, mevcut al ascii + 30h ('0')
        
        mov     ah, 0Eh
        int     10h     ; dakikayi yazdir.
                                                          
                                                         
        mov     ax, dx  ; kalani tasi,
        add     al, '0' ; ascii donusturucu, mevcut al ascii + 30h ('0') 
             
        mov     ah, 0Eh
        int     10h     ; "dakikay'",141, "yazd",141,"r."
        
        
        mov     al, ':'
        mov     ah, 0Eh
        int     10h
         
        
        mov     ax, [saniye]
        
        xor     dx, dx  ; hizli sifirlama dx
        mov     bx, 0Ah ; saniyeyi boler
        div     bx      ; saniye / 10
        
        add     al, '0' ; ascii donusturucu, mevcut al ascii + 30h ('0') 
        
        mov     ah, 0Eh
        int     10h     ; saniyeyi yazdir.
                                                          
                                                         
        mov     ax, dx  ; kalani tasi,
        add     al, '0' ; ascii donusturucu, mevcut al ascii + 30h ('0') 
             
        mov     ah, 0Eh
        int     10h     ; saniyeyi yazdir.
        
        pop     dx
        pop     cx
        pop     bx
        pop     ax
    
    
    ret
get_clock endp  


get_hello proc near
    
; es'i ayarla:
push    cs
pop     es

mov     bh, 0    ; sayfa.
lea     bp, hmsg ; telafi etme.
mov     bl, 0f3h ; varsayilan ozellik.
mov     cx, 12   ; karakter numarasi
mov     dl, 2    ; sutun.
mov     dh, 1    ; sira.
mov     ah, 13h  ; islev.
mov     al, 1    ; alt islev.
int     10h

; gecerli imlec konumunu goster:
mov     al, '<'  
mov     ah, 0eh
int     10h

mov     bh, 0    ; sayfa.
lea     bp, cmsg ; niteliklere sahip dizenin ofseti.
mov     bl, 0f3h ; varsayilan ozellik.
mov     cx, 12   ; karakter numarasi
mov     dl, 2    ; sutun.
mov     dh, 3    ; sira.
mov     ah, 13h  ; islev.
mov     al, 3    ; alt islev.
int     10h

; gecerli imlec konumunu goster:
mov     al, '<'
mov     ah, 0eh
int     10h

; herhangi bir tusa basilmasini bekleyin....
mov     ah, 0
int     16h

ret  ; kontrolu isletim sistemine iade edin.

hmsg db 'hello world!'

cmsg db 'h', 0cfh, 'e', 8bh, 'l', 0f0h, 'l', 5fh, 'o', 3ch, ' ', 0e0h
     db 'w', 0b3h, 'o', 2eh, 'r', 0cah, 'l', 1ah, 'd', 0ach, '!', 2fh



get_hello endp   



