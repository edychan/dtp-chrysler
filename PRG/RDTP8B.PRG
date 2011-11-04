*
*  print envelope 
*
*  note: 09.29.99
*  1. mailing data into gvmspath+"vmsdata"
*  2. key = substr(fvin,10,8) 
*  3. if CAT = PE or LE ==> home address
*     otherwise ==> CIMS (interoffice) address
*  4. print ffname + flname + substr(fvin,10,8)
*           faddr
*           fcity
*           fstate + fzip
*           fcountry
* changes:
* 07/03/01: add filter start/end seq.
*
private y1, y2, yfil, yntx, ycc, ytot, xstart, xend, xprt, xnl

f_clrscn ("DTP - PRINT ENVELOPE")

yfil = gvmspath + "vmsdata.dbf"
yntx = gvmspath + "vmsdata.ntx"
if .not. file (yfil)
   f_valid (.f., "Missing VMS Data File...")
   return
endif

select 0
use &yfil index &yntx alias vmsdata

f_use ("radtrh")

f_box (01, 02, 08, 78)
@ 02, 04 say "Branch #......"
@ 03, 04 say "Report Date..."
@ 04, 04 say "Starting......"
@ 05, 04 say "Ending........"
@ 06, 04 say "VIN...........          [Last 8 digit  ]"
@ 07, 04 say "Printer.......          [LPT1/LPT2/LPT3]"

xloc = gloc
xdate = date()
xstart = 0
xend = 0
xvin = space(8)
xprt = [LPT1]
do while .t.
   @ 02, 19 get xloc pict "!!!" 
   @ 03, 19 get xdate valid rdtp8b_1 (xdate)
   @ 04, 19 get xstart pict "9999" valid f_valid (xstart >= 0)
   @ 05, 19 get xend pict "9999" valid f_valid (xend >= xstart)
   @ 06, 19 get xvin pict "!!!!!!!!" valid rdtp8b_2 (xvin)
   @ 07, 19 get xprt pict "!!!!" valid f_valid (xprt $ "LPT1;LPT2;LPT3")
   if f_rd () = 27
      close data
      return
   endif
   yret = f_confirm ("[C]ontinue  [E]dit  [Q]uit ","CEQ")
   if yret = "C"
      exit
   elseif yret = "Q"
      close data
      return
   endif
enddo

if f_popup ("Press <Enter> to Begin or <Esc> to Stop Printing...", .t.) = 27
   close data
   return
endif

* setup print condition

if empty(xvin)
   do rdtp8b_3
else
   do rdtp8b_4
endif

set filter to
close data
f_valid (.f., "Process Completed...")

*********************
function rdtp8b_1
parameter xdate

f_y2k (@xdate)     && 01.24.00
select radtrh
set softseek on
seek xloc+dtos(xdate)
set softseek off
if eof ()
   f_valid (.f., "Invalid Report Date...")
   return .f.
else
   return .t.
endif

*********************
function rdtp8b_2
parameter xvin

if empty(xvin)
   return .t.
else
   select vmsdata
   seek xvin
   if eof ()
      f_valid (.f., "Cannot Locate VIN in VMS Data File...")
      return .f.
   else
      return .t.
   endif
endif

*********************************************
procedure rdtp8b_3

f_y2k (@xdate)    && leave this here

* set up condition
if xstart = 0
   xcond = ".t."
else
   xcond = "val(radtrh->fseq) >= xstart .and. val(radtrh->fseq) <= xend"
endif

set console off
set print on
* set printer to xxx   && debug
set printer to &xprt
ycc = "chr(27)+chr(65)+chr(12)+chr(27)+chr(67)+chr(66)"
?? &ycc
begin sequence
   select radtrh
   set softseek on
   seek xloc+dtos(xdate)+strtran(str(xstart,4,0)," ","0")
   set softseek off
   do while .not. eof () .and. radtrh->fbranch+dtos(radtrh->fdate)=xloc+dtos(xdate) .and. &xcond
      key = inkey ()
      if key = 27
         set console on
         set print off
         if f_confirm ("Do you want to quit ? ", "YN") = "N"
            set console off
            set print on
         else
            exit
         endif
      endif
      select vmsdata
      seek substr(radtrh->fvin,10,8)
      if eof ()
         select radtrh
         skip
         loop
      endif
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ? space(25) + substr(vmsdata->fvin,10,8)
      ? space(25) + alltrim(vmsdata->ffname) + [ ] + alltrim(vmsdata->flname)
      ? space(25) + alltrim (vmsdata->faddr)
      ? space(25) + alltrim (vmsdata->fcity) + [  ] + vmsdata->fstate + [  ] + vmsdata->fzip
      ? space(25) + vmsdata->fcountry
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      ?
      select radtrh
      skip
      if eof ()
         eject 
         exit
      endif
      select vmsdata
      seek substr(radtrh->fvin,10,8)
      if eof ()
         select radtrh
         skip
         loop
      endif
      ?
      ?
      ?
      ?
      ?
      ? space(25) + substr(vmsdata->fvin,10,8)
      ? space(25) + alltrim(vmsdata->ffname) + [ ] + alltrim(vmsdata->flname)
      ? space(25) + alltrim (vmsdata->faddr)
      ? space(25) + alltrim (vmsdata->fcity) + [  ] + vmsdata->fstate + [  ] + vmsdata->fzip
      ? space(25) + vmsdata->fcountry
      ?
      eject
      select radtrh
      skip
   enddo
end
* reset printer
* ? chr(18)

set print off
set printer to
set console on
set device to screen

*********************************************
procedure rdtp8b_4

set console off
set print on
set printer to &xprt
ycc = "chr(27)+chr(65)+chr(12)+chr(27)+chr(67)+chr(66)"
?? &ycc

select vmsdata
seek xvin
?
?
?
?
?
?
?
?
?
?
? space(25) + substr(vmsdata->fvin,10,8)
? space(25) + alltrim(vmsdata->ffname) + [ ] + alltrim(vmsdata->flname)
? space(25) + alltrim (vmsdata->faddr)
? space(25) + alltrim (vmsdata->fcity) + [  ] + vmsdata->fstate + [  ] + vmsdata->fzip
? space(25) + vmsdata->fcountry
?
eject

set print off
set printer to
set console on
set device to screen


