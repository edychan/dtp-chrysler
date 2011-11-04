*****************************
* process renewal
* notes: index on substr(fvin,10,8) to vmsdata
* changes
* 01.17.02: check plate configuration
*****************************
* 04.10.06: take out xref to VMSDATA
*
* 12.16.07: blue/white plate exchange (SOS mandatory change)
*   Important: Passenger White Plate AAA9999
*              Commercial White Plate AA99999
* 01.23.07: house keeping for old blue plates ...
* 06.12.07: check if blue plate been renewed already 
*****************************
private xtrans, xseq, xdate, xyear
private yfil, yscn,yret, yrow, ypdesc, yptype
private ydigits, yspldate, yeplate, yxplate

f_clrscn ("DTP/MI - Process Renewal Transactions")

yfil = gdbfpath + "renewal.dbf"
if .not. file (yfil)
   f_valid (.f., "Missing Renewal File...")
   return
endif

*yfil = gvmspath + "vmsdata.dbf"
*if .not. file (yfil)
*   f_valid (.f., "Missing VMS Data File...")
*   return
*endif

f_box (01, 02, 04, 78)
@ 02, 04 say "Branch #......"
@ 02, 41 say "Clerk ID......"
@ 03, 04 say "Date.........."
l_floc = gloc
l_fdate = date()
l_fclerk = gclerk
* check last post date
f_use ("rasys")
xdate = fpostdate
use
* 09.12.05: check holiday table
yfil = gmempath + substr(dtos(date()),1,4) + ".mem"
if file (yfil)
   restore from (yfil) additive
else
   xholiday = ""
endif

do while .t.
   @ 02, 19 get l_floc pict "!!!" valid ;
     f_valid (f_verify("raloc",1,l_floc))
   @ 02, 56 say l_fclerk
   @ 03, 19 get l_fdate valid rdtp8x ()
   if f_rd () = 27
      return
   endif
   yret = f_confirm ("[C]ontinue  [E]dit  [Q]uit ","CEQ")
   if yret = "C"
      exit
   elseif yret = "Q"
      return
   endif
enddo

f_use ("raloc")
seek l_floc
if eof ()
   f_valid (.f., "Please Setup Branch Office First...")
   close data
   return
else
   l_fbranch = fbranch
endif
use

f_use ("ractr")
f_use ("radtrh")
f_use ("ratab")
f_use ("raplate")

*yfil = gvmspath + "vmsdata"
*select 0
*use &yfil index &yfil alias vmsdata

* only 1 person can process renewal
yfil = gdbfpath+"renewal"
select 0
use &yfil excl alias renewal
index on descend(substr(fvin,10,8)) to &yfil
go top
if eof ()
   f_valid (.f., "All Processed...")
   close data
   return
endif

f_popup ("Please Wait While Loading ...")
count for .not. deleted() .and. empty(fcontrol) to ycnt
f_popback ()

f_box (06, 02, 09, 78, "Total of "+alltrim(str(ycnt,6))+" Renewals ")
@ 07, 04 say "Renewals to be processed.. "
@ 08, 04 say "  Starting TAB #.......... "

xtrans = 0
xtab = space(7)
do while .t.
   @ 07, 31 get xtrans pict "999" valid f_valid (xtrans>0, "Must be Greater than 0...")
   @ 08, 31 get xtab pict "9999999" valid ;
     f_valid (f_verify("ratab",1,xtab),"Invalid Tab #...")
   if f_rd () = 27
      close data
      return
   endif
   xyear = ratab->fyear
   @ 08, 50 say xyear
   yret = f_confirm ("[C]ontinue  [E]dit  [Q]uit ","CEQ")
   if yret = "C"
      exit
   elseif yret = "Q"
      close data
      return
   endif
enddo

select renewal
go top

* 12.16.06: Assume blue plate is the 1st record...
ydigits = 0
yptype = " "   
ypdesc = space(3)
ysplate = space(7)
yeplate = space(7)
chkplate (renewal->fplate, @ypdesc)
if ypdesc = [003]

   @ 7, 41 say "Passenger/Commercial .. "
   @ 7, 66 get yptype pict "!" valid (f_valid (yptype $ [PC]))
   f_rd ()
   @ 7, 41 say "Starting White Plate # :"
   @ 8, 41 say "  Ending White Plate # :"
   do while .t.
      @ 7, 66 get ysplate pict "!!!!!!!" valid rdtp8y (ysplate, yptype)
      @ 8, 66 get yeplate pict "!!!!!!!" valid rdtp8y (yeplate, yptype)
      f_rd ()
      yret = f_confirm ("[C]ontinue  [E]dit  [Q]uit ","CEQ")
      if yret = "C"
         if ysplate > yeplate
            f_valid (.f., "Ending Plate # must be greater than Starting Plate # !")
            loop
         else
            exit
         endif
      elseif yret = "Q"
         close data
         return
      endif
   enddo
   select raplate
   seek ysplate
   if raplate->ftype = [01] .and. yptype = [P]      && passenger
      ydigits = 4
   elseif raplate->ftype = [03] .and. yptype = [C]  && commercial
      ydigits = 5
   else       && just in case
      f_valid (.f., "Invalid Plate Type: "+ysplate)
      close data
      return
   endif

endif
* start processing
f_box (11, 05, 22, 65, "Processing Transaction")
yrow = 11
ntran = 0
l_ftab = xtab
* for i = 1 to xtrans
do while .t.
   if eof () .or. ntran = xtrans
      exit
   endif
   * 12.16.06: check for blue plate
   ypdesc = space(3)
   if .not. chkplate (renewal->fplate, @ypdesc)
      select renewal
      skip
      loop
   endif
   * check calculation by addnew.exe
   if renewal->freg <= 0 .or. .not.empty(renewal->fcontrol)
      select renewal
      skip
      loop
   endif

   * 12.16.06: exchange white plate
   if ypdesc = [003]
      if empty(ysplate)      && in case encounter blue plate later in the batch
         f_valid (.f., "Please enter White Plate for "+renewal->fplate)
         close data
         return
      endif
      yxplate = ysplate
      select raplate
      seek yxplate
      if raplate->fstatus <> [A]
         f_valid (.f., "Error: Plate "+yxplate+" is Invalid !")
         close data
         return
      endif
      * 06.12.07: check if blue plate been replaced already
      seek renewal->fplate
      if raplate->fstatus = [I]
         select renewal
         reclock ()
         replace fcontrol with [INACTIVE]
         commit
         unlock
         skip
         loop
      endif
   endif

   * xref vmsdata
   *select vmsdata
   *seek substr(renewal->fvin,10,8)
   *if eof ()
   *   select renewal
   *   replace fcontrol with [NIS]
   *   commit
   *   unlock
   *   skip
   *   loop
   *endif
   * assign tab
   select ratab
   seek l_ftab
   do while .not. eof ()
      if ratab->fstatus $ "A;X" .and. ratab->fyear = xyear
          reclock ()
          replace fstatus with "U"
          commit
          unlock
          exit
      endif
      skip
   enddo
   if eof ()
      f_valid (.f., "No more tabs available, Please load new ones...")
      close data
      return
   endif
   l_ftab = ratab->ftab
   * assign seq #
   select ractr
   locate for fbranch = l_fbranch
   reclock ()
   l_fseq = strtran(str(fseq1,4), " ", "0")     
   replace fseq1 with fseq1 + 1
   commit
   unlock
   select radtrh
   append blank
   * assign other value
   replace floc with l_floc       && 01/06/97 edc
   replace fbranch with l_fbranch
   replace fdate with l_fdate
   replace fseq with l_fseq
   replace ftab with l_ftab
   replace fclerk with l_fclerk
   if ypdesc = [003]
      replace fplate with yxplate    && 12.16.06   
   else
      replace fplate with renewal->fplate
   endif
   replace fpplate with renewal->fpplate
   replace fvin with renewal->fvin
   replace faction with renewal->faction
   replace fmonth with renewal->fmonth
   replace fowner with renewal->fowner
   replace fref with renewal->fref
   replace freg with renewal->freg
   replace fstate with renewal->fstate
   replace ftotal with renewal->ftotal
   replace fcontrol with renewal->fcontrol
   replace ftax with renewal->ftax
   replace ftfee with renewal->ftfee
   replace ftitle with renewal->ftitle
   replace ftr11 with renewal->ftr11
   replace funit with "TR-1C"          && for renewal only
   commit
   unlock 
   *
   ntran = ntran + 1
   if yrow >= 21
      scroll (12, 10, 21, 60, 1)
   else
      yrow = yrow + 1
   endif
   @ yrow, 13 say radtrh->fvin+[ ]+radtrh->fplate+[ ]+radtrh->fseq+[ ]+radtrh->ftab

   * 01.23.07: house keeping for old blue plates ...
   if ypdesc = [003]
      select raplate
      seek renewal->fplate
      if eof ()
         append blank
         replace fplate with renewal->fplate, fstatus with "I"
      else
         reclock ()
         replace fstatus with "I"
      endif
      commit
      unlock
   endif
   *
   select renewal
   delete
   skip

   * 12.16.06
   if ypdesc = [003]
      select raplate
      seek yxplate
      reclock ()
      replace fstatus with "U"
      commit
      unlock
      ysplate = substr(ysplate,1,7-ydigits) + ;
         strtran(str(val(substr(ysplate,8-ydigits))+1,ydigits)," ","0")
      if ysplate > yeplate
         f_valid (.f., "Please load More Plates !")
         close data
         return
      endif
      select renewal
   endif

enddo

* housekeeping
* select renewal
* pack

f_valid (.f., "Process Completed...")
set filter to
close data
return

**************************************
function rdtp8y
parameter xplate, xtype
private ypdesc, ymess, ytype

ypdesc = space (3)
if .not. chkplate (xplate, @ypdesc)
   f_valid (.f., "Invalid Plate !")
   return .f.
endif
if ypdesc = [003]
   f_valid (.f., "Cannot enter a Blue plate !")
   return .f.
endif
ytype = if(xtype="P","01","03")
select raplate
seek xplate
if raplate->ftype <> ytype
   ymess = if(xtype="P"," Passenger "," Commercial ")
   f_valid (.f., "Please enter a"+ymess+"Plate...")
   return .f.
endif
return .t.

**************************************
function rdtp8x

f_y2k(@l_fdate)
do case
case l_fdate<=xdate
   f_valid (.f.,"Must be later than "+dtoc(xdate)) 
   return .f.
case dow(l_fdate)=1.or.dow(l_fdate)=7
   f_valid (.f.,"Must be Monday thru Friday only...")
   return .f.
case dtos(l_fdate) $ xholiday
   f_valid (.f.,"Date entered is a state holiday...")
   return .f.
case date()>xdate .and. l_fdate>date() .and. dow(date()) <> 7   && open Sat
   f_valid (.f.,"Please run End of Day first...")
   return .f.
case l_fdate<>date()
   if dow(date())=6
      if date()+3<>l_fdate
         f_valid (.f.,"Valid date is "+dtoc(date()+3))
         return .f.
      endif
   elseif dow(date())=7        && open Sat
      if date()+2<>l_fdate
         f_valid (.f.,"Valid date is "+dtoc(date()+2))
         return .f.
      endif
   else
      if date()+1<>l_fdate
         f_valid (.f.,"Valid date is "+dtoc(date()+1))
         return .f.
      endif
   endif
endcase

return .t.


