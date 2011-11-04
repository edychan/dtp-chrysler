* ===========================================================================
* DTP - title/plate processing
*
* Date:   09/22/94
* Author: EDC
*
* Revision:
* 09.21.98: ADD LASER PRINTER ROUTINE (PR_INIT, PR_TR11, PR_TR1L)
* 03.31.99: add ability to print different expiration date (gexpmo)
*           (PR-TR11, PR_TR1L)
* 10.31.01: branch 806 GELCO -> gexpmo = 06 (June)
*           function pr_tr1l, pr_tr11
* 11.06.03: add parameter to set printer in (pr_tr11, pr_tr1l)
* 09.12.05: print odometer reading for branch 806 ONLY (pr_tr11)
* 03.07.06: print odometer reading for branch 811 ALSO (pr_tr11)
* 12.01.06: print previous plate # on registration form
*
* 04.01.07: determine ownership for 811 (Corp or Company LLC)
* 08.21.07: name change to CHRYSLER LLC
* --------------------------------------------------------------------------
* 07.01.09: For 811 No change 
*           For 806; starting 07.01.09 (including title done manually thru SOS)
*               Title name ::= [CHRYSLER GROUP LLC]
*               Lien ::= [NONE]
*               Lessee ::= [NONE]
*               Addres No change
* ===========================================================================

do while .t.
   xret1 = f_pushmenu (xoption0)
   if xret1 = 0
      exit
   endif
   xoption1 = substr (goption [xret1], 4)
   do case
      case xret1 = 1            && create tran
         do rdtp1
      case xret1 = 2            && edit tran
         do rdtp2
      case xret1 = 3            && print TR11
         do rdtp3 
      case xret1 = 4            && end of day processing
         do rdtp4
      case xret1 = 5            && print/reprint end of day reports
         do rdtp5
      case xret1 = 6            && view history record
         do rdtp6
      case xret1 = 7            && print end of month reports
         do rdtp7
      case xret1 = 8            && renewal sub system
         do rdtp8
   endcase
enddo

*********************
* initialize laser printer
function pr_init
parameter xprinter, xtype
private yfil

if xtype = 1
   yfil = "VTLE.DEF"
else
   yfil = "VREG.DEF"
endif
if .not. file (yfil)
   f_valid (.f., "Macro File Missing ...")
   return .f.
endif
f_popup ("Please Wait While Loading Macros...")

! copy &yfil &xprinter >null

if file ("jqp.ldf")    && signature file
   ! copy jqp.ldf &xprinter >null
endif

f_popback ()
return .t.

**********************
* print tab
* note: calling routine open radtrh, ravin, rabody, raplate
function pr_tr1l
parameter xprinter, xtype, xbranch, xdate, xseq, xprtsw
private yy1, xdate, xplate, xmsrp, xexp, xdesc
private yowner, ylessee

* set up transaction table
private ydesc [9]
ydesc [1] = "Original Title Only"
ydesc [2] = "Original Title/Original Plate"
ydesc [3] = "Original Title/Transfer Plate"
ydesc [4] = "Original Title/Renewal-Transfer"
ydesc [5] = "Original Plate Only"
ydesc [6] = "Renewal Plate Only"
ydesc [7] = "Transfer Plate Only"
ydesc [8] = "                   "
ydesc [9] = "Renewal-Transfer Plate Only"

if pcount() < 6       && 11.07.03
   xprtsw = .t.
endif

select radtrh
xplate = radtrh->fplate
xdate = radtrh->fdate
if empty(radtrh->fpplate) .or. (radtrh->fplate=radtrh->fpplate)  && 12.01.06
   xdesc = ydesc [val(radtrh->faction)]
else
   xdesc = ydesc [val(radtrh->faction)] + [ (] + trim(radtrh->fpplate) + [)]
endif
xexp = space(6)
select ravin
seek substr(radtrh->fvin,1,8)+substr(radtrh->fvin,10,1)
select rabody
seek ravin->fstyle
if ravin->ffee = [01]
   if radtrh->faction $ [3;7]
      select raplate
      seek radtrh->fplate
      if .not. eof()
         xexp = strtran(dtoc(raplate->frenewal),"/","") 
      endif
   endif
   if empty(xexp)
      yy1 = val(substr(dtos(xdate),1,4))         && 199X
      do case
      case gexpmo = [04]
          yy1 = if(substr(dtoc(xdate),1,2)=[12], yy1+2, yy1+1)    && for month = dec
      case gexpmo = [06]    && 10.31.01:branch 806 GELCO
          yy1 = if(val(substr(dtoc(xdate),1,2)) > 1, yy1 + 1, yy1)  
      case gexpmo = [08]
          yy1 = if(val(substr(dtoc(xdate),1,2)) > 3, yy1 + 1, yy1)  
      endcase
      yy1 = str(yy1,4)
      xexp = gexpmo + [01]+substr(yy1,3,2)
   endif
   if ravin->fmsrp > 99999
      xmsrp = strtran(str(ravin->fmsrp,6,0)," ","0")
   else
      xmsrp = strtran(str(int(ravin->fmsrp/1000),6,0)," ","0")
   endif
else
   if radtrh->faction $ [3;7]
      select raplate
      seek radtrh->fplate
      if .not. eof()
         xexp = strtran(dtoc(raplate->frenewal),"/","") 
      endif
   endif
   if empty(xexp)
      yy1 = val(substr(dtos(xdate),1,4))         && 199X
      if substr(dtoc(xdate),1,2) $ [10,11,12]    && for month = oct,nov,dec
          yy1 = yy1 + 2
      else
          yy1 = yy1 + 1
      endif
      yy1 = str(yy1,4)
      xexp = [0228]+substr(yy1,3,2)
   endif
   if ravin->fmsrp > 99999
      xmsrp = strtran(str(ravin->fmsrp,6,0)," ","0")
   elseif ravin->fmsrp > 10000
      xmsrp = strtran(str(int(ravin->fmsrp/1000),6,0)," ","0")
   else
      xmsrp = strtran(str(ravin->fmsrp,6,0)," ","0")
   endif
endif
xexp1 = substr(xexp,1,2)+"/"+substr(xexp,3,2)+"/"+substr(xexp,5,2)

* --determine ownership

if radtrh->fbranch = [806]
   * --07.01.09: 806 will be title exclusively using [CHRYSLER GROUP LLC]
   if radtrh->fowner = [CHRYSLER GROUP LLC]    && 
      yowner = radtrh->fowner
      ylessee = []
   else
      yowner = [GELCO CORPORATION]
      * -------- 10.01.07 --------------------------------
      f_use ("ravm")
      seek substr(radtrh->fvin,10,8)
      if eof() .or. empty(ravm->fdate1)   && error !!!
         ylessee = [CHRYSLER LLC LESSEE]
      else
         if ravm->fdate1 >= ctod('08/21/2007')
            ylessee = [CHRYSLER LLC LESSEE]
         elseif ravm->fdate1 >= ctod('04/01/2007')
            ylessee = [DAIMLERCHRYSLER COMPANY LLC LESSEE]
         else 
            ylessee = [DAIMLERCHRYSLER CORPORATION LESSEE]
         endif
      endif
      select ravm
      use      
   endif

   * ----------------------------------------------------
else            && branch 811
   do case
   case radtrh->fowner = [CHRYSLER LLC]    && 08.21.07
      yowner = [CHRYSLER LLC]
   case radtrh->fowner = [CHRYSLER CORPORATION]
      yowner = [DAIMLERCHRYSLER CORPORATION]
   case radtrh->fowner = [CHRYSLER COMPANY]
      yowner = [DAIMLERCHRYSLER COMPANY LLC]
   otherwise
      yowner = [DAIMLERCHRYSLER CORPORATION]
   endcase
   ylessee = space(34)
endif

* --07.13.09

if xprtsw      && 11.07.03
   set console off
   set print on
   set printer to &xprinter
   * set printer to tr-1l         && debug
endif

if xtype = 1
   * ? [!R! FSET 1p50v4s3b4148t;SMB 7;UNIT C;CASS 0;COPY 1;MAP 14,11;EXIT;]+substr(xexp,1,2)
   * ? [!R! FSET 1p33v4s3b1418t; MAP 15.75,10.75;TEXT '-'; MAP 16,10.7;EXIT;]+substr(xexp,5,2)
   * ? [!R! FSET 1p12v4s3b4148t; MAP 15.55,9.55;EXIT;]+xplate
   * ? [!R! FONT 42; MAP 14.25,9.5;TEXT 'MI';MAP 15,11.5;EXIT;]+radtrh->fbranch+jdate(xdate)
   * ? [!R! EXIT;]
   * ---------- 08.10.00: changes per SOS
   xmonth = "JAN;FEB;MAR;APR;MAY;JUN;JUL;AUG;SEP;OCT;NOV;DEC;"
   xmth = substr(xmonth, ((val(substr(xexp,1,2))-1) * 4) + 1, 3)
   if substr(xexp,1,2) $ "03;05"    && just in case: for 800 branches, exp. [02,04,08,09]
      ? [!R! FSET 1p53v4s3b4148t;UNIT C;CASS 0;COPY 1;MAP 13.80,11;EXIT;]+xmth
      ? [!R! FSET 1p12v4s3b4148t;MAP 15.55,9.55;EXIT;]+xplate
      ? [!R! FONT 42;MAP 14.25,9.5;TEXT 'MI';EXIT;]
      ? [!R! FONT 42;MAP 14.25,11.5;EXIT;]+[20]+substr(xexp,5,2)+[  ]+radtrh->fbranch+jdate(xdate)
      ? [!R! EXIT;]
   else
      ? [!R! FSET 1p55v4s3b4148t;UNIT C;CASS 0;COPY 1;MAP 14,11;EXIT;]+xmth
      ? [!R! FSET 1p12v4s3b4148t;MAP 15.55,9.55;EXIT;]+xplate
      ? [!R! FONT 42;MAP 14.25,9.5;TEXT 'MI';EXIT;]
      ? [!R! FONT 42;MAP 14.25,11.5;EXIT;]+[20]+substr(xexp,5,2)+[  ]+radtrh->fbranch+jdate(xdate)
      ? [!R! EXIT;]
   endif
   * ----------
   ? [!R! FONT 62; MAP 0,-.015; CALL VREG; EXIT,E;]
else
   ? [!R! SMB 7;UNIT C;CASS 1;COPY 1;EXIT;]
   ? [!R! FONT 62; MAP 0,-.015; CALL VREG; EXIT,E;]
endif
?
?
? space(6)+xplate+space(15)+xexp1+space(39)+xplate+space(16)+xexp1
? space(3)+f_truncate(xdesc,40)+space(26)+xdesc                && 12.01.06
? space(3)+ravin->fyear+space(6)+f_truncate(ravin->fmake,15)+space(5)+f_truncate(rabody->fdesc,15)+ ;
  space(23)+ravin->fyear+space(6)+f_truncate(ravin->fmake,15)+space(5)+f_truncate(rabody->fdesc,15)
? space(12)+radtrh->fvin+space(24)+xmsrp+space(22)+radtrh->fvin+space(26)+xmsrp
? space(46)+f_truncate(raloc->fctyname,15)+space(55)+f_truncate(raloc->fctyname,15)
?
?
? space(3)+f_truncate(yowner,30)+space(36)+yowner
* -- 10.23.01: print lessee
* -- ? space(3)+f_truncate(raloc->fcaddr,35)+space(31)+raloc->fcaddr
? space(3)+f_truncate(ylessee,35)+space(31)+ylessee
? space(3)+f_truncate(raloc->fcaddr,35)+space(31)+raloc->fcaddr
? space(3)+f_truncate(raloc->fccity,20)+"  "+raloc->fcstate+"  "+raloc->fczip+  ;
  space(30)+f_truncate(raloc->fccity,20)+"  "+raloc->fcstate+"  "+raloc->fczip
? space(53)+str(radtrh->ftotal,6,2)+space(65)+str(radtrh->ftotal,6,2) 
yy1 = strtran(dtoc(xdate),"/","")
? space(5)+yy1+" "+radtrh->fclerk+"1 "+jdate(xdate)+" "+radtrh->fbranch+" "+ ;
  radtrh->fseq+space(4)+str(radtrh->ftotal,6,2)+space(31)+ ;
  yy1+" "+radtrh->fclerk+"1 "+jdate(xdate)+" "+radtrh->fbranch+" "+ ;
  radtrh->fseq+space(4)+str(radtrh->ftotal,6,2)
?                                                                                                           
?
?
?
? [!R! EXIT,E;]
? [!R! FSET 0p12h12v0s3b0t; CALL VREG; EXIT,E;]
? space(5)+xplate+" "+ravin->ffee+space(8)+xexp1+"  "+str(radtrh->fmonth,2)
? space(2)+xdesc
? 
? space(2)+ravin->fyear+space(6)+f_truncate(ravin->fmake,15)+space(5)+f_truncate(rabody->fdesc,15)
? space(9)+radtrh->fvin+space(14)+xmsrp
?
? space(31)+f_truncate(raloc->fctyname,12)+" "+raloc->fcounty
? "  "+yowner
* -- 10.23.01: print lessee
* -- ? "  "+raloc->fcaddr
? "  "+ylessee
? "  "+raloc->fcaddr
? "  "+f_truncate(raloc->fccity,20)+"  "+raloc->fcstate+"  "+raloc->fczip+ ;
  space(2)+str(radtrh->ftotal,6,2)
?
yy1 = strtran(dtoc(xdate),"/","")
? space(2)+yy1+" "+radtrh->fclerk+"1 "+jdate(xdate)+" "+radtrh->fbranch+" "+ ;
  radtrh->fseq+space(4)+str(radtrh->ftotal,6,2)+space(4)+radtrh->ftab
?
? [!R! PAGE;CASS 1;EXIT,E;]

if xprtsw      && 11.07.03
   set print off
   set printer to
   set console on
   set device to screen
endif

return .t.


***********************
* print tr11
* note: calling routine open radtrh, ravin, rabody, raplate
function pr_tr11
parameter xprinter, xcopy, xbranch, xdate, xseq, xprtsw
private yowner, ylessee
* set up transaction table
private ydesc [9]
ydesc [1] = "Original Title Only"
ydesc [2] = "Original Title/Original Plate"
ydesc [3] = "Original Title/Transfer Plate"
ydesc [4] = "Original Title/Renewal-Transfer"
ydesc [5] = "Original Plate Only"
ydesc [6] = "Renewal Plate Only"
ydesc [7] = "Transfer Plate Only"
ydesc [8] = "                   "
ydesc [9] = "Renewal-Transfer Plate Only"

if pcount() < 6     && 11.07.03
   xprtsw = .t.
endif

select radtrh
seek xbranch+dtos(xdate)+xseq
xplate = radtrh->fplate
xdate = radtrh->fdate
xdesc = ydesc [val(radtrh->faction)]
xexp = space(6)
select ravin
seek substr(radtrh->fvin,1,8)+substr(radtrh->fvin,10,1)
select rabody
seek ravin->fstyle
if ravin->ffee = [01]
   if radtrh->faction $ [3;7]
      select raplate
      seek radtrh->fplate
      if .not. eof()
         xexp = strtran(dtoc(raplate->frenewal),"/","") 
      endif
   endif
   if empty(xexp)
      yy1 = val(substr(dtos(xdate),1,4))         && 199X
      do case
      case gexpmo = [04]
          yy1 = if(substr(dtoc(xdate),1,2)=[12], yy1+2, yy1+1)    && for month = dec
      case gexpmo = [06]    && 10.31.01: branch 806 GELCO
          yy1 = if(val(substr(dtoc(xdate),1,2)) > 1, yy1 + 1, yy1)  
      case gexpmo = [08]
          yy1 = if(val(substr(dtoc(xdate),1,2)) > 3, yy1 + 1, yy1)  
      endcase
      yy1 = str(yy1,4)
      xexp = gexpmo + [01]+substr(yy1,3,2)
   endif
   if ravin->fmsrp > 99999
      xmsrp = strtran(str(ravin->fmsrp,6,0)," ","0")
   else
      xmsrp = strtran(str(int(ravin->fmsrp/1000),6,0)," ","0")
   endif
else
   if radtrh->faction $ [3;7]
      select raplate
      seek radtrh->fplate
      if .not. eof()
         xexp = strtran(dtoc(raplate->frenewal),"/","") 
      endif
   endif
   if empty(xexp)
      yy1 = val(substr(dtos(xdate),1,4))         && 199X
      if substr(dtoc(xdate),1,2) $ [10,11,12]    && for month = oct,nov,dec
          yy1 = yy1 + 2
      else
          yy1 = yy1 + 1
      endif
      yy1 = str(yy1,4)
      xexp = [0228]+substr(yy1,3,2)
   endif
   if ravin->fmsrp > 99999
      xmsrp = strtran(str(ravin->fmsrp,6,0)," ","0")
   elseif ravin->fmsrp > 10000
      xmsrp = strtran(str(int(ravin->fmsrp/1000),6,0)," ","0")
   else
      xmsrp = strtran(str(ravin->fmsrp,6,0)," ","0")
   endif
endif
xexp1 = substr(xexp,1,2)+"/"+substr(xexp,3,2)+"/"+substr(xexp,5,2)


* --determine ownership
ylien = []     && lien ... 
yoaddr = []    && lien address ...
yocity = []
yostate = []
yozip = []

if radtrh->fbranch = [806]
   * --07.01.09: 806 will be title exclusively using [CHRYSLER GROUP LLC]
   if radtrh->fowner = [CHRYSLER GROUP LLC]    && 
      yowner = radtrh->fowner
      ylessee = []
   else
      yowner = [GELCO CORPORATION]
      * -------- 10.01.07 --------------------------------
      f_use ("ravm")
      seek substr(radtrh->fvin,10,8)
      if eof() .or. empty(ravm->fdate1)   && error !!!
         ylessee = [CHRYSLER LLC LESSEE]
      else
         if ravm->fdate1 >= ctod('08/21/2007')
            ylessee = [CHRYSLER LLC LESSEE]
         elseif ravm->fdate1 >= ctod('04/01/2007')
            ylessee = [DAIMLERCHRYSLER COMPANY LLC LESSEE]
         else 
            ylessee = [DAIMLERCHRYSLER CORPORATION LESSEE]
         endif
      endif
      select ravm
      use      
   endif

   if .not.empty (ylessee)
      * --ylien = raloc->fowner           && ie. [GE CAPITAL CORPORATION]
      ylien = [GE CAPITAL CORPORATION]
      yoaddr = raloc->foaddr
      yocity = raloc->focity
      yostate = raloc->fostate
      yozip = raloc->fozip
   endif
   * ----------------------------------------------------

else            && branch 811
   do case
   case radtrh->fowner = [CHRYSLER LLC]    && 08.21.07
      yowner = [CHRYSLER LLC]
   case radtrh->fowner = [CHRYSLER CORPORATION]
      yowner = [DAIMLERCHRYSLER CORPORATION]
   case radtrh->fowner = [CHRYSLER COMPANY]
      yowner = [DAIMLERCHRYSLER COMPANY LLC]
   otherwise
      yowner = [DAIMLERCHRYSLER CORPORATION]
   endcase
   ylessee = space(34)
endif
* --07.13.09

if xprtsw     && 11.07.03
   set console off
   set print on
   set printer to &xprinter
   * set printer to tr-11l         && debug
endif

if xcopy = 1
   ? [!R! FSET 0p12h12v0s3b0t;CASS 1;COPY 1;CALL VTLE;EXIT,E;]
else
   ? [!R! FSET 0p12h12v0s3b0t;CASS 1;COPY 2;CALL VTLE;EXIT,E;]
endif
?
?
? "  "+f_truncate(xdesc,30)+"   "+xplate+space(10)+xexp1+space(22)+ ;
  str(radtrh->freg,6,2)
?
? space(4)+ravin->fyear+space(7)+f_truncate(ravin->fmake,15)+space(29)+radtrh->fvin+ ;
  space(8)+str(radtrh->ftitle,6,2)
?
?
* -- 09.12.05: print blanket mileage: [10]
* -- 03.07.06: print blanket mileage: [10]
? "  "+f_truncate(rabody->fdesc,15)+"  "+xmsrp+ ;
  space(8)+[10]+space(35)+ ;
  "N"+space(11)+str(radtrh->ftax,6,2)
?
? "  "+f_truncate(yowner,35)+space(45)+str(radtrh->ftfee,6,2)
* -- 10.23.01: print lessee
* -- ? "  "+raloc->fcaddr
* -- ? "  "+f_truncate(ylessee,35)+space(45)+str(radtrh->ftotal,6,2)
? "  "+ylessee
? "  "+f_truncate(raloc->fcaddr,35)+space(45)+str(radtrh->ftotal,6,2)
? "  "+f_truncate(raloc->fccity,20)+"  "+raloc->fcstate+"  "+raloc->fczip+ ;
  space(15)+radtrh->ftab+space(15)+raloc->fcounty
?
?
* -- ?
* -- ? [  NONE                                 ]+dtoc(xdate)+[    NONE]
? "  "+if(empty(ylien),[NONE]+space(33),f_truncate(ylien,37))+ ;
  dtoc(xdate)+[    NONE]
? "  "+f_truncate(yoaddr,35)
? "  "+f_truncate(yocity,20)+"  "+yostate+"  "+yozip
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
yy1 = strtran (dtoc(xdate),"/","")
? "  "+yy1+" "+radtrh->fclerk+"1 "+jdate(xdate)+" "+radtrh->fbranch+ ;
  " "+radtrh->fseq+"   "+str(radtrh->ftotal,6,2)+space(13)+ ;
  radtrh->fbranch+jdate(xdate)+radtrh->fseq
?
? [!R! PAGE;EXIT,E;]

if xprtsw      && 11.07.03
   set print off
   set printer to
   set console on
   set device to screen
endif

return .t.
