* --------------------------------------------------
* DTP interface program for chrysler 
* process rcv files into dtp data files
*
* changes: 04.05.98
* y2k: model year from 2 digit to 4 digit
* 01.03.07: skip [U] & use [V] for year 2007 (jdate)
* --------------------------------------------------
* 07.13.09: add path parameter (for 806 to starting using 811's download file)
*     default is h:\product => 811
*                t:\product => 806 (t:\product -> h:\product)
* --------------------------------------------------
set excl off
set delete on
clear

? "Please Wait ..."

yfil = "transit.dbf"
if .not. file (yfil)
   ytmp = "tmp.dbf"
   create &ytmp
   append blank
   replace field_name with "FIELD1"
   replace field_type with "C"
   replace field_len with 1
   replace field_dec with 0 
   append blank
   replace field_name with "FIELD2"
   replace field_type with "C"
   replace field_len with 1
   replace field_dec with 0 
   append blank
   replace field_name with "FIELD3"
   replace field_type with "N"
   replace field_len with 5
   replace field_dec with 2 
   append blank
   replace field_name with "FIELD4"
   replace field_type with "N"
   replace field_len with 5
   replace field_dec with 2 
   append blank
   replace field_name with "FIELD5"     && plate info.
   replace field_type with "C"
   replace field_len with 8
   replace field_dec with 0 
   append blank
   replace field_name with "FIELD6"
   replace field_type with "N"
   replace field_len with 1
   replace field_dec with 0 
   append blank
   replace field_name with "FIELD7"
   replace field_type with "N"
   replace field_len with 1
   replace field_dec with 0 
   append blank
   replace field_name with "FIELD8"
   replace field_type with "N"
   replace field_len with 1
   replace field_dec with 0 
   append blank
   replace field_name with "FIELD9"
   replace field_type with "N"
   replace field_len with 1
   replace field_dec with 0 
   append blank
   replace field_name with "FIELD10"
   replace field_type with "N"
   replace field_len with 1
   replace field_dec with 0 
   append blank
   replace field_name with "YEAR"
   replace field_type with "N"
   replace field_len with 4               && 04.05.98: 
   replace field_dec with 0 
   append blank
   replace field_name with "MAKE"
   replace field_type with "C"
   replace field_len with 10
   replace field_dec with 0 
   append blank
   replace field_name with "VIN"
   replace field_type with "C"
   replace field_len with 17
   replace field_dec with 0 
   create &yfil from &ytmp
   use
   erase &ytmp
endif
   
select 0 
set excl on
use transit alias transit
zap
*
* append data
* vehicles.txt is the daily download
* renewal.txt is the renewal file
*
declare nfile[5]
nf = 0

* --07.13.09
* ylocal = "h:\product\"     && 811
  ylocal = "t:\product\"     && 806
* --

yfil = ylocal + "vehicles.txt"
if file (yfil)
   nf = nf + 1
   nfile [nf] = "vehicles.txt"
endif

yfil = ylocal + "renewal.txt"
if file (yfil)
   nf = nf + 1
   nfile [nf] = "renewal.txt"
endif

if nf = 0  
  ?
  ? "Error:  Missing Data Files..."
  ?
  ? "Status: Please report to Supervisor..."
  ?
  ? "Press any key..."
  ?
  inkey (0)
  close data
  quit
endif
* copy data files & save as GXXX.ftp
ypath = "h:\dtp\ftp\"
for i = 1 to nf
   y1 = ylocal + nfile [i]
   y2 = ypath + nfile [i]
   copy file &y1 to &y2
   erase &y1              
next i
*
* start process into data files
*
ypath = "h:\dtp\dbf\"
yfil = ypath+"ravin"
select 0
use &yfil index &yfil alias ravin

yfil = ypath+"rabody"
select 0
use &yfil index &yfil alias rabody

yfil = ypath+"ravm"
y1 = ypath+"ravm1"
select 0
use &yfil index &y1 alias ravm

yfil = ypath+"radtr"
y1 = ypath+"radtr1"
y2 = ypath+"radtr2"
select 0
use &yfil index &y2, &y1 alias radtr
*
for i=1 to nf
   ypath = "h:\dtp\ftp\"
   yfil = ypath+alltrim(upper(nfile[i]))
   ysav = ypath + jdate(date()) + ".ftp"
   j = 1
   do while file (ysav)
      ysav = ypath + jdate(date()) + "-" + str(j,1)+ ".ftp"
      j = j + 1
      if j > 5
         ?
         ? "Error:  Trying to process more than 5 Data Files..."
         ?
         ? "Status: Please report to supervisor..."
         ?
         ? "Press any key..."
         ?
         inkey (0)
         close data
         quit
      endif
   enddo 
   ? "Copying Data from ..." + yfil
   if file (yfil)
      yfunit = fopen (yfil)
      ylen = fseek (yfunit, 0, 2)
      fclose (yfunit)
      if ylen > 0
         select transit
         zap
         append from &yfil sdf delimited 
         if file (ysav)       && just in case
            erase &ysav
         endif
         rename &yfil to &ysav
         do dtpp1           && process transit -> ravin, rabody, ravm, radtr
      else
         erase &yfil
      endif
   endif
next i

close data        && close tmp files

?
? "Status: Process Completed..."
?
? "Press any key..."
?
inkey (0)

close data

***********************
procedure dtpp1

select transit
go top
do while .not. eof ()
   l_fyear = str(transit->year,4)         && 04.05.98
   l_fmake = transit->make
   l_fvin = transit->vin
   if .not. empty (l_fvin)
      ? "Adding -----> " + l_fvin + " " + l_fyear + " " + l_fmake
   endif

   * update ravm, ravin, radtr
   if .not. empty(l_fvin)
      select ravm
      seek substr(l_fvin,10,8)
      if eof ()
         append blank
         replace fvin with l_fvin
         commit 
         unlock
      endif 

      select ravin
      seek substr(l_fvin,1,8)+substr(l_fvin,10,1)
      if eof ()
         append blank
         replace fvin with substr(l_fvin,1,8)+substr(l_fvin,10,1)
         replace fmake with l_fmake
         replace fyear with substr(l_fyear,3,2), fdesc with "CVMS"
         commit 
         unlock
      endif

      * only 1 vin record in radtr at 1 time
      select radtr
      seek substr(l_fvin,10,8)
      if eof()
         seek space(8)          && re-use record
         if eof ()
            append blank
         else
            rlock ()
         endif
         replace fvin with l_fvin
      else
         rlock ()
      endif      
      if .not. empty(transit->field5)     && plate info
         replace faction with [6], fplate with strtran(transit->field5," ","")
      endif
      commit 
      unlock 
   endif
   select transit
   skip
enddo

**********************************************************************
function jdate

parameter xdate
private ydate, y1, y2, y3, yday, yyr

ydate = dtoc (xdate)

y1 = substr(ydate,4,2)
y2 = substr(ydate,1,2)
y3 = substr(ydate,7,2)

yday = ctod (y2+"/01/"+y3) - ctod ("01/01/"+y3) + val(y1)

do case
   case y3 = "96"
     yyr = "G"
   case y3 = "97"
     yyr = "H"
   case y3 = "98"
     yyr = "J"
   case y3 = "99"
     yyr = "K"
   case y3 = "00"
     yyr = "L"
   case y3 = "01"
     yyr = "M"
   case y3 = "02"
     yyr = "N"
   case y3 = "03"
     yyr = "P"
   case y3 = "04"
     yyr = "R"
   case y3 = "05"
     yyr = "S"
   case y3 = "06"
     yyr = "T"
*   case y3 = "07"
*     yyr = "U"
   case y3 = "07"
     yyr = "V"
   case y3 = "08"
     yyr = "W"
   case y3 = "09"
     yyr = "X"
   case y3 = "10"
     yyr = "Y"
   case y3 = "11"
     yyr = "Z"
endcase
return (yyr+strtran(str(yday,3)," ","0"))

