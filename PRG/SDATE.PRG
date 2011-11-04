clear
xdate = date()
do while .t.
   set century on
   @ 05, 05 say "Enter Date: " get xdate 
   read
   set century off
   if lastkey() = 27
      exit
   endif
   @ 05, 40 say jdate(xdate)
enddo


function jdate

parameter xdate
private ydate, y1, y2, y3, yday, yyr, y4

ydate = dtoc (xdate)

y1 = substr(ydate,4,2)
y2 = substr(ydate,1,2)
y3 = substr(ydate,7,2)
y4 = substr(dtos(xdate),1,4)

yday = ctod (y2+"/01/"+y4) - ctod ("01/01/"+y4) + val(y1)

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


