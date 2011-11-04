*
* check for duplicate title/plate applications
*
parameter xdummy
set delete on
set excl off

if pcount() < 1 
   quit
elseif xdummy = "P"
   set print on
   set printer to lpt1
endif
*
select 0
use h:\dtp\dbf\radtrh index h:\dtp\dbf\radtrh2   && index on vin #
go top
ykey = ""
yreg = 0
do while .not. eof ()
   if ykey <> substr(fvin,10,8)
      if yreg > 1
         ? ykey + "==> dup" + str(yreg,5)
      endif
      ykey = substr(fvin,10,8)
      yreg = 0
   endif 
   if faction $ [1;2;3;4;5]
      yreg = yreg + 1
   endif
   skip
enddo

set printer to
set console on
set print off
set device to screen

close all

