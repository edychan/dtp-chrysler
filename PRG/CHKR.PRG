*
* check the plate from download file
*
select 0
use h:\dtp\dbf\raplate index h:\dtp\dbf\raplate.ntx alias raplate
select 0
use h:\dtp\dbf\radtr index h:\dtp\dbf\radtr1.ntx, h:\dtp\dbf\radtr2.ntx alias radtr
go top
do while .not. eof ()
   if radtr->faction <> "6" .or. empty(radtr->fvin) .or. empty(radtr->fplate)
      skip
      loop
   endif
   ? radtr->fplate
   select raplate
   seek radtr->fplate
   if .not. eof()
      if raplate->frenewal > date()
         select radtr
         delete
      endif
   endif
   select radtr
   skip
enddo
close all

