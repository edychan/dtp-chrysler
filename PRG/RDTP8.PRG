* 
* SOS Renewal Module
*
do while .t.
   xret2 = f_pushmenu (xoption1)
   if xret2 = 0
      exit
   endif
   xoption2 = substr (goption [xret2], 4)
   do case
      case xret2 = 1     
         do rdtp8a        && process renewal
      case xret2 = 2
         do rdtp8b        && print envelope
      case xret2 = 3
         do rdtp8c        && view exception
   endcase
enddo
close database


