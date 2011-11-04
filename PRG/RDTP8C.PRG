*
* view exception
*
* changes.
*
private yfil
f_clrscn ("DTP/MI - View Exception Renewals")

yfil = gdbfpath + "renewal.dbf"
if .not. file (yfil)
   f_valid (.f., "Missing Renewal File...")
   return
endif

yfil = gdbfpath+"renewal"
select 0
use &yfil index &yfil alias renewal
if f_confirm ("Do you want to view Exception only [Y/N] ","YN") = [Y]
   set filter to .not.empty(fcontrol) .or. freg <= 0
endif

go top
if eof ()
   f_valid (.f., "No Exception Error...")
else
   f_pick_f (2, 10, "", "", "fvin+[     ]+" +   ;
         "fplate+[    ]+str(freg,6,2)+[    ]+fcontrol")
endif

close data
