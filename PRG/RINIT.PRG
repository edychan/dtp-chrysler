* ===========================================================================
* Initialize system variables
* changes:
* 02.22.99: gcompath ::= communication path
* 03.31.99: gexpmo ::= expiration month for passenger vehicles (04, 08, 09)
* 09.29.99: gvmspath ::= pathname for vmsdata.dbf for renewal purpose
*                        (defined in rasys.dbf)
* ===========================================================================
public gstation, gdbfpath, gmempath, gstnpath, gmsgpath, gcompath, gexpmo
public gsup, gvmspath
public gdbffiles, gstnfiles, gcclog, gusrid, gusrgp, gcntl
public gmesscolor, gbluecolor, gblueget, gmenuback, glgincolor
public gsubcolor, gsubget, gpickfore, gpickback, gredcolor
public gmenulevel, goption [9], gopts
public gmenupk [4], gmenuscn [4], gscrsav [81], gboxsav [10]
public gmcol1 [4], gmcol2 [4], gbluecolor, gredcolor, gmucolor

set exclusive off
set delete on
set exact off
set confirm on
set scoreboard off
set cursor off

set key 28 to
set key -1 to
set key -2 to

if empty (gete ("RACPTH"))
   xpath = "\"
else
   xpath = alltrim (upper (gete ("RACPTH")))
endif

if empty (gete ("RACSID"))
   gstation = "STN00"      && must be capital letter
else
   gstation = alltrim (upper (gete ("RACSID")))
endif
gdbfpath = "H:" + xpath + "DTP\DBF\"
gstnpath = "H:" + xpath + "DTP\STN\" + gstation + "\"
gmempath = "H:" + xpath + "DTP\MEM\"
gmsgpath = "H:" + xpath + "DTP\MAIL\"
gcompath = "H:" + xpath + "DTP\COMM\"

if gete ("RACCOLOR") = "Y"
   gmucolor = .t.
elseif gete ("RACCOLOR") = "N"
   gmucolor = .f.
else
   gmucolor = iscolor ()
endif

use (gdbfpath + "RASYS")
go top
if gmucolor
   gmesscolor = fmesscolor
   gbluecolor = fbluecolor
   gblueget = substr (fbluecolor, at (",", fbluecolor) + 1)
   gmenuback = fmenuback
   glgincolor = flgincolor
   gsubcolor = fsubcolor
   gsubget = substr (fsubcolor, at (",", fsubcolor) + 1)
   gpickfore = fpickcolor
   gpickback = substr (fpickcolor, at (",", fpickcolor) + 1)
   gredcolor = fredcolor
else
   gmesscolor = "w/n"
   gbluecolor = "w/n, n/w"
   gblueget = "n/w"
   gmenuback = "/n"
   glgincolor = "w/n, w/n"
   gsubcolor = "n/w, w/n"
   gsubget = "w/n"
   gpickfore = "w/n"
   gpickback = "n/w"
   gredcolor = "n/w, n/w"
endif
gvmspath = alltrim(fdbfpath)    && 09.29.99
afill (gboxsav, .f.)
afill (gscrsav, .f.)

gmenulevel = 0

setcolor (gmenuback)
clear
@ 23, 00 say replicate (chr (196), 80)

save screen to gmenuscn [1]
f_popup ("Please Wait While Initializing...")

afill (gmenupk, 0)
if gmucolor
   n = at (",", fmcol1)
   gmcol1 [1] = substr (fmcol1, 1, n - 1)
   gmcol2 [1] = substr (fmcol1, n + 1)
   n = at (",", fmcol2)
   gmcol1 [2] = substr (fmcol2, 1, n - 1)
   gmcol2 [2] = substr (fmcol2, n + 1)
   n = at (",", fmcol3)
   gmcol1 [3] = substr (fmcol3, 1, n - 1)
   gmcol2 [3] = substr (fmcol3, n + 1)
   n = at (",", fmcol4)
   gmcol1 [4] = substr (fmcol4, 1, n - 1)
   gmcol2 [4] = substr (fmcol4, n + 1)
else
   afill (gmcol1, "/W")
   afill (gmcol2, "W/N")
endif

use

public gloc, gserver, gmaxusr, glgouttme
public gtitle, glocname, gpostdate, gprint1, gprint2

restore from (gmempath + "RAPATH") additive
f_use ("RASTN")
seek f_truncate (gstation, 8)
if .not. found ()
   append blank
   restore from (gmempath + "RASTN") additive
   l_fstn = gstation
   f_replace ()
endif
gloc = upper(alltrim(floc))
gprint1 = if(empty(fprint1), "LPT1", fprint1) 
gprint2 = if(empty(fprint2), "LPT1", fprint2) 

use

f_use ("RASYS")
go top
gsup = .f.           && edc 
gmaxusr = fmaxusr
glgouttme = flgouttme
gtitle = alltrim (ftitle)
gpostdate = fpostdate
gexpmo = fexpmo      && 03.31.99: expiration month
use

f_popback ()


******************************
procedure cleanup

quit
