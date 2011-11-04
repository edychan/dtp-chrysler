*************************************************************************
* tac - travel agent maint
*************************************************************************

rest from (gmempath+"taven") additive
do while .t.
   xret1 = f_pushmenu (xoption0)
   if xret1 = 0
      exit
   else
      xoption1 = substr (goption [xret1], 4)
      do case
      case xret1 = 1
      do while .t.
         f_use ("taven")
         f_clrscn ("TRAVEL AGENT FILE MAINTENANCE")
         @ 02, 02 say "TA Number... " get l_fvendno picture "!!!!!!!!!!"
         if f_rd() = 27	.or. empty(l_fvendno)
            exit
         endif
         set exact on
         seek l_fvendno
         set exact off
         if eof()
            f_add ("taven", "taven")
            loop
         else
            tone (500, 9)
            f_popup ("Duplicate TA #. Press Any Key to Retry ...",.t.)
         endif
      enddo
      case xret1 = 2
      do while .t.
         f_use ("taven")
         set filter to
         f_clrscn ("TRAVEL AGENT FILE MAINTENANCE")
         @ 02, 02 say "TA Number... " get l_fvendno picture "!!!!!!!!!!"
         if f_rd ()	= 27 .or. empty (l_fvendno)
            exit
         endif
         set softseek on
         seek l_fvendno
         set softseek off
         if f_pick_f (02, 05, "", "TA Number컴Company컴컴컴컴" ;
               + "컴컴컴컴컴컴컴횾hone컴컴컴�", ;
               "fvendno+[ ]+fcompany+[ ]+fphone")
            f_update ("taven", "taven")
         endif
      enddo
      endcase
   endif
enddo

release all like l_*
close data
