      REAL FUNCTION pion_tracker(htype)
*********************************************************
*                                                       *
* This file was generated by HUWFUN.                    *
* ntu/uwfunc                                            *
*********************************************************
*
*     Ntuple Id:      10   
*     Ntuple Title:   SANE Ntuple
*     Creation:       21/11/2013 15.21.37
*
*********************************************************
	Real pi0_e
        integer htype,hid,i
        logical cer_fire
	include ?

        cer_fire = .false.
        do i=1,nclust
           if (cer_h(i).gt.0) cer_fire=.true.
           if ( i .eq. 1) call hf1(htype+7,float(cer_h(i)),1.)
        enddo
        if (cer_fire ) then 
           call hf1(htype+4,float(nclust),1.)
           call hf1(htype+5,tht*57.3,1.)
           call hf1(htype+6,phi,1.)
        endif
        if (nclust.eq.1) then
           if(cer_fire.and.E_r(1).gt.0.1)then
            call hf1(htype, E_r(1), 1.)
           endif
        endif

        if (nclust.eq.2.) then
         if(cer_h(1).gt.0.and.cer_h(2).gt.0.and.
     ,	   E_r(1).gt.0.6.and.E_r(2).gt.0.6)then
           call hf1(htype+1, E_r(1), 1.)
           call hf1(htype+2, E_r(2), 1.)
        endif
       endif

      pion_tracker = 1.
*
      END