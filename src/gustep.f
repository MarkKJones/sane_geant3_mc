*CMZ :          30/08/94  14.16.48  by  S.Ravndal
*-- Author :
      SUBROUTINE GUSTEP

      implicit none

C.
C.    *
C.    *       User routine called at the end of each tracking step
C.    *       INWVOL is different from 0 when the track has reached
C.    *              a volume boundary
C.    *       ISTOP is different from 0 if the track has stopped
C.    *
C.


      include 'beta_geom.inc'
      include 'geant.inc'
      include 'sane.inc'
      include 'sane_misc.inc'
      include 'materials.inc'
      include 'sane_cwn.inc' 
      save
      integer*2 i,npesum
      integer mkj
      integer ihhit
      integer ig,ip
      character*4 chcase
c      write(*,*)'gustep 1'
C
C Ngkin is > 0 if secondary particle have been generated,
C store them to the particle stak
C

*      if (lvolum(nlevel).ge.9.and.lvolum(nlevel).le.18.and.ipart.ge.2.and.ipart.le.3.and.getot.lt.0.010) then
*        istop=2
*        write(*,*) 'stopping',getot
*      endif
      if(xsn.eq.0)goto 1102
      
      CALL UHTOC(KCASE,4,CHCASE,4)
c      write(*,*)CHCASE
      IF(CHCASE.EQ.'HADR') THEN
           DO 10 IG=1, NGKINE
              IP = GKIN(5,IG)
c                 write(*,*)CHCASE,ip
              IF(IP.EQ.4) THEN
*---  Discard neutrinos
                 IFLGK(IG)=-1
              ELSEIF(IP.EQ.14) THEN
*---  Save protons in JKINE and transport them
                 IFLGK(IG)=1
               ELSEIF(IP.EQ.9) THEN
*---  Save pi- in JKINE and transport them
                 IFLGK(IG)=1
               ELSEIF(IP.EQ.8) THEN
*---  Save pi+ in JKINE and transport them
                 IFLGK(IG)=1
               ELSEIF(IP.EQ.7) THEN
*---  Save pi0 in JKINE and transport them
                 IFLGK(IG)=1
           ELSE
*---  Simply transport the rest
               IFLGK(IG)=0
            ENDIF
 10      CONTINUE
      ENDIF
      

*---  Perfrom action on all the particles
c      write(*,*)'NGKINE = ',NGKINE,' IP = ',IP
      IF (NGKINE.GT.0) THEN
c      write(*,*)NGKINE,IP,IG
c      write(*,*)'NGKINE gt 0 ',NGKINE,' IP = ',IP
        CALL GSKING(0)
      ENDIF
c      write(*,*)'gustep 1.1'

*      if (ipart.eq.3.and.vect(3).gt.130. .and.vect(3).lt.150.) 
*     1 write(*,*) 'tracking',numed,lvolum(nlevel),nlevel,inwvol,ipart,vect(3)

*      if (getot.gt.0.1.and.vect(3).gt.130.) then
*     write(*,'(f5.3,1x,6f9.3,6i3)') getot,(vect(j),j=1,6),inwvol,nlevel
C     ,lvolu m(nlevel),numed,ingoto,ipart
*      endif

*      IP = GKIN(5,)

      if (lvolum(nlevel).eq.vol_gain.and.inwvol.eq.2) then ! exiting Gain Sheet
*        write(*,*) 'leaving Gain Sheet',ipart,vect
       if ((ipart.ge.1 .and. ipart.le.3) .and.getot.gt.0.020 .and.
     2      vect(6).gt.0.5 ) then
            num_gc = num_gc + 1
            gc_x(num_gc) = vect(1)
            gc_y(num_gc) = vect(2)
            gc_eng(num_gc) = getot
            gc_part(num_gc) = ipart
*            write(*,*) 'Recorded Particle leaving Gain Sheet',ipart,getot
        endif
      endif
c      write(*,*)'gustep 1.2'

c
C    Front hodoscope tracking code - JDM - 06/20/07 
c
      call step_cal(IP)
      if (getot.gt.0.020 .and.vect(6).gt.0.5 ) then
c
c     Check tracker and Lucite
c
         
         call step_tracker()
         call step_lucite()
       
      endif

c      write(*,*)'gustep 1.3'
c
C     --- Begin magnet tracking code - JDM  5/7/07
c
      if ((lvolum(nlevel).eq.vol_magn).and.(inwvol.eq.1)) then ! Entering magnet
*        write(*,*) 'Entering Magnet',ipart,vect
        istop=2                 ! Kill trace
        magcnt = magcnt + 1
C        write(*,*) 'incremented to ', magcnt
      else

      endif

C --- End magnet tracking code - JDM

      if (numed.eq.NMED_LG) then
C        write(*,*) 'gustep :',number(nlevel),number(nlevel-1),destep
        dEBloc(NUMBER(NLEVEL),NUMBER(NLEVEL-1)) = 
     1    dEBloc(NUMBER(NLEVEL),NUMBER(NLEVEL-1)) + DESTEP
        photBloc(NUMBER(NLEVEL),NUMBER(NLEVEL-1)) = 
     1    photBloc(NUMBER(NLEVEL),NUMBER(NLEVEL-1)) + ngphot
      endif


      if (DESTEP.GT.0.) then
        if     (numed.eq.NMED_LG) then
           ELoss(4) = ELoss(4) + destep
        elseif (lvolum(nlevel).eq.vol_hodo) then
           ELoss(2) = ELoss(2) + destep
        elseif (lvolum(nlevel).eq.vol_gain) then
           Eloss(3) = ELoss(3) + destep
        elseif (lvolum(nlevel).eq.vol_veto) then
           Eloss(5) = ELoss(5) + destep
        elseif (lvolum(nlevel).eq.vol_cgas) then
           Eloss(1) = ELoss(1) + destep
        else
           ELoss(6) = ELoss(6) + destep
        endif



*        write(*,'("ELOSS: ",i3,7f6.1)') lvolum(nlevel),(Eloss(i)*1000.,i=1,6),destep*1000.
      endif
c      if ( ipart .eq. 3 .and. lvolum(nlevel).eq.vol_cgas) then
c      write(*,*) ' vol_cgas = ',lvolum(nlevel),vol_cgas,ngphot,ipart,getot
c      read(*,*) mkj
c      endif
c      if (lvolum(nlevel).eq. 35 .or. lvolum(nlevel).eq. 34 .or. lvolum(nlevel).eq. 33) then
c         write(*,*) ' tracker destep = ',destep,lvolum(nlevel),ipart,getot
c         endif
      if (lvolum(nlevel).eq.vol_cgas.and.ngphot.gt.0) then
        photCer = photCer + ngphot
c        write(*,*) 'call ltrace Cerenkov photons:',ngphot
        call ltrace(npesum)
        photGood = photGood + npesum
*        write(*,'("vect = ",7f10.4)') vect
*        write(*,*) '--------------------------'
      endif

C
C
C Store the current step position in the JXYZ data structure
C if the DEBUG data card has been set to ON
C
c      write(*,*)'gustep 1.4'
       IF (IDEBUG.NE.0) THEN
         CALL GSXYZ
         IF (ISWIT(2).EQ.1) THEN
            CALL GPGKIN
            CALL GPCXYZ
         ENDIF
        if(ISWIT(2).EQ.6)then
            CALL GDCXYZ  ! Plot everything
            CALL GSXYZ

         endif
          CALL GDEBUG
       ENDIF
 1102  continue
c      write(*,*)'gustep 1.5'
C
      END
















