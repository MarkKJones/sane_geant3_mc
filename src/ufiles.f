      SUBROUTINE UFILES

      implicit none

      include 'sane_cwn.inc'
      include 'beta_geom.inc'
      include 'sane_misc.inc'

      CHARACTER*(*) FILNAM
      CHARACTER*26 outfile
      PARAMETER (FILNAM='a1p.dat')
      character*80 arg
      integer istat
      integer iquest
      common /quest/iquest(100)
*
*
      OPEN(UNIT=4,FILE=FILNAM,STATUS='OLD',FORM='FORMATTED')

 
C Check command line inputs
      
      if (iargc().lt.2) then
         write(*,*)iargc()
        stop 'usage: a1p_mc <iter #> <part #>'
      endif

      call getarg(1,arg)
      read(arg,'(i3)') iteration
      call getarg(2,arg)
      read(arg,'(i1)') particle

      call Srand(iteration)
!      write(outfile, '("ntup/a1p_geant.",i3,".",i1,".hbook")') 
      write(outfile, '("a1p_geant.",i3,".",i1,".hbook")') 
     1      iteration,particle

      write(*,*) 'SETTING UP NTUPLE'

      iquest(10) = 65000
!      call hropen(1,'SANE',outfile,'N',1024,ISTAT)
      call hropen(1,'SANE',outfile,'NQ',1024,ISTAT)
      call hbnt(nt_geant,'SANE Ntuple',' ')
c      call hbname(nt_geant,'ENCOR',ipartc,
c     ,     'ipart[1,10]:I,EE:R,'
c     ,   //' Xv:R,Yv:R,'
c     ,   //'nnca[1,25]:I,nnc[1,25]:I,Etot:R,'
c     ,   //'Etot9:R ,Emax:R,'
c     ,   //'ixmax[1,32]:I,iymax[1,56]:I,'
c     ,   //'iE3pp[1,20]:I,iE3pm[1,20]:I,'
c     ,   //'iE3mp[1,20]:I,iE3mm[1,20]:I')
  
c      call hbname(nt_geant,'ENCOR1',ipartc1,
c     ,     'ipartc1[1,10]:I,Ec1:R, Xv1:R,Yv1,'
c     ,     //'ixmax[1,32]:I,iymax[1,56]:I,eyx(5,5):R')
      call hbname(nt_geant,'THROW',cwn_E,
     1            'E:R'
     1          //',p:R'
     1          //',xsn:R'
     1          //',ratrad:R'
     1          //',xsngp:R'
     1          //',xsnep:R'
     1          //',stopvol:R'
     1          //',tht:R'
     1          //',phi:R'
     1          //',u(6):R'
     1          //',part:R'
     1          //',z:R'
     1          //',n:R'
     1          //',SRx:R'
     1          //',SRy:R'
     1          //',normrate:R'
     1          //',dedl(6):R'
c$$$     1          //',F1tot:R'
c$$$     1          //',Rtot:R'
     1                             )

      call hbname(nt_geant,'RECON',nclust,
     1       'nclust[1,5]:I'
     1     //',ncell(nclust):I'
     1     //',E_m(nclust):R'
     1     //',E_r(nclust):R'
     1     //',th_r(nclust):R'
     1     //',ph_r(nclust):R'
     1     //',x_r(nclust):R'
     1     //',y_r(nclust):R'
     1     //',z_r(nclust):R'
     1     //',Q2_r(nclust):R'
     1     //',xb_r(nclust):R'
     1     //',W_r(nclust):R'
     1     //',x_ur(nclust):R'
     1     //',y_ur(nclust):R'
     1     //',z_ur(nclust):R'
     1     //',th_ucr(nclust):R'
     1     //',ph_ucr(nclust):R'
     1     //',cer_phot(nclust):I'
     1     //',cer_npe(nclust):I'
     1                              )
      
      call hbname(nt_geant,'BLOCKS',cwn_nb,
     1          'nb[0,2000]:I'
     1          //',bx(nb)[0,100]:I'
     1          //',by(nb)[0,100]:I'
     1          //',bg(nb):I'
     1          //',be(nb):R'
     1                             )

      call hbname(nt_geant,'PART',cwn_p_ng,
     1  'p_ng[0,20]:I'
     1  //',p_ne[0,20]:I'
     1  //',p_np[0,20]:I'
     1  //',p_gx(p_ng):R'
     1  //',p_gy(p_ng):R'
     1  //',p_ge(p_ng):R'
     1  //',p_ex(p_ne):R'
     1  //',p_ey(p_ne):R'
     1  //',p_ee(p_ne):R'
     1  //',p_px(p_np):R'
     1  //',p_py(p_np):R'
     1  //',p_pe(p_np):R'
     1  )

      if ( 1 .eq. -1) then
            
      call hbname(nt_geant,'CERENKOV',cwn_cergood,
     1  'cergood[0,1000]:I'
     1  //',cg_x(cergood):R'
     1  //',cg_y(cergood):R'
     1  //',cg_xp(cergood):R'
     1  //',cg_yp(cergood):R'
     1  )


c$$$      
c$$$C     ---   Hodoscopes - JDM - 6/20/07
c$$$      
c$$$      call hbname(nt_geant,'TRACKY1',itrackY1Hit,
c$$$     1  'itrackY1Hit[0,25]:I'
c$$$     1  //',itrackY1Bar(itracky1hit):I'
c$$$     1  //',y1Track(itracky1hit):R'
c$$$     1  //',zY1Track(itracky1hit):R'
c$$$     1  //',timeY1Track(itracky1hit):R'
c$$$     1  //',tdcY1Track(itracky1hit):R'
c$$$     1  )  
      call hbname(nt_geant,'TRACKY2',itrackY2Hit,
     1  'itrackY2Hit[0,25]:I'
     1  //',itrackY2Bar(itracky2hit):I'
     1  //',y2Track(itracky2hit):R'
     1  //',zY2Track(itracky2hit):R'
     1  //',timeY2Track(itracky2hit):R'
     1  //',tdcY2Track(itracky2hit):R'
     1  )  
      call hbname(nt_geant,'TRACKX1',itrackX1Hit,
     1  'itrackX1Hit[0,25]:I'
     1  //',itrackX1Bar(itrackX1hit):I'
     1  //',X1Track(itrackX1hit):R'
     1  //',zX1Track(itrackX1hit):R'
     1  //',pTrack(itrackX1hit):R'
     1  //',timeX1Track(itrackX1hit):R'
     1  //',tdcX1Track(itrackX1hit):R'
     1  )  


      call hbname(nt_geant,'HODO',ihodoHit,
     1  'ihodohit[0,25]:I'
     1  //',ihodoLuc(ihodohit):I'
     1  //',pHodo(ihodohit):R'
     1  //',cxHodo(ihodohit):R'
     1  //',cyHodo(ihodohit):R'
     1  //',czHodo(ihodohit):R'
     1  //',xHodo(ihodohit):R'
     1  //',yHodo(ihodohit):R'
     1  //',zHodo(ihodohit):R'
     1  //',timeHodo(ihodohit):R'
     1  //',eLosHodo(ihodohit):R'
     1  //',tdcHLeft(ihodohit):R'
     1  //',tdcHRigh(ihodohit):R'
     1  //',adcHLeft(ihodohit):R'
     1  //',adcHRigh(ihodohit):R')
CC ---- End  hodoscopes

      endif



      return
      END
