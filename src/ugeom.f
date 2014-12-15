      SUBROUTINE UGEOM

      implicit none
      
      include 'constants.inc'
      include 'geant.inc'
      include 'sane.inc'
      include 'materials.inc'
      include 'beta_geom.inc'
      include 'sane_misc.inc'
      include 'sane_accp.inc'



C
C Define user geometry set up
C
 
      real*4 PAR( 8)
      real*4 ZLG(5),ALG(5),WLG(5)
      real*4 ZKap(4),AKap(4),WKap(4)
      real*4 ZScin(2), AScin(2), WScin(2)
      real*4 ZLuc(2), ALuc(2), WLuc(2)
      real*4 ZNH3(3),ANH3(3),WNH3(3),DNH3(3) 
      real*4 ZKelF(3),AKelF(3),WKelF(3)
      real*4 ZCarb(1),ACarb(1),DCarb(1)
      real*4 ZLHe(1),ALHe(1),DLHe(1)
      real*4 ZALU(1),AALU(1),DALU(1)
      real*4 x,y
      real tpar(5)
      data tpar/0.001, 0.001, 0.01, 0.01, 0.01/



C
C Lead glass mixture parameters nucleus charge, atomic wheight, rel. wheight
C of the different compounds
C
      DATA ALG/ 207.19,  15.999, 28.086, 39.098, 74.922/
      DATA ZLG/  82.00,   8.00,  14.00,  19.00,  33.00/
      DATA WLG/    .475,   .270,   .193,   .058,   .004/
C
C Scintillator
C
      DATA AScin/1.00794,12.0107/
      DATA ZScin/1.,6.0/
      DATA WScin/0.0848,0.9152/
C
C Lucite
C
      DATA ALuc/1.00794,12.0107/
      DATA ZLuc/1.,6.0/
      DATA WLuc/0.1435,0.8565/
C
C Kapton 
C
      DATA ZKap/ 1.000,   6.000,   7.000,  8.000/
      DATA AKap/ 1.008,  12.011,  14.007, 15.999/
      DATA WKap/ 0.0264,  0.6911, 0.0733,  0.2092/
C
C Kel-F: (Poly)ChloroTriFluoroEthylene (Cl F3 C2)
C
      DATA ZKelF/ 6.000,   9.000, 17.000/  
      DATA AKelF/12.011,  18.998, 35.453/ 
      DATA WKelF/ 0.2063,  0.4893, 0.3044/
c
c     TF-1 optical parameters. Needed to define Cherenkov light generating vol.
c

      real*4 refrind,wlmn,wlmx,hc,pphmn,pphmx
      parameter (refrind=1.65)        !TF-1 refractive index.
      parameter (wlmn=280.,wlmx=630.) !PMT XP3462B sensitivity range, [nm].
      parameter (hc=1.239842442E-6)   !h*c, [GeV*nm].
      parameter (pphmn=hc/wlmx,pphmx=hc/wlmn)

C
C     NH3 target.  Assume 50% packing uncertainty
C
      real*4 WHyd,WHel,WNit 

      DATA ZNH3/1.000,  2.000,  7.000/
      DATA ANH3/1.008,  4.003, 14.000/
      DATA DNH3/0.153,  0.145,  0.714/ !! Multiply by pf for lumin NK 03/01/11 
c      DATA WNH3/0.151,  0.145,  0.704/ ! OR 2/10
!      DATA WNH3/0.153,  0.145,  0.714/ ! does not add up to 1. Where did it come from?
C! Read in NH3 densities for varying packing fractions NK 03/31/10

C     Carbon target.
C
      DATA ZCARB/ 6.000/
      DATA ACARB/12.010/
      DATA DCARB/ 2.265/

C     Oustide LHe.
C
      DATA ZLHE/ 2.000/
      DATA ALHE/4.000/
      DATA DLHE/ 0.145/

C     Oustide Aluminum Caps
C
      DATA ZALU/ 13.000/
      DATA AALU/27.000/
      DATA DALU/ 2.7/

c     Cher. photon min., max. momentums, [GeV/c].

      real pph(2)               !Cher. photon min & max momentums (GeV/c).
      real absl(2)              !TF-1 absorption length.
      real qef(2)               !PMT quantum eff.
      real rind(2)              !TF-1 refr. index.
      data pph/pphmn,pphmx/,absl/2*100./,qef/2*1./,rind/2*refrind/

      real pph_n2(2)               !Cher. photon min & max momentums (GeV/c).
      real absl_n2(2)              !TF-1 absorption length.
      real qef_n2(2)               !PMT quantum eff.
      real rind_n2(2)              !TF-1 refr. index.
      data pph_n2/pphmn,pphmx/,absl_n2/2*100./,qef_n2/2*1./
     ,     ,rind_n2/2*1.000298/

      real*4 fieldmax,tmax_fd,ste_max,dee_max,epsilon,st_min,fieldmax2
      integer*4 i_field,i_field2

      real*4 cer_back,earm_length,z0
      real*4 front_drift,back_drift,guard_angle,guard_horz,wall_horz
      integer*4 imt,ivol,ilum,jlum
      real*4 rotmf
c     LUCITE PARAMETERS
c     
c
      real*4 inRadL,ouRadL,hightL,phiMinL,phiMaxL
      
      real*4 parL(5)

      parameter (inRadL = 240.0,ouRadL = 243.5, hightL=6.0, 
     ,     phiMinL = -11.5, phiMaxL = 11.5)

C! Read in NH3 densities for varying packing-fractions NK 03/31/10
      WNH3(1)=Hyddens
      WNH3(2)=Heldens
      WNH3(3)=Nitdens
C
C! Get pf-scaled densities and atomic numbers for calculating luminosity
      pfdens(1) = Pckfrc * 0.867 ! 0.867 density of 14NH3 H
      pfdens(2) = (1-Pckfrc) * DNH3(2) ! He
      pfdens(3) = Pckfrc * 0.867 ! 0.867 density of 14NH3 N
      pfdens(4) = DLHE(1) ! He
      pfdens(5) = DALU(1) !Al
      pfdens(6) = DCARB(1) !C
cc      pfdens(4) = DCARB(1)

c      atomnum(1) = ACARB(1)
      atomnum(1) = ANH3(1)
      atomnum(2) = ANH3(2)
      atomnum(3) = ANH3(3)
      atomnum(4) = ALHE(1)
      atomnum(5) = AALU(1)
      atomnum(6) = ACARB(1)
cc      atomnum(4) = ACARB(1)

      write(*,*)pfdens,atomnum
C Rotation of the coils
      rotmf = 180. + theta_0 + theta_Bfield
      write(*,*)'Start UGEOM'

C
C
C Definition of 16 default Geant materials, see manual CONS100-1
C
      CALL GIDROP
      CALL GMATE
C
C Define the default particles
C
      CALL GPART
      CALL GPIONS
C
C Defines USER particular materials
C

      CALL GSMIXT(22,'LEAD GLASS$',ALG,ZLG,3.86,5,WLG)
      CALL GSMIXT(23,'SCINTILLATOR$',AScin,ZScin,1.03,2,WScin)
      CALL GSMIXT(24,'KAPTON$',AKap,ZKap,1.42,4,WKap)
      CALL GSMIXT(26,'LUCITE$',ALuc,ZLuc,1.18,2,WLuc)
C      CALL GSMIXT(27,'NH3$',ANH3,ZNH3,0.5782,3,WNH3)
      CALL GSMIXT(27,'NH3$',ANH3,ZNH3,Effdens,3,WNH3)
      CALL GSMIXT(28,'KELF$',AKelF,ZKelF,2.39,3,WKelF)

      CALL GSMATE(25,'N2 GAS$',14.007,7.0,0.001165,32623.,0.,0,0)
      CALL GSMATE(29,'He 1K',4.0,2.0,0.145,650.5,0.,0,0)
C      CALL GSMATE(29,'He 1K',4.0,2.0,Heldens,650.5,0.,0,0)

c      write(*,*)Effdens,Heldens

      call init_lucite()
      call init_tracker()
      call init_cal()
C
C Defines USER tracking media parameters which describes the tracking
C throughout a material
C
      FIELDMAX =  0.
      I_FIELD =  0
      TMAX_FD =  10.
      STE_MAX =  -1000.
      DEE_MAX =  -0.05
      EPSILON  =  0.001
      ST_MIN  =  -0.001
      
      write(*,*)"FIELD TYPE IS=",field_type
      if (field_type.eq.0) then
        write(*,*) '***** Bypassing field code'
        I_FIELD2 = 0
        FIELDMAX2 = 0.
      else if (field_type.eq.1) then
        I_FIELD2 = 1
        FIELDMAX2 = 50.
        write(*,*) '***** Using field code',I_FIELD2
      else
        write(*,*) target_type,field_type
        STOP 'BAD FTYP (field_type)'
      endif
C
C Define two tracking media, first consists of Air, the second of
C either BGO or Lead Glass, depending on the IMAT value.
C
c      igauto = 0
      write(*,*)'Define Medium ',igauto


      CALL GSTMED( NMED_air,'AIR'                  , 15 , 0 , I_FIELD2,
     +     FIELDMAX2,TMAX_FD,STE_MAX,DEE_MAX, EPSILON, ST_MIN, 0 , 0 )
      CALL GSTMED( NMED_Pb,'Pb-Shielding'          , 13 , 0 , I_FIELD2,
     +     FIELDMAX2,TMAX_FD,STE_MAX,DEE_MAX, EPSILON, ST_MIN, 0 , 0 )
c      CALL GSTMED( NMED_LG,'Pb-Glass'              , 22 , 0 , I_FIELD2,
c     +     FIELDMAX2,TMAX_FD,STE_MAX,DEE_MAX, EPSILON, ST_MIN, 0 , 0 )
      CALL GSTMED( NMED_Sc,'Scintillator'          , 23 , 0 , I_FIELD2,
     +     FIELDMAX2,TMAX_FD,STE_MAX,DEE_MAX, EPSILON, ST_MIN, 0 , 0 )
      CALL GSTMED( NMED_Kap,'Kapton'               , 24 , 0 , I_FIELD2,
     +     FIELDMAX2,TMAX_FD,STE_MAX,DEE_MAX, EPSILON, ST_MIN, 0 , 0 )
      CALL GSTMED( NMED_N2,'N2 Gas'                , 25 , 0 , I_FIELD2,
     +     FIELDMAX2,TMAX_FD,STE_MAX,DEE_MAX, EPSILON, ST_MIN, 0 , 0 )
c      CALL GSTMED( NMED_PLG,'Pb-Glass'              , 22 , 0 , I_FIELD,
c     +     FIELDMAX,TMAX_FD,STE_MAX,DEE_MAX, EPSILON, ST_MIN, 0 , 0 )
      CALL GSTMED( NMED_Gain, 'Lucite Gain Monitor'   , 26 , 0 , I_FIELD2,
     +     FIELDMAX2,TMAX_FD,STE_MAX,DEE_MAX, EPSILON, ST_MIN, 0 , 0 )
      CALL GSTMED( NMED_NH3, 'NH3 + Helium'         , 27 , 0 , I_FIELD2,
     +     FIELDMAX2,TMAX_FD,0.1,DEE_MAX, EPSILON, ST_MIN, 0 , 0 )
      CALL GSTMED( NMED_Vac, 'Vacuum'               , 16 , 0 , I_FIELD2,
     +     FIELDMAX2,TMAX_FD,1.0,DEE_MAX, EPSILON, ST_MIN, 0 , 0 )
      CALL GSTMED( NMED_Al,  'Aluminum'             , 9 , 0 , I_FIELD2,
     +     FIELDMAX2,TMAX_FD,STE_MAX,DEE_MAX, EPSILON, ST_MIN, 0 , 0 )
      CALL GSTMED( NMED_KelF,'Kel-F'             , 28 , 0 , I_FIELD2,
     +     FIELDMAX2,TMAX_FD,STE_MAX,DEE_MAX, EPSILON, ST_MIN, 0 , 0 )
      CALL GSTMED( NMED_LHe,'LHe 1K'             , 29 , 0 , I_FIELD2,
     +     FIELDMAX2,TMAX_FD,0.1,DEE_MAX, EPSILON, ST_MIN, 0 , 0
     +  )



      CALL GSTMED( NMED_Fe,'Iron'             , 10 , 0 , I_FIELD2,
     +     FIELDMAX2,TMAX_FD,STE_MAX,DEE_MAX, EPSILON, ST_MIN, 0 , 0 )
C Initial take on Iron, recheck for accuracy. JDM 7/9/07
      CALL GSTMED( NMED_C,'Carbon'             , 6 , 0 , I_FIELD2,
     +     FIELDMAX2,TMAX_FD,STE_MAX,DEE_MAX, EPSILON, ST_MIN, 0 , 0 )
C Initial take on Carbon, recheck for accuracy. JDM 7/27/07


C All the default material defined via GMATE are also defined as
C tracking media, even if they are not needed right now.
C
      DO 100 IMT= 1,14
         CALL GSTMED( IMT+13,'DUMMY-MEDIUM'    , IMT , 0 , I_FIELD2,
     +                FIELDMAX2,TMAX_FD,STE_MAX,DEE_MAX, EPSILON, ST_MIN, 0 , 0 )
  100 CONTINUE
C
C
C
      call GSTPAR(NMED_N2,'LOSS',1.)
      call GSTPAR(NMED_N2,'DRAY',1.)
      call GSTPAR(NMED_N2,'DCUTE',0.00001)
      call GSTPAR(NMED_N2,'DCUTM',0.00001)
c      call SetMedPar(NMED_Luc,tpar)
c      call SetMedPar(NMED_Fx1,tpar)
c      call SetMedPar(NMED_Fy1,tpar)
c      call SetMedPar(NMED_Fy2,tpar)

c      call gstpar(NMED_Luc,'CUTGAM',0.00001)
c      call gstpar(NMED_Luc,'CUTELE',0.00001)
c      call gstpar(NMED_Luc,'CUTNEU',0.00001)
c      call gstpar(NMED_Luc,'CUTHAD',0.00001)
c      call gstpar(NMED_Luc,'CUTMUO',0.00001)

c$$$      call GSTPAR(NMED_NH3,'LOSS',1)
c$$$      call GSTPAR(NMED_NH3,'DRAY',1)
c$$$      call GSTPAR(NMED_NH3,'DCUTE',0.02)
c$$$      call GSTPAR(NMED_NH3,'DCUTM',0.02)
c$$$      call GDRPRT(8,NMED_NH3,1.,90)

*      call GSTPAR(NMED_Al,'LOSS',1)
*      call GSTPAR(NMED_Al,'DRAY',1)
*      call GSTPAR(NMED_Al,'DCUTE',0.02)
*      call GSTPAR(NMED_Al,'DCUTM',0.02)
*      call GDRPRT(8,NMED_Al,1.,90)

C
      CALL GSCKOV(NMED_LG,2,pph,absl,qef,rind)
      CALL GSCKOV(NMED_N2,2,pph_n2,absl_n2,qef_n2,rind_n2)

C
C Energy loss and cross-sections initialisations, creating LUT banks
C
      CALL GPHYSI
      write(*,*) 'CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC'
      write(*,*) 'DELTA RAY INFO'
      write(*,*) 'CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC'
      call GDRPRT(8,25,150.,9)
      write(*,*) 'CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC'
C
C Define the reference volume ECAL via Geant Store VOLUme routine
C


      cer_back = cer_drift+cer_length
*      cer_win_thk = 0.0127

      earm_length = cal_drift/2.+cal_depth/2.

      PAR(1) = cal_width/2.*1.2
      PAR(2) = cal_height/2.*1.2
      PAR(3) = earm_length+50
      write(*,*) ' earm =',par
      CALL GSVOLU( 'EARM' , 'BOX ' ,NMED_Air, PAR , 3 , IVOL ) ! vol 1


c
c     Implement Lucite Hodoscope into detector
c      
      call ugeom_lucite(ivol) 

      call  ugeom_tracker(ivol)

      call def_calspace(ivol)
      call ugeom_cal(ivol)


      PAR(1) = cal_width/2.
      PAR(2) = cal_height/2.
      PAR(3) = .3
C      CALL GSVOLU( 'FESH' , 'BOX ' ,NMED_Fe, PAR , 3 , IVOL )

      PAR(1) = cal_width/2.
      PAR(2) = cal_height/2.
      PAR(3) = gain_thk
      CALL GSVOLU( 'GAIN' , 'BOX ' ,NMED_Gain, PAR , 3 , IVOL ) ! vol 3 
      vol_gain = IVOL


      PAR(1) = cal_width/2.*1.2
      PAR(2) = cal_height/2.*1.2
      PAR(3) = 2.
      CALL GSVOLU( 'VETO' , 'BOX ' ,NMED_Sc, PAR , 3 , IVOL )
      vol_veto = ivol

      PAR(1) = cal_width/2.*cer_drift/cal_drift*1.0
      PAR(2) = cal_height/2.*cer_drift/cal_drift*1.0
      PAR(3) = cer_win_thk
*      CALL GSVOLU( 'CFRW' , 'BOX ' ,NMED_Vac, PAR , 3 , IVOL )
      CALL GSVOLU( 'CFRW' , 'BOX ' ,NMED_Kap, PAR , 3 , IVOL )
C
      PAR(1) = cal_width/2.*cer_back/cal_drift*1.0
      PAR(2) = cal_height/2.*cer_back/cal_drift*1.0
      PAR(3) = cer_win_thk
      CALL GSVOLU( 'CBKW' , 'BOX ' ,NMED_Kap, PAR , 3 , IVOL )

      PAR(1) = cal_width/2.*cer_drift/cal_drift*1.0
      PAR(2) = cal_width/2.*cer_back/cal_drift*1.0
      PAR(3) = cal_height/2.*cer_drift/cal_drift*1.0
      PAR(4) = cal_height/2.*cer_back/cal_drift*1.0
      PAR(5) = cer_length/2.
*      CALL GSVOLU( 'CGAS' , 'TRD2' ,NMED_Vac, PAR , 5 , IVOL )
      CALL GSVOLU( 'CGAS' , 'TRD2' ,NMED_N2, PAR , 5 , IVOL )
       write(*,*) ' cgas = ', ivol
      vol_cgas = IVOL
 
      PAR(1) = 2.
      PAR(2) = 2.
c      PAR(3) = 0.05
      PAR(3) = 0.395
      CALL GSVOLU('CRBN','BOX ', NMED_C,PAR,3,IVOL )
cc      thick(4) = 2*PAR(3)
      thick(6) = 2*PAR(3)

      PAR(1) = 0.
      PAR(2) = 1.25
      PAR(3) = 1.5
      CALL GSROTM(1,90.,SNGL(theta_0),0.,SNGL(theta_0),90.,310.)
      CALL GSVOLU( 'CELL' , 'TUBE' ,NMED_NH3, PAR , 3 , IVOL )
c      CALL GSVOLU( 'CELL' , 'TUBE' ,NMED_Pb, PAR , 3 , IVOL )
      thick(1) = 2*PAR(3)
      thick(2) = 2*PAR(3) 
      thick(3) = 2*PAR(3) 

      PAR(1) = 1.251
      PAR(2) = 1.25+0.127
      PAR(3) = 1.5
      CALL GSVOLU( 'CWAL' , 'TUBE' ,NMED_KelF, PAR , 3 , IVOL )

      PAR(1) = 0.
      PAR(2) = 50.043
      PAR(3) = 50.0
      CALL GSROTM(2,90.,0.,0.,0.,90.,270.)
      CALL GSVOLU( 'TCAN' , 'TUBE' ,NMED_Vac, PAR , 3 , IVOL )

      PAR(1) = 50.
      PAR(2) = 50.043
      PAR(3) = 50.0
      CALL GSVOLU( 'TWIN' , 'TUBE' ,NMED_Al, PAR , 3 , IVOL )

      PAR(1) = 4.000-0.001905
      PAR(2) = 4.000+0.001905
      PAR(3) = 30
      CALL GSVOLU( '4KSH' , 'TUBE' ,NMED_Al, PAR , 3 , IVOL )

      PAR(1) = 2.100-0.00254
      PAR(2) = 2.100+0.00254
      PAR(3) = 20
      CALL GSVOLU( 'TAIL' , 'TUBE' ,NMED_Al, PAR , 3 , IVOL )

      PAR(1) = 0.0
      PAR(2) = 2.100+0.00254
      PAR(3) = 20
      CALL GSVOLU( 'NOSE' , 'TUBE' ,NMED_LHe, PAR , 3 , IVOL )

      PAR(1) = 45.000-0.001905
      PAR(2) = 45.000+0.001905
      PAR(3) = 40
      CALL GSVOLU( 'LN2C' , 'TUBE' ,NMED_Al, PAR , 3 , IVOL )

      PAR(1) = 0.
      PAR(2) = 33
C     PAR(3) = 22 
      PAR(3) = 25.   !  JDM
      CALL GSVOLU( 'MAGN' , 'TUBE' ,NMED_Vac, PAR , 3 , IVOL )

      PAR(1) = 10.
      PAR(2) = 5./tan(17.0*0.0174533)
*      PAR(3) = 10./tan(48.5*0.0174533)
      PAR(3) = 5.
      PAR(4) = 25.
      PAR(5) = 65.
      CALL GSVOLU( 'BRA1' , 'TUBS', NMED_Al, PAR, 5 , IVOL)
      PAR(4) = 115.
      PAR(5) = 155.
      CALL GSVOLU( 'BRA2' , 'TUBS', NMED_Al, PAR, 5 , IVOL)
      PAR(4) = 205.
      PAR(5) = 245.
      CALL GSVOLU( 'BRA3' , 'TUBS', NMED_Al, PAR, 5 , IVOL)
      PAR(4) = 295.
      PAR(5) = 335.
      CALL GSVOLU( 'BRA4' , 'TUBS', NMED_Al, PAR, 5 , IVOL)
C
C      PAR(1) = 5.
C      PAR(2) = 10.
C      PAR(3) = 5./tan(17.0*0.0174533)
C      PAR(4) = (5+2.*PAR(1))*tan(48.5*0.0174533)
C      PAR(5) = PAR(4) + PAR(3) - PAR(2)


      PAR(1) = 8.4 
      PAR(2) = 10.
      PAR(3) = 18.
C      PAR(3) = 8.846*tan(73*.0174533)
      PAR(4) = 29. 
C      PAR(4) = 18.3*tan(48.5*.0174533)
      PAR(5) = 37.
C      PAR(5) = 18.3/tan(17*.0174533)
      CALL GSROTM(3,90.,90.,90.,0.,180.,0.)
      CALL GSROTM(4,90.,rotmf,0.,rotmf,90.,270.+rotmf)
      CALL GSVOLU( 'MAG2' , 'CONE', NMED_Al, PAR, 5 , IVOL)
      vol_magn = IVOL



      front_drift = cer_drift-5.
      back_drift  = cer_back
      guard_angle = atan(cal_width/cal_drift/2.0)/d2r
      guard_horz  = cal_width/2.*(cer_drift+cer_length/2.)/cal_drift+0.7
      wall_horz = cal_width/2.*cer_drift/cal_drift+10

      PAR(2) = cal_width*front_drift/cal_drift+5
      PAR(1) = 10.
      PAR(3) = 1
      CALL GSVOLU('WAL2','BOX ',NMED_Pb,PAR,3,IVOL)

      PAR(1) = 0.
      PAR(2) = 4.
      PAR(3) = 1.
      CALL GSVOLU('PLUG','TUBE',NMED_Pb,PAR,3,IVOL)

      PAR(1) = cal_width*front_drift/cal_drift
      PAR(2) = cal_width*back_drift/cal_drift
      PAR(3) = 1.
      PAR(4) = (back_drift-front_drift)/2.
      CALL GSVOLU('GARD','TRD1',NMED_Pb,PAR,4,IVOL)
      
      
      CALL GSROTM(5,90.,90.,90.-guard_angle,180.,guard_angle,0.)
      CALL GSROTM(6,90.,90.,90.+guard_angle,180.,guard_angle,180.)
      
C     
C     Adding Front Tracking Hodoscope - JDM 5/22/07  -  Three planes of
C     bars
C     

c*****************
      
      

C
C Position volumes
C

      z0 = -earm_length

C   Position (general)target related volumes
cmkj

        CALL GSPOS('NOSE',1,'TCAN',0.,0.,TargVrtzOff  , 0,'MANY')
        CALL GSPOS('TAIL',1,'NOSE',0.,0.,TargVrtzOff  , 0,'MANY')
        CALL GSPOS('CWAL',1,'NOSE',0.,0.,TargVrtzOff  , 1,'ONLY')
        CALL GSPOS('LN2C',1,'TCAN',0.,0.,0.  , 0,'ONLY')
        CALL GSPOS('4KSH',1,'TCAN',0.,0.,0.  , 0,'ONLY')
        CALL GSPOS('BRA1',1,'MAGN',0.,0.,0.  , 0,'ONLY')
        CALL GSPOS('BRA2',1,'MAGN',0.,0.,0.  , 0,'ONLY')
        CALL GSPOS('BRA3',1,'MAGN',0.,0.,0.  , 0,'ONLY')
        CALL GSPOS('BRA4',1,'MAGN',0.,0.,0.  , 0,'ONLY')



        CALL GSPOS('MAG2',1,'MAGN',0.,0.,+17.28, 0,'MANY')
        CALL GSPOS('MAG2',2,'MAGN',0.,0.,-17.28, 3,'MANY')
        CALL GSPOS('MAGN',1,'TCAN',0.,0.,0.,   4,' MANY')



       CALL GSPOS('TCAN',1,'EARM',0.,0.,z0, 2,' ONLY')
       CALL GSPOS('TWIN',1,'TCAN',0.,0.,0., 0,'ONLY')
cmkj

C   Position Detectors

      x = 0.
      y = 0.
      CALL GSPOS('CGAS',1,'EARM',x,y,z0+cer_drift+cer_length/2.,0,'ONLY'
     +  )
      CALL GSPOS('CFRW',1,'CGAS',x,y,-cer_length/2., 0,'ONLY')
      CALL GSPOS('CBKW',1,'CGAS',x,y,+cer_length/2.,   0,'ONLY')

C      CALL GSPOS('FESH',1,'EARM',x,y,z0+cal_drift-5,0,'ONLY') ! Iron Shield test, JDM
c      CALL GSPOS('GAIN',1,'EARM',x,y,z0+cal_drift-gain_thk*2., 0,'ONLY')
c      CALL GSPOS('VETO',1,'EARM',x,y,z0+cal_drift+2.*cal_depth/2.+30.,0,
c     + 'ONLY')

      
C     Postion Detector Shielding
      
      x = wall_horz
*     CALL GSPOS('WAL2',1,'EARM',x,y,z0+cer_drift-2.5, 0,'ONLY')
      
      x = -guard_horz
*     CALL GSPOS('GARD',1,'EARM',x,y,z0+cer_drift+cer_length/2-4.5, 6,'
C     ONLY')
      
C     

     
CCC Add on outside LHe NK 03/16/11
      CALL GSROTM(1,90.,SNGL(theta_0),0.,SNGL(theta_0),90.,310.)
      PAR(1) = 0.
      PAR(2) = 1.25+0.127
      PAR(3) = 0.25
      CALL GSVOLU( 'OLHE' , 'TUBE' ,NMED_LHe, PAR , 3 , IVOL )
      thick(4) = 4*PAR(3) 

      PAR(1) = 0.
      PAR(2) = 1.25+0.127
      PAR(3) = 0.001905
      CALL GSVOLU( 'CUAL' , 'TUBE' ,NMED_Al, PAR , 3 , IVOL )
      thick(5) = 4*PAR(3) 
 

      if (target_type.EQ.0) then    ! define polarized target
        write(*,*) '***** Configuring Polarized Target'
cmkj
        CALL GSPOS('CELL',1,'NOSE',0.,0.,TargVrtzOff  , 1,'ONLY')
        CALL GSPOS('OLHE',1,'NOSE',0.,0.,+1.75+TargVrtzOff+0.000162  , 1,'ONLY')
        CALL GSPOS('OLHE',2,'NOSE',0.,0.,-1.75+TargVrtzOff-0.000162  , 1,'ONLY')
        CALL GSPOS('CUAL',1,'NOSE',0.,0.,+1.5+TargVrtzOff+0.000081  , 1,'ONLY')
        CALL GSPOS('CUAL',2,'NOSE',0.,0.,-1.5+TargVrtzOff-0.000081  , 1,'ONLY')
cmkj
cc      do ilum=1,3  
c      do ilum=1,5  
        lumin(1) = 3.      !! 3*H
     >                /(ANH3(1)*3+ANH3(3)) 
        lumin(2) = 1.        !! 4He cup+1cm out
     >                /(ANH3(2)) 
        lumin(3) = 1.        !! 14N
     >                /(ANH3(1)*3+ANH3(3)) 
        lumin(4) = thick(5)*pfdens(5)        !! Aluminum in outer edges of cup 
     >                /(AALU(1))
 
c        lumin(ilum) = pfdens(ilum)
c     >                /atomnum(ilum) !per nbarn 
c        write(*,*)pfdens(ilum),atomnum(ilum)
c      enddo
      elseif (target_type.EQ.1) then          ! define standard carbon target

        write(*,*) '***** Configuring Carbon Target'
        CALL GSPOS('CRBN',1,'TCAN',0.,0.,TargVrtzOff+CdiscOff,1,'ONLY')

c
cc      do ilum=1,1  
cc      do ilum=4,4  
      do ilum=5,5  
        lumin(ilum) = thick(ilum)*pfdens(ilum)*beam_current
     >                /atomnum(ilum)*N_A/Q_E*1000. !per nbarn 

      enddo
      else
        write(*,*) target_type,field_type
        STOP 'BAD TTYP (target_type)'
      endif

        write(*,*)'Luminosity=',lumin
      call gprint('VOLU',0)

      CALL GGCLOS
C     
      write(*,*)'DONE UGEOM'
      END
      
      Subroutine SetMedPar(medium,tpar)
      integer medium
      real tpar(5)
      call gstpar(medium,'CUTGAM',tpar(1))
      call gstpar(medium,'CUTELE',tpar(2))
      call gstpar(medium,'CUTNEU',tpar(3))
      call gstpar(medium,'CUTHAD',tpar(4))
      call gstpar(medium,'CUTMUO',tpar(5))
c      call gstpar(medium,'ILOSS',1)
      call GSTPAR(medium,'DCUTE',0.00001)
      call GSTPAR(medium,'DCUTM',0.00001)
      call GSTPAR(medium,'DRAY',1.)

      call gstpar(medium,'BIRK1',1.)
      call gstpar(medium,'BIRK2',0.013)
      call gstpar(medium,'BIRK3',9.6E-6)
      call gstpar(medium,'GHCOR1',1.0)
      end
