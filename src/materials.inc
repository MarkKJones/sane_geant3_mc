C-- materials.inc        Glen Warren  8/03
C--
C-- define some NMED's for materials used in BETA

      integer*4 NMED_air,NMED_Pb,NMED_LG,NMED_Sc,NMED_Kap,NMED_N2
      integer*4 NMED_PLG,NMED_Luc,NMED_NH3,NMED_Vac,NMED_Al
      integer*4 NMED_KelF,NMED_LHe,NMED_C,NMED_Fe
      integer*4 NMED_Gain,NMED_Fx1,NMED_Fy1,NMED_Fy2

C
C MMB: Material and Medium number Bias
      integer MMB
      parameter (MMB=60)
C
      data NMED_air  / 61 / !MMB+1
      data NMED_Pb   / 62 / !MMB+2
      data NMED_LG   / 63 /
      data NMED_Sc   / 64 /
      data NMED_Kap  / 65 /
      data NMED_N2   / 66 /
      data NMED_PLG  / 67 /
      data NMED_Gain / 68 /
      data NMED_NH3  / 69 /
      data NMED_Vac  / 70 /
      data NMED_Al   / 71 /
      data NMED_KelF / 72 /
      data NMED_LHe  / 73 /
      data NMED_C    / 74 /
      data NMED_Fe   / 75 / 
      data NMED_Luc  / 76 /
      data NMED_Fx1  / 77 /
      data NMED_Fy1  / 78 /
      data NMED_Fy2  / 79 /


C-- Densities for NH3 components

      real*4 Hyddens,Heldens,Nitdens,Effdens,Pckfrc 
     
      COMMON /AMMDENS/Hyddens,Heldens,Nitdens,Effdens,Pckfrc

C-- Position of Target components

      real*4 TargVrtzOff,CdiscOff
     
      COMMON /TargPos/TargVrtzOff,CdiscOff

C-- Thickness of Target components

C      real*4 thick(20),pfdens(20),atomnum(20)
      real*4 lumin(6),thick(6),pfdens(6),atomnum(6)
C      real*4 lumin(4),thick(4),pfdens(4),atomnum(4)
     
      COMMON /TargThick/lumin,thick,pfdens,atomnum


