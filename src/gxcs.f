*
* $Id: gxcs.f,v 1.1 2010/04/22 14:13:57 jones Exp $
*
* $Log: gxcs.f,v $
* Revision 1.1  2010/04/22 14:13:57  jones
* in new subdirectory
*
* Revision 1.2  2010/04/01 19:47:34  jones
* Updated files from Narbe
*
* Revision 1.1.1.1  2007/12/04 20:12:14  jones
*  SANE GEANT and MC
*
* Revision 1.1.1.1  2003/03/28 21:06:28  gwarren
* moved from a1p directory
*
* Revision 1.1.1.1  2003/02/11 21:12:50  gwarren
* Imported sources
*
* Revision 1.1.1.1  1995/10/24 10:21:49  cernlib
* Geant
*
*
*CMZ :          20/06/95  09.32.44  by  S.Ravndal
*-- Author :
      SUBROUTINE GXCS
C.
C.    ******************************************************************
C.    *                                                                *
C.    *        To initialize the COMIS package                         *
C.    *        To declare addresses of FORTRAN routines and COMMONs    *
C.    *        which may be invoked from COMIS routines                *
C.    *        (one can call CSOMAP instead)                           *
C.    *                                                                *
C.    ******************************************************************
*
* $Id: gxcs.f,v 1.1 2010/04/22 14:13:57 jones Exp $
*
* $Log: gxcs.f,v $
* Revision 1.1  2010/04/22 14:13:57  jones
* in new subdirectory
*
* Revision 1.2  2010/04/01 19:47:34  jones
* Updated files from Narbe
*
* Revision 1.1.1.1  2007/12/04 20:12:14  jones
*  SANE GEANT and MC
*
* Revision 1.1.1.1  2003/03/28 21:06:28  gwarren
* moved from a1p directory
*
* Revision 1.1.1.1  2003/02/11 21:12:50  gwarren
* Imported sources
*
* Revision 1.1.1.1  1995/10/24 10:20:31  cernlib
* Geant
*
*
*
*
* gcbank.inc
*
      PARAMETER (KWBANK=690000,KWWORK=52000)
      COMMON/GCBANK/NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,FENDQ(16)
     +             ,LMAIN,LR1,WS(KWBANK)
      DIMENSION IQ(2),Q(2),LQ(8000),IWS(2)
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN),(IWS(1),WS(1))
      EQUIVALENCE (JCG,JGSTAT)
*
* $Id: gxcs.f,v 1.1 2010/04/22 14:13:57 jones Exp $
*
* $Log: gxcs.f,v $
* Revision 1.1  2010/04/22 14:13:57  jones
* in new subdirectory
*
* Revision 1.2  2010/04/01 19:47:34  jones
* Updated files from Narbe
*
* Revision 1.1.1.1  2007/12/04 20:12:14  jones
*  SANE GEANT and MC
*
* Revision 1.1.1.1  2003/03/28 21:06:28  gwarren
* moved from a1p directory
*
* Revision 1.1.1.1  2003/02/11 21:12:50  gwarren
* Imported sources
*
* Revision 1.1.1.1  1995/10/24 10:20:31  cernlib
* Geant
*
*
*
*
* gclink.inc
*
      COMMON/GCLINK/JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
*
* gclink.inc
*
*
* gcbank.inc
*
*
* $Id: gxcs.f,v 1.1 2010/04/22 14:13:57 jones Exp $
*
* $Log: gxcs.f,v $
* Revision 1.1  2010/04/22 14:13:57  jones
* in new subdirectory
*
* Revision 1.2  2010/04/01 19:47:34  jones
* Updated files from Narbe
*
* Revision 1.1.1.1  2007/12/04 20:12:14  jones
*  SANE GEANT and MC
*
* Revision 1.1.1.1  2003/03/28 21:06:28  gwarren
* moved from a1p directory
*
* Revision 1.1.1.1  2003/02/11 21:12:50  gwarren
* Imported sources
*
* Revision 1.1.1.1  1995/10/24 10:20:32  cernlib
* Geant
*
*
*
*
* gcmate.inc
*
      COMMON/GCMATE/NMAT,NAMATE(5),A,Z,DENS,RADL,ABSL
C
      INTEGER NMAT,NAMATE
      REAL A,Z,DENS,RADL,ABSL
C
*
* gcmate.inc
*
*
* $Id: gxcs.f,v 1.1 2010/04/22 14:13:57 jones Exp $
*
* $Log: gxcs.f,v $
* Revision 1.1  2010/04/22 14:13:57  jones
* in new subdirectory
*
* Revision 1.2  2010/04/01 19:47:34  jones
* Updated files from Narbe
*
* Revision 1.1.1.1  2007/12/04 20:12:14  jones
*  SANE GEANT and MC
*
* Revision 1.1.1.1  2003/03/28 21:06:28  gwarren
* moved from a1p directory
*
* Revision 1.1.1.1  2003/02/11 21:12:50  gwarren
* Imported sources
*
* Revision 1.1.1.1  1995/10/24 10:20:33  cernlib
* Geant
*
*
*
*
* gctmed.inc
*
      COMMON/GCTMED/NUMED,NATMED(5),ISVOL,IFIELD,FIELDM,TMAXFD,STEMAX
     +      ,DEEMAX,EPSIL,STMIN,CFIELD,PREC,IUPD,ISTPAR,NUMOLD
      COMMON/GCTLIT/THRIND,PMIN,DP,DNDL,JMIN,ITCKOV,IMCKOV,NPCKOV
C
*
* gctmed.inc
*
*
* $Id: gxcs.f,v 1.1 2010/04/22 14:13:57 jones Exp $
*
* $Log: gxcs.f,v $
* Revision 1.1  2010/04/22 14:13:57  jones
* in new subdirectory
*
* Revision 1.2  2010/04/01 19:47:34  jones
* Updated files from Narbe
*
* Revision 1.1.1.1  2007/12/04 20:12:14  jones
*  SANE GEANT and MC
*
* Revision 1.1.1.1  2003/03/28 21:06:28  gwarren
* moved from a1p directory
*
* Revision 1.1.1.1  2003/02/11 21:12:50  gwarren
* Imported sources
*
* Revision 1.1.1.1  1995/10/24 10:20:33  cernlib
* Geant
*
*
*
*
* gcvolu.inc
*
      COMMON/GCVOLU/NLEVEL,NAMES(15),NUMBER(15),
     +LVOLUM(15),LINDEX(15),INFROM,NLEVMX,NLDEV(15),LINMX(15),
     +GTRAN(3,15),GRMAT(10,15),GONLY(15),GLX(3)
C
*
* gcvolu.inc
*
*
* $Id: gxcs.f,v 1.1 2010/04/22 14:13:57 jones Exp $
*
* $Log: gxcs.f,v $
* Revision 1.1  2010/04/22 14:13:57  jones
* in new subdirectory
*
* Revision 1.2  2010/04/01 19:47:34  jones
* Updated files from Narbe
*
* Revision 1.1.1.1  2007/12/04 20:12:14  jones
*  SANE GEANT and MC
*
* Revision 1.1.1.1  2003/03/28 21:06:28  gwarren
* moved from a1p directory
*
* Revision 1.1.1.1  2003/02/11 21:12:50  gwarren
* Imported sources
*
* Revision 1.1.1.1  1995/10/24 10:20:31  cernlib
* Geant
*
*
*
*
* gcflag.inc
*
      COMMON/GCFLAG/IDEBUG,IDEMIN,IDEMAX,ITEST,IDRUN,IDEVT,IEORUN
     +        ,IEOTRI,IEVENT,ISWIT(10),IFINIT(20),NEVENT,NRNDM(2)
      COMMON/GCFLAX/BATCH, NOLOG
      LOGICAL BATCH, NOLOG
C
*
* gcflag.inc
*
*
* $Id: gxcs.f,v 1.1 2010/04/22 14:13:57 jones Exp $
*
* $Log: gxcs.f,v $
* Revision 1.1  2010/04/22 14:13:57  jones
* in new subdirectory
*
* Revision 1.2  2010/04/01 19:47:34  jones
* Updated files from Narbe
*
* Revision 1.1.1.1  2007/12/04 20:12:14  jones
*  SANE GEANT and MC
*
* Revision 1.1.1.1  2003/03/28 21:06:28  gwarren
* moved from a1p directory
*
* Revision 1.1.1.1  2003/02/11 21:12:50  gwarren
* Imported sources
*
* Revision 1.1.1.1  1995/10/24 10:20:33  cernlib
* Geant
*
*
*
*
* gctrak.inc
*
      PARAMETER (MAXMEC=30)
      COMMON/GCTRAK/VECT(7),GETOT,GEKIN,VOUT(7),NMEC,LMEC(MAXMEC)
     + ,NAMEC(MAXMEC),NSTEP ,MAXNST,DESTEP,DESTEL,SAFETY,SLENG
     + ,STEP  ,SNEXT ,SFIELD,TOFG  ,GEKRAT,UPWGHT,IGNEXT,INWVOL
     + ,ISTOP ,IGAUTO,IEKBIN, ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN
     + ,NLVSAV,ISTORY
      PARAMETER (MAXME1=30)
      COMMON/GCTPOL/POLAR(3), NAMEC1(MAXME1)
C
*
* gctrak.inc
*
*
* $Id: gxcs.f,v 1.1 2010/04/22 14:13:57 jones Exp $
*
* $Log: gxcs.f,v $
* Revision 1.1  2010/04/22 14:13:57  jones
* in new subdirectory
*
* Revision 1.2  2010/04/01 19:47:34  jones
* Updated files from Narbe
*
* Revision 1.1.1.1  2007/12/04 20:12:14  jones
*  SANE GEANT and MC
*
* Revision 1.1.1.1  2003/03/28 21:06:28  gwarren
* moved from a1p directory
*
* Revision 1.1.1.1  2003/02/11 21:12:50  gwarren
* Imported sources
*
* Revision 1.1.1.1  1995/10/24 10:20:31  cernlib
* Geant
*
*
*
*
* gckine.inc
*
      COMMON/GCKINE/IKINE,PKINE(10),ITRA,ISTAK,IVERT,IPART,ITRTYP
     +      ,NAPART(5),AMASS,CHARGE,TLIFE,VERT(3),PVERT(4),IPAOLD
C
*
* gckine.inc
*
*
* $Id: gxcs.f,v 1.1 2010/04/22 14:13:57 jones Exp $
*
* $Log: gxcs.f,v $
* Revision 1.1  2010/04/22 14:13:57  jones
* in new subdirectory
*
* Revision 1.2  2010/04/01 19:47:34  jones
* Updated files from Narbe
*
* Revision 1.1.1.1  2007/12/04 20:12:14  jones
*  SANE GEANT and MC
*
* Revision 1.1.1.1  2003/03/28 21:06:28  gwarren
* moved from a1p directory
*
* Revision 1.1.1.1  2003/02/11 21:12:50  gwarren
* Imported sources
*
* Revision 1.1.1.1  1995/10/24 10:20:32  cernlib
* Geant
*
*
*
*
* gcking.inc
*
*
* $Id: gxcs.f,v 1.1 2010/04/22 14:13:57 jones Exp $
*
* $Log: gxcs.f,v $
* Revision 1.1  2010/04/22 14:13:57  jones
* in new subdirectory
*
* Revision 1.2  2010/04/01 19:47:34  jones
* Updated files from Narbe
*
* Revision 1.1.1.1  2007/12/04 20:12:14  jones
*  SANE GEANT and MC
*
* Revision 1.1.1.1  2003/03/28 21:06:28  gwarren
* moved from a1p directory
*
* Revision 1.1.1.1  2003/02/11 21:12:50  gwarren
* Imported sources
*
* Revision 1.1.1.1  1995/10/24 10:20:32  cernlib
* Geant
*
*
*
*
* gckmax.inc
*
      INTEGER MXGKIN
      PARAMETER (MXGKIN=100)
*
* gckmax.inc
*
      COMMON/GCKING/KCASE,NGKINE,GKIN(5,MXGKIN),
     +                           TOFD(MXGKIN),IFLGK(MXGKIN)
      INTEGER       KCASE,NGKINE ,IFLGK,MXPHOT,NGPHOT
      REAL          GKIN,TOFD,XPHOT
C
      PARAMETER (MXPHOT=800)
      COMMON/GCKIN2/NGPHOT,XPHOT(11,MXPHOT)
C
      COMMON/GCKIN3/GPOS(3,MXGKIN)
      REAL          GPOS
C
*
* gcking.inc
*
*
* $Id: gxcs.f,v 1.1 2010/04/22 14:13:57 jones Exp $
*
* $Log: gxcs.f,v $
* Revision 1.1  2010/04/22 14:13:57  jones
* in new subdirectory
*
* Revision 1.2  2010/04/01 19:47:34  jones
* Updated files from Narbe
*
* Revision 1.1.1.1  2007/12/04 20:12:14  jones
*  SANE GEANT and MC
*
* Revision 1.1.1.1  2003/03/28 21:06:28  gwarren
* moved from a1p directory
*
* Revision 1.1.1.1  2003/02/11 21:12:50  gwarren
* Imported sources
*
* Revision 1.1.1.1  1995/10/24 10:20:31  cernlib
* Geant
*
*
*
*
* gccuts.inc
*
      COMMON/GCCUTS/CUTGAM,CUTELE,CUTNEU,CUTHAD,CUTMUO,BCUTE,BCUTM
     +             ,DCUTE ,DCUTM ,PPCUTM,TOFMAX,GCUTS(5)
C
*
* gccuts.inc
*
*
* $Id: gxcs.f,v 1.1 2010/04/22 14:13:57 jones Exp $
*
* $Log: gxcs.f,v $
* Revision 1.1  2010/04/22 14:13:57  jones
* in new subdirectory
*
* Revision 1.2  2010/04/01 19:47:34  jones
* Updated files from Narbe
*
* Revision 1.1.1.1  2007/12/04 20:12:14  jones
*  SANE GEANT and MC
*
* Revision 1.1.1.1  2003/03/28 21:06:28  gwarren
* moved from a1p directory
*
* Revision 1.1.1.1  2003/02/11 21:12:50  gwarren
* Imported sources
*
* Revision 1.1.1.1  1995/10/24 10:20:32  cernlib
* Geant
*
*
*
*
* gclist.inc
*
      COMMON/GCLIST/NHSTA,NGET ,NSAVE,NSETS,NPRIN,NGEOM,NVIEW,NPLOT
     +       ,NSTAT,LHSTA(20),LGET (20),LSAVE(20),LSETS(20),LPRIN(20)
     +             ,LGEOM(20),LVIEW(20),LPLOT(20),LSTAT(20)
C
*
* gclist.inc
*
*
* $Id: gxcs.f,v 1.1 2010/04/22 14:13:57 jones Exp $
*
* $Log: gxcs.f,v $
* Revision 1.1  2010/04/22 14:13:57  jones
* in new subdirectory
*
* Revision 1.2  2010/04/01 19:47:34  jones
* Updated files from Narbe
*
* Revision 1.1.1.1  2007/12/04 20:12:14  jones
*  SANE GEANT and MC
*
* Revision 1.1.1.1  2003/03/28 21:06:28  gwarren
* moved from a1p directory
*
* Revision 1.1.1.1  2003/02/11 21:12:50  gwarren
* Imported sources
*
* Revision 1.1.1.1  1995/10/24 10:20:32  cernlib
* Geant
*
*
*
*
* gcnum.inc
*
      COMMON/GCNUM/NMATE ,NVOLUM,NROTM,NTMED,NTMULT,NTRACK,NPART
     +            ,NSTMAX,NVERTX,NHEAD,NBIT
      COMMON /GCNUMX/ NALIVE,NTMSTO
C
*
* gcnum.inc
*
*
* $Id: gxcs.f,v 1.1 2010/04/22 14:13:57 jones Exp $
*
* $Log: gxcs.f,v $
* Revision 1.1  2010/04/22 14:13:57  jones
* in new subdirectory
*
* Revision 1.2  2010/04/01 19:47:34  jones
* Updated files from Narbe
*
* Revision 1.1.1.1  2007/12/04 20:12:14  jones
*  SANE GEANT and MC
*
* Revision 1.1.1.1  2003/03/28 21:06:28  gwarren
* moved from a1p directory
*
* Revision 1.1.1.1  2003/02/11 21:12:50  gwarren
* Imported sources
*
* Revision 1.1.1.1  1995/10/24 10:20:32  cernlib
* Geant
*
*
*
*
* gconst.inc
*
      COMMON/GCONST/PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS
      COMMON/GCONSX/EMMU,PMASS,AVO
C
*
* gconst.inc
*
*
* $Id: gxcs.f,v 1.1 2010/04/22 14:13:57 jones Exp $
*
* $Log: gxcs.f,v $
* Revision 1.1  2010/04/22 14:13:57  jones
* in new subdirectory
*
* Revision 1.2  2010/04/01 19:47:34  jones
* Updated files from Narbe
*
* Revision 1.1.1.1  2007/12/04 20:12:14  jones
*  SANE GEANT and MC
*
* Revision 1.1.1.1  2003/03/28 21:06:28  gwarren
* moved from a1p directory
*
* Revision 1.1.1.1  2003/02/11 21:12:50  gwarren
* Imported sources
*
* Revision 1.1.1.1  1995/10/24 10:20:32  cernlib
* Geant
*
*
*
*
* gcphys.inc
*
      COMMON/GCPHYS/IPAIR,SPAIR,SLPAIR,ZINTPA,STEPPA
     +             ,ICOMP,SCOMP,SLCOMP,ZINTCO,STEPCO
     +             ,IPHOT,SPHOT,SLPHOT,ZINTPH,STEPPH
     +             ,IPFIS,SPFIS,SLPFIS,ZINTPF,STEPPF
     +             ,IDRAY,SDRAY,SLDRAY,ZINTDR,STEPDR
     +             ,IANNI,SANNI,SLANNI,ZINTAN,STEPAN
     +             ,IBREM,SBREM,SLBREM,ZINTBR,STEPBR
     +             ,IHADR,SHADR,SLHADR,ZINTHA,STEPHA
     +             ,IMUNU,SMUNU,SLMUNU,ZINTMU,STEPMU
     +             ,IDCAY,SDCAY,SLIFE ,SUMLIF,DPHYS1
     +             ,ILOSS,SLOSS,SOLOSS,STLOSS,DPHYS2
     +             ,IMULS,SMULS,SOMULS,STMULS,DPHYS3
     +             ,IRAYL,SRAYL,SLRAYL,ZINTRA,STEPRA
      COMMON/GCPHLT/ILABS,SLABS,SLLABS,ZINTLA,STEPLA
     +             ,ISYNC
     +             ,ISTRA
*
*
* gcphys.inc
*
*
* $Id: gxcs.f,v 1.1 2010/04/22 14:13:57 jones Exp $
*
* $Log: gxcs.f,v $
* Revision 1.1  2010/04/22 14:13:57  jones
* in new subdirectory
*
* Revision 1.2  2010/04/01 19:47:34  jones
* Updated files from Narbe
*
* Revision 1.1.1.1  2007/12/04 20:12:14  jones
*  SANE GEANT and MC
*
* Revision 1.1.1.1  2003/03/28 21:06:28  gwarren
* moved from a1p directory
*
* Revision 1.1.1.1  2003/02/11 21:12:50  gwarren
* Imported sources
*
* Revision 1.1.1.1  1995/10/24 10:20:33  cernlib
* Geant
*
*
*
*
* gcunit.inc
*
      COMMON/GCUNIT/LIN,LOUT,NUNITS,LUNITS(5)
      INTEGER LIN,LOUT,NUNITS,LUNITS
      COMMON/GCMAIL/CHMAIL
      CHARACTER*132 CHMAIL
C
*
* gcunit.inc
*
*
* $Id: gxcs.f,v 1.1 2010/04/22 14:13:57 jones Exp $
*
* $Log: gxcs.f,v $
* Revision 1.1  2010/04/22 14:13:57  jones
* in new subdirectory
*
* Revision 1.2  2010/04/01 19:47:34  jones
* Updated files from Narbe
*
* Revision 1.1.1.1  2007/12/04 20:12:14  jones
*  SANE GEANT and MC
*
* Revision 1.1.1.1  2003/03/28 21:06:28  gwarren
* moved from a1p directory
*
* Revision 1.1.1.1  2003/02/11 21:12:50  gwarren
* Imported sources
*
* Revision 1.1.1.1  1995/10/24 10:20:34  cernlib
* Geant
*
*
*
*
* gcdraw.inc
*
      COMMON/GCDRAW/NUMNOD,MAXNOD,NUMND1,LEVVER,LEVHOR,MAXV,IPICK,
     + MLEVV,MLEVH,NWCUT,JNAM,JMOT,JXON,JBRO,JDUP,JSCA,JDVM,JPSM,
     + JNAM1,JMOT1,JXON1,JBRO1,JDUP1,JSCA1,JULEV,JVLEV,
     + LOOKTB(16),
     + GRMAT0(10),GTRAN0(3),IDRNUM,GSIN(41),GCOS(41),SINPSI,COSPSI,
     + GTHETA,GPHI,GPSI,GU0,GV0,GSCU,GSCV,NGVIEW,
     + ICUTFL,ICUT,CTHETA,CPHI,DCUT,NSURF,ISURF,
     + GZUA,GZVA,GZUB,GZVB,GZUC,GZVC,PLTRNX,PLTRNY,
     + LINATT,LINATP,ITXATT,ITHRZ,IPRJ,DPERS,ITR3D,IPKHIT,IOBJ,LINBUF,
     + MAXGU,MORGU,MAXGS,MORGS,MAXTU,MORTU,MAXTS,MORTS,
     + IGU,IGS,ITU,ITS,NKVIEW,IDVIEW,
     + NOPEN,IGMR,IPIONS,ITRKOP,IHIDEN,
     + ZZFU,ZZFV,MYISEL,
     + DDUMMY(15)
C
*
* gcdraw.inc
*
*
* $Id: gxcs.f,v 1.1 2010/04/22 14:13:57 jones Exp $
*
* $Log: gxcs.f,v $
* Revision 1.1  2010/04/22 14:13:57  jones
* in new subdirectory
*
* Revision 1.2  2010/04/01 19:47:34  jones
* Updated files from Narbe
*
* Revision 1.1.1.1  2007/12/04 20:12:14  jones
*  SANE GEANT and MC
*
* Revision 1.1.1.1  2003/03/28 21:06:28  gwarren
* moved from a1p directory
*
* Revision 1.1.1.1  2003/02/11 21:12:50  gwarren
* Imported sources
*
* Revision 1.1.1.1  1995/10/24 10:20:32  cernlib
* Geant
*
*
*
*
* gcmulo.inc
*
      COMMON/GCMULO/SINMUL(101),COSMUL(101),SQRMUL(101),OMCMOL,CHCMOL
     +  ,EKMIN,EKMAX,NEKBIN,NEK1,EKINV,GEKA,GEKB,EKBIN(200),ELOW(200)
C
      REAL SINMUL,COSMUL,SQRMUL,OMCMOL,CHCMOL,EKMIN,EKMAX,ELOW,EKINV
      REAL GEKA,GEKB,EKBIN
      INTEGER NEKBIN,NEK1
C
*
* gcmulo.inc
*
*
* $Id: gxcs.f,v 1.1 2010/04/22 14:13:57 jones Exp $
*
* $Log: gxcs.f,v $
* Revision 1.1  2010/04/22 14:13:57  jones
* in new subdirectory
*
* Revision 1.2  2010/04/01 19:47:34  jones
* Updated files from Narbe
*
* Revision 1.1.1.1  2007/12/04 20:12:14  jones
*  SANE GEANT and MC
*
* Revision 1.1.1.1  2003/03/28 21:06:28  gwarren
* moved from a1p directory
*
* Revision 1.1.1.1  2003/02/11 21:12:50  gwarren
* Imported sources
*
* Revision 1.1.1.1  1995/10/24 10:20:34  cernlib
* Geant
*
*
*
*
* gcomis.inc
*
      COMMON/GCOMIS/JUINIT,JUGEOM,JUKINE,JUSTEP,JUOUT,JULAST
      DIMENSION JPCOMS(6)
      EQUIVALENCE (JPCOMS,JUINIT)
*
*
* gcomis.inc
*
      DIMENSION P(1)
*
      EXTERNAL GINIT,GZINIT,GDINIT,GPRINT,GPSETS,GXCLOS
      EXTERNAL GSVERT,GSKINE,GSKING,GOPEN,GFIN,GCLOSE
      EXTERNAL GFOUT
      EXTERNAL GMATE,GSMATE,GSMIXT,GSTMED,GSTPAR,GPART,GPHYSI
      EXTERNAL GFMATE,GPIONS
      EXTERNAL GTRIG,GTRIGI,GTRIGC,GTREVE,GIDROP
      EXTERNAL GSVOLU,GSPOS,GSPOSP,GSDVN,GSDVS,GGCLOS,GOPTIM
      EXTERNAL GSROTM,GSORD,GSDET,GSDETH,GSDETV,GSATT
      EXTERNAL GPLMAT,GSAHIT,GSCHIT,GSDIGI,GSXYZ,GDEBUG
      EXTERNAL GPCXYZ,GDCXYZ,GDXYZ,GDAHIT,GDCHIT,GDHITS,GDHEAD
      EXTERNAL GDOPEN,GDCLOS,GDRAW,GDRAWC,GDSCAL,GDMAN,GDCOL
      EXTERNAL GDELET,GDAXIS,GDRAWT
      EXTERNAL GSCANK,GSCANU,GSCANO
      EXTERNAL UGLAST
*
C.
C.    ------------------------------------------------------------------
C.
      CALL PAWCS
!      CALL GPAW(NWGEAN,NWPAW)

*
      CALL CSCOM('GCLINK,GCBANK,GCCUTS,GCFLAG,GCKINE,GCLIST#'
     +,           JDIGI ,NZEBRA,CUTGAM,IDEBUG,IKINE ,NHSTA
     +,           P,P,P,P)
      CALL CSCOM('GCMATE,GCNUM,GCONST,GCPHYS,GCTMED,GCTRAK#'
     +,           NMAT  ,NMATE,PI    ,IPAIR ,NUMED ,VECT
     +,           P,P,P,P)
      CALL CSCOM('GCUNIT,GCVOLU,GCDRAW,GCKING,GCMULO#'
     +,           LIN   ,NLEVEL,NUMNOD,KCASE ,SINMUL
     +,           P,P,P,P,P)
*
      CALL CSEXT(
     +  'GINIT.S,GZINIT.S,GDINIT.S,GPRINT.S,GPSETS.S,GXCLOS.S#'
     +,  GINIT  ,GZINIT  ,GDINIT  ,GPRINT  ,GPSETS  ,GXCLOS
     +,  P,P,P,P)
      CALL CSEXT(
     +  'GSVERT.S,GSKINE.S,GSKING.S,GFIN.S,GOPEN.S,GCLOSE.S,GFOUT.S#'
     +,  GSVERT  ,GSKINE  ,GSKING  ,GFIN  ,GOPEN  ,GCLOSE  ,GFOUT
     +,  P,P,P)
      CALL CSEXT(
     +  'GMATE.S,GSMATE.S,GFMATE.S,GSMIXT.S,GSTMED.S,GSTPAR.S#'
     +,  GMATE  ,GSMATE  ,GFMATE  ,GSMIXT  ,GSTMED  ,GSTPAR
     +,  P,P,P,P)
      CALL CSEXT(
     +  'GPART.S,GPIONS.S,GPHYSI.S#'
     +,  GPART  ,GPIONS  ,GPHYSI
     +,  P,P,P,P,P,P,P)
      CALL CSEXT(
     +  'GTRIG.S,GTRIGI.S,GTRIGC.S,GTREVE.S,GIDROP.S#'
     +,  GTRIG  ,GTRIGI  ,GTRIGC  ,GTREVE  ,GIDROP
     +,  P,P,P,P,P)
      CALL CSEXT(
     +  'GSVOLU.S,GSPOS.S,GSPOSP.S,GSDVN.S,GSDVS.S,GGCLOS.S,GOPTIM.S#'
     +,  GSVOLU  ,GSPOS  ,GSPOSP  ,GSDVN  ,GSDVS  ,GGCLOS  ,GOPTIM
     +,  P,P,P)
      CALL CSEXT(
     +  'GSROTM.S,GSORD.S,GSDET.S,GSDETH.S,GSDETV.S,GSATT.S#'
     +,  GSROTM  ,GSORD  ,GSDET  ,GSDETH  ,GSDETV  ,GSATT
     +,  P,P,P,P)
      CALL CSEXT(
     +  'GPLMAT.S,GSAHIT.S,GSCHIT.S,GSDIGI.S,GSXYZ.S,GDEBUG.S#'
     +,  GPLMAT  ,GSAHIT  ,GSCHIT  ,GSDIGI  ,GSXYZ  ,GDEBUG
     +,  P,P,P,P)
      CALL CSEXT(
     +  'GPCXYZ.S,GDCXYZ.S,GDXYZ.S,GDAHIT.S,GDCHIT.S,GDHITS.S,GDHEAD.S#'
     +,  GPCXYZ  ,GDCXYZ  ,GDXYZ  ,GDAHIT  ,GDCHIT  ,GDHITS  ,GDHEAD
     +,  P,P,P)
      CALL CSEXT(
     +  'GDOPEN.S,GDCLOS.S,GDELET.S,GDRAW.S,GDRAWC.S#'
     +,  GDOPEN  ,GDCLOS  ,GDELET  ,GDRAW  ,GDRAWC
     +,  P,P,P,P,P)
      CALL CSEXT(
     +  'GDAXIS.S,GDSCAL.S,GDMAN.S,GDCOL.S#'
     +,  GDAXIS  ,GDSCAL  ,GDMAN  ,GDCOL
     +,  P,P,P,P,P,P)
      CALL CSEXT(
     +  'GDRAWT.S#'
     +,  GDRAWT
     +,  P,P,P,P,P,P,P,P,P)
      CALL CSEXT(
     +  'GSCANK.S,GSCANU.S,GSCANO.S,GBRSGE.R#'
     +  ,GSCANK  ,GSCANU  ,GSCANO  ,GBRSGE
     +,  P,P,P,P,P,P)
      CALL CSEXT(
     +  'UGLAST.S#'
     +,  UGLAST
     +,  P,P,P,P,P,P,P,P,P)
*
      CALL GUXCS
*
      END
 
