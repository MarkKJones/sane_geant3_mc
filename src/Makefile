
F77 = gfortran
FC = gfortran

#FFLAGS = -g -ffixed-line-length-132 -finit-local-zero
FFLAGS = -O -ffixed-line-length-none 
#-malign-double -fno-automatic 
#FFLAGS = -O -ffixed-line-length-none -finit-local-zero -malign-double
LDFLAGS =  -Wl,-E -L/site/cernlib/i386_rhel5/2005//lib -lgeant321 -lpawlib -lgraflib \
     -lgrafX11 -lpacklib -lmathlib -llapack3 -lblas -L/usr/X11R6/lib \
      -lX11 -lnsl -lcrypt -ldl

OBJECTS = guhadr.o guphad.o gxcs.o uglast.o gukine.o gustep.o \
          gxint.o ufiles.o  ugeom.o uhinit.o guout.o gutrev.o \
          gxphys.o uginit.o trg_track.o gen_evt.o cross_section.o \
          calc_asym.o mrst_sub.o mrst2001.o mrst_xsn.o pos_slac_func.o \
          wiser_func.o epc_or.o vector.o sigmot.o ltrace.o \
	  ggauss.o lucitehodo.o \
          forwardtracker.o calorimeter.o  \
          F1F209.o elastic.o cerenkov.o bicubic_spline.o \
          radiated_xn_h2_47.o radiated_xn_h2_59.o \
          radiated_xn_he_47.o radiated_xn_he_59.o \
          radiated_xn_N_47.o radiated_xn_N_59.o 

 
%.o: %.f
	$(F77) $(FFLAGS) -c $< -o $@

a1p_mc_b: $(OBJECTS)
	$(F77) -o $@ $(OBJECTS) $(LDFLAGS)

gen_evt.o: gen_evt.f sane.inc beta_geom.inc constants.inc sane_misc.inc \
           sane_accp.inc
	$(F77) $(FFLAGS) -c $< -o $@

ufiles.o: ufiles.f sane_cwn.inc sane_misc.inc beta_geom.inc
	$(F77) $(FFLAGS) -c $< -o $@

vector.o: vector.f constants.inc
	$(F77) $(FFLAGS) -c $< -o $@

uginit.o: uginit.f sane.inc beta_geom.inc geant.inc sane_misc.inc sane_accp.inc
	$(F77) $(FFLAGS) -c $< -o $@

uglast.o: uglast.f sane.inc beta_geom.inc sane_misc.inc sane_accp.inc
	$(F77) $(FFLAGS) -c $< -o $@

ugeom.o: ugeom.f sane.inc geant.inc beta_geom.inc constants.inc materials.inc
	$(F77) $(FFLAGS) -c $< -o $@

gustep.o: gustep.f geant.inc sane.inc beta_geom.inc sane_misc.inc materials.inc
	$(F77) $(FFLAGS) -c $< -o $@

guout.o: guout.f sane.inc beta_geom.inc sane_misc.inc geant.inc sane_cwn.inc
	$(F77) $(FFLAGS) -c $< -o $@

gukine.o: gukine.f sane.inc geant.inc beta_geom.inc sane_misc.inc sane_cwn.inc
	$(F77) $(FFLAGS) -c $< -o $@

ltrace.o: ltrace.f geant.inc beta_geom.inc sane_cwn.inc
	$(F77) $(FFLAGS) -c $< -o $@

ggauss.o: ggauss.f 
	$(F77) $(FFLAGS) -c $< -o $@ -I/site/cernlib/i386_rhel5/2005/include

lucitehodo.o: lucitehodo.f
	$(F77) $(FFLAGS) -c $< -o $@
forwardtracker.o: forwardtracker.f
	$(F77) $(FFLAGS) -c $< -o $@
elastic.o: elastic.f
	$(F77) $(FFLAGS) -c $< -o $@

htest: htest.o
	$(F77) -o $@ htest.o $(LDFLAGS) -I/site/cernlib/i386_rhel5/2005/include/
cerenkov: cerenkov.o
	$(F77) -o $@ cerenkov.o $(LDFLAGS) -I/site/cernlib/i386_rhel5/2005/include/

clean:
	rm *.o a1p_mc_b

