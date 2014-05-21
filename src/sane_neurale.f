      Subroutine neurale(x, index)
      implicit double precision (a-h,n-z)
      double precision x(12)
      double precision FixX,FixY,FixE
      common/SNEU/FixX,FixY,FixE
      double precision neural
c      write(*,*)'X=',X
C --- Last Layer
      neural=0
      if (index.eq.0) then
          neural=eneuron0x9913808(x)
          FixE=neural 
c          write(*,*)neural,eneuron0x9913808(x)
       else
          neural=0.d0
      endif
      end
C --- First and Hidden layers
      double precision function eneuron0x9903308(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      eneuron0x9903308 = (x(1) - 0d0)/1d0
      end
      double precision function eneuron0x99118a8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      eneuron0x99118a8 = (x(2) - 0d0)/1d0
      end
      double precision function eneuron0x99119a8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      eneuron0x99119a8 = (x(3) - 0d0)/1d0
      end
      double precision function eneuron0x9911b38(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      eneuron0x9911b38 = (x(4) - 0d0)/1d0
      end
      double precision function eneuron0x9911d10(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      eneuron0x9911d10 = (x(5) - 0d0)/1d0
      end
      double precision function eneuron0x9911ee8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      eneuron0x9911ee8 = (x(6) - 0d0)/1d0
      end
      double precision function eneuron0x99120c0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      eneuron0x99120c0 = (x(7) - 0d0)/1d0
      end
      double precision function eneuron0x99122b8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      eneuron0x99122b8 = (x(8) - 0d0)/1d0
      end
      double precision function eneuron0x99124b0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      eneuron0x99124b0 = (x(9) - 0d0)/1d0
      end
      double precision function eneuron0x99126a8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      eneuron0x99126a8 = (x(10) - 0d0)/1d0
      end
      double precision function eneuron0x99128a0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      eneuron0x99128a0 = (x(11) - 0d0)/1d0
      end
      double precision function eneuron0x9912a78(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      eneuron0x9912a78 = (x(12) - 0d0)/1d0
      end
      double precision function eneuron0x9912d88(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      eneuron0x9912d88 = 1.18189d0
      eneuron0x9912d88 = eneuron0x9912d88 + esynapse0x9903188(x)
      eneuron0x9912d88 = eneuron0x9912d88 + esynapse0x9912f18(x)
      eneuron0x9912d88 = eneuron0x9912d88 + esynapse0x9912f40(x)
      eneuron0x9912d88 = eneuron0x9912d88 + esynapse0x9912f68(x)
      eneuron0x9912d88 = eneuron0x9912d88 + esynapse0x9912f90(x)
      eneuron0x9912d88 = eneuron0x9912d88 + esynapse0x9912fb8(x)
      eneuron0x9912d88 = eneuron0x9912d88 + esynapse0x9912fe0(x)
      eneuron0x9912d88 = eneuron0x9912d88 + esynapse0x9913008(x)
      eneuron0x9912d88 = eneuron0x9912d88 + esynapse0x9913030(x)
      eneuron0x9912d88 = eneuron0x9912d88 + esynapse0x9913058(x)
      eneuron0x9912d88 = eneuron0x9912d88 + esynapse0x9913080(x)
      eneuron0x9912d88 = eneuron0x9912d88 + esynapse0x99130a8(x)
      eneuron0x9912d88= (exp(-eneuron0x9912d88*eneuron0x9912d88))
      end
      double precision function eneuron0x99130d0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      eneuron0x99130d0 = 0.584752d0
      eneuron0x99130d0 = eneuron0x99130d0 + esynapse0x99132a8(x)
      eneuron0x99130d0 = eneuron0x99130d0 + esynapse0x99132d0(x)
      eneuron0x99130d0 = eneuron0x99130d0 + esynapse0x99132f8(x)
      eneuron0x99130d0 = eneuron0x99130d0 + esynapse0x9913320(x)
      eneuron0x99130d0 = eneuron0x99130d0 + esynapse0x9913348(x)
      eneuron0x99130d0 = eneuron0x99130d0 + esynapse0x99133f8(x)
      eneuron0x99130d0 = eneuron0x99130d0 + esynapse0x9913420(x)
      eneuron0x99130d0 = eneuron0x99130d0 + esynapse0x9913448(x)
      eneuron0x99130d0 = eneuron0x99130d0 + esynapse0x9913470(x)
      eneuron0x99130d0 = eneuron0x99130d0 + esynapse0x9913498(x)
      eneuron0x99130d0 = eneuron0x99130d0 + esynapse0x99134c0(x)
      eneuron0x99130d0 = eneuron0x99130d0 + esynapse0x99134e8(x)
      eneuron0x99130d0= (exp(-eneuron0x99130d0*eneuron0x99130d0))
      end
      double precision function eneuron0x9913510(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      eneuron0x9913510 = 0.274168d0
      eneuron0x9913510 = eneuron0x9913510 + esynapse0x99136a0(x)
      eneuron0x9913510 = eneuron0x9913510 + esynapse0x99136c8(x)
      eneuron0x9913510 = eneuron0x9913510 + esynapse0x99136f0(x)
      eneuron0x9913510 = eneuron0x9913510 + esynapse0x9913718(x)
      eneuron0x9913510 = eneuron0x9913510 + esynapse0x9913740(x)
      eneuron0x9913510 = eneuron0x9913510 + esynapse0x9913768(x)
      eneuron0x9913510 = eneuron0x9913510 + esynapse0x9913790(x)
      eneuron0x9913510 = eneuron0x9913510 + esynapse0x99137b8(x)
      eneuron0x9913510 = eneuron0x9913510 + esynapse0x99137e0(x)
      eneuron0x9913510 = eneuron0x9913510 + esynapse0x9913370(x)
      eneuron0x9913510 = eneuron0x9913510 + esynapse0x9913398(x)
      eneuron0x9913510 = eneuron0x9913510 + esynapse0x99133c0(x)
      eneuron0x9913510= (exp(-eneuron0x9913510*eneuron0x9913510))
      end
      double precision function eneuron0x9913910(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      eneuron0x9913910 = -0.502317d0
      eneuron0x9913910 = eneuron0x9913910 + esynapse0x9913ae8(x)
      eneuron0x9913910 = eneuron0x9913910 + esynapse0x9913b10(x)
      eneuron0x9913910 = eneuron0x9913910 + esynapse0x9913b38(x)
      eneuron0x9913910 = eneuron0x9913910 + esynapse0x9913b60(x)
      eneuron0x9913910 = eneuron0x9913910 + esynapse0x9913b88(x)
      eneuron0x9913910 = eneuron0x9913910 + esynapse0x9913bb0(x)
      eneuron0x9913910 = eneuron0x9913910 + esynapse0x9913bd8(x)
      eneuron0x9913910 = eneuron0x9913910 + esynapse0x9913c00(x)
      eneuron0x9913910 = eneuron0x9913910 + esynapse0x9913c28(x)
      eneuron0x9913910 = eneuron0x9913910 + esynapse0x9913c50(x)
      eneuron0x9913910 = eneuron0x9913910 + esynapse0x9913c78(x)
      eneuron0x9913910 = eneuron0x9913910 + esynapse0x9913ca0(x)
      eneuron0x9913910= (exp(-eneuron0x9913910*eneuron0x9913910))
      end
      double precision function eneuron0x9913cc8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      eneuron0x9913cc8 = 0.534638d0
      eneuron0x9913cc8 = eneuron0x9913cc8 + esynapse0x9913f28(x)
      eneuron0x9913cc8 = eneuron0x9913cc8 + esynapse0x9913f50(x)
      eneuron0x9913cc8 = eneuron0x9913cc8 + esynapse0x9913f78(x)
      eneuron0x9913cc8 = eneuron0x9913cc8 + esynapse0x9913fa0(x)
      eneuron0x9913cc8 = eneuron0x9913cc8 + esynapse0x9913fc8(x)
      eneuron0x9913cc8 = eneuron0x9913cc8 + esynapse0x9913ff0(x)
      eneuron0x9913cc8 = eneuron0x9913cc8 + esynapse0x9914018(x)
      eneuron0x9913cc8 = eneuron0x9913cc8 + esynapse0x9914040(x)
      eneuron0x9913cc8 = eneuron0x9913cc8 + esynapse0x9914068(x)
      eneuron0x9913cc8 = eneuron0x9913cc8 + esynapse0x9914090(x)
      eneuron0x9913cc8 = eneuron0x9913cc8 + esynapse0x99140b8(x)
      eneuron0x9913cc8 = eneuron0x9913cc8 + esynapse0x99140e0(x)
      eneuron0x9913cc8= (exp(-eneuron0x9913cc8*eneuron0x9913cc8))
      end
      double precision function eneuron0x9914108(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      eneuron0x9914108 = 0.303604d0
      eneuron0x9914108 = eneuron0x9914108 + esynapse0x9914298(x)
      eneuron0x9914108 = eneuron0x9914108 + esynapse0x99142c0(x)
      eneuron0x9914108 = eneuron0x9914108 + esynapse0x99142e8(x)
      eneuron0x9914108 = eneuron0x9914108 + esynapse0x9914310(x)
      eneuron0x9914108 = eneuron0x9914108 + esynapse0x9914338(x)
      eneuron0x9914108 = eneuron0x9914108 + esynapse0x9835970(x)
      eneuron0x9914108 = eneuron0x9914108 + esynapse0x98ddac8(x)
      eneuron0x9914108 = eneuron0x9914108 + esynapse0x99030d0(x)
      eneuron0x9914108 = eneuron0x9914108 + esynapse0x99030f8(x)
      eneuron0x9914108 = eneuron0x9914108 + esynapse0x98359e8(x)
      eneuron0x9914108 = eneuron0x9914108 + esynapse0x9835a10(x)
      eneuron0x9914108 = eneuron0x9914108 + esynapse0x9835b48(x)
      eneuron0x9914108= (exp(-eneuron0x9914108*eneuron0x9914108))
      end
      double precision function eneuron0x9913808(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      eneuron0x9913808 = 0.15552d0
      eneuron0x9913808 = eneuron0x9913808 + esynapse0x9912cf8(x)
      eneuron0x9913808 = eneuron0x9913808 + esynapse0x9912d20(x)
      eneuron0x9913808 = eneuron0x9913808 + esynapse0x9912d48(x)
      eneuron0x9913808 = eneuron0x9913808 + esynapse0x9835be0(x)
      eneuron0x9913808 = eneuron0x9913808 + esynapse0x9914568(x)
      eneuron0x9913808 = eneuron0x9913808 + esynapse0x9914590(x)
      end
C --- Esynapses
      double precision function esynapse0x9903188(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9903188=eneuron0x9903308(x)*(-0.142507d0)
      end

      double precision function esynapse0x9912f18(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9912f18=eneuron0x99118a8(x)*0.816146d0
      end

      double precision function esynapse0x9912f40(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9912f40=eneuron0x99119a8(x)*0.178152d0
      end

      double precision function esynapse0x9912f68(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9912f68=eneuron0x9911b38(x)*(-0.332306d0)
      end

      double precision function esynapse0x9912f90(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9912f90=eneuron0x9911d10(x)*(-0.425368d0)
      end

      double precision function esynapse0x9912fb8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9912fb8=eneuron0x9911ee8(x)*0.260873d0
      end

      double precision function esynapse0x9912fe0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9912fe0=eneuron0x99120c0(x)*(-0.23777d0)
      end

      double precision function esynapse0x9913008(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9913008=eneuron0x99122b8(x)*0.0380382d0
      end

      double precision function esynapse0x9913030(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9913030=eneuron0x99124b0(x)*(-0.289298d0)
      end

      double precision function esynapse0x9913058(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9913058=eneuron0x99126a8(x)*0.419208d0
      end

      double precision function esynapse0x9913080(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9913080=eneuron0x99128a0(x)*(-0.0641742d0)
      end

      double precision function esynapse0x99130a8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x99130a8=eneuron0x9912a78(x)*(-1.53453d0)
      end

      double precision function esynapse0x99132a8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x99132a8=eneuron0x9903308(x)*0.118948d0
      end

      double precision function esynapse0x99132d0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x99132d0=eneuron0x99118a8(x)*1.18956d0
      end

      double precision function esynapse0x99132f8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x99132f8=eneuron0x99119a8(x)*(-0.0409325d0)
      end

      double precision function esynapse0x9913320(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9913320=eneuron0x9911b38(x)*(-0.429456d0)
      end

      double precision function esynapse0x9913348(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9913348=eneuron0x9911d10(x)*0.00912285d0
      end

      double precision function esynapse0x99133f8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x99133f8=eneuron0x9911ee8(x)*(-0.214434d0)
      end

      double precision function esynapse0x9913420(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9913420=eneuron0x99120c0(x)*(-0.534453d0)
      end

      double precision function esynapse0x9913448(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9913448=eneuron0x99122b8(x)*(-0.515743d0)
      end

      double precision function esynapse0x9913470(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9913470=eneuron0x99124b0(x)*(-0.0292264d0)
      end

      double precision function esynapse0x9913498(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9913498=eneuron0x99126a8(x)*0.358938d0
      end

      double precision function esynapse0x99134c0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x99134c0=eneuron0x99128a0(x)*(-1.06336d0)
      end

      double precision function esynapse0x99134e8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x99134e8=eneuron0x9912a78(x)*(-1.18585d0)
      end

      double precision function esynapse0x99136a0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x99136a0=eneuron0x9903308(x)*(-0.275394d0)
      end

      double precision function esynapse0x99136c8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x99136c8=eneuron0x99118a8(x)*1.1115d0
      end

      double precision function esynapse0x99136f0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x99136f0=eneuron0x99119a8(x)*(-0.138849d0)
      end

      double precision function esynapse0x9913718(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9913718=eneuron0x9911b38(x)*0.542724d0
      end

      double precision function esynapse0x9913740(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9913740=eneuron0x9911d10(x)*(-0.143361d0)
      end

      double precision function esynapse0x9913768(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9913768=eneuron0x9911ee8(x)*(-0.0235586d0)
      end

      double precision function esynapse0x9913790(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9913790=eneuron0x99120c0(x)*0.0808385d0
      end

      double precision function esynapse0x99137b8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x99137b8=eneuron0x99122b8(x)*0.0400261d0
      end

      double precision function esynapse0x99137e0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x99137e0=eneuron0x99124b0(x)*(-0.00178169d0)
      end

      double precision function esynapse0x9913370(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9913370=eneuron0x99126a8(x)*(-0.131219d0)
      end

      double precision function esynapse0x9913398(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9913398=eneuron0x99128a0(x)*(-1.03172d0)
      end

      double precision function esynapse0x99133c0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x99133c0=eneuron0x9912a78(x)*(-1.30888d0)
      end

      double precision function esynapse0x9913ae8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9913ae8=eneuron0x9903308(x)*(-0.291488d0)
      end

      double precision function esynapse0x9913b10(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9913b10=eneuron0x99118a8(x)*0.965291d0
      end

      double precision function esynapse0x9913b38(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9913b38=eneuron0x99119a8(x)*0.175321d0
      end

      double precision function esynapse0x9913b60(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9913b60=eneuron0x9911b38(x)*0.072091d0
      end

      double precision function esynapse0x9913b88(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9913b88=eneuron0x9911d10(x)*0.0377011d0
      end

      double precision function esynapse0x9913bb0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9913bb0=eneuron0x9911ee8(x)*(-0.137764d0)
      end

      double precision function esynapse0x9913bd8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9913bd8=eneuron0x99120c0(x)*0.0993152d0
      end

      double precision function esynapse0x9913c00(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9913c00=eneuron0x99122b8(x)*(-0.226042d0)
      end

      double precision function esynapse0x9913c28(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9913c28=eneuron0x99124b0(x)*(-0.251275d0)
      end

      double precision function esynapse0x9913c50(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9913c50=eneuron0x99126a8(x)*0.341805d0
      end

      double precision function esynapse0x9913c78(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9913c78=eneuron0x99128a0(x)*(-1.28528d0)
      end

      double precision function esynapse0x9913ca0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9913ca0=eneuron0x9912a78(x)*(-0.000632487d0)
      end

      double precision function esynapse0x9913f28(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9913f28=eneuron0x9903308(x)*(-0.0786329d0)
      end

      double precision function esynapse0x9913f50(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9913f50=eneuron0x99118a8(x)*0.0417116d0
      end

      double precision function esynapse0x9913f78(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9913f78=eneuron0x99119a8(x)*(-0.189d0)
      end

      double precision function esynapse0x9913fa0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9913fa0=eneuron0x9911b38(x)*0.373821d0
      end

      double precision function esynapse0x9913fc8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9913fc8=eneuron0x9911d10(x)*0.173425d0
      end

      double precision function esynapse0x9913ff0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9913ff0=eneuron0x9911ee8(x)*0.0174389d0
      end

      double precision function esynapse0x9914018(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9914018=eneuron0x99120c0(x)*(-0.355365d0)
      end

      double precision function esynapse0x9914040(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9914040=eneuron0x99122b8(x)*0.0859413d0
      end

      double precision function esynapse0x9914068(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9914068=eneuron0x99124b0(x)*(-0.136445d0)
      end

      double precision function esynapse0x9914090(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9914090=eneuron0x99126a8(x)*0.345708d0
      end

      double precision function esynapse0x99140b8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x99140b8=eneuron0x99128a0(x)*0.544788d0
      end

      double precision function esynapse0x99140e0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x99140e0=eneuron0x9912a78(x)*0.528477d0
      end

      double precision function esynapse0x9914298(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9914298=eneuron0x9903308(x)*(-0.463249d0)
      end

      double precision function esynapse0x99142c0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x99142c0=eneuron0x99118a8(x)*1.29271d0
      end

      double precision function esynapse0x99142e8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x99142e8=eneuron0x99119a8(x)*0.297151d0
      end

      double precision function esynapse0x9914310(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9914310=eneuron0x9911b38(x)*0.00364563d0
      end

      double precision function esynapse0x9914338(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9914338=eneuron0x9911d10(x)*(-0.390268d0)
      end

      double precision function esynapse0x9835970(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9835970=eneuron0x9911ee8(x)*0.0243479d0
      end

      double precision function esynapse0x98ddac8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x98ddac8=eneuron0x99120c0(x)*0.0379929d0
      end

      double precision function esynapse0x99030d0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x99030d0=eneuron0x99122b8(x)*0.0576228d0
      end

      double precision function esynapse0x99030f8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x99030f8=eneuron0x99124b0(x)*(-0.146554d0)
      end

      double precision function esynapse0x98359e8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x98359e8=eneuron0x99126a8(x)*0.157743d0
      end

      double precision function esynapse0x9835a10(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9835a10=eneuron0x99128a0(x)*(-0.0287887d0)
      end

      double precision function esynapse0x9835b48(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9835b48=eneuron0x9912a78(x)*(-1.52603d0)
      end

      double precision function esynapse0x9912cf8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9912cf8=eneuron0x9912d88(x)*(-1.85166d0)
      end

      double precision function esynapse0x9912d20(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9912d20=eneuron0x99130d0(x)*(-1.69095d0)
      end

      double precision function esynapse0x9912d48(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9912d48=eneuron0x9913510(x)*(-1.44296d0)
      end

      double precision function esynapse0x9835be0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9835be0=eneuron0x9913910(x)*3.49481d0
      end

      double precision function esynapse0x9914568(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9914568=eneuron0x9913cc8(x)*(-0.0215353d0)
      end

      double precision function esynapse0x9914590(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      esynapse0x9914590=eneuron0x9914108(x)*3.75209d0
      end

