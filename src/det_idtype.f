      integer function idet_idtype()

c_begin_doc

c  Documentation for integer function get_idtype()

c  Purpose:
c  --------
c     Returns unused idtype...simply counts upwards from 1

c_end_doc


c  include files:
c  --------------
c_end_inc

c  input/output variables:
c  -----------------------

c  local variables:
c  ----------------
      integer old_idtype/0/
c_end_var


c  executable code:
c  ----------------

      old_idtype=old_idtype+1
c      write(22,*)old_idtype
      idet_idtype=old_idtype

c      return
      end
