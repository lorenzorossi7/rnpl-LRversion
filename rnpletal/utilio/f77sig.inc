c
c     Any changes to common block must be reflected in f77sig.com ...
c
      integer       sig_interrupt
      common      / com_interrupt /
     *              sig_interrupt

      external      set_interrupt

      logical       query_interrupt
      external      query_interrupt

      external      reset_interrupt
      external      unset_interrupt
