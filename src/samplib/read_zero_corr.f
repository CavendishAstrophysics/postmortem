


*+READ_ZERO_CORR

       subroutine read_zero_corr (lun, data, s)
C
C  Reads the zero correction data block from the sample file.
C
C  Given:
C      LUN           integer       sample file unit number
C
C  Returned:
C      DATA          real(*)       zero correction data
C      S             integer       status value
C
C  Routine to read the zero correction data recorded with the current
C  sample file.  These corrections are derived during the observation
C  and are dumped as a record at the end of the sample file.  They may
C  be used to adjust the values on the 'zero-correction file' for use
C  during subsequent observations.
C
C  Routine appropriate for CLFST sample files only.
C
*-
       integer    lun, s
       real       data(*)
c
       include '/mrao/post/include/samplib_errors.inc'
       include '/mrao/post/include/control_tables.inc'
       include '/mrao/post/include/control_tab_v0.inc'
c
       integer    sf_type, block, words
c
       if (s.ne.0) return
c
       call read_ct( lun, s )
       call enq_sftype( lun, sf_type, s )
c
       if (ct_vers.eq.0 .and. sf_type.eq.1 .and. istop.eq.1) then
         words = lrecb
         block = (nsamp+1)*(lrecb/1024)+ictab+1
         call io_rdfile( lun, block, data, words, s )
       else
         s = NO_ZCORR
       endif
c
       if (s.ne.0) then
         call smp_wrerr(s, 'in routine READ_ZERO_CORR')
       endif
c
       end
